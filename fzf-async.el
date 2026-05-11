;;; fzf-async.el --- Async fuzzy completion via `fzf-native' -*- lexical-binding: t; -*-

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Package-Requires: ((emacs "29.1") (fzf-native "0.3") (cl-lib "0.5"))
;; Keywords: matching, completion, fzf, fuzzy, fussy
;; Homepage: https://github.com/jojojames/fzf-async

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides async shell command completion using fzf-native for scoring.
;; The native layer handles process I/O on a background thread, ANSI
;; stripping, and parallel fzf scoring.  The Elisp layer provides
;; while-no-input responsiveness, a candidate cap, and a live stats overlay.
;;
;; Quick start:
;;   (fzf-async-setup)   ; register completion style + category override
;;   (fzf-async-find-files)

(require 'cl-lib)
(require 'fzf-native)

(defgroup fzf-async nil
  "Async fuzzy completion via fzf-native."
  :group 'completion
  :link '(url-link :tag "GitHub" "https://github.com/jojojames/fzf-async"))

(defvar embark-keymap-alist)
(defvar fzf-native-case-mode)
(defvar fzf-native-async-highlight)
(defvar fzf-native-max-line-length)
(defvar fzf-native-async-cache-size)
(defvar marginalia-annotate-file)
(defvar marginalia-annotator-registry)
(defvar marginalia-command-categories)
(declare-function bookmark-all-names "bookmark")
(declare-function bookmark-maybe-load-default-file "bookmark")
(declare-function icomplete-exhibit "icomplete")
(defvar ivy-text)
(defvar ivy--index)
(defvar ivy-count-format)
(defvar ivy-completing-read-dynamic-collection)
(declare-function ivy--set-candidates "ivy")
(declare-function ivy--exhibit "ivy")
(defvar ivy-pre-prompt-function)
(defvar helm-alive-p)
(defvar helm-pattern)
(declare-function helm "helm-core")
(declare-function helm-make-source "helm-source")
(declare-function helm-force-update "helm-core")

;;; Debug logging

(defvar fzf-async-debug nil
  "When non-nil at load time, compile debug logging into fzf-async.
Set before loading or re-evaluating fzf-async.el; toggling at runtime
has no effect (the check is macro-expanded at load time, like #ifdef).")

(defmacro fzf-async--log (fmt &rest args)
  "Emit a debug message if `fzf-async-debug' is non-nil at load time.
Expands to nothing when disabled — zero runtime cost."
  (when (bound-and-true-p fzf-async-debug)
    `(message ,fmt ,@args)))

;;; Customization

(defcustom fzf-async-max-candidates 10000
  "Max candidates returned to Elisp from `fzf-async-completing-read'.
The full filtered/total counts are still tracked and shown in the prompt.
Set to nil or 0 to disable the cap (may be slow for very large result sets)."
  :type '(choice (const  :tag "No cap" nil)
                 (integer :tag "Max candidates"))
  :group 'fzf-async)

(defcustom fzf-async-refresh-delay 0.05
  "Seconds between polls for new C-side candidate generations.
The background reader thread increments a generation counter as lines
arrive; this timer checks that counter and schedules a display refresh
when new data is available.  Lower values feel more responsive but burn
more CPU on the polling loop.  Analogous to `consult-async-refresh-delay'."
  :type 'float
  :group 'fzf-async)

(defcustom fzf-async-input-debounce 0.1
  "Seconds of idle time to wait before retrying after interrupted scoring.
When the user types fast, `while-no-input' aborts the scoring call.  This
idle timer fires once typing pauses and re-triggers the display so results
self-heal."
  :type 'float
  :group 'fzf-async)

(defcustom fzf-async-input-throttle 0.2
  "Minimum seconds between display refreshes driven by new incoming data.
Even when new candidate generations arrive continuously (e.g. a fast `find'
streaming thousands of files), the completion UI is only re-exhibited once
per this interval.  The debounce retry path is unaffected — after the user
pauses typing the display always self-heals regardless of this value."
  :type 'float
  :group 'fzf-async)

(defcustom fzf-async-highlight 200
  "Controls C-side match highlighting of completion candidates.
nil or a negative integer — no highlighting.
t                        — highlight every returned candidate.
a positive integer N     — highlight only the top N candidates.
The C layer applies `completions-common-part' face to each contiguous
run of matched bytes via fzf_get_positions."
  :type '(choice (const   :tag "Disabled" nil)
                 (const   :tag "All candidates" t)
                 (integer :tag "Top N candidates"))
  :group 'fzf-async)

(defcustom fzf-async-max-line-length 256
  "Maximum character length of a candidate line.
nil           — no limit.
positive N    — exclude lines longer than N characters.
negative -N   — include but truncate lines to N characters.

Applies at read time: lines from the subprocess are filtered or
truncated before entering the candidate pool, so scoring never
sees the excess characters.

For the rg / ag / ugrep grep-style commands the cap is also pushed
upstream into the search tool itself (via `--max-columns' / `--width'),
so long lines never traverse the pipe in the first place — saves
IPC bandwidth and avoids work in the search tool on minified files,
base64 blobs, and other pathological inputs."
  :type '(choice (const   :tag "No limit" nil)
                 (integer :tag "N (positive = exclude, negative = truncate)"))
  :group 'fzf-async)

(defcustom fzf-async-case-mode 'smart
  "Case-sensitivity mode propagated to `fzf-native-case-mode'.
Mirrors fzf-native's enum:
smart    Case-insensitive when the query is all lowercase; case-sensitive
         once it contains any uppercase character (fzf's default).
ignore   Always case-insensitive.
respect  Always case-sensitive.

Applied via `setq-local' inside each fzf-async completing-read function
so the C scorer sees the chosen mode for the duration of the session."
  :type '(choice (const :tag "Smart case (default)" smart)
                 (const :tag "Ignore case"          ignore)
                 (const :tag "Respect case"         respect))
  :group 'fzf-async)

(defcustom fzf-async-cache-size 40
  "Maximum number of scored snapshots cached per async session.
Each entry stores the top-K results and the full matched-candidate
index for one query, enabling exact-fresh hits (skip scoring) and
prefix-refinement hits (rescore only previously-matched candidates
plus deltas) without re-scanning the full pool.

A larger value keeps a longer typing trail in cache, which improves
backspace coverage — backspacing past N keystrokes will still hit
the LRU as long as those intermediate queries weren't evicted by
unrelated lookups.

Read at session start; changing it does not affect running sessions."
  :type 'integer
  :group 'fzf-async)

(defvar fzf-async--multi-mode nil
  "Dispatch flag for `fzf-async-completing-read' / `fzf-sync-completing-read'.
- `:extract'         — throw `fzf-async-extracted' with the call's keyword args.
- (`:inject' . CAND) — return CAND directly without prompting.
Bound by `fzf-async-multi-from-commands' to derive multi-source sources from
existing single-source commands without modifying their definitions.")

(defvar fzf-async-directory nil
  "Per-call directory override for fzf-async commands.
When non-nil, supersedes `fzf-async-project-backend' and `default-directory'.
Intended for `let'-binding when extending built-in commands:

  (let ((fzf-async-directory default-directory))
    (fzf-async-rg))

Priority: `fzf-async-directory' > project backend > `default-directory'.")

(defcustom fzf-async-project-backend 'project
  "How to resolve the root directory for fzf-async commands.
project    Use `project.el' to find the project root (default, matches consult).
projectile Use `projectile-project-root'.
nil        Use `default-directory' (no project detection).
function   Call the function with no arguments;
 it should return a directory string."
  :type '(choice (const :tag "project.el" project)
                 (const :tag "Projectile" projectile)
                 (const :tag "None (default-directory)" nil)
                 (function :tag "Custom function"))
  :group 'fzf-async)

(defcustom fzf-async-spotlight-audio-directories
  '("~/Music" "~/Downloads" "~/Desktop")
  "Directories searched by `fzf-async-spotlight-audio'.
Each directory is passed to `mdfind -onlyin'; results are concatenated.
Set to nil to search the whole index."
  :type '(repeat directory)
  :group 'fzf-async)

;;; Completion style

(defun fzf-async-try-completion (string _table _pred _point)
  "Try-completion for the fzf-async completion style.
Always accepts STRING as-is; scoring is done in C."
  (cons string (length string)))

(defun fzf-async-all-completions (string table pred _point)
  "All-completions for the fzf-async completion style.
Passes STRING through to the collection TABLE without transformation.
Highlighting is applied by the C layer (see `fzf-async-highlight')."
  (funcall table string pred t))

;;; Frontend abstraction

(defun fzf-async--frontend-index ()
  "Return the active completion UI's selection index (0-based), or nil.
Returns nil for frontends that do not expose a selection index (e.g. icomplete)."
  (cond
   ((bound-and-true-p vertico-mode) (max 0 vertico--index))
   ((bound-and-true-p ivy-mode) (and (boundp 'ivy--index) (max 0 ivy--index)))
   (t nil)))

(defun fzf-async--frontend-exhibit ()
  "Trigger a display refresh in the active completion UI.
Handles vertico and icomplete.  Ivy's push path is handled separately
via the `ivy-push' closure in `fzf-async-completing-read': calling
`ivy--exhibit' alone re-renders stale candidates without re-scoring."
  (when-let* ((win (active-minibuffer-window)))
    (with-selected-window win
      (cond
       ((bound-and-true-p vertico-mode)
        (setq vertico--input t)
        (vertico--exhibit))
       ((bound-and-true-p icomplete-mode)
        (icomplete-exhibit))))))

(defun fzf-async--commas (n)
  "Format integer N with comma thousand-separators (e.g., 1234567 → \"1,234,567\")."
  (let ((s (number-to-string n))
        (out ""))
    (while (> (length s) 3)
      (setq out (concat "," (substring s -3) out)
            s   (substring s 0 -3)))
    (concat s out)))

;;; Completing-read

(cl-defun fzf-async--helm-completing-read (&key prompt command directory
                                                skip-executable-check)
  "Helm path for `fzf-async-completing-read'.
Starts an fzf-native async session and opens a helm buffer driven by a
`helm-source-sync' with `:match-dynamic t' so helm never re-filters the
already-scored candidates.  A timer polls the C-side generation counter and
calls `helm-force-update' when new results arrive.
Returns the selected candidate string, or nil on cancel."
  (unless skip-executable-check
    (when-let* ((prog (and command (car (split-string command nil t)))))
      (unless (executable-find prog)
        (user-error "%s not found in exec-path" prog))))
  (require 'helm)
  (require 'helm-source)
  (let* ((prompt  (or prompt
                      (when command
                        (concat (car (split-string command nil t)) ": "))))
         (dir     (expand-file-name (or directory default-directory)))
         (handle  (fzf-native-async-start command dir))
         (limit   (and fzf-async-max-candidates
                       (> fzf-async-max-candidates 0)
                       fzf-async-max-candidates))
         (last-gen -1)
         (stopped  nil)
         (result   nil)
         (cleanup  (lambda ()
                     (unless stopped
                       (setq stopped t)
                       (fzf-native-async-stop handle))))
         timer)
    (setq timer
          (run-with-timer
           0 fzf-async-refresh-delay
           (lambda ()
             (when helm-alive-p
               (let ((gen (fzf-native-async-generation handle)))
                 (when (and gen (> gen last-gen))
                   (setq last-gen gen)
                   (helm-force-update)))))))
    (unwind-protect
        (let ((default-directory dir))
          (helm
           :sources
           (helm-make-source
            (or prompt "fzf-async") 'helm-source-sync
            :header-name
            (lambda (name)
              (format "%s [%s]" name (abbreviate-file-name dir)))
            :candidates
            (lambda ()
              ;; case-mode and other defcustoms are bridged onto the
              ;; canonical fzf-native names by :around advice on
              ;; `fzf-native-async-candidates' (see EOF).
              (fzf-native-async-candidates handle helm-pattern limit))
            :match-dynamic t
            :nohighlight t
            :candidate-number-limit (or limit 10000)
            :cleanup cleanup
            :action (lambda (cand) (setq result cand)))
           :buffer "*helm fzf-async*"))
      (cancel-timer timer)
      (funcall cleanup))
    result))

;;;###autoload
(cl-defun fzf-async-completing-read (&key
                                     prompt
                                     command
                                     (directory (fzf-async--default-dir))
                                     group
                                     skip-executable-check)
  "Run shell COMMAND and completing-read its output.

:PROMPT                 Minibuffer prompt.  Derived from the first token of
                        COMMAND (e.g. \"find: \" for \"find .\") when omitted.
:COMMAND                Shell command whose stdout lines become candidates.
:DIRECTORY              Working directory for COMMAND.  Defaults to
                        `fzf-async--default-dir' (respects
                        `fzf-async-project-backend').
:SKIP-EXECUTABLE-CHECK  When non-nil, skip the `executable-find' guard on
                        the first token of COMMAND.

The prompt overlay shows: DIR IDX/[FILTERED](TOTAL)
  DIR      — abbreviated working directory
  IDX      — current selection index (omitted for frontends without one)
  FILTERED — candidates matching the current query
  TOTAL    — total candidates collected so far"
  (unless skip-executable-check
    (when-let* ((prog (and command (car (split-string command nil t)))))
      (unless (executable-find prog)
        (user-error "%s not found in exec-path" prog))))
  (let ((prompt (or prompt
                    (when command
                      (concat (car (split-string command nil t)) ": ")))))
    (cond
     ((eq fzf-async--multi-mode :extract)
      (throw 'fzf-async-extracted
             (list :prompt prompt :command command
                   :directory directory :group group)))
     ((eq (car-safe fzf-async--multi-mode) :inject)
      (cl-return-from fzf-async-completing-read
        (cdr fzf-async--multi-mode))))
    (when (bound-and-true-p helm-mode)
      (cl-return-from fzf-async-completing-read
        (fzf-async--helm-completing-read
         :prompt prompt :command command :directory directory
         :skip-executable-check skip-executable-check)))
    (let* ((handle (fzf-native-async-start command (expand-file-name directory)))
           (dir (abbreviate-file-name directory))
           (last-gen -1)
           (last-result nil)
           (last-query nil)
           (stats-overlay nil)
           (last-filtered 0)
           (last-total 0)
           (limit (and fzf-async-max-candidates
                       (> fzf-async-max-candidates 0)
                       fzf-async-max-candidates))
           (last-exhibit-scheduled 0.0)
           (refresh-overlay
            (lambda ()
              (when (and stats-overlay (active-minibuffer-window))
                (with-selected-window (active-minibuffer-window)
                  (let ((idx (fzf-async--frontend-index)))
                    (fzf-async--log "DEBUG: %s%s %s[%d](%d) "
                                    prompt dir
                                    (if idx (format "%d/" (1+ idx)) "")
                                    last-filtered last-total)
                    (overlay-put stats-overlay 'display
                                 (if idx
                                     (format "%s%s %d/[%s](%s) "
                                             prompt dir (1+ idx)
                                             (fzf-async--commas last-filtered)
                                             (fzf-async--commas last-total))
                                   (format "%s%s [%s](%s) "
                                           prompt dir
                                           (fzf-async--commas last-filtered)
                                           (fzf-async--commas last-total)))))))))
           ;; Ivy push path: score the current query and push into ivy--all-candidates
           ;; directly.  Used instead of fzf-async--frontend-exhibit for ivy because
           ;; ivy does not re-call the collection lambda on timer ticks.
           (ivy-push
            (lambda ()
              (when (and handle (active-minibuffer-window))
                (when-let* ((query (and (boundp 'ivy-text) ivy-text)))
                  (let ((cands (while-no-input
                                 (fzf-native-async-candidates handle query limit))))
                    (when (and cands (not (eq cands t)))
                      (when-let* ((stats (fzf-native-async-stats handle)))
                        (setq last-filtered (car stats)
                              last-total    (cdr stats)))
                      (setq last-query query
                            last-result cands)
                      (ivy--set-candidates cands)
                      (ivy--exhibit)))))))
           retry-timer
           timer)
      (setq timer
            (run-with-timer
             0 fzf-async-refresh-delay
             (lambda ()
               (when handle
                 (let ((gen (fzf-native-async-generation handle)))
                   (when (and gen (not (= gen last-gen)) (not (input-pending-p)))
                     (when (>= (- (float-time) last-exhibit-scheduled)
                               fzf-async-input-throttle)
                       (setq last-gen gen)
                       (setq last-exhibit-scheduled (float-time))
                       (run-with-idle-timer
                        0 nil
                        (if (bound-and-true-p ivy-mode)
                            ivy-push
                          #'fzf-async--frontend-exhibit)))))))))
      (add-hook 'post-command-hook refresh-overlay)
      (sit-for fzf-async-refresh-delay)
      (unwind-protect
          (minibuffer-with-setup-hook
              (lambda ()
                ;; case-mode and other defcustoms are bridged onto the
                ;; canonical fzf-native names by :around advice on
                ;; `fzf-native-async-candidates' (see EOF), so no
                ;; setq-local needed here for the async path.
                (when (boundp 'vertico-count-format)
                  (setq-local vertico-count-format nil))
                (when (boundp 'icomplete-matches-format)
                  (setq-local icomplete-matches-format nil)))
            (let ((ivy-completing-read-dynamic-collection t)
                  (ivy-count-format
                   (when (bound-and-true-p ivy-mode) ""))
                  (ivy-pre-prompt-function
                   (when (bound-and-true-p ivy-mode)
                     (lambda ()
                       (let ((idx (fzf-async--frontend-index)))
                         (if idx
                             (format "%s %d/[%s](%s) "
                                     dir (1+ idx)
                                     (fzf-async--commas last-filtered)
                                     (fzf-async--commas last-total))
                           (format "%s [%s](%s) "
                                   dir
                                   (fzf-async--commas last-filtered)
                                   (fzf-async--commas last-total))))))))
              (completing-read
               prompt
               (lambda (str _pred action)
                 (pcase action
                   ('metadata `(metadata (category . fzf-async)
                                         (display-sort-function . identity)
                                         (cycle-sort-function . identity)
                                         ,@(when group `((group-function . ,group)))))
                   ;; Treat the whole input as one field; prevents space-splitting.
                   (`(boundaries . ,_) (cons 0 0))
                   ('t (let* (;; Str is sometimes empty when there's a valid query.
                              ;; Prefer str when non-empty to avoid calculations
                              ;; in the minibuffer but fall back if str is empty.
                              (query (if (not (string-empty-p str))
                                         str
                                       (when-let* ((win (active-minibuffer-window)))
                                         (with-current-buffer (window-buffer win)
                                           (minibuffer-contents-no-properties))))))
                         (if (null query)
                             last-result
                           (let ((r (while-no-input
                                      (fzf-native-async-candidates handle query limit))))
                             (if (eq r t)
                                 ;; Scoring was interrupted by pending input.
                                 ;; Debounce a retry so the display self-heals once
                                 ;; the user pauses typing.
                                 (progn
                                   (when retry-timer (cancel-timer retry-timer))
                                   (setq retry-timer
                                         (run-with-idle-timer
                                          fzf-async-input-debounce nil
                                          (lambda ()
                                            (setq retry-timer nil)
                                            (if (bound-and-true-p ivy-mode)
                                                (funcall ivy-push)
                                              (fzf-async--frontend-exhibit))))))
                               (when-let* ((stats (fzf-native-async-stats handle)))
                                 (setq last-filtered (car stats)
                                       last-total    (cdr stats)))
                               (unless (bound-and-true-p ivy-mode)
                                 (when-let* ((win (active-minibuffer-window)))
                                   (with-selected-window win
                                     (unless stats-overlay
                                       (setq stats-overlay
                                             (make-overlay (point-min) (minibuffer-prompt-end))))
                                     (funcall refresh-overlay))))
                               (setq last-query query
                                     last-result r))
                             (when (equal query last-query) last-result)))))
                   (_ t))))))
        (cancel-timer timer)
        (when retry-timer (cancel-timer retry-timer))
        (remove-hook 'post-command-hook refresh-overlay)
        (when stats-overlay (delete-overlay stats-overlay))
        (when handle (fzf-native-async-stop handle))))))

(cl-defun fzf-sync-completing-read (&key
                                    candidates
                                    (prompt "fzf > ")
                                    (category 'fzf-async)
                                    annotate
                                    affix
                                    group)
  "Run completing-read over CANDIDATES using fzf-native for scoring.

:CANDIDATES List of strings to score with `fzf-native-score-all'.
:PROMPT     Minibuffer prompt string.  Defaults to \"fzf > \".
:CATEGORY   Completion category symbol.  Defaults to `fzf-async'.
            Use `fzf-async-file' for file-path candidates so marginalia
            can annotate them with file metadata.
:ANNOTATE   Optional function (CANDIDATE) -> string appended after each
            candidate.  Exposed as `annotation-function' in completion
            metadata.  Annotations start immediately after the candidate
            string, so column alignment depends on candidate widths.
:AFFIX      Optional function (CANDIDATES) -> list of (CANDIDATE PREFIX
            SUFFIX).  Exposed as `affixation-function'.  Vertico
            right-pads each candidate to a consistent width before
            appending SUFFIX, giving true column alignment.  Prefer this
            over :ANNOTATE when alignment matters.
:GROUP      Optional function (CANDIDATE TRANSFORM) -> string.  When
            TRANSFORM is nil return the group name; when non-nil return
            the display string for CANDIDATE within its group.  Frontends
            like vertico render group headers between sections."
  (cond
   ((eq fzf-async--multi-mode :extract)
    (throw 'fzf-async-extracted
           ;; Translate :candidates → :items so multi consumes one key.
           (list :items candidates :prompt prompt :category category
                 :annotate annotate :affix affix :group group)))
   ((eq (car-safe fzf-async--multi-mode) :inject)
    (cl-return-from fzf-sync-completing-read
      (cdr fzf-async--multi-mode))))
  (completing-read
   prompt
   (lambda (str _pred action)
     (pcase action
       ('metadata
        `(metadata
          (category . ,category)
          (display-sort-function . identity)
          (cycle-sort-function . identity)
          ,@(when annotate `((annotation-function  . ,annotate)))
          ,@(when affix    `((affixation-function  . ,affix)))
          ,@(when group    `((group-function       . ,group)))))
       (`(boundaries . ,_) (cons 0 0))
       ('lambda t)
       ('t (let ((query (if (not (string-empty-p str))
                            str
                          (when-let* ((win (active-minibuffer-window)))
                            (with-current-buffer (window-buffer win)
                              (minibuffer-contents-no-properties))))))
             (if (or (null query) (string-empty-p query))
                 candidates
               (fzf-async--bridge-defcustoms
                #'fzf-native-score-all candidates query))))))
   nil t nil nil nil))

;;; Multi-source

(defvar fzf-async-find-any-commands
  '(fzf-async-buffer
    fzf-async-recent-file
    fzf-async-find-hungry
    fzf-async-swiper-hungry)
  "Commands shown by `fzf-async-find-any'.
Each is a command symbol whose body calls `fzf-async-completing-read' or
`fzf-sync-completing-read'.  The source is derived automatically; see
`fzf-async-multi-from-commands'.")

(defun fzf-async--multi-tag (cand idx hash)
  "Tag CAND with source IDX (text-prop + HASH lookup table); return CAND."
  (when (> (length cand) 0)
    (put-text-property 0 1 'fzf-async-src-idx idx cand))
  (puthash cand idx hash)
  cand)

(defun fzf-async--multi-source-of (cand sources-v hash)
  "Return the source plist responsible for CAND, or nil."
  (and (stringp cand) (> (length cand) 0)
       (let ((idx (or (get-text-property 0 'fzf-async-src-idx cand)
                      (gethash cand hash))))
         (and idx (aref sources-v idx)))))

(defun fzf-async--multi-rank (results query async-p)
  "Top fzf score for RESULTS under QUERY.
For async sources (ASYNC-P non-nil) the C async path does not attach
`completion-score' text properties, so we re-score the top candidate
once via `fzf-native-score'.  For sync sources the score is read off
the property set by `fzf-native-score-all'.  Returns 0 on empty input."
  (cond
   ((or (null results) (string-empty-p query)) 0)
   (async-p
    (or (car (fzf-async--bridge-defcustoms
              #'fzf-native-score (car results) query))
        0))
   (t (or (get-text-property 0 'completion-score (car results)) 0))))

;;;###autoload
(cl-defun fzf-async-multi (sources &key (prompt "fzf-multi: "))
  "Run completing-read across multiple SOURCES, fzf-async style.

SOURCES is a list of plists.  Each source contributes a labeled group of
candidates; group order is recomputed on every keystroke from each
group's top fzf score, so the strongest-matching group floats to the
top.  Within a group, candidates stay in fzf order.  An empty query
falls back to declared source order.

Per-source plist keys:
  :name      Group header (required).
  :items     Sync items: list of strings, or zero-arg function returning one.
             Mutually exclusive with :command.
  :command   Async source: shell command string.
  :directory Working directory for :command (default `default-directory').
  :annotate  Optional (cand) -> string annotation function.
  :action    Optional (cand) -> any.  Called with the selection.  When
             omitted, the raw selection string is returned."
  (when (bound-and-true-p helm-mode)
    (user-error "fzf-async-multi does not yet support helm-mode"))
  (let* ((n            (length sources))
         (sources-v    (vconcat sources))
         (handles      (make-vector n nil))
         (sync-items   (make-vector n nil))
         (last-results (make-vector n nil))
         (rank         (make-vector n 0))
         (totals       (make-vector n 0))
         (filtered     (make-vector n 0))
         (last-gen     (make-vector n -1))
         (limit        (and fzf-async-max-candidates
                            (> fzf-async-max-candidates 0)
                            fzf-async-max-candidates))
         (cand->src    (make-hash-table :test 'equal :size 1024))
         (last-exhibit 0.0)
         (stats-overlay nil)
         ;; Captured by `minibuffer-exit-hook' from the propertized text
         ;; in the minibuffer before `completing-read' returns and strips
         ;; properties.  Reliable per-instance source dispatch even when
         ;; the same string appears in multiple sources.
         (selected-idx nil)
         (refresh-overlay
          (lambda ()
            (when (and stats-overlay (active-minibuffer-window))
              (with-selected-window (active-minibuffer-window)
                (let* ((idx (fzf-async--frontend-index))
                       (f   (fzf-async--commas
                             (cl-loop for x across filtered sum x)))
                       (tot (fzf-async--commas
                             (cl-loop for x across totals sum x)))
                       (text (if idx
                                 (format "%s%d/[%s](%s) "
                                         prompt (1+ idx) f tot)
                               (format "%s[%s](%s) "
                                       prompt f tot))))
                  (overlay-put stats-overlay 'display text))))))
         retry-timer timer result)
    (dotimes (i n)
      (let* ((src   (aref sources-v i))
             (cmd   (plist-get src :command))
             (items (plist-get src :items)))
        (cond
         (cmd
          (aset handles i
                (fzf-native-async-start
                 cmd
                 (expand-file-name
                  (or (plist-get src :directory) default-directory)))))
         (items
          (let ((tagged
                 (mapcar (lambda (s)
                           (fzf-async--multi-tag (copy-sequence s) i cand->src))
                         (if (functionp items) (funcall items) items))))
            (aset sync-items i tagged)
            (aset totals i (length tagged))
            (aset filtered i (length tagged)))))))
    (unwind-protect
        (progn
          (setq timer
                (run-with-timer
                 0 fzf-async-refresh-delay
                 (lambda ()
                   (when (active-minibuffer-window)
                     (let (bumped)
                       (dotimes (i n)
                         (when-let* ((h (aref handles i))
                                     (g (fzf-native-async-generation h)))
                           (when (/= g (aref last-gen i))
                             (aset last-gen i g)
                             (setq bumped t))))
                       (when (and bumped (not (input-pending-p))
                                  (>= (- (float-time) last-exhibit)
                                      fzf-async-input-throttle))
                         (setq last-exhibit (float-time))
                         (run-with-idle-timer
                          0 nil #'fzf-async--frontend-exhibit)))))))
          (add-hook 'post-command-hook refresh-overlay)
          (sit-for fzf-async-refresh-delay)
          (setq result
                (minibuffer-with-setup-hook
                    (lambda ()
                      (when (boundp 'vertico-count-format)
                        (setq-local vertico-count-format nil))
                      (when (boundp 'icomplete-matches-format)
                        (setq-local icomplete-matches-format nil))
                      ;; Capture source idx from the propertized minibuffer
                      ;; text before completing-read returns and strips text
                      ;; properties from its return value.  Reliable
                      ;; per-instance dispatch even for cross-source
                      ;; duplicate strings.
                      (add-hook 'minibuffer-exit-hook
                                (lambda ()
                                  (let ((s (buffer-substring
                                            (minibuffer-prompt-end)
                                            (point-max))))
                                    (when (> (length s) 0)
                                      (setq selected-idx
                                            (get-text-property
                                             0 'fzf-async-src-idx s)))))
                                nil 'local))
                  (completing-read
                   prompt
                   (lambda (str _pred action)
                     (pcase action
                       ('metadata
                        `(metadata
                          (category . fzf-async-multi)
                          (display-sort-function . identity)
                          (cycle-sort-function . identity)
                          (group-function
                           . ,(lambda (cand transform)
                                (if transform
                                    cand
                                  (or (plist-get
                                       (fzf-async--multi-source-of
                                        cand sources-v cand->src)
                                       :name)
                                      ""))))
                          (affixation-function
                           . ,(lambda (cands)
                                (let ((maxw (apply #'max 0
                                                   (mapcar #'string-width
                                                           cands))))
                                  (mapcar
                                   (lambda (cand)
                                     (let* ((src (fzf-async--multi-source-of
                                                  cand sources-v cand->src))
                                            (ann (and src (plist-get src :annotate)))
                                            (s   (and ann (funcall ann cand)))
                                            (pad (- (1+ maxw)
                                                    (string-width cand))))
                                       (list cand ""
                                             (if s
                                                 (concat
                                                  (make-string (max 1 pad) ?\s)
                                                  s)
                                               ""))))
                                   cands))))))
                       (`(boundaries . ,_) (cons 0 0))
                       ('lambda t)
                       ('t
                        (let* ((query
                                (if (not (string-empty-p str))
                                    str
                                  (or (when-let* ((win (active-minibuffer-window)))
                                        (with-current-buffer (window-buffer win)
                                          (minibuffer-contents-no-properties)))
                                      "")))
                               (interrupted nil))
                          (dotimes (i n)
                            (let* ((h     (aref handles i))
                                   (items (aref sync-items i))
                                   (out
                                    (cond
                                     (h (while-no-input
                                          (fzf-native-async-candidates
                                           h query limit)))
                                     (items
                                      (if (string-empty-p query)
                                          items
                                        (while-no-input
                                          (fzf-async--bridge-defcustoms
                                           #'fzf-native-score-all
                                           items query)))))))
                              (cond
                               ((eq out t) (setq interrupted t))
                               (t
                                ;; Async returns fresh strings each call;
                                ;; re-tag them so group/action lookup works.
                                ;; out may be nil (zero matches) — still valid.
                                (when h
                                  (dolist (c out)
                                    (fzf-async--multi-tag c i cand->src)))
                                (aset last-results i out)
                                (aset rank i
                                      (fzf-async--multi-rank out query h))
                                (cond
                                 (h (when-let* ((s (fzf-native-async-stats h)))
                                      (aset filtered i (car s))
                                      (aset totals   i (cdr s))))
                                 (t (aset filtered i (length out))))))))
                          (when interrupted
                            (when retry-timer (cancel-timer retry-timer))
                            (setq retry-timer
                                  (run-with-idle-timer
                                   fzf-async-input-debounce nil
                                   (lambda ()
                                     (setq retry-timer nil)
                                     (fzf-async--frontend-exhibit)))))
                          (when-let* ((win (active-minibuffer-window)))
                            (with-selected-window win
                              (unless stats-overlay
                                (setq stats-overlay
                                      (make-overlay (point-min)
                                                    (minibuffer-prompt-end))))
                              (funcall refresh-overlay)))
                          (let* ((order (number-sequence 0 (1- n)))
                                 ;; `sort' is stable since Emacs 25, so equal
                                 ;; ranks preserve declared source order.
                                 (sorted
                                  (if (string-empty-p query)
                                      order
                                    (sort order
                                          (lambda (a b)
                                            (> (aref rank a)
                                               (aref rank b)))))))
                            (apply #'append
                                   (mapcar (lambda (i) (aref last-results i))
                                           sorted)))))
                       (_ t)))
                   nil t))))
      (when timer (cancel-timer timer))
      (when retry-timer (cancel-timer retry-timer))
      (remove-hook 'post-command-hook refresh-overlay)
      (when stats-overlay (delete-overlay stats-overlay))
      (dotimes (i n)
        (when-let* ((h (aref handles i)))
          (fzf-native-async-stop h))))
    (when result
      (let* ((src    (or (and selected-idx (aref sources-v selected-idx))
                         (fzf-async--multi-source-of
                          result sources-v cand->src)))
             (action (and src (plist-get src :action))))
        (if action (funcall action result) result)))))

;;;###autoload
(defun fzf-async-multi-from-commands (commands &rest options)
  "Run `fzf-async-multi' over COMMANDS by extracting each command's call args.
Each command in COMMANDS is funcalled twice per multi session — once in
`:extract' mode (capture keyword args, abort), once in `:inject' mode after
the user picks (so the command's post-action runs).  OPTIONS is passed to
`fzf-async-multi'.  Commands whose body does not reach
`fzf-async-completing-read' or `fzf-sync-completing-read' are skipped.
Commands must be arg-less (no interactive `read-*' prompts in their body)."
  (let ((sources
         (mapcar
          (lambda (cmd)
            (let ((args (condition-case nil
                            (catch 'fzf-async-extracted
                              (let ((fzf-async--multi-mode :extract))
                                (funcall cmd))
                              nil)
                          (error nil))))
              (when args
                (let* ((cat (plist-get args :category))
                       (default-annotate
                        (cond
                         ((memq cat '(fzf-async-buffer buffer))
                          (lambda (c)
                            (when (fboundp 'marginalia-annotate-buffer)
                              (marginalia-annotate-buffer c))))
                         ((memq cat '(fzf-async-file file))
                          (lambda (c)
                            (when (fboundp 'marginalia-annotate-file)
                              (marginalia-annotate-file c)))))))
                  (append
                   (list :name (replace-regexp-in-string
                                "^fzf-async-" "" (symbol-name cmd))
                         :annotate (or (plist-get args :annotate)
                                       default-annotate)
                         :action (lambda (cand)
                                   (let ((fzf-async--multi-mode
                                          (cons :inject cand)))
                                     (funcall cmd))))
                   args)))))
          commands)))
    (apply #'fzf-async-multi (delq nil sources) options)))

;;;###autoload
(defun fzf-async-find-any ()
  "Multi-source fuzzy completion over `fzf-async-find-any-commands'.
Defaults: live buffers, `recentf' files, and a hungry-find over the parent
directories of all file-visiting buffers.  Each command's source is
derived automatically — see `fzf-async-multi-from-commands'."
  (interactive)
  (fzf-async-multi-from-commands
   fzf-async-find-any-commands
   :prompt "any?: "))

;;; Commands

(defun fzf-async--max-columns-flag (tool)
  "Return a max-line-length CLI flag string for grep-style TOOL.

Note: rg's `--max-columns' DROPS the line; ag's and ugrep's
`--width' TRUNCATE display.  Practical effect for our use case
is similar (bounded line length into our pipe), but the
underlying semantics differ slightly.  Either way the
reader-side cap still runs as a backstop."
  (let ((mll fzf-async-max-line-length))
    (if (not (and (integerp mll) (> mll 0)))
        ""                                ; nil / 0 / negative → no flag
      (pcase tool
        ('rg    (format "--max-columns=%d" mll))
        ('ugrep (format "--width=%d" mll))
        ('ag    (format "--width=%d" mll))
        (_      "")))))

;;;###autoload
(defun fzf-async-find ()
  "Find a file under `default-directory' using find."
  (interactive)
  (when-let* ((result (fzf-async-completing-read
                       :command "find .")))
    (find-file result)))

;;;###autoload
(defun fzf-async-fd ()
  "Find a file under `default-directory' using fd."
  (interactive)
  (when-let* ((result (fzf-async-completing-read
                       :command "fd --no-ignore")))
    (find-file result)))

;;;###autoload
(defun fzf-async-rg-files ()
  "Find a file under `default-directory' using rg --files."
  (interactive)
  (when-let* ((result (fzf-async-completing-read
                       :prompt "rg files: "
                       :command "rg --files")))
    (find-file result)))

;;;###autoload
(defun fzf-async-ag-files ()
  "Find a file under `default-directory' using ag."
  (interactive)
  (when-let* ((result (fzf-async-completing-read
                       :prompt "ag files: "
                       :command "ag -g .")))
    (find-file result)))

;;;###autoload
(defun fzf-async-rg ()
  "Search file contents under `default-directory' with rg.
Streams all file contents as FILE:LINE:CONTENT; type to
 fuzzy-filter across them.
Selecting a candidate opens the file at that line."
  (interactive)
  (when-let* ((r (fzf-async-completing-read
                  :command (format "rg --line-number --no-heading --with-filename %s ''"
                                   (fzf-async--max-columns-flag 'rg))
                  :group #'fzf-async--grep-group))
              (match (string-match "\\(.*\\):\\([0-9]+\\):" r))
              (file (match-string 1 r))
              (line (string-to-number (match-string 2 r))))
    (find-file file)
    (goto-char (point-min))
    (forward-line (1- line))))

;;;###autoload
(defun fzf-async-ag ()
  "Search file contents under `default-directory' with ag.
Streams all file contents as FILE:LINE:CONTENT; type to
 fuzzy-filter across them.
Selecting a candidate opens the file at that line."
  (interactive)
  (when-let* ((r (fzf-async-completing-read
                  :command (format "ag --nocolor --nogroup --line-number %s \".\""
                                   (fzf-async--max-columns-flag 'ag))
                  :group #'fzf-async--grep-group))
              (match (string-match "\\(.*\\):\\([0-9]+\\):" r))
              (file (match-string 1 r))
              (line (string-to-number (match-string 2 r))))
    (find-file file)
    (goto-char (point-min))
    (forward-line (1- line))))

;;;###autoload
(defun fzf-async-git-grep ()
  "Search file contents under `default-directory' with git grep.
Streams all file contents as FILE:LINE:CONTENT; type to
 fuzzy-filter across them.
Selecting a candidate opens the file at that line."
  (interactive)
  (unless (locate-dominating-file default-directory ".git")
    (error "Not a Git repo"))
  (when-let* ((r (fzf-async-completing-read
                  :command "git --no-pager grep -n \"\""
                  :group #'fzf-async--grep-group))
              (match (string-match "\\(.*\\):\\([0-9]+\\):" r))
              (file (match-string 1 r))
              (line (string-to-number (match-string 2 r))))
    (find-file file)
    (goto-char (point-min))
    (forward-line (1- line))))

;;;###autoload
(defun fzf-async-grep ()
  "Search file contents under `default-directory' with grep.
Streams all file contents as FILE:LINE:CONTENT; type
 to fuzzy-filter across them.
Selecting a candidate opens the file at that line."
  (interactive)
  (when-let* ((r (fzf-async-completing-read
                  :command "grep -Rn ''"
                  :group #'fzf-async--grep-group))
              (match (string-match "\\(.*\\):\\([0-9]+\\):" r))
              (file (match-string 1 r))
              (line (string-to-number (match-string 2 r))))
    (find-file file)
    (goto-char (point-min))
    (forward-line (1- line))))

;;;###autoload
(defun fzf-async-grep-current-file ()
  "Search the current buffer's file with grep.
Streams non-blank lines as LINE:CONTENT; type to fuzzy-filter across them.
Selecting a candidate jumps to that line in the file."
  (interactive)
  (when-let* ((bf buffer-file-name) ;; Track buffer.
              (r (fzf-async-completing-read
                  :command (format "grep -vn '^[[:space:]]*$' %s"
                                   (shell-quote-argument buffer-file-name))))
              (match (string-match "^\\([0-9]+\\):\\(.*\\)$" r))
              (line (string-to-number (match-string 1 r))))
    (find-file bf) ;; In case they swapped windows.
    (goto-char (point-min))
    (forward-line (1- line))))

;;;###autoload
(defun fzf-async-ugrep ()
  "Search file contents under `default-directory' with ugrep.
Streams all file contents as FILE:LINE:CONTENT; type to
 fuzzy-filter across them.

Selecting a candidate opens the file at that line."
  (interactive)
  (when-let* ((r (fzf-async-completing-read
                  :command (format "ugrep -RIn --no-heading %s ''"
                                   (fzf-async--max-columns-flag 'ugrep))
                  :group #'fzf-async--grep-group))
              (match (string-match "\\(.*\\):\\([0-9]+\\):" r))
              (file (match-string 1 r))
              (line (string-to-number (match-string 2 r))))
    (find-file file)
    (goto-char (point-min))
    (forward-line (1- line))))

;;;###autoload
(defun fzf-async-git-ls-files ()
  "Find a tracked file in the current Git repo using git ls-files."
  (interactive)
  (unless (locate-dominating-file default-directory ".git")
    (error "Not a Git repo"))
  (when-let* ((result (fzf-async-completing-read
                       :prompt "git ls files: "
                       :command "git ls-files")))
    (find-file result)))

;;;###autoload
(defun fzf-async-hg-files ()
  "Find a tracked file in the current Mercurial repo using hg files."
  (interactive)
  (unless (locate-dominating-file default-directory ".hg")
    (error "Not a Mercurial repo"))
  (when-let* ((result (fzf-async-completing-read
                       :prompt "hg files: "
                       :command "hg files")))
    (find-file result)))

;;;###autoload
(defun fzf-async-recent-file ()
  "Find a recently visited file using `recentf'."
  (interactive)
  (require 'recentf)
  (recentf-mode 1)
  (unless recentf-list
    (user-error "No recent files"))
  (when-let* ((result (fzf-sync-completing-read :candidates recentf-list
                                                :prompt "recent: "
                                                :category 'fzf-async-file)))
    (find-file result)))

;;;###autoload
(defun fzf-async-buffer ()
  "Switch to an open buffer."
  (interactive)
  (let* ((names (cl-loop for b in (buffer-list)
                         unless (or (minibufferp b)
                                    (string-prefix-p " " (buffer-name b)))
                         collect (buffer-name b))))
    (when-let* ((result (fzf-sync-completing-read
                         :candidates names :prompt "buffer: "
                         :category 'fzf-async-buffer)))
      (switch-to-buffer result))))

;;;###autoload
(defun fzf-async-bookmark ()
  "Jump to a bookmark."
  (interactive)
  (require 'bookmark)
  (bookmark-maybe-load-default-file)
  (let ((names (bookmark-all-names)))
    (unless names
      (user-error "No bookmarks defined"))
    (when-let* ((result (fzf-sync-completing-read
                         :candidates names :prompt "bookmark: ")))
      (bookmark-jump result))))

;;;###autoload
(defun fzf-async-locate ()
  "Find a file system-wide using locate."
  (interactive)
  (when-let* ((result (fzf-async-completing-read
                       :command "locate ''")))
    (find-file result)))

;;;###autoload
(defun fzf-async-spotlight ()
  "Find a file system-wide using Spotlight (mdfind).
.app bundles are opened with `open'; all other results open with `find-file'."
  (interactive)
  (when-let* ((result (fzf-async-completing-read
                       :prompt "spotlight: "
                       :command "mdfind 'kMDItemFSName != ''")))
    (if (string-suffix-p ".app" result)
        (start-process "default-app" nil "open" result)
      (find-file result))))

;;;###autoload
(defun fzf-async-spotlight-apps ()
  "Find an installed application using Spotlight.
Opens the selection with `open'."
  (interactive)
  (when-let*
      ((result
        (fzf-async-completing-read
         :prompt "spotlight: "
         :command
         "mdfind 'kMDItemContentTypeTree == \"com.apple.application-bundle\"'")))
    (start-process "default-app" nil "open" result)))

;;;###autoload
(defun fzf-async-spotlight-audio ()
  "Find audio and play it using Spotlight.
Constrained to `fzf-async-spotlight-audio-directories'."
  (interactive)
  (let* ((query "'kMDItemContentTypeTree == \"public.audio\"'")
         (command
          (if fzf-async-spotlight-audio-directories
              (mapconcat
               (lambda (dir)
                 (format "mdfind -onlyin %s %s"
                         (shell-quote-argument (expand-file-name dir))
                         query))
               fzf-async-spotlight-audio-directories
               "; ")
            (concat "mdfind " query))))
    (when-let* ((result (fzf-async-completing-read
                         :prompt "spotlight: "
                         :command command)))
      (start-process "default-app" nil "open" result))))

;;;###autoload
(defun fzf-async-tramp ()
  "Connect to a remote host via TRAMP, with hosts from ~/.ssh/config."
  (interactive)
  (let* ((hosts (fzf-async--ssh-hosts))
         (host (completing-read "SSH host: " hosts nil t)))
    (find-file (concat "/ssh:" host ":"))))

;;;###autoload
(defun fzf-async-swiper ()
  "Search lines of the current buffer using fzf."
  (interactive)
  (let* ((buffer (current-buffer))
         (candidates
          (with-current-buffer buffer
            (let (lines)
              (save-excursion
                (goto-char (point-min))
                (let ((i 1))
                  (while (not (eobp))
                    (let ((content (buffer-substring-no-properties
                                    (line-beginning-position)
                                    (line-end-position))))
                      (unless (string-empty-p content)
                        (push (format "%d:%s" i content) lines)))
                    (forward-line 1)
                    (cl-incf i))))
              (nreverse lines)))))
    (when-let* ((r (fzf-sync-completing-read :candidates candidates :prompt "swiper: "))
                (match (string-match "^\\([0-9]+\\):" r))
                (line (string-to-number (match-string 1 r))))
      (switch-to-buffer buffer)
      (goto-char (point-min))
      (forward-line (1- line)))))

;;;###autoload
(defun fzf-async-swiper-all ()
  "Search lines across all open buffers using fzf."
  (interactive)
  (let* ((buffers (cl-remove-if
                   (lambda (b)
                     (or (minibufferp b)
                         (string-prefix-p " " (buffer-name b))))
                   (buffer-list)))
         (buf-vec (vconcat buffers))
         (candidates
          (cl-loop for buf across buf-vec
                   for i from 0
                   for pfx = (format "%d-%s" i (fzf-async--sanitize-filename
                                                (buffer-name buf)))
                   append (with-current-buffer buf
                            (let (lines)
                              (save-excursion
                                (goto-char (point-min))
                                (let ((j 1))
                                  (while (not (eobp))
                                    (let ((content
                                           (buffer-substring-no-properties
                                            (line-beginning-position)
                                            (line-end-position))))
                                      (unless (string-empty-p content)
                                        (push (format "%s:%d:%s" pfx j content)
                                              lines)))
                                    (forward-line 1)
                                    (cl-incf j))))
                              (nreverse lines))))))
    (when-let* ((r (fzf-sync-completing-read
                    :candidates candidates
                    :prompt "swiper-all: "
                    :group (lambda (cand transform)
                             ;; Candidates: "IDX-BUFNAME:LINE:CONTENT"
                             (if transform
                                 ;; Display: strip "IDX-BUFNAME:" prefix
                                 (when (string-match "^[^:]+:\\(.*\\)$" cand)
                                   (match-string 1 cand))
                               ;; Group header: actual buffer name from index
                               (when (string-match "^\\([0-9]+\\)-" cand)
                                 (let ((i (string-to-number (match-string 1 cand))))
                                   (when (< i (length buf-vec))
                                     (buffer-name (aref buf-vec i)))))))))
                (match (string-match "^\\([0-9]+\\)-[^:]*:\\([0-9]+\\):" r))
                (idx (string-to-number (match-string 1 r)))
                (line (string-to-number (match-string 2 r)))
                ((< idx (length buf-vec)))
                (buffer (aref buf-vec idx))
                ((buffer-live-p buffer)))
      (switch-to-buffer buffer)
      (goto-char (point-min))
      (forward-line (1- line)))))

;;;###autoload
(defun fzf-async-swiper-hungry ()
  "Grep across the parent directories of all file-visiting buffers.
Collects unique parent directories, drops any that are subdirectories of
another in the set, then streams rg (or grep) output through fzf.
Selecting a match opens the file and jumps to the line."
  (interactive)
  (let* ((raw-dirs (cl-loop for buf in (buffer-list)
                            for file = (buffer-file-name buf)
                            when file
                            collect (file-name-directory (expand-file-name file))))
         (dirs (fzf-async--deduplicate-dirs raw-dirs)))
    (unless dirs
      (user-error "No file-visiting buffers found"))
    (let* ((rg   (executable-find "rg"))
           (grep (executable-find "grep"))
           (dir-args (mapconcat #'shell-quote-argument dirs " "))
           (command
            (cond
             (rg   (concat (shell-quote-argument rg)
                           " --line-number --no-heading --with-filename '' "
                           dir-args))
             (grep (concat (shell-quote-argument grep)
                           " -Rn '' "
                           dir-args))
             (t (user-error "Neither rg nor grep found in exec-path")))))
      (when-let* ((r (fzf-async-completing-read
                      :prompt "hungry swiper: "
                      :command command
                      :directory default-directory
                      :group #'fzf-async--grep-group))
                  (match (string-match "\\(.*\\):\\([0-9]+\\):" r))
                  (file (match-string 1 r))
                  (line (string-to-number (match-string 2 r))))
        (find-file file)
        (goto-char (point-min))
        (forward-line (1- line))))))

;;;###autoload
(defun fzf-async-find-hungry ()
  "Find files across the parent directories of all file-visiting buffers.
Collects unique parent directories, drops subdirectories already covered
by a shallower parent, then streams fd (or find) output through fzf."
  (interactive)
  (let* ((raw-dirs (cl-loop for buf in (buffer-list)
                            for file = (buffer-file-name buf)
                            when file
                            collect (file-name-directory (expand-file-name file))))
         (dirs (fzf-async--deduplicate-dirs raw-dirs)))
    (unless dirs
      (user-error "No file-visiting buffers found"))
    (let* ((fd   (executable-find "fd"))
           (find (executable-find "find"))
           (dir-args (mapconcat #'shell-quote-argument dirs " "))
           (command
            (cond
             (fd   (concat (shell-quote-argument fd)
                           " --no-ignore . "
                           dir-args))
             (find (concat (shell-quote-argument find)
                           " "
                           dir-args
                           " -type f"))
             (t (user-error "Neither fd nor find found in exec-path")))))
      (when-let* ((result (fzf-async-completing-read
                           :prompt "hungry find: "
                           :command command
                           :directory default-directory)))
        (find-file result)))))

;;; Helpers

(defun fzf-async--grep-group (cand transform)
  "Group function for FILE:LINE:CONTENT grep candidates.
TRANSFORM nil  → return the filename as the section header.
TRANSFORM non-nil → strip the filename prefix; display LINE:CONTENT only."
  (let ((i (string-search ":" cand)))
    (if transform
        (if i (substring cand (1+ i)) cand)
      (if i (substring cand 0 i) cand))))

(defun fzf-async--default-dir ()
  "Return the working directory for fzf-async commands.
Priority: `fzf-async-directory' >
          `fzf-async-project-backend' >
          `default-directory'."
  (or fzf-async-directory
      (pcase fzf-async-project-backend
        ((pred functionp)
         (funcall fzf-async-project-backend))
        ('project
         (when-let* ((pr (project-current)))
           (project-root pr)))
        ('projectile
         (when (bound-and-true-p projectile-mode)
           (projectile-project-root))))
      default-directory))

(defun fzf-async--deduplicate-dirs (dirs)
  "Remove duplicates and subdirectory entries from DIRS.
If directory A is a prefix of directory B, B is dropped — A's recursive
search already covers it.  Exception: B is kept when it is itself a git
root (contains a .git entry), since rg honors per-repo gitignores and a
descend from A may exclude files the user expects to search."
  (let ((unique (cl-delete-duplicates dirs :test #'string=)))
    (cl-loop for dir in unique
             unless (and (not (file-exists-p (expand-file-name ".git" dir)))
                         (cl-some (lambda (other)
                                    (and (not (string= dir other))
                                         (string-prefix-p other dir)))
                                  unique))
             collect dir)))

(defun fzf-async--sanitize-filename (name)
  "Replace filename-unsafe characters in NAME with hyphens."
  (replace-regexp-in-string "[/\\*?<>|: ]" "-" name))

(defun fzf-async--ssh-hosts ()
  "Return SSH host names from ~/.ssh/config, excluding wildcard patterns."
  (let ((config (expand-file-name "~/.ssh/config"))
        hosts)
    (when (file-readable-p config)
      (with-temp-buffer
        (insert-file-contents config)
        (while (re-search-forward "^[Hh]ost[[:space:]]+\\(.+\\)" nil t)
          (dolist (host (split-string (match-string 1)))
            (unless (string-match-p "[*?!]" host)
              (push host hosts))))))
    (nreverse hosts)))

(defun fzf-async--require-executable (program)
  "Signal `user-error' if PROGRAM is not found in `exec-path'."
  (unless (executable-find program)
    (user-error "%s not found in exec-path" program)))

;;; Shell command

(defvar fzf-async-shell-command-history nil
  "Minibuffer history for `fzf-async-shell-command'.")

;;;###autoload
(defun fzf-async-shell-command (command &optional directory)
  "Fuzzy-search the output of a user-provided shell COMMAND.
Runs in DIRECTORY, defaulting to `default-directory'.
COMMAND is passed verbatim to `shell-file-name', so pipes,
redirections, and shell quoting all work as expected.  The selected
candidate is opened as a file if it exists relative to the working
directory; otherwise it is placed in the kill ring."
  (interactive
   (list (read-shell-command "Shell command: " nil
                             'fzf-async-shell-command-history)))
  (let* ((cmd (string-trim command))
         (dir (or directory default-directory)))
    (when (string-empty-p cmd)
      (user-error "Command cannot be empty"))
    (when-let* ((result (fzf-async-completing-read
                         :prompt (format "%s » " cmd)
                         :command cmd
                         :directory dir
                         :skip-executable-check t)))
      (let ((path (expand-file-name result dir)))
        (if (file-exists-p path)
            (find-file path)
          (kill-new result)
          (message "%s" result))))))

;;;###autoload
(defun fzf-async-project-shell-command (command)
  "Fuzzy-search the output of a user-provided shell COMMAND.
Like `fzf-async-shell-command' but runs in the project root."
  (interactive
   (list (read-shell-command "Shell command: "
                             nil 'fzf-async-shell-command-history)))
  (fzf-async-shell-command command (fzf-async--default-dir)))

;;; Setup

(defconst fzf-async--commands
  '(fzf-async-shell-command
    fzf-async-project-shell-command
    fzf-async-find
    fzf-async-fd
    fzf-async-rg-files
    fzf-async-ag-files
    fzf-async-rg
    fzf-async-ag
    fzf-async-git-grep
    fzf-async-grep
    fzf-async-grep-current-file
    fzf-async-ugrep
    fzf-async-git-ls-files
    fzf-async-hg-files
    fzf-async-locate
    fzf-async-spotlight
    fzf-async-spotlight-apps
    fzf-async-spotlight-audio
    fzf-async-swiper-hungry
    fzf-async-find-hungry)
  "All fzf-async commands that use `fzf-async-completing-read'.")

(defcustom fzf-async-file-commands
  '(fzf-async-find
    fzf-async-fd
    fzf-async-rg-files
    fzf-async-ag-files
    fzf-async-git-ls-files
    fzf-async-hg-files
    fzf-async-locate
    fzf-async-spotlight
    fzf-async-spotlight-apps
    fzf-async-spotlight-audio)
  "fzf-async commands whose candidates are plain file paths.
These are registered with marginalia under the `file' category so
marginalia can annotate them with file size and modification time.
Commands not listed here (grep-style commands returning FILE:LINE:CONTENT)
are registered under the `fzf-async' category and receive no annotation."
  :type '(repeat symbol)
  :group 'fzf-async)

(defun fzf-async--check-completion-setup (&rest _)
  "Signal a user-error if the completion configuration is incorrect.
Guards against two misconfiguration patterns:
- `fzf-async' in the global `completion-styles' list, which applies the
  style to every completing-read and breaks callers that pass plain lists.
- `fzf-async' absent from `completion-category-overrides', which means
  the style never activates and results are silently re-filtered."
  (when (memq 'fzf-async completion-styles)
    (user-error
     "fzf-async must not be in `completion-styles' globally (it is).  \
Remove it and ensure `fzf-async-setup' has been called so it is wired \
via `completion-category-overrides' only"))
  (unless (and (assq 'fzf-async completion-category-overrides)
               (assq 'fzf-async-file completion-category-overrides)
               (assq 'fzf-async-multi completion-category-overrides))
    (user-error
     "fzf-async is missing from `completion-category-overrides'.  \
Call `fzf-async-setup' before using fzf-async commands")))

(defun fzf-async--bridge-defcustoms (orig-fn &rest args)
  "Wrap a fzf-native call so the C scorer sees fzf-async-* values.

Bridges four knobs:
  `fzf-async-highlight'       -> `fzf-native-async-highlight'
  `fzf-async-max-line-length' -> `fzf-native-max-line-length'
  `fzf-async-cache-size'      -> `fzf-native-async-cache-size'
  `fzf-async-case-mode'       -> `fzf-native-case-mode'"
  (let ((fzf-native-async-highlight  fzf-async-highlight)
        (fzf-native-max-line-length  fzf-async-max-line-length)
        (fzf-native-async-cache-size fzf-async-cache-size)
        (fzf-native-case-mode        fzf-async-case-mode))
    (apply orig-fn args)))

;;;###autoload
(defun fzf-async-setup ()
  "Register the fzf-async completion style and category override.
Call this once during init before using `fzf-async-completing-read'."
  (add-to-list 'completion-styles-alist
               '(fzf-async fzf-async-try-completion fzf-async-all-completions
                           "Passthrough style for pre-scored async fzf completions."))
  (add-to-list 'completion-category-overrides
               '(fzf-async (styles fzf-async)))

  (advice-add 'fzf-native-async-start      :around #'fzf-async--bridge-defcustoms)
  (advice-add 'fzf-native-async-candidates :around #'fzf-async--bridge-defcustoms)

  ;; Shadow categories for `file' / `buffer' / multi: keep marginalia +
  ;; embark integration but force the passthrough style so other styles
  ;; (e.g. fussy on `file', `basic' on multi) don't re-filter our
  ;; pre-scored candidates or cache them client-side past the first call.
  (add-to-list 'completion-category-overrides
               '(fzf-async-file (styles fzf-async)))
  (add-to-list 'completion-category-overrides
               '(fzf-async-buffer (styles fzf-async)))
  (add-to-list 'completion-category-overrides
               '(fzf-async-multi (styles fzf-async)))

  (dolist (command fzf-async--commands)
    (advice-add command :before #'fzf-async--check-completion-setup)
    (with-eval-after-load 'embark
      (add-to-list 'embark-keymap-alist
                   `(,command . embark-file-map))))

  (with-eval-after-load 'embark
    (add-to-list 'embark-keymap-alist
                 '(fzf-async-buffer . embark-buffer-map)))

  (with-eval-after-load 'marginalia
    ;; Register the file annotator for fzf-async-file so file commands still
    ;; show size/date without handing control to the `file' completion styles.
    (add-to-list 'marginalia-annotators
                 '(fzf-async-file marginalia-annotate-file none))
    (add-to-list 'marginalia-annotators
                 '(fzf-async-buffer marginalia-annotate-buffer none))
    (dolist (command fzf-async--commands)
      (add-to-list 'marginalia-command-categories
                   `(,command . ,(if (memq command fzf-async-file-commands)
                                     'fzf-async-file
                                   'fzf-async))))))

(provide 'fzf-async)
;;; fzf-async.el ends here
