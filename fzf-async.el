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
(defvar embark-default-action-overrides)
(defvar embark-general-map)
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
(declare-function imenu--make-index-alist "imenu")
(declare-function imenu--subalist-p "imenu")
(defvar ivy-text)
(defvar ivy--index)
(defvar ivy--all-candidates)
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
  (when (bound-and-true-p fzf-async-debug) `(message ,fmt ,@args)))

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
upstream into the search tool itself (via `--max-columns' / `--width')."
  :type '(choice (const   :tag "No limit" nil)
                 (integer :tag "N (positive = exclude, negative = truncate)"))
  :group 'fzf-async)

(defcustom fzf-async-case-mode 'smart
  "Case-sensitivity mode propagated to `fzf-native-case-mode'.

Mirrors fzf-native's enum:
smart    Case-insensitive when the query is all lowercase; case-sensitive
         once it contains any uppercase character (fzf's default).
ignore   Always case-insensitive.
respect  Always case-sensitive."
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

(defcustom fzf-async-extensions '(pass spotlight music)
  "List of fzf-async extensions to load from `fzf-async-setup'.
Each SYMBOL causes `fzf-async-setup' to `require' the feature
`fzf-async-SYMBOL' and, if defined, call `fzf-async-SYMBOL-setup'.
Extensions live in the `extensions/' subdirectory of this package;
that directory is added to `load-path' the first time
`fzf-async-setup' runs."
  :type '(set (const :tag "password-store (pass)" pass)
              (const :tag "macOS Spotlight (mdfind)" spotlight)
              (const :tag "macOS Music.app" music))
  :group 'fzf-async)

(defconst fzf-async--extensions-dir
  (when (or load-file-name buffer-file-name)
    (expand-file-name "extensions"
                      (file-name-directory
                       (or load-file-name buffer-file-name))))
  "Absolute path to the bundled `extensions/' directory.
Captured at load time so `fzf-async-setup' can add it to `load-path'
regardless of where it is called from.")

(defvar fzf-async--multi-mode nil
  "Dispatch flag for `fzf-async-completing-read' / `fzf-sync-completing-read'.
- `:extract'         — throw `fzf-async-extracted' with the call's keyword args.
- (`:inject' . CAND) — return CAND directly without prompting.
Bound by `fzf-async-multi-read' to derive multi-source sources from
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
function   Call the function with no arguments; Returns a directory string."
  :type '(choice (const :tag "project.el" project)
                 (const :tag "Projectile" projectile)
                 (const :tag "None (default-directory)" nil)
                 (function :tag "Custom function"))
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

(defun fzf-async--frontend-candidate ()
  "Return the currently highlighted candidate string in the active UI, or nil.
Used to implement live preview (e.g. `fzf-async-theme')."
  (cond
   ((bound-and-true-p vertico-mode)
    (when (and (boundp 'vertico--candidates) vertico--candidates)
      (nth (max 0 vertico--index) vertico--candidates)))
   ((bound-and-true-p ivy-mode)
    (when (and (boundp 'ivy--all-candidates) ivy--all-candidates)
      (nth (max 0 ivy--index) ivy--all-candidates)))
   ((bound-and-true-p icomplete-mode)
    (car (completion-all-sorted-completions)))))

(defun fzf-async--frontend-exhibit ()
  "Trigger a display refresh in the active completion UI.
Handles vertico and icomplete. `ivy' is handled separately."
  (when-let* ((win (active-minibuffer-window)))
    (with-selected-window win
      (cond
       ((bound-and-true-p vertico-mode)
        (setq vertico--input t)
        (vertico--exhibit))
       ((bound-and-true-p icomplete-mode)
        (icomplete-exhibit))))))

(defun fzf-async--commas (n)
  "Format integer N with comma thousand-separators.

e.g., 1234567 → 1,234,567."
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

(defun fzf-async--maybe-expand (result directory resolve-paths)
  "Return RESULT expanded against DIRECTORY when RESOLVE-PATHS is non-nil.

For RESOLVE-PATHS=t the whole RESULT is passed through `expand-file-name'
— this works for both plain paths and FILE:LINE:CONTENT grep candidates,
since `expand-file-name' prepends DIRECTORY and leaves the suffix
untouched.  Returns RESULT unchanged for non-strings, empty strings, or
when RESOLVE-PATHS is nil."
  (if (and resolve-paths (stringp result) (not (string-empty-p result)))
      (expand-file-name result directory)
    result))

;;;###autoload
(cl-defun fzf-async-completing-read (&key
                                     prompt
                                     command
                                     (directory (fzf-async--default-dir))
                                     (category 'fzf-async-file)
                                     group
                                     (resolve-paths t)
                                     skip-executable-check)
  "Run shell COMMAND and completing-read its output.

:PROMPT                 Minibuffer prompt.  Derived from the first token of
                        COMMAND (e.g. \"find: \" for \"find .\") when omitted.
:COMMAND                Shell command whose stdout lines become candidates.
:DIRECTORY              Working directory for COMMAND.  Defaults to
                        `fzf-async--default-dir' (respects
                        `fzf-async-project-backend').
:CATEGORY               Completion category symbol.  Defaults to
                        `fzf-async-file' (most async commands return file
                        paths).  Pass `fzf-async-grep' for FILE:LINE:CONTENT
                        candidates, `fzf-async-misc' for non-file output, etc.
:RESOLVE-PATHS          When non-nil (the default), the returned
                        candidate is passed through `expand-file-name'
                        against :DIRECTORY before being handed back to the
                        caller.  Lets file and grep commands stay agnostic
                        of the caller's `default-directory'.  Pass nil for
                        commands that return non-path output (e.g. shell
                        output where the raw text matters).
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
                   :directory directory :category category :group group
                   :resolve-paths resolve-paths)))
     ((eq (car-safe fzf-async--multi-mode) :inject)
      (cl-return-from fzf-async-completing-read
        (fzf-async--maybe-expand (cdr fzf-async--multi-mode)
                                 directory resolve-paths))))
    (fzf-async--check-completion-setup)
    (when (bound-and-true-p helm-mode)
      (cl-return-from fzf-async-completing-read
        (fzf-async--maybe-expand
         (fzf-async--helm-completing-read
          :prompt prompt :command command :directory directory
          :skip-executable-check skip-executable-check)
         directory resolve-paths)))
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
           ;; Ivy push path: score the current query and push into
           ;; `ivy--all-candidates' directly. Used instead of
           ;; `fzf-async--frontend-exhibit' for ivy because
           ;; ivy does not re-call the collection lambda on timer ticks.
           (ivy-push
            (lambda ()
              (when (and handle (active-minibuffer-window))
                (when-let* ((query (and (boundp 'ivy-text) ivy-text)))
                  (let ((cands (while-no-input
                                 (fzf-native-async-candidates
                                  handle query limit))))
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
      (fzf-async--maybe-expand
       (unwind-protect
           (minibuffer-with-setup-hook
               (lambda ()
                 ;; Bind the minibuffer's default-directory so that callers
                 ;; running outside fzf-async (notably embark, which captures
                 ;; default-directory and rebinds it around the action) resolve
                 ;; relative candidates against the working directory the
                 ;; command actually ran in.
                 (setq-local default-directory directory)
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
                    ('metadata `(metadata (category . ,category)
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
         ;; Defer `fzf-native-async-stop' off the synchronous unwind path.
         ;; The C-side destroy does pthread_join on the scoring thread
         ;; (uninterruptible snapshot/score work for huge pools) and frees
         ;; the candidate arena — easily hundreds of ms for a `find ~'-scale
         ;; session.  None of it is needed before minibuffer dismissal, so
         ;; we let the user see ESC return instantly and clean up on the
         ;; next idle tick.
         (when handle
           (let ((h handle))
             (run-at-time 0 nil (lambda () (fzf-native-async-stop h))))))
       directory resolve-paths))))

(cl-defun fzf-sync-completing-read (&key
                                    candidates
                                    (prompt "fzf > ")
                                    (category 'fzf-async-misc)
                                    annotate
                                    affix
                                    group)
  "Run completing-read over CANDIDATES using fzf-native for scoring.

:CANDIDATES List of strings to score with `fzf-native-score-all'.
:PROMPT     Minibuffer prompt string.  Defaults to \"fzf > \".
:CATEGORY   Completion category symbol.  Defaults to `fzf-async-misc'.
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
  (fzf-async--check-completion-setup)
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

(cl-defun fzf-async--multi-read (sources &key (prompt "fzf-multi: "))
  "Run completing-read across multiple SOURCES, fzf-async style.

Internal — users should call `fzf-async-multi-read' which derives sources
from existing single-source commands.  This function takes pre-built
source plists directly.

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
  (cond
   ;; Composability: when this multi is being extracted by an outer
   ;; `fzf-async-multi-read', throw our merged SOURCES so the
   ;; outer can flatten them into its own source list.  Each source
   ;; already carries its own :action closure, so dispatch from the
   ;; outer multi routes back to the correct underlying command.
   ((eq fzf-async--multi-mode :extract)
    (throw 'fzf-async-extracted (list :multi-sources sources)))
   ((eq (car-safe fzf-async--multi-mode) :inject)
    (cl-return-from fzf-async--multi-read (cdr fzf-async--multi-mode))))
  (when (bound-and-true-p helm-mode)
    (user-error "fzf-async--multi-read does not yet support helm-mode"))
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
                                (let ((src (fzf-async--multi-source-of
                                            cand sources-v cand->src)))
                                  (if transform
                                      ;; Per-source :group transform —
                                      ;; lets a source strip an internal
                                      ;; "IDX:" prefix or otherwise
                                      ;; reshape its display string while
                                      ;; keeping the raw value as the
                                      ;; lookup/match key.  Falls back to
                                      ;; the raw candidate when a source
                                      ;; has no :group function (or its
                                      ;; transform returns nil).
                                      (or (when-let* ((g (plist-get src :group)))
                                            (funcall g cand t))
                                          cand)
                                    (or (plist-get src :name) "")))))
                          (affixation-function
                           . ,(lambda (cands)
                                (let* ((displays
                                        (mapcar
                                         (lambda (c)
                                           (let* ((src (fzf-async--multi-source-of
                                                        c sources-v cand->src))
                                                  (g (and src (plist-get src :group))))
                                             (or (and g (funcall g c t)) c)))
                                         cands))
                                       (maxw (apply #'max 0
                                                    (mapcar #'string-width
                                                            displays))))
                                  (cl-mapcar
                                   (lambda (cand display)
                                     (let* ((src (fzf-async--multi-source-of
                                                  cand sources-v cand->src))
                                            (ann (and src (plist-get src :annotate)))
                                            (s   (and ann (funcall ann cand)))
                                            (pad (- (1+ maxw)
                                                    (string-width display))))
                                       (list cand ""
                                             (if s
                                                 (concat
                                                  (make-string (max 1 pad) ?\s)
                                                  s)
                                               ""))))
                                   cands displays))))))
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
      ;; Defer the async-stops so ESC returns instantly — see the same
      ;; comment in `fzf-async-completing-read'.  Stops are scheduled
      ;; together so the runtime can decide its own scheduling, and the
      ;; closure owns the handle vector to keep it alive across the gap.
      (let ((live nil))
        (dotimes (i n)
          (when-let* ((h (aref handles i)))
            (push h live)))
        (when live
          (run-at-time 0 nil
                       (lambda ()
                         (dolist (h live) (fzf-native-async-stop h)))))))
    (when result
      (let* ((src    (or (and selected-idx (aref sources-v selected-idx))
                         (fzf-async--multi-source-of
                          result sources-v cand->src)))
             (action (and src (plist-get src :action))))
        (if action (funcall action result) result)))))

;;;###autoload
(defun fzf-async-multi-read (commands &rest options)
  "Run a multi-source completing-read over COMMANDS.
Each command in COMMANDS is funcalled twice per multi session — once in
`:extract' mode (capture keyword args, abort), once in `:inject' mode after
the user picks (so the command's post-action runs).  OPTIONS is forwarded
to `fzf-async--multi-read'.  Commands whose body does not reach
`fzf-async-completing-read' or `fzf-sync-completing-read' are skipped.
Commands must be arg-less (no interactive `read-*' prompts in their body).

Composes: if a command in COMMANDS itself calls `fzf-async--multi-read'
\(e.g. `fzf-async-find-any'), its inner sources are flattened in alongside
the other commands' sources, with each inner source keeping its own
:action."
  (let* ((source-lists
          (mapcar
           (lambda (cmd)
             (let ((args (condition-case nil
                             (catch 'fzf-async-extracted
                               (let ((fzf-async--multi-mode :extract))
                                 (funcall cmd))
                               nil)
                           (error nil))))
               (when args
                 (if-let* ((nested (plist-get args :multi-sources)))
                     ;; Flatten: nested multi command's sources are
                     ;; already fully built with :action closures.
                     nested
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
                     ;; Wrap a single source in a list so `append'
                     ;; below treats single and nested cases uniformly.
                     (list
                      (append
                       (list :name (replace-regexp-in-string
                                    "^fzf-async-" "" (symbol-name cmd))
                             :annotate (or (plist-get args :annotate)
                                           default-annotate)
                             :action (lambda (cand)
                                       (let ((fzf-async--multi-mode
                                              (cons :inject cand)))
                                         (funcall cmd))))
                       args)))))))
           commands))
         (sources (apply #'append (delq nil source-lists))))
    (apply #'fzf-async--multi-read sources options)))

(defcustom fzf-async-find-any-commands
  '(fzf-async-imenu
    fzf-async-buffer
    fzf-async-recent-file
    fzf-async-find-hungry
    fzf-async-imenu-all-but-current
    fzf-async-swiper-hungry)
  "Commands shown by `fzf-async-find-any'."
  :type '(repeat function)
  :group 'fzf-async)

(defcustom fzf-async-find-some-commands
  '(fzf-async-imenu
    fzf-async-buffer
    fzf-async-recent-file
    fzf-async-find
    fzf-async-swiper)
  "Commands shown by `fzf-async-find-some'."
  :type '(repeat function)
  :group 'fzf-async)

;;;###autoload
(defun fzf-async-find-any ()
  "Multi-source fuzzy completion over `fzf-async-find-any-commands'."
  (interactive)
  (fzf-async-multi-read fzf-async-find-any-commands :prompt "any?: "))

;;;###autoload
(defun fzf-async-find-some ()
  "Multi-source fuzzy completion over `fzf-async-find-some-commands'."
  (interactive)
  (fzf-async-multi-read fzf-async-find-some-commands :prompt "some?: "))

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
  (when-let* ((result (fzf-async-completing-read :command "find .")))
    (find-file result)))

;;;###autoload
(defun fzf-async-fd ()
  "Find a file under `default-directory' using fd."
  (interactive)
  (when-let* ((result (fzf-async-completing-read :command "fd --no-ignore")))
    (find-file result)))

;;;###autoload
(defun fzf-async-rg-files ()
  "Find a file under `default-directory' using rg --files."
  (interactive)
  (when-let* ((result (fzf-async-completing-read
                       :prompt "rg files: " :command "rg --files")))
    (find-file result)))

;;;###autoload
(defun fzf-async-ag-files ()
  "Find a file under `default-directory' using ag."
  (interactive)
  (when-let* ((result (fzf-async-completing-read
                       :prompt "ag files: " :command "ag -g .")))
    (find-file result)))

;;;###autoload
(defun fzf-async-rg ()
  "Search file contents under `default-directory' with rg.
Streams all file contents as FILE:LINE:CONTENT; type to
 fuzzy-filter across them.
Selecting a candidate opens the file at that line."
  (interactive)
  (when-let* ((r (fzf-async-completing-read
                  :command (format
                            "rg --line-number --no-heading --with-filename %s ''"
                            (fzf-async--max-columns-flag 'rg))
                  :category 'fzf-async-grep
                  :group #'fzf-async--grep-group)))
    (fzf-async--grep-jump r)))

;;;###autoload
(defun fzf-async-ag ()
  "Search file contents under `default-directory' with ag.
Streams all file contents as FILE:LINE:CONTENT; type to
 fuzzy-filter across them.
Selecting a candidate opens the file at that line."
  (interactive)
  (when-let* ((r (fzf-async-completing-read
                  :command (format
                            "ag --nocolor --nogroup --line-number %s \".\""
                            (fzf-async--max-columns-flag 'ag))
                  :category 'fzf-async-grep
                  :group #'fzf-async--grep-group)))
    (fzf-async--grep-jump r)))

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
                  :category 'fzf-async-grep
                  :group #'fzf-async--grep-group)))
    (fzf-async--grep-jump r)))

;;;###autoload
(defun fzf-async-grep ()
  "Search file contents under `default-directory' with grep.
Streams all file contents as FILE:LINE:CONTENT; type
 to fuzzy-filter across them.
Selecting a candidate opens the file at that line."
  (interactive)
  (when-let* ((r (fzf-async-completing-read
                  :command "grep -Rn ''"
                  :category 'fzf-async-grep
                  :group #'fzf-async--grep-group)))
    (fzf-async--grep-jump r)))

;;;###autoload
(defun fzf-async-grep-current-file ()
  "Search the current buffer's file with grep.
Streams non-blank lines as FILE:LINE:CONTENT; type to fuzzy-filter across them.
Selecting a candidate jumps to that line in the file."
  (interactive)
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  (when-let* ((r (fzf-async-completing-read
                  :command (format "grep -vnH '^[[:space:]]*$' %s"
                                   (shell-quote-argument buffer-file-name))
                  :category 'fzf-async-grep)))
    (fzf-async--grep-jump r)))

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
                  :category 'fzf-async-grep
                  :group #'fzf-async--grep-group)))
    (fzf-async--grep-jump r)))

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
(defun fzf-async-yank-pop ()
  "Yank from `kill-ring' using fzf for selection.
When invoked immediately after a yank command, replaces the previously
yanked text with the selection (mirroring `yank-pop' / `consult-yank-pop')."
  (interactive "*")
  (unless kill-ring
    (user-error "Kill ring is empty"))
  (let* ((seen (make-hash-table :test 'equal))
         (lookup (make-hash-table :test 'equal))
         (entries
          (cl-loop
           for s in kill-ring
           for clean = (substring-no-properties s)
           unless (or (string-empty-p clean) (gethash clean seen))
           do (puthash clean t seen)
           and collect
           (let ((display
                  (replace-regexp-in-string
                   "\n" (propertize "⏎" 'face 'shadow)
                   (if (> (length clean) 200)
                       (concat (substring clean 0 200) "…")
                     clean))))
             ;; Disambiguate displays collapsed to the same string
             ;; (e.g. entries differing only in stripped properties).
             (while (gethash display lookup)
               (setq display (concat display " ")))
             (puthash display s lookup)
             display))))
    (when-let* ((result (fzf-sync-completing-read
                         :candidates entries
                         :prompt "yank-pop: "))
                (text (gethash result lookup)))
      (cond
       ((eq last-command 'yank)
        (let ((inhibit-read-only t)
              (pt (point))
              (mk (mark t)))
          (funcall (or yank-undo-function #'delete-region)
                   (min pt mk) (max pt mk))
          (setq yank-undo-function nil)
          (set-marker (mark-marker) pt (current-buffer))
          (insert-for-yank text)))
       (t
        (push-mark)
        (insert-for-yank text)))
      (setq this-command 'yank))))

(defcustom fzf-async-shell-history-file nil
  "Path to a shell history file (bash or zsh).
When nil, defaults to `$HISTFILE' if set, otherwise `~/.zsh_history'."
  :type '(choice (const :tag "Auto ($HISTFILE or ~/.zsh_history)" nil)
                 file)
  :group 'fzf-async)

;;;###autoload
(defun fzf-async-shell-history ()
  "Select a command from the shell history file and insert it at point.
Supports bash and zsh history file formats (including zsh
`EXTENDED_HISTORY' and bash `HISTTIMEFORMAT' timestamp comments).
If the current buffer is read-only the selection is copied to the
kill ring instead.  Override the location via
`fzf-async-shell-history-file'."
  (interactive)
  (cl-labels
      ((parse-entry (raw)
         (cond
          ((string-match "\\`: [0-9]+:[0-9]+;\\(\\(?:.\\|\n\\)*\\)\\'" raw)
           (match-string 1 raw))
          ((string-match-p "\\`#[0-9]+\\'" raw) nil)
          (t raw)))
       (read-entries (file)
         (let ((seen (make-hash-table :test 'equal)) results)
           (with-temp-buffer
             (let ((coding-system-for-read 'utf-8-auto))
               (insert-file-contents file))
             (while (not (eobp))
               (let ((start (point)))
                 (end-of-line)
                 ;; Continuations: trailing backslash escapes the newline.
                 (while (and (eq (char-before) ?\\) (not (eobp)))
                   (forward-char 1) (end-of-line))
                 (when-let* ((cmd (parse-entry
                                   (buffer-substring-no-properties
                                    start (point))))
                             (cmd (string-trim cmd))
                             ((not (string-empty-p cmd)))
                             ((not (gethash cmd seen))))
                   (puthash cmd t seen)
                   (push cmd results))
                 (unless (eobp) (forward-char 1)))))
           results)))
    (let* ((file (expand-file-name
                  (or fzf-async-shell-history-file
                      (getenv "HISTFILE")
                      "~/.zsh_history")))
           (cmds (and (or (file-readable-p file)
                          (user-error "Cannot read shell history: %s" file))
                      (or (read-entries file)
                          (user-error "Shell history is empty")))))
      (when-let* ((result (fzf-sync-completing-read
                           :candidates cmds :prompt "shell-history: ")))
        (if buffer-read-only
            (progn (kill-new result) (message "Copied: %s" result))
          (insert result))))))

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
                         :candidates names :prompt "bookmark: "
                         :category 'fzf-async-bookmark)))
      (bookmark-jump result))))

;;;###autoload
(defun fzf-async-theme ()
  "Prompt for a theme to enable, with live preview as the selection moves.
Aborting (e.g. \\[keyboard-quit]) restores the themes that were enabled
when the command was invoked.  Selecting \"default\" disables all themes."
  (interactive)
  (cl-labels ((switch (sym)
                (dolist (th custom-enabled-themes)
                  (unless (eq th sym) (disable-theme th)))
                (when (and sym (not (memq sym custom-enabled-themes)))
                  (if (custom-theme-p sym)
                      (enable-theme sym)
                    (load-theme sym :no-confirm)))))
    (let* ((saved custom-enabled-themes)
           (last 'unset)
           (preview
            (lambda ()
              (when-let* ((cand (fzf-async--frontend-candidate)))
                (unless (equal cand last)
                  (setq last cand)
                  (condition-case _
                      (switch (and (not (equal cand "default")) (intern cand)))
                    (error nil))))))
           selection)
      (unwind-protect
          (minibuffer-with-setup-hook
              (lambda ()
                (add-hook 'post-command-hook preview nil t))
            (setq selection
                  (fzf-sync-completing-read
                   :candidates (cons "default"
                                     (mapcar #'symbol-name
                                             (custom-available-themes)))
                   :prompt "theme: "
                   :category 'fzf-async-theme)))
        (if selection
            (switch (and (not (equal selection "default")) (intern selection)))
          (mapc #'disable-theme custom-enabled-themes)
          (mapc #'enable-theme (reverse saved)))))))

;;;###autoload
(defun fzf-async-locate ()
  "Find a file system-wide using locate."
  (interactive)
  (when-let* ((result (fzf-async-completing-read :command "locate ''")))
    (find-file result)))

;;;###autoload
(defun fzf-async-tramp ()
  "Connect to a remote host via TRAMP, with hosts from ~/.ssh/config."
  (interactive)
  (cl-labels ((ssh-hosts ()
                (let ((config (expand-file-name "~/.ssh/config"))
                      hosts)
                  (when (file-readable-p config)
                    (with-temp-buffer
                      (insert-file-contents config)
                      (while (re-search-forward
                              "^[Hh]ost[[:space:]]+\\(.+\\)" nil t)
                        (dolist (host (split-string (match-string 1)))
                          (unless (string-match-p "[*?!]" host)
                            (push host hosts))))))
                  (nreverse hosts))))
    (when-let* ((hosts (or (ssh-hosts)
                           (user-error "No SSH hosts in ~/.ssh/config")))
                (host (fzf-sync-completing-read
                       :candidates hosts :prompt "ssh: ")))
      (find-file (concat "/ssh:" host ":")))))

;;;###autoload
(defun fzf-async-swiper ()
  "Search lines of the current buffer using fzf."
  (interactive)
  (let* ((source (or (buffer-file-name) (buffer-name)))
         (candidates
          (let (lines)
            (save-excursion
              (goto-char (point-min))
              (let ((i 1))
                (while (not (eobp))
                  (let ((content (buffer-substring-no-properties
                                  (line-beginning-position)
                                  (line-end-position))))
                    (unless (string-empty-p content)
                      (push (format "%s:%d:%s" source i content) lines)))
                  (forward-line 1)
                  (cl-incf i))))
            (nreverse lines))))
    (when-let* ((r (fzf-sync-completing-read :candidates candidates :prompt "swiper: "
                                             :category 'fzf-async-grep)))
      (fzf-async--grep-jump r))))

;;;###autoload
(defun fzf-async-swiper-all ()
  "Search lines across all open buffers using fzf.
Candidates are formatted as SOURCE:LINE:CONTENT where SOURCE is the
buffer's file path when file-backed, else its buffer name.  Buffer
names containing `:DIGITS:' substrings are not encoded specially and
may parse ambiguously — a rare-enough hazard to accept."
  (interactive)
  (let* ((buffers (cl-remove-if
                   (lambda (b)
                     (or (minibufferp b)
                         (string-prefix-p " " (buffer-name b))))
                   (buffer-list)))
         (candidates
          (cl-loop
           for buf in buffers
           for source = (or (buffer-file-name buf) (buffer-name buf))
           append (with-current-buffer buf
                    (let (lines)
                      (save-excursion
                        (goto-char (point-min))
                        (let ((j 1))
                          (while (not (eobp))
                            (let ((content (buffer-substring-no-properties
                                            (line-beginning-position)
                                            (line-end-position))))
                              (unless (string-empty-p content)
                                (push (format "%s:%d:%s" source j content)
                                      lines)))
                            (forward-line 1)
                            (cl-incf j))))
                      (nreverse lines))))))
    (when-let* ((r (fzf-sync-completing-read
                    :candidates candidates
                    :prompt "swiper-all: "
                    :category 'fzf-async-grep
                    :group #'fzf-async--grep-group)))
      (fzf-async--grep-jump r))))

(defun fzf-async--imenu (scope)
  "Implementation of `fzf-async-imenu' / `fzf-async-imenu-all'.
SCOPE selects which buffers to walk:
  nil / `current'  — just the current buffer.
  `all'            — every live non-internal buffer.
  `others'         — every live non-internal buffer except the current one.
Display differences:
- Single buffer: display = NAME (with \"(CATEGORY)\" appended on
  cross-category name collision); group header = imenu category.
- Multi buffer:  display = \"[CATEGORY] NAME\" (no collision possible —
  entries are already partitioned by buffer); group header = buffer name."
  (require 'imenu)
  (let* ((multi (memq scope '(all others)))
         (buf-vec (vconcat
                   (pcase scope
                     ((or 'all 'others)
                      (cl-remove-if
                       (lambda (b)
                         (or (minibufferp b)
                             (string-prefix-p " " (buffer-name b))
                             (and (eq scope 'others)
                                  (eq b (current-buffer)))))
                       (buffer-list)))
                     (_ (list (current-buffer))))))
         (entries nil)
         (lookup (make-hash-table :test 'equal))
         (groups (make-hash-table :test 'equal)))
    (cl-loop
     for buf across buf-vec
     for i from 0
     for index = (with-current-buffer buf
                   (ignore-errors (imenu--make-index-alist t)))
     when index do
     (cl-labels
         ((walk (alist category)
            (dolist (entry alist)
              (cond
               ((or (null entry) (equal (car entry) "*Rescan*")))
               ((imenu--subalist-p entry)
                (walk (cdr entry) (car entry)))
               (t
                (let* ((name (car entry))
                       (display
                        (if multi
                            (format "%d:%s%s"
                                    i
                                    (if category (format "[%s] " category) "")
                                    name)
                          ;; Disambiguate cross-category name collisions
                          ;; (e.g. an elisp function and variable named foo).
                          (if (and category (gethash name lookup))
                              (format "%s (%s)" name category)
                            name))))
                  (push display entries)
                  (puthash display (cons i entry) lookup)
                  (when (and (not multi) category)
                    (puthash display category groups))))))))
       (walk index nil)))
    (unless entries
      (user-error "No imenu entries%s" (if multi " in any buffer" "")))
    (when-let* ((result
                 (fzf-sync-completing-read
                  :candidates (nreverse entries)
                  :prompt (pcase scope
                            ('all    "imenu-all: ")
                            ('others "imenu-others: ")
                            (_       "imenu: "))
                  :category 'fzf-async-imenu
                  :group
                  (lambda (cand transform)
                    (cond
                     ((not multi)
                      (if transform cand (or (gethash cand groups) "")))
                     (transform
                      ;; Strip "IDX:" prefix for display.
                      (when (string-match "^[0-9]+:\\(.*\\)$" cand)
                        (match-string 1 cand)))
                     (t
                      ;; Header: reverse-map IDX → buffer name.
                      (when (string-match "^\\([0-9]+\\):" cand)
                        (let ((i (string-to-number (match-string 1 cand))))
                          (when (< i (length buf-vec))
                            (buffer-name (aref buf-vec i))))))))))
                (hit (gethash result lookup))
                (idx (car hit))
                ((< idx (length buf-vec)))
                (buffer (aref buf-vec idx))
                ((buffer-live-p buffer)))
      (unless (eq buffer (current-buffer))
        (switch-to-buffer buffer))
      (push-mark nil t)
      (imenu (cdr hit)))))

;;;###autoload
(defun fzf-async-imenu ()
  "Jump to an imenu entry in the current buffer using fzf."
  (interactive)
  (fzf-async--imenu 'current))

;;;###autoload
(defun fzf-async-imenu-all ()
  "Jump to an imenu entry across all open buffers using fzf.
Buffers without an imenu index (or whose major mode does not support
imenu) are skipped silently."
  (interactive)
  (fzf-async--imenu 'all))

;;;###autoload
(defun fzf-async-imenu-all-but-current ()
  "Jump to an imenu entry across all open buffers except the current one.
Buffers without an imenu index (or whose major mode does not support
imenu) are skipped silently."
  (interactive)
  (fzf-async--imenu 'others))

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
                      :category 'fzf-async-grep
                      :group #'fzf-async--grep-group)))
        (fzf-async--grep-jump r)))))

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

(defconst fzf-async--grep-line-regexp "\\`\\(.*?\\):\\([0-9]+\\):"
  "Lazy parser for `fzf-async-grep' category candidates.
Group 1 is SOURCE (file path or buffer name).  Group 2 is LINE.
Lazy match anchors on the first `:DIGITS:' boundary so a colon-bearing
buffer name does not corrupt the split.")

(defun fzf-async--goto-source (source line)
  "Open SOURCE and jump to LINE.
SOURCE is a file path when `file-exists-p'; otherwise it is treated as
a buffer name."
  (cond
   ((file-exists-p source) (find-file source))
   ((get-buffer source)    (switch-to-buffer source))
   (t (user-error "Source not found: %s" source)))
  (goto-char (point-min))
  (forward-line (1- line)))

(defun fzf-async--grep-jump (cand)
  "Open the SOURCE and jump to the LINE referenced by CAND.
Used both by the grep commands' selected-candidate handling and by the
embark default action for the `fzf-async-grep' category."
  (when (string-match fzf-async--grep-line-regexp cand)
    (fzf-async--goto-source
     (match-string 1 cand)
     (string-to-number (match-string 2 cand)))))

(defvar-keymap fzf-async-grep-map
  :doc "Embark keymap for `fzf-async-grep' candidates.
Composed with `embark-general-map' via `embark-keymap-alist'.")

(defun fzf-async--grep-group (cand transform)
  "Group function for FILE:LINE:CONTENT grep candidates.
TRANSFORM nil  → return the filename as the section header.
TRANSFORM non-nil → strip the filename prefix; display LINE:CONTENT only."
  (if (string-match fzf-async--grep-line-regexp cand)
      (if transform
          (substring cand (match-beginning 2))
        (match-string 1 cand))
    cand))

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
                         :category 'fzf-async-misc
                         :resolve-paths nil
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

(defconst fzf-async--categories
  '(fzf-async-misc
    fzf-async-file
    fzf-async-buffer
    fzf-async-grep
    fzf-async-bookmark
    fzf-async-theme
    fzf-async-imenu
    fzf-async-multi)
  "Completion categories owned by fzf-async.
Each is registered in `completion-category-overrides' so the
pre-scored passthrough style runs instead of style re-filtering.")

(defun fzf-async--check-completion-setup ()
  "Signal a user-error if the completion configuration is incorrect.
Guards against two misconfiguration patterns:
- `fzf-async' in the global `completion-styles' list, which applies the
  style to every completing-read and breaks callers that pass plain lists.
- fzf-async categories absent from `completion-category-overrides',
  which means the style never activates and results are silently
  re-filtered."
  (when (memq 'fzf-async completion-styles)
    (user-error
     "fzf-async must not be in `completion-styles' globally (it is).  \
Remove it and ensure `fzf-async-setup' has been called so it is wired \
via `completion-category-overrides' only"))
  (unless (and (assq 'fzf-async-misc completion-category-overrides)
               (assq 'fzf-async-file completion-category-overrides)
               (assq 'fzf-async-multi completion-category-overrides))
    (user-error
     "fzf-async categories missing from `completion-category-overrides'.  \
Call `fzf-async-setup' before using fzf-async commands")))

(defun fzf-async--bridge-defcustoms (orig-fn &rest args)
  "Wrap a fzf-native call so the C scorer sees fzf-async-* values."
  (let ((fzf-native-async-highlight  fzf-async-highlight)
        (fzf-native-max-line-length  fzf-async-max-line-length)
        (fzf-native-async-cache-size fzf-async-cache-size)
        (fzf-native-case-mode        fzf-async-case-mode))
    (apply orig-fn args)))

;;;###autoload
(defun fzf-async-setup ()
  "Register the fzf-async completion style and category overrides."
  (add-to-list 'completion-styles-alist
               '(fzf-async fzf-async-try-completion fzf-async-all-completions
                           "Passthrough style for pre-scored async fzf completions."))

  (advice-add 'fzf-native-async-start      :around #'fzf-async--bridge-defcustoms)
  (advice-add 'fzf-native-async-candidates :around #'fzf-async--bridge-defcustoms)

  ;; Register each fzf-async category with the passthrough style so other
  ;; styles (e.g. fussy on `file', `basic' on multi) don't re-filter our
  ;; pre-scored candidates or cache them client-side past the first call.
  (dolist (cat fzf-async--categories)
    (add-to-list 'completion-category-overrides `(,cat (styles fzf-async))))

  (with-eval-after-load 'embark
    (dolist (entry '((fzf-async-file     . embark-file-map)
                     (fzf-async-buffer   . embark-buffer-map)
                     (fzf-async-bookmark . embark-bookmark-map)
                     (fzf-async-grep     fzf-async-grep-map embark-general-map)))
      (add-to-list 'embark-keymap-alist entry))
    (setf (alist-get 'fzf-async-grep embark-default-action-overrides)
          #'fzf-async--grep-jump))

  (with-eval-after-load 'marginalia
    (dolist (entry '((fzf-async-file     marginalia-annotate-file     none)
                     (fzf-async-buffer   marginalia-annotate-buffer   none)
                     (fzf-async-bookmark marginalia-annotate-bookmark none)
                     (fzf-async-theme    marginalia-annotate-theme    none)
                     (fzf-async-imenu    marginalia-annotate-imenu    none)))
      (add-to-list 'marginalia-annotators entry)))

  (when fzf-async-extensions
    (when (and fzf-async--extensions-dir
               (file-directory-p fzf-async--extensions-dir))
      (add-to-list 'load-path fzf-async--extensions-dir))
    (dolist (ext fzf-async-extensions)
      (require (intern (format "fzf-async-%s" ext)))
      (let ((setup-fn (intern (format "fzf-async-%s-setup" ext))))
        (when (fboundp setup-fn) (funcall setup-fn))))))

(provide 'fzf-async)
;;; fzf-async.el ends here
