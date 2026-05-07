;;; fzf-async.el --- Async fuzzy completion via fzf-native  -*- lexical-binding: t; -*-

;; Author: James Nguyen <james@jojojames.com>
;; Package-Requires: ((emacs "29.1") (fzf-native "0.3") (cl-lib "0.5"))
;; Keywords: matching, completion, fzf, fuzzy, fussy

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

;;; Customization

(defcustom fzf-async-max-candidates 10000
  "Max candidates returned to Elisp from `fzf-async-completing-read'.
The full filtered/total counts are still tracked and shown in the prompt.
Set to nil or 0 to disable the cap (may be slow for very large result sets)."
  :type '(choice (const  :tag "No cap" nil)
                 (integer :tag "Max candidates"))
  :group 'fzf-async)

;;; Completion style

(defun fzf-async-try-completion (string _table _pred _point)
  "Try-completion for the fzf-async completion style.
Always accepts STRING as-is; scoring is done in C."
  (cons string (length string)))

(defun fzf-async-all-completions (string table pred point)
  "All-completions for the fzf-async completion style.
Passes STRING through to the collection TABLE without transformation,
preventing other styles (e.g. fussy) from re-filtering pre-scored results."
  (let* ((beforepoint (substring string 0 point))
         (afterpoint (substring string point))
         (bounds (completion-boundaries beforepoint table pred afterpoint)))
    (if t
        (funcall table string pred t)
      (when-let* ((collection (funcall table string pred t)))
        (fzf-async--highlight-collection
         (fzf-async--recreate-regex-pattern
          beforepoint afterpoint bounds)
         collection)))))

;;; Highlighting

(defun fzf-async--pcm-highlight (pattern collection)
  "Highlight with pcm-style for COLLECTION using PATTERN.

Exact copy of `fussy--pcm-highlight'."
  (completion-pcm--hilit-commonality pattern collection))

(defun fzf-async--highlight-collection (pattern collection)
  "Highlight COLLECTION using PATTERN.

Shortened version of `fussy--highlight-collection'"
  (when collection
    (fzf-async--pcm-highlight pattern collection)))

(defun fzf-async--recreate-regex-pattern (beforepoint afterpoint bounds)
  "Utility function to create regex pattern for highlighting.

Shortened version of `fussy--recreate-regex-pattern'."
  (fzf-async--make-fzf-highlight-pattern
   (concat (substring beforepoint (car bounds))
           (substring afterpoint 0 (cdr bounds)))))

(defun fzf-async--make-fzf-highlight-pattern (infix)
  "Create a pcm pattern based on fzf rules for highlighting.
INFIX is the fzf query string.

Exact Copy of `fussy-make-fzf-highlight-pattern'."
  (let ((tokens (split-string infix " " t))
        (pattern (list 'prefix)))
    (dolist (token tokens)
      (cond
       ;; inverse (skip for highlighting)
       ((string-prefix-p "!" token) nil)
       ;; OR operator (skip for highlighting to avoid breaking AND groups)
       ((string= "|" token) nil)
       ;; exact boundary-match (quoted both ends)
       ((and (string-prefix-p "'" token)
             (string-suffix-p "'" token)
             (length> token 1))
        (push 'any pattern)
        (push (substring token 1 -1) pattern))
       ;; exact-match (quoted)
       ((string-prefix-p "'" token)
        (push 'any pattern)
        (push (substring token 1) pattern))
       ;; prefix-exact-match
       ((string-prefix-p "^" token)
        (push 'any pattern)
        (push (substring token 1) pattern))
       ;; suffix-exact-match
       ((string-suffix-p "$" token)
        (push 'any pattern)
        (push (substring token 0 -1) pattern))
       ;; fuzzy
       (t
        (push 'any pattern)
        (dolist (char (append token nil))
          (push (string char) pattern)
          (push 'any pattern))
        (pop pattern)))) ;; remove last 'any
    (completion-pcm--optimize-pattern (nreverse pattern))))

;;; Completing-read

;;;###autoload
(cl-defun fzf-async-completing-read (&key
                                     (prompt "fzf > ")
                                     command
                                     (directory default-directory))
  "Run shell COMMAND and completing-read its output with async fzf scoring.

:PROMPT     Minibuffer prompt string.  Defaults to \"fzf > \".
:COMMAND    Shell command whose stdout lines become candidates.
:DIRECTORY  Working directory for COMMAND.  Defaults to `default-directory'.

The prompt overlay shows: DIR IDX/[FILTERED](TOTAL)
  DIR      — abbreviated working directory
  IDX      — current vertico selection index
  FILTERED — candidates matching the current query
  TOTAL    — total candidates collected so far"
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
         (refresh-overlay
          (lambda ()
            (when (and stats-overlay (active-minibuffer-window))
              (with-selected-window (active-minibuffer-window)
                (let ((idx (1+ (max 0 (if (boundp 'vertico--index)
                                          vertico--index 0)))))
                  (overlay-put stats-overlay 'display
                               (format "%s%s %d/[%d](%d) "
                                       prompt dir idx last-filtered last-total)))))))
         retry-timer
         timer)
    (setq timer
          (run-with-timer
           0 0.05
           (lambda ()
             (when handle
               (let ((gen (fzf-native-async-generation handle)))
                 (when (and gen (not (= gen last-gen)) (not (input-pending-p)))
                   (setq last-gen gen)
                   (run-with-idle-timer
                    0 nil
                    (lambda ()
                      (when-let* ((win (active-minibuffer-window)))
                        (with-selected-window win
                          (when (fboundp 'vertico--exhibit)
                            (setq vertico--input t)
                            (vertico--exhibit))))))))))))
    (add-hook 'post-command-hook refresh-overlay)
    (unwind-protect
        (let ((vertico-count-format nil))
          (completing-read
           prompt
           (lambda (str _pred action)
             (pcase action
               ('metadata '(metadata (category . fzf-async)
                                     (display-sort-function . identity)
                                     (cycle-sort-function . identity)))
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
                                      0.35 nil
                                      (lambda ()
                                        (setq retry-timer nil)
                                        (when-let (win (active-minibuffer-window))
                                          (with-selected-window win
                                            (when (fboundp 'vertico--exhibit)
                                              (setq vertico--input t)
                                              (vertico--exhibit))))))))
                           (when-let* ((stats (fzf-native-async-stats handle)))
                             (setq last-filtered (car stats)
                                   last-total    (cdr stats)))
                           (when-let* ((win (active-minibuffer-window)))
                             (with-selected-window win
                               (unless stats-overlay
                                 (setq stats-overlay
                                       (make-overlay (point-min) (minibuffer-prompt-end))))
                               (funcall refresh-overlay)))
                           (setq last-query query
                                 last-result r))
                         (when (equal query last-query) last-result)))))
               (_ t)))))
      (cancel-timer timer)
      (when retry-timer (cancel-timer retry-timer))
      (remove-hook 'post-command-hook refresh-overlay)
      (when stats-overlay (delete-overlay stats-overlay))
      (when handle (fzf-native-async-stop handle)))))

;;; Commands

;;;###autoload
(defun fzf-async-find ()
  "Find a file under `default-directory' using find and async fzf scoring."
  (interactive)
  (when-let* ((result (fzf-async-completing-read
                       :prompt "find: "
                       :command (fzf-async--normalize "find .")
                       :directory default-directory)))
    (find-file result)))

;;;###autoload
(defun fzf-async-fd ()
  "Find a file under `default-directory' using fd and async fzf scoring."
  (interactive)
  (when-let* ((result (fzf-async-completing-read
                       :prompt "fd: "
                       :command (fzf-async--normalize "fd --no-ignore")
                       :directory default-directory)))
    (find-file result)))

;;;###autoload
(defun fzf-async-rg-files ()
  "Find a file under `default-directory' using rg --files and async fzf scoring."
  (interactive)
  (when-let* ((result (fzf-async-completing-read
                       :prompt "rg files: "
                       :command (fzf-async--normalize "rg --files")
                       :directory default-directory)))
    (find-file result)))

;;;###autoload
(defun fzf-async-ag-files ()
  "Find a file under `default-directory' using rg --files and async fzf scoring."
  (interactive)
  (when-let* ((result (fzf-async-completing-read
                       :prompt "ag files: "
                       :command (fzf-async--normalize "ag -g .")
                       :directory default-directory)))
    (find-file result)))

;;;###autoload
(defun fzf-async-rg ()
  "Search file contents under `default-directory' with rg and async fzf scoring.
Streams all file contents as FILE:LINE:CONTENT; type to fuzzy-filter across them.
Selecting a candidate opens the file at that line."
  (interactive)
  (when-let* ((r (fzf-async-completing-read
                  :prompt "rg: "
                  :command (fzf-async--normalize
                            "rg  --line-number --no-heading --with-filename ''")
                  :directory default-directory))
              (match (string-match "\\(.*\\):\\([0-9]+\\):" r))
              (file (match-string 1 r))
              (line (string-to-number (match-string 2 r))))
    (find-file file)
    (goto-char (point-min))
    (forward-line (1- line))))

;;;###autoload
(defun fzf-async-ag ()
  "Search file contents under `default-directory' with rg and async fzf scoring.
Streams all file contents as FILE:LINE:CONTENT; type to fuzzy-filter across them.
Selecting a candidate opens the file at that line."
  (interactive)
  (when-let* ((r (fzf-async-completing-read
                  :prompt "ag: "
                  :command (fzf-async--normalize
                            "ag --nocolor --nogroup --line-number \".\"")
                  :directory default-directory))
              (match (string-match "\\(.*\\):\\([0-9]+\\):" r))
              (file (match-string 1 r))
              (line (string-to-number (match-string 2 r))))
    (find-file file)
    (goto-char (point-min))
    (forward-line (1- line))))

;;;###autoload
(defun fzf-async-git-grep ()
  "Search file contents under `default-directory' with git grep and async fzf scoring.
Streams all file contents as FILE:LINE:CONTENT; type to fuzzy-filter across them.
Selecting a candidate opens the file at that line."
  (interactive)
  (unless (locate-dominating-file default-directory ".git")
    (error "Not a Git repo"))
  (when-let* ((r (fzf-async-completing-read
                  :prompt "git grep: "
                  :command (fzf-async--normalize "git --no-pager grep -n \"\"")
                  :directory default-directory))
              (match (string-match "\\(.*\\):\\([0-9]+\\):" r))
              (file (match-string 1 r))
              (line (string-to-number (match-string 2 r))))
    (find-file file)
    (goto-char (point-min))
    (forward-line (1- line))))

;;;###autoload
(defun fzf-async-grep ()
  "Search file contents under `default-directory' with git grep and async fzf scoring.
Streams all file contents as FILE:LINE:CONTENT; type to fuzzy-filter across them.
Selecting a candidate opens the file at that line."
  (interactive)
  (when-let* ((r (fzf-async-completing-read
                  :prompt "grep: "
                  :command (fzf-async--normalize "grep -Rn ''")
                  :directory default-directory))
              (match (string-match "\\(.*\\):\\([0-9]+\\):" r))
              (file (match-string 1 r))
              (line (string-to-number (match-string 2 r))))
    (find-file file)
    (goto-char (point-min))
    (forward-line (1- line))))

;;;###autoload
(defun fzf-async-grep-current-file ()
  "Search file contents under `default-directory' with git grep and async fzf scoring.
Streams all file contents as FILE:LINE:CONTENT; type to fuzzy-filter across them.
Selecting a candidate opens the file at that line."
  (interactive)
  (when-let* ((bf buffer-file-name) ;; Track buffer.
              (r (fzf-async-completing-read
                  :prompt "grep: "
                  :command (fzf-async--normalize
                            (format "grep -vn '^[[:space:]]*$' %s"
                                    buffer-file-name))
                  :directory default-directory))
              (match (string-match "^\\([0-9]+\\):\\(.*\\)$" r))
              (line (string-to-number (match-string 1 r))))
    (find-file bf) ;; In case they swapped windows.
    (goto-char (point-min))
    (forward-line (1- line))))

;;;###autoload
(defun fzf-async-ugrep ()
  "Search file contents under `default-directory' with git grep and async fzf scoring.
Streams all file contents as FILE:LINE:CONTENT; type to fuzzy-filter across them.
Selecting a candidate opens the file at that line."
  (interactive)
  (when-let* ((r (fzf-async-completing-read
                  :prompt "ugrep: "
                  :command (fzf-async--normalize "ugrep -RIn --no-heading ''")
                  :directory default-directory))
              (match (string-match "\\(.*\\):\\([0-9]+\\):" r))
              (file (match-string 1 r))
              (line (string-to-number (match-string 2 r))))
    (find-file file)
    (goto-char (point-min))
    (forward-line (1- line))))

;;;###autoload
(defun fzf-async-git-ls-files ()
  "Search file contents under `default-directory' with git ls files and async fzf scoring.
Streams all file contents as FILE:LINE:CONTENT; type to fuzzy-filter across them.
Selecting a candidate opens the file at that line."
  (interactive)
  (unless (locate-dominating-file default-directory ".git")
    (error "Not a Git repo"))
  (when-let* ((result (fzf-async-completing-read
                       :prompt "git ls files: "
                       :command (fzf-async--normalize "git ls-files")
                       :directory default-directory)))
    (find-file result)))

;;;###autoload
(defun fzf-async-locate ()
  "Search file contents under `default-directory' with git ls files and async fzf scoring.
Streams all file contents as FILE:LINE:CONTENT; type to fuzzy-filter across them.
Selecting a candidate opens the file at that line."
  (interactive)
  (when-let* ((result (fzf-async-completing-read
                       :prompt "locate: "
                       :command (fzf-async--normalize "locate ''")
                       :directory default-directory)))
    (find-file result)))

;;;###autoload
(defun fzf-async-spotlight ()
  "Search file contents under `default-directory' with git ls files and async fzf scoring.
Streams all file contents as FILE:LINE:CONTENT; type to fuzzy-filter across them.
Selecting a candidate opens the file at that line."
  (interactive)
  (when-let* ((result (fzf-async-completing-read
                       :prompt "spotlight: "
                       :command (fzf-async--normalize "mdfind .")
                       ;; :command (fzf-async--normalize "mdfind . 2>/dev/null")
                       :directory default-directory)))
    (if (string-suffix-p ".app" result)
        (start-process "default-app" nil "open" result)
      (find-file result))))

;;;###autoload
(defun fzf-async-spotlight-apps ()
  "Search file contents under `default-directory' with git ls files and async fzf scoring.
Streams all file contents as FILE:LINE:CONTENT; type to fuzzy-filter across them.
Selecting a candidate opens the file at that line."
  (interactive)
  (when-let* ((result (fzf-async-completing-read
                       :prompt "spotlight: "
                       :command (format "%s 'kMDItemFSName == \"*.app\"'"
                                        (executable-find "mdfind"))
                       :directory default-directory)))
    (start-process "default-app" nil "open" result)))

;;;###autoload
(defun fzf-async-spotlight-pdfs ()
  "Search file contents under `default-directory' with git ls files and async fzf scoring.
Streams all file contents as FILE:LINE:CONTENT; type to fuzzy-filter across them.
Selecting a candidate opens the file at that line."
  (interactive)
  (when-let* ((result (fzf-async-completing-read
                       :prompt "spotlight: "
                       :command (format "mdfind 'kMDItemFSName == \"*.pdf\"'"
                                        (executable-find "mdfind"))
                       :directory default-directory)))
    (start-process "default-app" nil "open" result)))

;;;###autoload
(defun fzf-async-swiper ()
  "Search file contents under `default-directory' with git ls files and async fzf scoring.
Streams all file contents as FILE:LINE:CONTENT; type to fuzzy-filter across them.
Selecting a candidate opens the file at that line."
  (interactive)
  (swiper))

;;;###autoload
(defun fzf-async-swiper-all ()
  "Search file contents under `default-directory' with git ls files and async fzf scoring.
Streams all file contents as FILE:LINE:CONTENT; type to fuzzy-filter across them.
Selecting a candidate opens the file at that line."
  (interactive)
  (swiper-all))

;;; Helpers

(defun fzf-async--normalize (command)
  "Normalize COMMAND.

Run `executable-find' on COMMAND to find real path.
Shell quote arguments."
  (let* ((parts (split-string-and-unquote command))
         (program (car parts))
         (args (mapcar
                (lambda (arg)
                  (if (string= arg "''")
                      ""
                    arg))
                (cdr parts)))
         (exe (or (executable-find program)
                  (user-error "%s not found in exec-path" program))))
    (mapconcat #'shell-quote-argument
               (cons exe args)
               " ")))

;;; Setup

;;;###autoload
(defun fzf-async-setup ()
  "Register the fzf-async completion style and category override.
Call this once during init before using `fzf-async-completing-read'."
  (add-to-list 'completion-styles-alist
               '(fzf-async fzf-async-try-completion fzf-async-all-completions
                           "Passthrough style for pre-scored async fzf completions."))
  (add-to-list 'completion-category-overrides
               '(fzf-async (styles fzf-async)))

  (dolist (command '(fzf-async-find
                     fzf-async-fd
                     fzf-async-rg-files
                     fzf-async-ag-files
                     fzf-async-git-ls-files
                     fzf-async-locate
                     fzf-async-spotlight))
    ;; (with-eval-after-load 'marginalia
    ;;   (push `(,command . file)
    ;;         marginalia-command-categories))
    (with-eval-after-load 'embark
      (add-to-list 'embark-keymap-alist
                   `(,command . embark-file-map)))))

;; (transient-define-prefix matcha-fzf-async ()
;;   "fzf"
;;   [["Find Files"
;;     ("f" "Find" fzf-async-find)
;;     ("d" "Fd" fzf-async-fd)
;;     ("r" "Rg" fzf-async-rg-files)
;;     ("a" "Ag" fzf-async-ag-files)]
;;    ["Git"
;;     ("g" "Git Ls-Files" fzf-async-git-ls-files)
;;     ("G" "Git Grep" fzf-async-git-grep)]
;;    ["Grep"
;;     ("F" "Grep Current File" fzf-async-grep-current-file)
;;     ("g" "Grep" fzf-async-grep)
;;     ("R" "Rg" fzf-async-rg)
;;     ("A" "Ag" fzf-async-ag)]
;;    ["Search"
;;     ("l" "Locate" fzf-async-locate)
;;     ("s" "Spotlight" fzf-async-spotlight)]
;;    ["Swiper"
;;     ("w" "Swiper" fzf-async-swiper)
;;     ("W" "Swiper All" fzf-async-swiper-all)]])

(provide 'fzf-async)
;;; fzf-async.el ends here
