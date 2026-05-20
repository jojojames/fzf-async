;;; fzf-async-spotlight.el --- Spotlight (mdfind) integration for fzf-async -*- lexical-binding: t; -*-

;; Author: James Nguyen <james@jojojames.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (fzf-async "1.0"))
;; Keywords: convenience, files, matching
;; Homepage: https://github.com/jojojames/fzf-async

;;; Commentary:

;; macOS Spotlight (mdfind) commands for fzf-async.
;;
;; Loaded automatically when `spotlight' is in `fzf-async-extensions'
;; (the default) and `fzf-async-setup' has been called.  No setup
;; function is registered — the commands are usable immediately.
;;
;; Commands:
;;   `fzf-async-spotlight'        Find any indexed file or .app bundle
;;   `fzf-async-spotlight-apps'   Find an installed application
;;   `fzf-async-spotlight-audio'  Find audio and play it with the default app

;;; Code:

(require 'fzf-async)

(defcustom fzf-async-spotlight-audio-directories
  '("~/Music" "~/Downloads" "~/Desktop")
  "Directories searched by `fzf-async-spotlight-audio'.
Each directory is passed to `mdfind -onlyin'; results are concatenated.
Set to nil to search the whole index."
  :type '(repeat directory)
  :group 'fzf-async)

;;;###autoload
(defun fzf-async-spotlight ()
  "Find a file system-wide using Spotlight (mdfind).
.app bundles are opened with `open'; all other results open with `find-file'."
  (interactive)
  (when-let* ((result (fzf-async-completing-read
                       :prompt "spotlight: "
                       :command "mdfind 'kMDItemFSName != \"\"'")))
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

(provide 'fzf-async-spotlight)
;;; fzf-async-spotlight.el ends here
