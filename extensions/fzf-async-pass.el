;;; fzf-async-pass.el --- fzf-async interface for pass -*- lexical-binding: t; -*-

;; Author: James Nguyen <james@jojojames.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (fzf-async "1.0"))
;; Keywords: pass, password, convenience
;; Homepage: https://github.com/jojojames/fzf-async

;;; Commentary:

;; fzf-async interface to `password-store' (pass), modeled on `ivy-pass'.
;;
;; Loaded automatically when `pass' is in `fzf-async-extensions' (the
;; default) and `fzf-async-setup' has been called.  Requires the
;; `password-store' package to be installed and available on `load-path';
;; it is loaded lazily on first use.
;;
;; M-x fzf-async-pass copies the selected entry's password to the kill ring.
;; With embark configured, these actions are available on a candidate:
;;
;;   c  copy password         (`fzf-async-pass-copy')
;;   e  edit                  (`fzf-async-pass-edit')
;;   d  delete                (`fzf-async-pass-delete')
;;   a  add (using as seed)   (`fzf-async-pass-add')
;;   r  rename                (`fzf-async-pass-rename')
;;   g  generate              (`fzf-async-pass-generate')
;;   u  open url field        (`fzf-async-pass-url')

;;; Code:

(require 'fzf-async)

(defvar embark-keymap-alist)
(defvar embark-general-map)

(declare-function password-store-list      "password-store" (&optional subdir))
(declare-function password-store-dir       "password-store" ())
(declare-function password-store-copy      "password-store" (entry))
(declare-function password-store-edit      "password-store" (entry))
(declare-function password-store-rename    "password-store" (entry new-entry))
(declare-function password-store-remove    "password-store" (entry))
(declare-function password-store-generate  "password-store" (entry &optional password-length))
(declare-function password-store-url       "password-store" (entry))

(defun fzf-async-pass--read (prompt)
  "Fuzzy-select a password-store entry with PROMPT."
  (require 'password-store)
  (let ((entries (password-store-list (password-store-dir))))
    (unless entries
      (user-error "No password-store entries found"))
    (fzf-sync-completing-read
     :candidates entries
     :prompt prompt
     :category 'fzf-async-pass)))

;;;###autoload
(defun fzf-async-pass-copy (key)
  "Copy the password for KEY to the kill ring."
  (interactive (list (fzf-async-pass--read "Copy password: ")))
  (password-store-copy key))

;;;###autoload
(defalias 'fzf-async-pass #'fzf-async-pass-copy
  "Default `fzf-async-pass' action: copy the password to the kill ring.")

;;;###autoload
(defun fzf-async-pass-edit (key)
  "Edit password-store entry KEY."
  (interactive (list (fzf-async-pass--read "Edit entry: ")))
  (password-store-edit key))

;;;###autoload
(defun fzf-async-pass-rename (key)
  "Rename password-store entry KEY."
  (interactive (list (fzf-async-pass--read "Rename entry: ")))
  (password-store-rename
   key (read-string (format "Rename `%s' to: " key) key)))

;;;###autoload
(defun fzf-async-pass-delete (key)
  "Delete password-store entry KEY, after confirmation."
  (interactive (list (fzf-async-pass--read "Delete entry: ")))
  (when (yes-or-no-p (format "Really delete the entry `%s'? " key))
    (password-store-remove key)))

;;;###autoload
(defun fzf-async-pass-add (&optional seed)
  "Add a new password-store entry, optionally seeded by SEED."
  (interactive)
  (require 'password-store)
  (password-store-edit (read-string "New entry: " seed)))

;;;###autoload
(defun fzf-async-pass-generate (&optional seed)
  "Generate a new password-store entry, optionally seeded by SEED."
  (interactive)
  (require 'password-store)
  (let ((new (read-string "Generate password for new entry: " seed)))
    (password-store-generate new)
    (password-store-edit new)))

;;;###autoload
(defun fzf-async-pass-url (key)
  "Open the url field of password-store entry KEY."
  (interactive (list (fzf-async-pass--read "URL of entry: ")))
  (password-store-url key))

(defvar-keymap fzf-async-pass-map
  :doc "Embark keymap for `fzf-async-pass' candidates.
Composed with `embark-general-map' via `embark-keymap-alist'."
  "c" #'fzf-async-pass-copy
  "e" #'fzf-async-pass-edit
  "d" #'fzf-async-pass-delete
  "a" #'fzf-async-pass-add
  "r" #'fzf-async-pass-rename
  "g" #'fzf-async-pass-generate
  "u" #'fzf-async-pass-url)

;;;###autoload
(defun fzf-async-pass-setup ()
  "Register the `fzf-async-pass' completion category and embark keymap."
  (add-to-list 'completion-category-overrides
               '(fzf-async-pass (styles fzf-async)))
  (with-eval-after-load 'embark
    (add-to-list 'embark-keymap-alist
                 '(fzf-async-pass fzf-async-pass-map embark-general-map))))

(provide 'fzf-async-pass)
;;; fzf-async-pass.el ends here
