;;; fzf-async-test.el --- Tests for fzf-async  -*- lexical-binding: t; -*-
(require 'ert)
(require 'fzf-async)

;;; fzf-async--deduplicate-dirs

(ert-deftest fzf-async-deduplicate-dirs-no-overlap ()
  "Unrelated directories are all kept."
  (should (equal (sort (fzf-async--deduplicate-dirs
                        '("/a/b/" "/c/d/" "/e/f/"))
                       #'string<)
                 '("/a/b/" "/c/d/" "/e/f/"))))

(ert-deftest fzf-async-deduplicate-dirs-drops-subdirectory ()
  "A subdirectory is dropped when its parent is present."
  (should (equal (fzf-async--deduplicate-dirs
                  '("/home/user/project/" "/home/user/project/src/"))
                 '("/home/user/project/"))))

(ert-deftest fzf-async-deduplicate-dirs-keeps-sibling-dirs ()
  "Sibling directories (same parent, different names) are both kept."
  (let ((result (fzf-async--deduplicate-dirs
                 '("/home/user/foo/" "/home/user/bar/"))))
    (should (member "/home/user/foo/" result))
    (should (member "/home/user/bar/" result))))

(ert-deftest fzf-async-deduplicate-dirs-removes-exact-duplicates ()
  "Exact duplicate entries are collapsed to one."
  (should (equal (fzf-async--deduplicate-dirs
                  '("/a/b/" "/a/b/" "/a/b/"))
                 '("/a/b/"))))

(ert-deftest fzf-async-deduplicate-dirs-deep-nesting ()
  "Only the shallowest ancestor survives when multiple levels are present."
  (let ((result (fzf-async--deduplicate-dirs
                 '("/a/" "/a/b/" "/a/b/c/" "/a/b/c/d/"))))
    (should (equal result '("/a/")))))

(ert-deftest fzf-async-deduplicate-dirs-empty-input ()
  "Empty input returns nil."
  (should (null (fzf-async--deduplicate-dirs '()))))

;;; fzf-async--default-dir

(ert-deftest fzf-async-project-dir-nil-backend-returns-default-directory ()
  "With nil backend, returns `default-directory'."
  (let ((fzf-async-project-backend nil)
        (default-directory "/some/dir/"))
    (should (string= (fzf-async--default-dir) "/some/dir/"))))

(ert-deftest fzf-async-project-dir-project-backend-uses-project-root ()
  "With `project' backend, returns the project root when in a project."
  (let ((fzf-async-project-backend 'project))
    (cl-letf (((symbol-function 'project-current)
               (lambda (&rest _) '(vc Git "/mock/project/")))
              ((symbol-function 'project-root)
               (lambda (_) "/mock/project/")))
      (should (string= (fzf-async--default-dir) "/mock/project/")))))

(ert-deftest fzf-async-project-dir-project-backend-fallback ()
  "With `project' backend, falls back to `default-directory' outside a project."
  (let ((fzf-async-project-backend 'project)
        (default-directory "/fallback/"))
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
      (should (string= (fzf-async--default-dir) "/fallback/")))))

(ert-deftest fzf-async-project-dir-custom-function ()
  "A function value is called and its return value used."
  (let ((fzf-async-project-backend (lambda () "/custom/root/")))
    (should (string= (fzf-async--default-dir) "/custom/root/"))))

;;; fzf-async-swiper line collection

(ert-deftest fzf-async-swiper-line-format ()
  "Lines are formatted as LINE:content with 1-based numbering."
  (with-temp-buffer
    (insert "alpha\nbeta\ngamma\n")
    (let* ((candidates
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
              (nreverse lines))))
      (should (equal candidates '("1:alpha" "2:beta" "3:gamma"))))))

(ert-deftest fzf-async-swiper-skips-empty-lines ()
  "Empty lines are excluded from candidates."
  (with-temp-buffer
    (insert "first\n\nthird\n")
    (let* ((candidates
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
              (nreverse lines))))
      (should (equal candidates '("1:first" "3:third"))))))

;;; fzf-async-tramp (ssh-hosts via :extract)

(defun fzf-async-test--extract (cmd)
  "Run CMD under the multi `:extract' mode and return the captured plist.
Returns nil if CMD completes without invoking a fzf completing-read."
  (let ((fzf-async--multi-mode :extract))
    (catch 'fzf-async-extracted
      (funcall cmd)
      nil)))

(defmacro fzf-async-test--with-ssh-config (content &rest body)
  "Run BODY with a temp file containing CONTENT as the ssh config.
Mocks `expand-file-name' so `fzf-async-tramp' reads the temp file."
  (declare (indent 1))
  `(let ((tmpfile (make-temp-file "fzf-async-test-ssh-config")))
     (unwind-protect
         (progn
           (with-temp-file tmpfile (insert ,content))
           (cl-letf (((symbol-function 'expand-file-name)
                      (lambda (&rest _) tmpfile)))
             ,@body))
       (delete-file tmpfile))))

(ert-deftest fzf-async-tramp-hosts-basic ()
  "Parses plain Host entries from ~/.ssh/config."
  (fzf-async-test--with-ssh-config
      "Host foo\n  HostName foo.example.com\nHost bar\n"
    (let ((args (fzf-async-test--extract #'fzf-async-tramp)))
      (should (equal (plist-get args :items) '("foo" "bar"))))))

(ert-deftest fzf-async-tramp-hosts-skips-wildcards ()
  "Wildcard Host patterns (*, ?, !) are excluded."
  (fzf-async-test--with-ssh-config
      "Host *\nHost prod\nHost *.internal\nHost dev\n"
    (let ((args (fzf-async-test--extract #'fzf-async-tramp)))
      (should (equal (plist-get args :items) '("prod" "dev"))))))

(ert-deftest fzf-async-tramp-hosts-multiple-on-one-line ()
  "Multiple hosts on a single Host line are each returned."
  (fzf-async-test--with-ssh-config
      "Host alpha beta gamma\n"
    (let ((args (fzf-async-test--extract #'fzf-async-tramp)))
      (should (equal (plist-get args :items)
                     '("alpha" "beta" "gamma"))))))

(ert-deftest fzf-async-tramp-missing-config ()
  "Signals a `user-error' when ~/.ssh/config does not exist."
  (cl-letf (((symbol-function 'file-readable-p) (lambda (_) nil)))
    (should-error (fzf-async-test--extract #'fzf-async-tramp)
                  :type 'user-error)))

;;; fzf-async-swiper-all (sanitize via :extract)

(ert-deftest fzf-async-swiper-all-sanitizes-buffer-names ()
  "Buffer-name characters /\\*?<>|: and space become hyphens in candidates."
  (let ((buf (generate-new-buffer " *fzf-async-test-src*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (rename-buffer "test:buf with space/x" t)
            (insert "hello\n"))
          (cl-letf (((symbol-function 'buffer-list) (lambda () (list buf))))
            (let* ((args (fzf-async-test--extract #'fzf-async-swiper-all))
                   (cands (plist-get args :items)))
              ;; Buffer name "test:buf with space/x" should sanitize to
              ;; "test-buf-with-space-x"; candidate prefix is "0-<name>:".
              (should (cl-some
                       (lambda (c)
                         (string-prefix-p "0-test-buf-with-space-x:" c))
                       cands)))))
      (kill-buffer buf))))

(provide 'fzf-async-test)
;;; fzf-async-test.el ends here
