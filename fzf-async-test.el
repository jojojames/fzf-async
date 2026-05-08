;;; fzf-async-test.el --- Tests for fzf-async  -*- lexical-binding: t; -*-
(require 'ert)
(require 'fzf-async)

;;; fzf-async--normalize

;; Mock executable-find so tests are hermetic and don't depend on PATH.
(defmacro fzf-async-test--with-mock-exe (&rest body)
  "Run BODY with `executable-find' returning /mock/bin/<program>."
  `(cl-letf (((symbol-function 'executable-find)
               (lambda (prog) (concat "/mock/bin/" prog))))
     ,@body))

(ert-deftest fzf-async-normalize-resolves-executable ()
  "The first token is replaced with the full executable path."
  (fzf-async-test--with-mock-exe
   (should (string-prefix-p "/mock/bin/git"
                             (fzf-async--normalize "git ls-files")))))

(ert-deftest fzf-async-normalize-preserves-args ()
  "Arguments after the program name are preserved."
  (fzf-async-test--with-mock-exe
   (let ((result (fzf-async--normalize "git --no-pager log")))
     (should (string-match-p "--no-pager" result))
     (should (string-match-p "log" result)))))

(ert-deftest fzf-async-normalize-empty-arg-single-quotes ()
  "A bare '' argument becomes a shell-quoted empty string in output."
  (fzf-async-test--with-mock-exe
   (let ((result (fzf-async--normalize "grep -Rn ''")))
     ;; shell-quote-argument on "" produces ''
     (should (string-match-p "''" result)))))

(ert-deftest fzf-async-normalize-empty-arg-double-quotes ()
  "A bare \"\" argument becomes a shell-quoted empty string in output."
  (fzf-async-test--with-mock-exe
   (let ((result (fzf-async--normalize "git --no-pager grep -n \"\"")))
     (should (string-match-p "''" result)))))

(ert-deftest fzf-async-normalize-single-and-double-quote-equivalent ()
  "'' and \"\" sentinels produce identical normalized output."
  (fzf-async-test--with-mock-exe
   (should (string= (fzf-async--normalize "git --no-pager grep -n ''")
                    (fzf-async--normalize "git --no-pager grep -n \"\"")))))

(ert-deftest fzf-async-normalize-shell-quotes-special-chars ()
  "Arguments with spaces or special characters are shell-quoted."
  (fzf-async-test--with-mock-exe
   (let ((result (fzf-async--normalize "find . -name '*.el'")))
     ;; *.el must appear quoted in the output
     (should (string-match-p "\\*\\.el" result)))))

(ert-deftest fzf-async-normalize-unknown-program-errors ()
  "An unknown program signals a user-error."
  (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
    (should-error (fzf-async--normalize "nonexistent-prog-xyz")
                  :type 'user-error)))

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

;;; fzf-async--sanitize-filename

(ert-deftest fzf-async-sanitize-filename-replaces-slash ()
  (should (string= (fzf-async--sanitize-filename "a/b") "a-b")))

(ert-deftest fzf-async-sanitize-filename-replaces-space ()
  (should (string= (fzf-async--sanitize-filename "foo bar") "foo-bar")))

(ert-deftest fzf-async-sanitize-filename-replaces-colon ()
  (should (string= (fzf-async--sanitize-filename "buf:name") "buf-name")))

(ert-deftest fzf-async-sanitize-filename-plain-name-unchanged ()
  (should (string= (fzf-async--sanitize-filename "plain-name") "plain-name")))

;;; fzf-async--project-dir

(ert-deftest fzf-async-project-dir-nil-backend-returns-default-directory ()
  "With nil backend, returns `default-directory'."
  (let ((fzf-async-project-backend nil)
        (default-directory "/some/dir/"))
    (should (string= (fzf-async--project-dir) "/some/dir/"))))

(ert-deftest fzf-async-project-dir-project-backend-uses-project-root ()
  "With `project' backend, returns the project root when in a project."
  (let ((fzf-async-project-backend 'project))
    (cl-letf (((symbol-function 'project-current)
               (lambda (&rest _) '(vc Git "/mock/project/")))
              ((symbol-function 'project-root)
               (lambda (_) "/mock/project/")))
      (should (string= (fzf-async--project-dir) "/mock/project/")))))

(ert-deftest fzf-async-project-dir-project-backend-fallback ()
  "With `project' backend, falls back to `default-directory' outside a project."
  (let ((fzf-async-project-backend 'project)
        (default-directory "/fallback/"))
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
      (should (string= (fzf-async--project-dir) "/fallback/")))))

(ert-deftest fzf-async-project-dir-custom-function ()
  "A function value is called and its return value used."
  (let ((fzf-async-project-backend (lambda () "/custom/root/")))
    (should (string= (fzf-async--project-dir) "/custom/root/"))))

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

;;; fzf-async--ssh-hosts

(defmacro fzf-async-test--with-ssh-config (content &rest body)
  "Run BODY with a temp file containing CONTENT as the ssh config."
  (declare (indent 1))
  `(let ((tmpfile (make-temp-file "fzf-async-test-ssh-config")))
     (unwind-protect
         (progn
           (with-temp-file tmpfile (insert ,content))
           (cl-letf (((symbol-function 'expand-file-name)
                      (lambda (&rest _) tmpfile)))
             ,@body))
       (delete-file tmpfile))))

(ert-deftest fzf-async-ssh-hosts-basic ()
  "Parses plain Host entries."
  (fzf-async-test--with-ssh-config
      "Host foo\n  HostName foo.example.com\nHost bar\n"
    (should (equal (fzf-async--ssh-hosts) '("foo" "bar")))))

(ert-deftest fzf-async-ssh-hosts-skips-wildcards ()
  "Wildcard Host patterns (*, ?, !) are excluded."
  (fzf-async-test--with-ssh-config
      "Host *\nHost prod\nHost *.internal\nHost dev\n"
    (should (equal (fzf-async--ssh-hosts) '("prod" "dev")))))

(ert-deftest fzf-async-ssh-hosts-multiple-on-one-line ()
  "Multiple hosts on a single Host line are each returned."
  (fzf-async-test--with-ssh-config
      "Host alpha beta gamma\n"
    (should (equal (fzf-async--ssh-hosts) '("alpha" "beta" "gamma")))))

(ert-deftest fzf-async-ssh-hosts-missing-config ()
  "Returns nil when ~/.ssh/config does not exist."
  (cl-letf (((symbol-function 'file-readable-p) (lambda (_) nil)))
    (should (null (fzf-async--ssh-hosts)))))

(provide 'fzf-async-test)
;;; fzf-async-test.el ends here
