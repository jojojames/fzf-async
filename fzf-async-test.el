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
