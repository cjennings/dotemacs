;;; test-host-environment-match-localtime.el --- Tests for cj/match-localtime-to-zoneinfo -*- lexical-binding: t; -*-

;;; Commentary:
;; Sibling tests cover the predicates and `cj/detect-system-timezone'.
;; `cj/match-localtime-to-zoneinfo' (the file-comparison path that's
;; tried first by detect-system-timezone) was uncovered.
;;
;; The function hits two hard-coded paths (`/etc/localtime' and
;; `/usr/share/zoneinfo').  Stubbing `file-exists-p' / `file-directory-p'
;; via `cl-letf' on a native-comp build of Emacs triggers trampoline
;; install errors, so we let the function touch the real filesystem
;; and only assert on the contract (string-or-nil, string lives under
;; the zoneinfo directory).

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'host-environment)

;;; cj/match-localtime-to-zoneinfo

(ert-deftest test-host-environment-match-localtime-returns-string-or-nil ()
  "Normal: the function returns either a string (a zoneinfo-relative path)
or nil.  On any reasonable Linux box `/etc/localtime' is a copy of one
of the files under `/usr/share/zoneinfo', so we expect a string here;
on systems missing either path we still want the function to not
crash, so accept nil as well."
  (let ((result (cj/match-localtime-to-zoneinfo)))
    (should (or (null result) (stringp result)))
    (when (stringp result)
      ;; The returned path should have its prefix stripped — no leading
      ;; "/usr/share/zoneinfo/" in the result.
      (should-not (string-prefix-p "/usr/share/zoneinfo/" result))
      ;; And the resolved file under zoneinfo should actually exist.
      (should (file-exists-p (expand-file-name result "/usr/share/zoneinfo"))))))

(ert-deftest test-host-environment-match-localtime-result-matches-localtime-bytes ()
  "Normal: when the function returns a path, its file content matches
`/etc/localtime' byte-for-byte (that's the function's whole job)."
  (skip-unless (and (file-exists-p "/etc/localtime")
                    (file-directory-p "/usr/share/zoneinfo")))
  (let ((result (cj/match-localtime-to-zoneinfo)))
    (when (stringp result)
      (let ((matched-path (expand-file-name result "/usr/share/zoneinfo"))
            (read-bytes
             (lambda (p)
               (with-temp-buffer
                 (set-buffer-multibyte nil)
                 (insert-file-contents-literally p)
                 (buffer-string)))))
        (should (equal (funcall read-bytes "/etc/localtime")
                       (funcall read-bytes matched-path)))))))

(provide 'test-host-environment-match-localtime)
;;; test-host-environment-match-localtime.el ends here
