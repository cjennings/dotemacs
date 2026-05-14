;;; test-custom-buffer-file-print-diff-eww.el --- Tests for the print/diff/eww/email helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Sibling tests cover the move/rename/copy/clear family.  This file
;; fills in the remaining helpers and dispatchers:
;;
;;   cj/print--resolve-spooler
;;   cj/copy-buffer-name
;;   cj/--diff-with-regular-diff
;;   cj/diff-buffer-with-file
;;   cj/view-buffer-in-eww
;;   cj/--email-handle-is-type-p
;;
;; External processes (`call-process', `executable-find', `lpr', `lp',
;; `difft', `diff', `eww-open-file') are stubbed so the tests don't
;; require any of those tools.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'custom-buffer-file)

;;; ---------- cj/print--resolve-spooler ----------

(ert-deftest test-cbf-resolve-spooler-explicit-string-and-on-path ()
  "Normal: explicit string spooler on PATH is returned verbatim."
  (let ((cj/print-spooler-command "lpr")
        (cj/print--spooler-cache nil))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (cmd) (when (equal cmd "lpr") "/usr/bin/lpr"))))
      (should (equal (cj/print--resolve-spooler) "lpr")))))

(ert-deftest test-cbf-resolve-spooler-explicit-string-missing-errors ()
  "Error: explicit string spooler not on PATH signals user-error."
  (let ((cj/print-spooler-command "notathing")
        (cj/print--spooler-cache nil))
    (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
      (should-error (cj/print--resolve-spooler) :type 'user-error))))

(ert-deftest test-cbf-resolve-spooler-auto-detects-lpr-first ()
  "Normal: `auto' prefers lpr over lp and caches the result."
  (let ((cj/print-spooler-command 'auto)
        (cj/print--spooler-cache nil))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (cmd) (when (equal cmd "lpr") "/usr/bin/lpr"))))
      (should (equal (cj/print--resolve-spooler) "lpr"))
      (should (equal cj/print--spooler-cache "lpr")))))

(ert-deftest test-cbf-resolve-spooler-auto-falls-back-to-lp ()
  "Boundary: when lpr is missing but lp is on PATH, lp wins."
  (let ((cj/print-spooler-command 'auto)
        (cj/print--spooler-cache nil))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (cmd) (when (equal cmd "lp") "/usr/bin/lp"))))
      (should (equal (cj/print--resolve-spooler) "lp")))))

(ert-deftest test-cbf-resolve-spooler-auto-no-tool-errors ()
  "Error: `auto' with neither lpr nor lp signals user-error."
  (let ((cj/print-spooler-command 'auto)
        (cj/print--spooler-cache nil))
    (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
      (should-error (cj/print--resolve-spooler) :type 'user-error))))

(ert-deftest test-cbf-resolve-spooler-auto-returns-cached-value ()
  "Boundary: a non-nil cache short-circuits the auto-detect."
  (let ((cj/print-spooler-command 'auto)
        (cj/print--spooler-cache "cached-cmd"))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_) (error "should not be called"))))
      (should (equal (cj/print--resolve-spooler) "cached-cmd")))))

(ert-deftest test-cbf-resolve-spooler-invalid-value-errors ()
  "Error: a value that's neither a string nor `auto' signals user-error."
  (let ((cj/print-spooler-command 'something-weird)
        (cj/print--spooler-cache nil))
    (should-error (cj/print--resolve-spooler) :type 'user-error)))

;;; ---------- cj/copy-buffer-name ----------

(ert-deftest test-cbf-copy-buffer-name-kills-buffer-name ()
  "Normal: the current buffer's name lands on the kill ring with a message."
  (let ((killed nil)
        (msg nil))
    (with-temp-buffer
      (rename-buffer "*test-cbf-copy-name*" t)
      (cl-letf (((symbol-function 'kill-new)
                 (lambda (s) (setq killed s)))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq msg (apply #'format fmt args)))))
        (cj/copy-buffer-name)))
    (should (string-prefix-p "*test-cbf-copy-name*" killed))
    (should (string-match-p "Copied" msg))))

;;; ---------- cj/view-buffer-in-eww ----------

(ert-deftest test-cbf-view-buffer-in-eww-normal-hands-off-to-eww ()
  "Normal: with a file-visiting buffer, `eww-open-file' is called with the file."
  (let ((opened nil))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/sample.html")
      (cl-letf (((symbol-function 'eww-open-file)
                 (lambda (f) (setq opened f))))
        (cj/view-buffer-in-eww))
      (setq buffer-file-name nil))
    (should (equal opened "/tmp/sample.html"))))

(ert-deftest test-cbf-view-buffer-in-eww-error-not-visiting-file ()
  "Error: a buffer not visiting a file signals user-error; eww isn't called."
  (with-temp-buffer
    (let ((called nil))
      (cl-letf (((symbol-function 'eww-open-file)
                 (lambda (_) (setq called t))))
        (should-error (cj/view-buffer-in-eww) :type 'user-error))
      (should-not called))))

;;; ---------- cj/--email-handle-is-type-p ----------

(ert-deftest test-cbf-email-handle-is-type-p-matches-prefix ()
  "Normal: `text/html' matches a content-type with charset suffix."
  (let ((handle '("buffer" ("text/html" "charset" . "utf-8"))))
    (cl-letf (((symbol-function 'mm-handle-type)
               (lambda (_) '("text/html" "charset" . "utf-8"))))
      (should (cj/--email-handle-is-type-p handle "text/html")))))

(ert-deftest test-cbf-email-handle-is-type-p-no-match ()
  "Boundary: a plain-text handle doesn't match a text/html request."
  (let ((handle '("buffer" ("text/plain"))))
    (cl-letf (((symbol-function 'mm-handle-type)
               (lambda (_) '("text/plain"))))
      (should-not (cj/--email-handle-is-type-p handle "text/html")))))

(ert-deftest test-cbf-email-handle-is-type-p-nil-handle-returns-nil ()
  "Error: a nil handle returns nil instead of signaling."
  (should-not (cj/--email-handle-is-type-p nil "text/html")))

;;; ---------- cj/--diff-with-regular-diff ----------

(ert-deftest test-cbf-diff-with-regular-diff-renders-buffer-content ()
  "Normal: regular diff helper writes a header and diff output into BUFFER."
  (let ((called-with nil))
    (let ((buf (generate-new-buffer "*test-cbf-diff*")))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'call-process)
                       (lambda (prog _infile _buf _disp &rest args)
                         (setq called-with (cons prog args))
                         (with-current-buffer (current-buffer)
                           (insert "@@ -1 +1 @@\n-old\n+new\n"))
                         0))
                      ((symbol-function 'diff-mode) #'ignore))
              (cj/--diff-with-regular-diff "/tmp/a.txt" "/tmp/b.txt" buf))
            (with-current-buffer buf
              (should (string-match-p "Unified diff" (buffer-string)))
              (should (string-match-p "@@" (buffer-string)))))
        (when (buffer-live-p buf) (kill-buffer buf))))
    (should (equal (car called-with) "diff"))))

;;; ---------- cj/diff-buffer-with-file ----------

(ert-deftest test-cbf-diff-buffer-with-file-error-not-visiting-file ()
  "Error: a buffer not visiting a file signals user-error."
  (with-temp-buffer
    (should-error (cj/diff-buffer-with-file) :type 'user-error)))

(ert-deftest test-cbf-diff-buffer-with-file-no-changes-messages ()
  "Normal: when the saved file and buffer match, diff returns 0 -> messages \"No differences\"."
  (let ((tmp (make-temp-file "test-cbf-diff-")))
    (with-temp-file tmp (insert "content"))
    (unwind-protect
        (let ((msg nil))
          (with-temp-buffer
            (insert "content")
            (setq buffer-file-name tmp)
            (cl-letf (((symbol-function 'call-process)
                       (lambda (&rest _) 0))   ; pretend "diff -q" exits 0
                      ((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (setq msg (apply #'format fmt args)))))
              (cj/diff-buffer-with-file))
            (setq buffer-file-name nil))
          (should (string-match-p "No differences" msg)))
      (delete-file tmp))))

(provide 'test-custom-buffer-file-print-diff-eww)
;;; test-custom-buffer-file-print-diff-eww.el ends here
