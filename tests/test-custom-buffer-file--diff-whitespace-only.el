;;; test-custom-buffer-file--diff-whitespace-only.el --- whitespace-only diff detection -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for cj/--diff-whitespace-only-p, the route-1 detector behind the
;; buffer-differs save prompt: two files differ ONLY in whitespace when a plain
;; diff finds changes but `diff -w' (ignore all whitespace) finds none.  Uses
;; real temp files and the real diff(1) binary (a system boundary we keep), so
;; nothing is mocked.

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'custom-buffer-file)

(declare-function cj/--diff-whitespace-only-p "custom-buffer-file" (file-a file-b))

(defun test-cbf-ws--two-files (content-a content-b fn)
  "Write CONTENT-A and CONTENT-B to temp files, call FN with their paths, clean up."
  (let ((a (make-temp-file "cbf-ws-a-"))
        (b (make-temp-file "cbf-ws-b-")))
    (unwind-protect
        (progn
          (with-temp-file a (insert content-a))
          (with-temp-file b (insert content-b))
          (funcall fn a b))
      (delete-file a)
      (delete-file b))))

(ert-deftest test-cbf-diff-whitespace-only-trailing ()
  "Normal: files differing only by trailing whitespace are whitespace-only."
  (test-cbf-ws--two-files
   "alpha\nbeta\n" "alpha   \nbeta\n"
   (lambda (a b) (should (cj/--diff-whitespace-only-p a b)))))

(ert-deftest test-cbf-diff-whitespace-only-indentation ()
  "Normal: files differing only by leading indentation are whitespace-only."
  (test-cbf-ws--two-files
   "(foo)\n(bar)\n" "(foo)\n    (bar)\n"
   (lambda (a b) (should (cj/--diff-whitespace-only-p a b)))))

(ert-deftest test-cbf-diff-whitespace-only-real-content ()
  "Normal: files differing in actual content are NOT whitespace-only."
  (test-cbf-ws--two-files
   "alpha\nbeta\n" "alpha\nGAMMA\n"
   (lambda (a b) (should-not (cj/--diff-whitespace-only-p a b)))))

(ert-deftest test-cbf-diff-whitespace-only-identical ()
  "Boundary: identical files do not differ at all, so not whitespace-only."
  (test-cbf-ws--two-files
   "alpha\nbeta\n" "alpha\nbeta\n"
   (lambda (a b) (should-not (cj/--diff-whitespace-only-p a b)))))

(ert-deftest test-cbf-diff-whitespace-only-mixed ()
  "Boundary: whitespace change plus a real content change is NOT whitespace-only."
  (test-cbf-ws--two-files
   "alpha\nbeta\n" "alpha   \nGAMMA\n"
   (lambda (a b) (should-not (cj/--diff-whitespace-only-p a b)))))

;;; -------------------- cj/--diff-buffer-renderer -----------------------------
;; Which renderer the diff command uses: whitespace-only diffs go to a plain
;; unified diff (trailing whitespace highlighted, so it is actually visible)
;; because difftastic treats trailing-whitespace as no change and renders it
;; blank.  Real content diffs use difftastic when available, else plain diff.

(declare-function cj/--diff-buffer-renderer "custom-buffer-file" (ws-only difft-available))

(ert-deftest test-cbf-diff-renderer-whitespace-over-difftastic ()
  "Normal: a whitespace-only diff uses the whitespace renderer even when difft is present."
  (should (eq (cj/--diff-buffer-renderer t t) 'whitespace)))

(ert-deftest test-cbf-diff-renderer-whitespace-no-difft ()
  "Boundary: whitespace-only still uses the whitespace renderer without difft."
  (should (eq (cj/--diff-buffer-renderer t nil) 'whitespace)))

(ert-deftest test-cbf-diff-renderer-content-uses-difftastic ()
  "Normal: a content diff uses difftastic when it is available."
  (should (eq (cj/--diff-buffer-renderer nil t) 'difftastic)))

(ert-deftest test-cbf-diff-renderer-content-no-difft-regular ()
  "Boundary: a content diff falls back to the regular renderer without difft."
  (should (eq (cj/--diff-buffer-renderer nil nil) 'regular)))

;;; --------------- cj/--buffer-file-whitespace-only-p (buffer) ----------------
;; Buffer-vs-its-file variant: writes the buffer to a temp file and reuses the
;; detector against the buffer's visited file.  Uses a real visited-file buffer.

(declare-function cj/--buffer-file-whitespace-only-p "custom-buffer-file" (buffer))

(defun test-cbf-ws--with-visited (disk-content edit-fn body-fn)
  "Visit a temp file holding DISK-CONTENT, apply EDIT-FN in it, call BODY-FN with the buffer."
  (let ((f (make-temp-file "cbf-bws-" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file f (insert disk-content))
          (let ((buf (find-file-noselect f)))
            (unwind-protect
                (with-current-buffer buf
                  (funcall edit-fn)
                  (funcall body-fn buf))
              (with-current-buffer buf (set-buffer-modified-p nil))
              (kill-buffer buf))))
      (when (file-exists-p f) (delete-file f)))))

(ert-deftest test-cbf-buffer-file-ws-only-trailing ()
  "Normal: an unsaved trailing-whitespace edit is whitespace-only vs the file."
  (test-cbf-ws--with-visited
   "alpha\nbeta\n"
   (lambda () (goto-char (point-min)) (end-of-line) (insert "   "))
   (lambda (buf) (should (cj/--buffer-file-whitespace-only-p buf)))))

(ert-deftest test-cbf-buffer-file-ws-only-content ()
  "Normal: an unsaved content edit is NOT whitespace-only vs the file."
  (test-cbf-ws--with-visited
   "alpha\nbeta\n"
   (lambda () (goto-char (point-max)) (insert "gamma\n"))
   (lambda (buf) (should-not (cj/--buffer-file-whitespace-only-p buf)))))

;;; --------- cj/diff-buffer-with-file return value (for the toggle) -----------

(ert-deftest test-cbf-diff-returns-buffer-when-differs ()
  "Normal: cj/diff-buffer-with-file returns the live diff buffer when the buffer differs."
  (test-cbf-ws--with-visited
   "x\ny\n"
   (lambda () (goto-char (point-max)) (insert "ADDED\n"))
   (lambda (buf)
     (let ((db (with-current-buffer buf (cj/diff-buffer-with-file))))
       (should (bufferp db))
       (should (buffer-live-p db))))))

(ert-deftest test-cbf-diff-returns-nil-when-identical ()
  "Boundary: with no differences, cj/diff-buffer-with-file returns nil."
  (test-cbf-ws--with-visited
   "x\ny\n"
   (lambda () nil)
   (lambda (buf)
     (should-not (with-current-buffer buf (cj/diff-buffer-with-file))))))

(ert-deftest test-cbf-buffer-file-ws-only-non-file ()
  "Boundary: a buffer not visiting a file is not whitespace-only."
  (with-temp-buffer
    (insert "scratch")
    (should-not (cj/--buffer-file-whitespace-only-p (current-buffer)))))

(provide 'test-custom-buffer-file--diff-whitespace-only)
;;; test-custom-buffer-file--diff-whitespace-only.el ends here
