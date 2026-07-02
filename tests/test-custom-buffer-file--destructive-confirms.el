;;; test-custom-buffer-file--destructive-confirms.el --- C-; b destructive-op confirms -*- lexical-binding: t; -*-

;;; Commentary:
;; The C-; b map carries high-blast-radius operations.  Policy:
;; - delete file (D) always confirms, naming the file (vc-delete-file
;;   already prompts on the VC path, so the wrapper guards only non-VC).
;; - erase (x), clear-to-top (t), clear-to-bottom (b), and revert (g)
;;   confirm only when a file-visiting buffer has unsaved edits --
;;   destroying unsaved work is the hazard; anything else is cheap to
;;   restore.  Non-file buffers and unmodified buffers stay fast.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")
(provide 'ps-print)

(require 'custom-buffer-file)

(defmacro test-destructive--with-file-buffer (content &rest body)
  "Run BODY in a buffer visiting a temp file whose saved content is CONTENT."
  (declare (indent 1))
  `(let* ((file (make-temp-file "destructive-confirm-" nil ".txt" ,content))
          (buf (find-file-noselect file)))
     (unwind-protect
         (with-current-buffer buf ,@body)
       (with-current-buffer buf (set-buffer-modified-p nil))
       (kill-buffer buf)
       (ignore-errors (delete-file file)))))

(defun test-destructive--deny (&rest _) nil)
(defun test-destructive--allow (&rest _) t)
(defun test-destructive--forbid-prompt (&rest _)
  (error "yes-or-no-p should not have been called"))

;;; erase

(ert-deftest test-destructive-erase-modified-file-buffer-denied-keeps-content ()
  "Error: unsaved edits + user says no -> buffer untouched."
  (test-destructive--with-file-buffer "saved\n"
    (goto-char (point-max))
    (insert "unsaved edit")
    (cl-letf (((symbol-function 'yes-or-no-p) #'test-destructive--deny))
      (cj/erase-buffer))
    (should (string-match-p "unsaved edit" (buffer-string)))))

(ert-deftest test-destructive-erase-modified-file-buffer-confirmed-erases ()
  "Normal: unsaved edits + user says yes -> erased."
  (test-destructive--with-file-buffer "saved\n"
    (insert "more")
    (cl-letf (((symbol-function 'yes-or-no-p) #'test-destructive--allow))
      (cj/erase-buffer))
    (should (= (buffer-size) 0))))

(ert-deftest test-destructive-erase-unmodified-file-buffer-no-prompt ()
  "Normal: no unsaved edits -> erases without prompting."
  (test-destructive--with-file-buffer "saved\n"
    (cl-letf (((symbol-function 'yes-or-no-p) #'test-destructive--forbid-prompt))
      (cj/erase-buffer))
    (should (= (buffer-size) 0))))

(ert-deftest test-destructive-erase-non-file-buffer-no-prompt ()
  "Boundary: modified non-file buffer -> erases without prompting."
  (with-temp-buffer
    (insert "scratch content")
    (cl-letf (((symbol-function 'yes-or-no-p) #'test-destructive--forbid-prompt))
      (cj/erase-buffer))
    (should (= (buffer-size) 0))))

;;; clear to top / bottom

(ert-deftest test-destructive-clear-to-bottom-modified-denied-keeps-content ()
  "Error: unsaved edits + no -> clear-to-bottom leaves buffer alone."
  (test-destructive--with-file-buffer "line1\nline2\n"
    (insert "edit")
    (goto-char (point-min))
    (cl-letf (((symbol-function 'yes-or-no-p) #'test-destructive--deny))
      (cj/clear-to-bottom-of-buffer))
    (should (string-match-p "line2" (buffer-string)))))

(ert-deftest test-destructive-clear-to-top-modified-denied-keeps-content ()
  "Error: unsaved edits + no -> clear-to-top leaves buffer alone."
  (test-destructive--with-file-buffer "line1\nline2\n"
    (goto-char (point-max))
    (insert "edit")
    (cl-letf (((symbol-function 'yes-or-no-p) #'test-destructive--deny))
      (cj/clear-to-top-of-buffer))
    (should (string-match-p "line1" (buffer-string)))))

;;; revert

(ert-deftest test-destructive-revert-unmodified-no-prompt-rereads-disk ()
  "Normal: unmodified buffer reverts silently to disk content."
  (test-destructive--with-file-buffer "old content\n"
    (let ((file buffer-file-name))
      (with-temp-file file (insert "new disk content\n"))
      (cl-letf (((symbol-function 'yes-or-no-p) #'test-destructive--forbid-prompt))
        (cj/revert-buffer))
      (should (string-match-p "new disk content" (buffer-string))))))

(ert-deftest test-destructive-revert-modified-denied-keeps-edits ()
  "Error: unsaved edits + no -> edits kept."
  (test-destructive--with-file-buffer "saved\n"
    (insert "precious edit")
    (cl-letf (((symbol-function 'yes-or-no-p) #'test-destructive--deny))
      (cj/revert-buffer))
    (should (string-match-p "precious edit" (buffer-string)))))

(ert-deftest test-destructive-revert-modified-confirmed-rereads-disk ()
  "Normal: unsaved edits + yes -> disk content restored."
  (test-destructive--with-file-buffer "saved content\n"
    (insert "discard me")
    (cl-letf (((symbol-function 'yes-or-no-p) #'test-destructive--allow))
      (cj/revert-buffer))
    (should-not (string-match-p "discard me" (buffer-string)))
    (should (string-match-p "saved content" (buffer-string)))))

(ert-deftest test-destructive-revert-non-file-buffer-user-errors ()
  "Error: revert in a non-file buffer signals `user-error'."
  (with-temp-buffer
    (should-error (cj/revert-buffer) :type 'user-error)))

;;; delete file

(ert-deftest test-destructive-delete-denied-keeps-file-and-buffer ()
  "Error: user says no -> file stays on disk, buffer stays alive."
  (test-destructive--with-file-buffer "keep me\n"
    (let ((file buffer-file-name)
          (buf (current-buffer)))
      (cl-letf (((symbol-function 'yes-or-no-p) #'test-destructive--deny))
        (cj/delete-buffer-and-file))
      (should (file-exists-p file))
      (should (buffer-live-p buf)))))

(ert-deftest test-destructive-delete-confirmed-removes-file ()
  "Normal: user says yes -> file deleted and buffer killed."
  (let* ((file (make-temp-file "destructive-delete-" nil ".txt" "bye\n"))
         (buf (find-file-noselect file)))
    (unwind-protect
        (with-current-buffer buf
          (cl-letf (((symbol-function 'yes-or-no-p) #'test-destructive--allow)
                    ((symbol-function 'vc-backend) (lambda (&rest _) nil)))
            (cj/delete-buffer-and-file))
          (should-not (file-exists-p file)))
      (when (buffer-live-p buf)
        (with-current-buffer buf (set-buffer-modified-p nil))
        (kill-buffer buf))
      (ignore-errors (delete-file file)))))

;;; keymap

(ert-deftest test-destructive-keymap-points-at-guarded-commands ()
  "Smoke: x and g on C-; b now run the guarded wrappers."
  (should (eq (keymap-lookup cj/buffer-and-file-map "x") #'cj/erase-buffer))
  (should (eq (keymap-lookup cj/buffer-and-file-map "g") #'cj/revert-buffer)))

(provide 'test-custom-buffer-file--destructive-confirms)
;;; test-custom-buffer-file--destructive-confirms.el ends here
