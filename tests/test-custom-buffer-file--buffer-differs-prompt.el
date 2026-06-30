;;; test-custom-buffer-file--buffer-differs-prompt.el --- disk-changed save prompt pieces -*- lexical-binding: t; -*-

;;; Commentary:
;; Pure-logic tests for the disk-changed save prompt (the C-x C-s case where the
;; buffer is modified AND the file changed on disk): the question string (with a
;; terse whitespace-only parenthetical), the labeled-but-terse choice list, and
;; the key->action mapping.  The interactive read loop, the diff display, and the
;; save/revert dispatch are exercised live, not here.

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'custom-buffer-file)

(declare-function cj/--buffer-differs-prompt-string "custom-buffer-file" (name ws-only-p))
(declare-function cj/--buffer-differs-choices "custom-buffer-file" ())
(declare-function cj/--buffer-differs-action "custom-buffer-file" (key))

;;; ------------------- cj/--buffer-differs-prompt-string ----------------------

(ert-deftest test-cbf-buffer-differs-prompt-plain ()
  "Normal: without whitespace-only, the prompt names the buffer, no parenthetical."
  (let ((s (cj/--buffer-differs-prompt-string "todo.org" nil)))
    (should (string-match-p "todo\\.org" s))
    (should-not (string-match-p "whitespace" s))))

(ert-deftest test-cbf-buffer-differs-prompt-whitespace-only ()
  "Normal: whitespace-only folds in a terse \"(whitespace only)\" parenthetical."
  (let ((s (cj/--buffer-differs-prompt-string "todo.org" t)))
    (should (string-match-p "todo\\.org" s))
    (should (string-match-p "(whitespace only)" s))))

;;; ---------------------- cj/--buffer-differs-choices -------------------------

(ert-deftest test-cbf-buffer-differs-choices-keys ()
  "Normal: the menu offers save, diff, clean, revert, and cancel."
  (let ((c (cj/--buffer-differs-choices)))
    (dolist (key '(?s ?d ?w ?r ?c))
      (should (assq key c)))))

(ert-deftest test-cbf-buffer-differs-choices-terse-names ()
  "Boundary: inline names stay terse (one word) so the menu fits at a glance."
  (dolist (entry (cj/--buffer-differs-choices))
    (let ((name (nth 1 entry)))
      (should (stringp name))
      (should-not (string-match-p " " name)))))

(ert-deftest test-cbf-buffer-differs-choices-clean-help-mentions-whitespace ()
  "Normal: the clean option's description (the ? help) names whitespace."
  (let ((entry (assq ?w (cj/--buffer-differs-choices))))
    (should (string-match-p "whitespace" (or (nth 2 entry) "")))))

(ert-deftest test-cbf-buffer-differs-choices-revert-help-mentions-disk ()
  "Normal: the revert option's description makes clear it rereads from disk."
  (let ((entry (assq ?r (cj/--buffer-differs-choices))))
    (should (string-match-p "disk" (or (nth 2 entry) "")))))

;;; ---------------------- cj/--buffer-differs-action --------------------------

(ert-deftest test-cbf-buffer-differs-action-save ()
  "Normal: s overwrites the file with the buffer."
  (should (eq (cj/--buffer-differs-action ?s) 'save)))

(ert-deftest test-cbf-buffer-differs-action-clean ()
  "Normal: w cleans whitespace, then saves."
  (should (eq (cj/--buffer-differs-action ?w) 'clean-save)))

(ert-deftest test-cbf-buffer-differs-action-revert ()
  "Normal: r discards edits and rereads from disk."
  (should (eq (cj/--buffer-differs-action ?r) 'revert)))

(ert-deftest test-cbf-buffer-differs-action-diff ()
  "Normal: d peeks at the diff (re-prompt is the caller's concern)."
  (should (eq (cj/--buffer-differs-action ?d) 'diff)))

(ert-deftest test-cbf-buffer-differs-action-cancel ()
  "Boundary: c cancels, leaving the buffer untouched."
  (should (eq (cj/--buffer-differs-action ?c) 'cancel)))

(ert-deftest test-cbf-buffer-differs-action-unknown ()
  "Error: an unmapped key returns nil."
  (should-not (cj/--buffer-differs-action ?z)))

;;; ------------------- cj/--buffer-changed-on-disk-p --------------------------
;; Real visited-file buffers; modtime state is driven (set-visited-file-modtime),
;; not mocked.  The trigger is the disk-changed conflict: modified AND the file
;; changed on disk since visited.

(declare-function cj/--buffer-changed-on-disk-p "custom-buffer-file" (buffer))

(defun test-cbf-cod--with-visited (edit-fn body-fn)
  "Visit a temp file, run EDIT-FN in its buffer, call BODY-FN with the buffer."
  (let ((f (make-temp-file "cbf-cod-" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file f (insert "original\n"))
          (let ((buf (find-file-noselect f)))
            (unwind-protect
                (with-current-buffer buf (funcall edit-fn) (funcall body-fn buf))
              (with-current-buffer buf (set-buffer-modified-p nil))
              (kill-buffer buf))))
      (when (file-exists-p f) (delete-file f)))))

(ert-deftest test-cbf-changed-on-disk-detects ()
  "Normal: modified buffer whose recorded modtime no longer matches is changed-on-disk."
  (test-cbf-cod--with-visited
   (lambda () (goto-char (point-max)) (insert "edit\n") (set-visited-file-modtime '(0 0)))
   (lambda (buf) (should (cj/--buffer-changed-on-disk-p buf)))))

(ert-deftest test-cbf-changed-on-disk-clean-modtime ()
  "Boundary: modified buffer whose modtime still matches is not changed-on-disk."
  (test-cbf-cod--with-visited
   (lambda () (goto-char (point-max)) (insert "edit\n"))
   (lambda (buf) (should-not (cj/--buffer-changed-on-disk-p buf)))))

(ert-deftest test-cbf-changed-on-disk-unmodified ()
  "Boundary: an unmodified buffer is never changed-on-disk (nothing of mine to lose)."
  (test-cbf-cod--with-visited
   (lambda () (set-visited-file-modtime '(0 0)))
   (lambda (buf) (should-not (cj/--buffer-changed-on-disk-p buf)))))

;;; -------------- cj/--buffer-differs-dispatch (data direction) ---------------
;; The destructive directions, driven against real files: `save' overwrites the
;; disk with the buffer (buffer wins); `revert' discards the buffer's edits and
;; rereads the disk (disk wins); `clean-save' strips trailing whitespace first.

(declare-function cj/--buffer-differs-dispatch "custom-buffer-file" (buffer action))
(declare-function cj/save-buffer "custom-buffer-file" ())

(defun test-cbf-disp--with-conflict (buffer-insert disk-content body-fn)
  "Visit a temp file, BUFFER-INSERT into the buffer, overwrite the file with
DISK-CONTENT underneath, then call BODY-FN with the buffer and file path."
  (let ((f (make-temp-file "cbf-disp-" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file f (insert "original\n"))
          (let ((buf (find-file-noselect f)))
            (unwind-protect
                (with-current-buffer buf
                  (goto-char (point-max)) (insert buffer-insert)
                  (with-temp-file f (insert disk-content))
                  (funcall body-fn buf f))
              (with-current-buffer buf (set-buffer-modified-p nil))
              (kill-buffer buf))))
      (when (file-exists-p f) (delete-file f)))))

(defun test-cbf-disp--disk (f)
  "Return the on-disk contents of F."
  (with-temp-buffer (insert-file-contents f) (buffer-string)))

(ert-deftest test-cbf-buffer-differs-dispatch-save-overwrites-disk ()
  "Normal: save writes the buffer over the disk version (buffer wins)."
  (test-cbf-disp--with-conflict
   "my edit\n" "disk changed underneath\n"
   (lambda (buf f)
     (cj/--buffer-differs-dispatch buf 'save)
     (should-not (buffer-modified-p buf))
     (should (string= (test-cbf-disp--disk f) "original\nmy edit\n")))))

(ert-deftest test-cbf-buffer-differs-dispatch-revert-discards-edits ()
  "Normal: revert discards the buffer's edits and rereads the disk (disk wins)."
  (test-cbf-disp--with-conflict
   "my edit\n" "disk version\n"
   (lambda (buf f)
     (cj/--buffer-differs-dispatch buf 'revert)
     (should-not (buffer-modified-p buf))
     (should (string= (with-current-buffer buf (buffer-string)) "disk version\n")))))

(ert-deftest test-cbf-buffer-differs-dispatch-clean-save-strips-whitespace ()
  "Normal: clean-save strips trailing whitespace, then overwrites the disk."
  (test-cbf-disp--with-conflict
   "edit   \n" "disk changed\n"
   (lambda (buf f)
     (cj/--buffer-differs-dispatch buf 'clean-save)
     (should-not (buffer-modified-p buf))
     (should (string= (test-cbf-disp--disk f) "original\nedit\n")))))

(ert-deftest test-cbf-save-buffer-fast-path-no-conflict ()
  "Boundary: with no disk conflict, cj/save-buffer just saves (no prompt path)."
  (let ((f (make-temp-file "cbf-fast-" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file f (insert "base\n"))
          (let ((buf (find-file-noselect f)))
            (unwind-protect
                (with-current-buffer buf
                  (goto-char (point-max)) (insert "added\n")
                  (cj/save-buffer)               ; modtime matches -> fast path
                  (should-not (buffer-modified-p))
                  (should (string= (test-cbf-disp--disk f) "base\nadded\n")))
              (with-current-buffer buf (set-buffer-modified-p nil))
              (kill-buffer buf))))
      (when (file-exists-p f) (delete-file f)))))

(provide 'test-custom-buffer-file--buffer-differs-prompt)
;;; test-custom-buffer-file--buffer-differs-prompt.el ends here
