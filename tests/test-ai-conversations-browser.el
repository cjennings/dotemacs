;;; test-ai-conversations-browser.el --- Tests for ai-conversations-browser -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the saved-conversations browser.  Pure helpers (topic
;; parsing, header stripping, preview, rename target) are tested
;; against fixed inputs.  File-touching actions (load / delete /
;; rename) are tested against a temp conversations directory.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(require 'testutil-ai-config)
;; Force real ai-conversations to override testutil's stub.
(setq features (delq 'ai-conversations features))
(require 'ai-conversations)
(require 'ai-conversations-browser)

;; ----------------------------- temp-dir helper

(defun test-ai-conversations-browser--with-temp-dir (fn)
  "Run FN inside a fresh conversations directory.  Clean up after."
  (let* ((dir (make-temp-file "test-ai-conversations-browser-" t))
         (cj/gptel-conversations-directory dir))
    (unwind-protect
        (funcall fn dir)
      (when (file-exists-p dir)
        (delete-directory dir t)))))

(defun test-ai-conversations-browser--write (dir name content)
  "Write CONTENT to NAME in DIR.  Return the absolute path."
  (let ((path (expand-file-name name dir)))
    (with-temp-file path (insert content))
    path))

;; ----------------------------- topic-from-filename

(ert-deftest test-ai-conversations-browser-topic-normal ()
  "Normal: topic slug extracted from a well-formed filename."
  (should (equal (cj/gptel-browser--topic-from-filename
                  "my-topic_20260315-101530.gptel")
                 "my-topic")))

(ert-deftest test-ai-conversations-browser-topic-error-malformed ()
  "Boundary: malformed filename returns nil."
  (should-not (cj/gptel-browser--topic-from-filename "garbage.gptel"))
  (should-not (cj/gptel-browser--topic-from-filename "topic.gptel"))
  (should-not (cj/gptel-browser--topic-from-filename "topic_20260315.gptel")))

;; ----------------------------- strip-headers

(ert-deftest test-ai-conversations-browser-strip-headers-normal ()
  "Strip the two visibility headers plus the blank line after them."
  (should (equal (cj/gptel-browser--strip-headers
                  "#+STARTUP: showeverything\n#+VISIBILITY: all\n\nrest\n")
                 "rest\n")))

(ert-deftest test-ai-conversations-browser-strip-headers-no-headers ()
  "Boundary: input without headers is unchanged."
  (should (equal (cj/gptel-browser--strip-headers "plain body\n")
                 "plain body\n")))

;; ----------------------------- last-message

(ert-deftest test-ai-conversations-browser-last-message-normal ()
  "Last-message picks the body of the last org heading."
  (let ((text "* user [2026-01-01]\nhello there\n* AI [2026-01-01]\nthe latest reply\n"))
    (should (equal (cj/gptel-browser--last-message text)
                   "the latest reply"))))

(ert-deftest test-ai-conversations-browser-last-message-no-heading ()
  "Boundary: text without headings returns the (collapsed) body."
  (let ((text "just some body\nwith two lines\n"))
    (should (equal (cj/gptel-browser--last-message text)
                   "just some body with two lines"))))

;; ----------------------------- preview

(ert-deftest test-ai-conversations-browser-preview-truncates ()
  "Preview is ellipsized when the message is longer than LENGTH."
  (let ((text "* AI\nthis is a very long response that should get truncated for the preview\n"))
    (let ((preview (cj/gptel-browser--preview text 30)))
      (should (= (length preview) 30))
      (should (string-suffix-p "…" preview)))))

(ert-deftest test-ai-conversations-browser-preview-short ()
  "Preview is returned verbatim when shorter than LENGTH."
  (let ((text "* AI\nshort\n"))
    (should (equal (cj/gptel-browser--preview text 60) "short"))))

(ert-deftest test-ai-conversations-browser-preview-empty ()
  "Preview of empty body returns empty string."
  (should (equal (cj/gptel-browser--preview "" 60) "")))

;; ----------------------------- row-for-file

(ert-deftest test-ai-conversations-browser-row-for-file-normal ()
  "Row contains date, topic, and a preview; carries file metadata."
  (test-ai-conversations-browser--with-temp-dir
   (lambda (dir)
     (let ((file (test-ai-conversations-browser--write
                  dir "alpha_20260315-101530.gptel"
                  "#+STARTUP: showeverything\n\n* AI\nresult body\n")))
       (let ((row (cj/gptel-browser--row-for-file file dir)))
         (should row)
         (should (string-match-p "2026-03-15 10:15" row))
         (should (string-match-p "alpha" row))
         (should (string-match-p "result body" row))
         (should (equal (get-text-property 0 'cj/gptel-browser-file row)
                        "alpha_20260315-101530.gptel")))))))

(ert-deftest test-ai-conversations-browser-row-for-file-non-conversation ()
  "Files that don't match the conversation pattern return nil."
  (test-ai-conversations-browser--with-temp-dir
   (lambda (dir)
     (let ((file (test-ai-conversations-browser--write
                  dir "not-a-conversation.gptel" "body")))
       (should-not (cj/gptel-browser--row-for-file file dir))))))

;; ----------------------------- rows / render

(ert-deftest test-ai-conversations-browser-rows-from-empty-dir ()
  "Empty conversations directory yields no rows."
  (test-ai-conversations-browser--with-temp-dir
   (lambda (_dir)
     (should-not (cj/gptel-browser--rows)))))

(ert-deftest test-ai-conversations-browser-rows-multiple-conversations ()
  "Multiple conversations produce a row per file."
  (test-ai-conversations-browser--with-temp-dir
   (lambda (dir)
     (test-ai-conversations-browser--write
      dir "a_20260101-100000.gptel" "* AI\nfirst\n")
     (test-ai-conversations-browser--write
      dir "b_20260102-100000.gptel" "* AI\nsecond\n")
     (let ((rows (cj/gptel-browser--rows)))
       (should (= 2 (length rows)))))))

(ert-deftest test-ai-conversations-browser-render-empty ()
  "Render shows a 'no conversations' line when directory is empty."
  (test-ai-conversations-browser--with-temp-dir
   (lambda (_dir)
     (with-temp-buffer
       (cj/gptel-browser-mode)
       (cj/gptel-browser--render)
       (should (string-match-p "no saved conversations" (buffer-string)))))))

(ert-deftest test-ai-conversations-browser-render-newest-first ()
  "Render sorts rows newest first by timestamp."
  (test-ai-conversations-browser--with-temp-dir
   (lambda (dir)
     (test-ai-conversations-browser--write
      dir "old_20260101-100000.gptel" "* AI\nx\n")
     (test-ai-conversations-browser--write
      dir "new_20260301-100000.gptel" "* AI\ny\n")
     (with-temp-buffer
       (cj/gptel-browser-mode)
       (cj/gptel-browser--render)
       (let ((text (buffer-substring-no-properties (point-min) (point-max))))
         ;; New (March) should appear before old (January) in the buffer.
         (should (< (string-match "2026-03-01" text)
                    (string-match "2026-01-01" text))))))))

;; ----------------------------- rename-target

(ert-deftest test-ai-conversations-browser-rename-target-normal ()
  "Rename-target preserves the timestamp and slugifies the new topic."
  (should (equal (cj/gptel-browser--rename-target
                  "/tmp/old-topic_20260101-100000.gptel"
                  "Brand New Topic")
                 "/tmp/brand-new-topic_20260101-100000.gptel")))

(ert-deftest test-ai-conversations-browser-rename-target-error-no-timestamp ()
  "Rename-target errors when the filename lacks a timestamp."
  (should-error (cj/gptel-browser--rename-target "/tmp/no-ts.gptel" "x")))

;; ----------------------------- delete / rename actions

(ert-deftest test-ai-conversations-browser-delete-removes-file ()
  "Delete with y removes the file under point and re-renders."
  (test-ai-conversations-browser--with-temp-dir
   (lambda (dir)
     (let ((file (test-ai-conversations-browser--write
                  dir "topic_20260101-100000.gptel" "* AI\nx\n")))
       (with-temp-buffer
         (cj/gptel-browser-mode)
         (cj/gptel-browser--render)
         ;; Point on the only data row
         (goto-char (point-min))
         (forward-line 2)
         (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
           (cj/gptel-browser-delete))
         (should-not (file-exists-p file)))))))

(ert-deftest test-ai-conversations-browser-delete-cancel-keeps-file ()
  "Delete with n leaves the file alone."
  (test-ai-conversations-browser--with-temp-dir
   (lambda (dir)
     (let ((file (test-ai-conversations-browser--write
                  dir "topic_20260101-100000.gptel" "* AI\nx\n")))
       (with-temp-buffer
         (cj/gptel-browser-mode)
         (cj/gptel-browser--render)
         (goto-char (point-min))
         (forward-line 2)
         (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
           (cj/gptel-browser-delete))
         (should (file-exists-p file)))))))

(ert-deftest test-ai-conversations-browser-rename-renames-file ()
  "Rename moves the file under a new slug while preserving timestamp."
  (test-ai-conversations-browser--with-temp-dir
   (lambda (dir)
     (let* ((file (test-ai-conversations-browser--write
                   dir "old-name_20260101-100000.gptel" "* AI\nx\n")))
       (with-temp-buffer
         (cj/gptel-browser-mode)
         (cj/gptel-browser--render)
         (goto-char (point-min))
         (forward-line 2)
         (cl-letf (((symbol-function 'read-string)
                    (lambda (&rest _) "renamed topic")))
           (cj/gptel-browser-rename))
         (should-not (file-exists-p file))
         (should (file-exists-p
                  (expand-file-name "renamed-topic_20260101-100000.gptel"
                                    dir))))))))

(ert-deftest test-ai-conversations-browser-rename-error-on-empty-line ()
  "Rename errors when point is on the header/empty area."
  (test-ai-conversations-browser--with-temp-dir
   (lambda (_dir)
     (with-temp-buffer
       (cj/gptel-browser-mode)
       (cj/gptel-browser--render)
       (goto-char (point-min))
       (should-error (cj/gptel-browser-rename))))))

(provide 'test-ai-conversations-browser)
;;; test-ai-conversations-browser.el ends here
