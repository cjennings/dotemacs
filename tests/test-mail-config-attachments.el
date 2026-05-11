;;; test-mail-config-attachments.el --- Tests for mu4e attachment helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests project-owned attachment save helpers without requiring a live mu4e
;; view buffer or real MIME handles.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'mail-config)

(defvar mu4e-uniquify-save-file-name-function)

(defun test-mail-config-attachment--part (filename index &optional handle)
  "Return a fake attachment part for FILENAME at INDEX."
  (list :filename filename
        :part-index index
        :mime-type "application/pdf"
        :target-dir "/tmp/mail-target"
        :attachment-like t
        :handle (or handle (format "handle-%s" index))))

(ert-deftest test-mail-config-attachments-filters-attachment-like-parts ()
  "Only attachment-like MIME parts should be saved by the custom commands."
  (let ((parts (list (test-mail-config-attachment--part "invoice.pdf" 1)
                     (list :filename "inline.png"
                           :part-index 2
                           :attachment-like nil
                           :handle "inline"))))
    (should (equal (mapcar (lambda (part) (plist-get part :filename))
                           (cj/mu4e--attachment-parts parts))
                   '("invoice.pdf")))))

(ert-deftest test-mail-config-attachments-candidates-disambiguate-duplicates ()
  "Duplicate filenames should remain individually selectable."
  (let* ((a (test-mail-config-attachment--part "report.pdf" 1))
         (b (test-mail-config-attachment--part "report.pdf" 2))
         (candidates (cj/mu4e--attachment-candidates (list a b))))
    (should (equal (mapcar #'car candidates)
                   '("report.pdf <part 1>" "report.pdf <part 2>")))
    (should (eq (cdr (assoc "report.pdf <part 1>" candidates)) a))
    (should (eq (cdr (assoc "report.pdf <part 2>" candidates)) b))))

(ert-deftest test-mail-config-attachments-candidates-keep-unique-names-simple ()
  "Unique filenames should be shown without extra part-index labels."
  (let* ((a (test-mail-config-attachment--part "invoice.pdf" 1))
         (b (test-mail-config-attachment--part "receipt.pdf" 2))
         (candidates (cj/mu4e--attachment-candidates (list a b))))
    (should (equal (mapcar #'car candidates)
                   '("invoice.pdf" "receipt.pdf")))))

(ert-deftest test-mail-config-attachments-save-part-uses-mu4e-save-path ()
  "Saving a part should use mu4e path joining, uniquifying, and MIME saving."
  (let ((part (test-mail-config-attachment--part "invoice.pdf" 3 "handle"))
        (mu4e-uniquify-save-file-name-function
         (lambda (path) (concat path ".unique")))
        saved)
    (cl-letf (((symbol-function 'mu4e-join-paths)
               (lambda (&rest pieces) (mapconcat #'identity pieces "/")))
              ((symbol-function 'mm-save-part-to-file)
               (lambda (handle path) (setq saved (list handle path)))))
      (should (equal (cj/mu4e--save-attachment-part part "/downloads")
                     "/downloads/invoice.pdf.unique"))
      (should (equal saved '("handle" "/downloads/invoice.pdf.unique"))))))

(ert-deftest test-mail-config-attachments-save-part-errors-without-handle ()
  "A malformed part without a MIME handle should fail clearly."
  (let ((part (test-mail-config-attachment--part "invoice.pdf" 3 nil)))
    (setq part (plist-put part :handle nil))
    (should-error (cj/mu4e--save-attachment-part part "/downloads")
                  :type 'user-error)))

(ert-deftest test-mail-config-attachments-save-all-prompts-once ()
  "The save-all command should prompt for a directory once and save all parts."
  (let ((parts (list (test-mail-config-attachment--part "a.pdf" 1)
                     (test-mail-config-attachment--part "b.pdf" 2)))
        saved
        prompts)
    (cl-letf (((symbol-function 'mu4e-view-mime-parts)
               (lambda () parts))
              ((symbol-function 'read-directory-name)
               (lambda (prompt &rest _)
                 (push prompt prompts)
                 "/downloads/"))
              ((symbol-function 'cj/mu4e--save-attachment-parts)
               (lambda (selected directory)
                 (setq saved (list selected directory))
                 '("/downloads/a.pdf" "/downloads/b.pdf")))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (should (equal (cj/mu4e-save-all-attachments)
                     '("/downloads/a.pdf" "/downloads/b.pdf")))
      (should (= 1 (length prompts)))
      (should (equal saved (list parts "/downloads/"))))))

(ert-deftest test-mail-config-attachments-save-selected-prompts-and-selects-one ()
  "The selected-attachment command should complete by label and save one part."
  (let* ((a (test-mail-config-attachment--part "a.pdf" 1))
         (b (test-mail-config-attachment--part "b.pdf" 2))
         (parts (list a b))
         saved)
    (cl-letf (((symbol-function 'mu4e-view-mime-parts)
               (lambda () parts))
              ((symbol-function 'read-directory-name)
               (lambda (&rest _) "/downloads/"))
              ((symbol-function 'completing-read)
               (lambda (_prompt candidates &rest _)
                 (should (equal (mapcar #'car candidates)
                                '("a.pdf" "b.pdf")))
                 "b.pdf"))
              ((symbol-function 'cj/mu4e--save-attachment-part)
               (lambda (part directory)
                 (setq saved (list part directory))
                 "/downloads/b.pdf"))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (should (equal (cj/mu4e-save-attachment-here) "/downloads/b.pdf"))
      (should (equal saved (list b "/downloads/"))))))

(ert-deftest test-mail-config-attachments-email-map-bindings ()
  "Attachment save commands should be available from the email prefix map."
  (should (eq (lookup-key cj/email-map (kbd "S"))
              #'cj/mu4e-save-all-attachments))
  (should (eq (lookup-key cj/email-map (kbd "s"))
              #'cj/mu4e-save-attachment-here)))

(ert-deftest test-mail-config-attachments-selection-buffer-renders-parts ()
  "Opening the selection buffer should render attachment rows with metadata."
  (let* ((a (test-mail-config-attachment--part "a.pdf" 1))
         (b (test-mail-config-attachment--part "b.pdf" 2))
         (buffer (get-buffer-create "*test-mail-attachments*")))
    (unwind-protect
        (with-current-buffer buffer
          (cj/mu4e-attachment-selection-mode)
          (cj/mu4e--attachment-selection-setup (list a b) "/downloads/")
          (should (eq major-mode 'cj/mu4e-attachment-selection-mode))
          (should (equal cj/mu4e-attachment-selection-directory "/downloads/"))
          (should (equal (mapcar (lambda (entry)
                                   (plist-get entry :label))
                                 cj/mu4e-attachment-selection-entries)
                         '("a.pdf" "b.pdf")))
          (should (string-match-p "\\[ \\] a\\.pdf" (buffer-string)))
          (should (string-match-p "application/pdf" (buffer-string))))
      (kill-buffer buffer))))

(ert-deftest test-mail-config-attachments-selection-toggle-current-row ()
  "RET should toggle the selected state for the attachment at point."
  (let* ((a (test-mail-config-attachment--part "a.pdf" 1))
         (buffer (get-buffer-create "*test-mail-attachments*")))
    (unwind-protect
        (with-current-buffer buffer
          (cj/mu4e-attachment-selection-mode)
          (cj/mu4e--attachment-selection-setup (list a) "/downloads/")
          (goto-char (point-min))
          (search-forward "a.pdf")
          (cj/mu4e-attachment-selection-toggle)
          (should (plist-get (car cj/mu4e-attachment-selection-entries) :selected))
          (should (string-match-p "\\[x\\] a\\.pdf" (buffer-string)))
          (cj/mu4e-attachment-selection-toggle)
          (should-not (plist-get (car cj/mu4e-attachment-selection-entries) :selected))
          (should (string-match-p "\\[ \\] a\\.pdf" (buffer-string))))
      (kill-buffer buffer))))

(ert-deftest test-mail-config-attachments-selection-mark-all-and-unmark-all ()
  "Selection buffer should support marking and unmarking all rows."
  (let ((parts (list (test-mail-config-attachment--part "a.pdf" 1)
                     (test-mail-config-attachment--part "b.pdf" 2)))
        (buffer (get-buffer-create "*test-mail-attachments*")))
    (unwind-protect
        (with-current-buffer buffer
          (cj/mu4e-attachment-selection-mode)
          (cj/mu4e--attachment-selection-setup parts "/downloads/")
          (cj/mu4e-attachment-selection-mark-all)
          (should (seq-every-p (lambda (entry) (plist-get entry :selected))
                               cj/mu4e-attachment-selection-entries))
          (cj/mu4e-attachment-selection-unmark-all)
          (should-not (seq-some (lambda (entry) (plist-get entry :selected))
                                cj/mu4e-attachment-selection-entries)))
      (kill-buffer buffer))))

(ert-deftest test-mail-config-attachments-selection-save-marked ()
  "Saving marked rows should save only selected attachment parts."
  (let* ((a (test-mail-config-attachment--part "a.pdf" 1))
         (b (test-mail-config-attachment--part "b.pdf" 2))
         (buffer (get-buffer-create "*test-mail-attachments*"))
         saved)
    (unwind-protect
        (with-current-buffer buffer
          (cj/mu4e-attachment-selection-mode)
          (cj/mu4e--attachment-selection-setup (list a b) "/downloads/")
          (setf (plist-get (cadr cj/mu4e-attachment-selection-entries) :selected) t)
          (cl-letf (((symbol-function 'cj/mu4e--save-attachment-parts)
                     (lambda (parts directory)
                       (setq saved (list parts directory))
                       '("/downloads/b.pdf")))
                    ((symbol-function 'message) (lambda (&rest _) nil)))
            (should (equal (cj/mu4e-attachment-selection-save-marked)
                           '("/downloads/b.pdf")))
            (should (equal saved (list (list b) "/downloads/")))))
      (kill-buffer buffer))))

(ert-deftest test-mail-config-attachments-selection-save-marked-errors-when-empty ()
  "Saving with no marked rows should fail clearly."
  (let ((buffer (get-buffer-create "*test-mail-attachments*")))
    (unwind-protect
        (with-current-buffer buffer
          (cj/mu4e-attachment-selection-mode)
          (cj/mu4e--attachment-selection-setup
           (list (test-mail-config-attachment--part "a.pdf" 1))
           "/downloads/")
          (should-error (cj/mu4e-attachment-selection-save-marked)
                        :type 'user-error))
      (kill-buffer buffer))))

(ert-deftest test-mail-config-attachments-save-some-opens-selection-buffer ()
  "The save-some command should prompt for a directory and open a selector."
  (let ((parts (list (test-mail-config-attachment--part "a.pdf" 1)))
        opened)
    (cl-letf (((symbol-function 'mu4e-view-mime-parts)
               (lambda () parts))
              ((symbol-function 'read-directory-name)
               (lambda (&rest _) "/downloads/"))
              ((symbol-function 'cj/mu4e--open-attachment-selection-buffer)
               (lambda (selected directory)
                 (setq opened (list selected directory))
                 :buffer)))
      (should (eq (cj/mu4e-save-some-attachments) :buffer))
      (should (equal opened (list parts "/downloads/"))))))

(ert-deftest test-mail-config-attachments-email-map-save-some-binding ()
  "The email prefix map should include a save-some binding."
  (should (eq (lookup-key cj/email-map (kbd "m"))
              #'cj/mu4e-save-some-attachments)))

(ert-deftest test-mail-config-attachments-default-directory-uses-target-dir ()
  "Normal: the default save directory comes from the part's :target-dir."
  (let ((parts (list (test-mail-config-attachment--part "a.pdf" 1))))
    (should (equal (cj/mu4e--attachment-default-directory parts)
                   "/tmp/mail-target/"))))

(ert-deftest test-mail-config-attachments-default-directory-falls-back-to-downloads ()
  "Boundary: with no :target-dir hint, the default is ~/Downloads/."
  (let ((parts (list (list :filename "a.pdf" :part-index 1 :attachment-like t))))
    (should (equal (cj/mu4e--attachment-default-directory parts)
                   (file-name-as-directory (expand-file-name "~/Downloads/"))))))

(ert-deftest test-mail-config-attachments-selection-entry-at-point-errors-off-row ()
  "Error: asking for the attachment at point on a header line fails clearly."
  (let* ((a (test-mail-config-attachment--part "a.pdf" 1))
         (buffer (get-buffer-create "*test-mail-attachments*")))
    (unwind-protect
        (with-current-buffer buffer
          (cj/mu4e-attachment-selection-mode)
          (cj/mu4e--attachment-selection-setup (list a) "/downloads/")
          (goto-char (point-min))      ; first line is the "Save attachments to:" header
          (should-error (cj/mu4e--attachment-selection-entry-at-point)
                        :type 'user-error))
      (kill-buffer buffer))))

(provide 'test-mail-config-attachments)
;;; test-mail-config-attachments.el ends here
