;;; test-mu4e-attachments.el --- Tests for mu4e attachment helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests the project-owned attachment save helpers in `mu4e-attachments'
;; without requiring a live mu4e view buffer or real MIME handles.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'mu4e-attachments)

(defvar mu4e-uniquify-save-file-name-function)

(defun test-mu4e-attachments--part (filename index &optional handle)
  "Return a fake attachment part for FILENAME at INDEX."
  (list :filename filename
        :part-index index
        :mime-type "application/pdf"
        :target-dir "/tmp/mail-target"
        :attachment-like t
        :handle (or handle (format "handle-%s" index))))

(ert-deftest test-mu4e-attachments-filters-attachment-like-parts ()
  "Normal: only attachment-like MIME parts are returned for saving."
  (let ((parts (list (test-mu4e-attachments--part "invoice.pdf" 1)
                     (list :filename "inline.png"
                           :part-index 2
                           :attachment-like nil
                           :handle "inline"))))
    (should (equal (mapcar (lambda (part) (plist-get part :filename))
                           (cj/mu4e--attachment-parts parts))
                   '("invoice.pdf")))))

(ert-deftest test-mu4e-attachments-candidates-disambiguate-duplicates ()
  "Boundary: duplicate filenames remain individually selectable."
  (let* ((a (test-mu4e-attachments--part "report.pdf" 1))
         (b (test-mu4e-attachments--part "report.pdf" 2))
         (candidates (cj/mu4e--attachment-candidates (list a b))))
    (should (equal (mapcar #'car candidates)
                   '("report.pdf <part 1>" "report.pdf <part 2>")))
    (should (eq (cdr (assoc "report.pdf <part 1>" candidates)) a))
    (should (eq (cdr (assoc "report.pdf <part 2>" candidates)) b))))

(ert-deftest test-mu4e-attachments-candidates-keep-unique-names-simple ()
  "Normal: unique filenames are shown without extra part-index labels."
  (let* ((a (test-mu4e-attachments--part "invoice.pdf" 1))
         (b (test-mu4e-attachments--part "receipt.pdf" 2))
         (candidates (cj/mu4e--attachment-candidates (list a b))))
    (should (equal (mapcar #'car candidates)
                   '("invoice.pdf" "receipt.pdf")))))

(ert-deftest test-mu4e-attachments-save-part-uses-mu4e-save-path ()
  "Normal: saving a part uses mu4e path joining, uniquifying, and MIME saving."
  (let ((part (test-mu4e-attachments--part "invoice.pdf" 3 "handle"))
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

(ert-deftest test-mu4e-attachments-save-part-errors-without-handle ()
  "Error: a malformed part without a MIME handle fails clearly.
The handle check runs before `cj/mu4e--ensure-attachment-save-functions',
so this fails the same way whether or not mu4e's MIME support is loadable."
  (let ((part (test-mu4e-attachments--part "invoice.pdf" 3 nil)))
    (setq part (plist-put part :handle nil))
    (should-error (cj/mu4e--save-attachment-part part "/downloads")
                  :type 'user-error)))

(ert-deftest test-mu4e-attachments-save-all-prompts-once ()
  "Normal: the save-all command prompts for a directory once and saves all parts."
  (let ((parts (list (test-mu4e-attachments--part "a.pdf" 1)
                     (test-mu4e-attachments--part "b.pdf" 2)))
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

(ert-deftest test-mu4e-attachments-save-selected-prompts-and-selects-one ()
  "Normal: the selected-attachment command completes by label and saves one part."
  (let* ((a (test-mu4e-attachments--part "a.pdf" 1))
         (b (test-mu4e-attachments--part "b.pdf" 2))
         (parts (list a b))
         saved)
    (cl-letf (((symbol-function 'mu4e-view-mime-parts)
               (lambda () parts))
              ((symbol-function 'read-directory-name)
               (lambda (&rest _) "/downloads/"))
              ((symbol-function 'completing-read)
               ;; the collection is an annotated function table now, so
               ;; query it instead of car-mapping an alist
               (lambda (_prompt collection &rest _)
                 (should (equal (all-completions "" collection)
                                '("a.pdf" "b.pdf")))
                 "b.pdf"))
              ((symbol-function 'cj/mu4e--save-attachment-part)
               (lambda (part directory)
                 (setq saved (list part directory))
                 "/downloads/b.pdf"))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (should (equal (cj/mu4e-save-attachment-here) "/downloads/b.pdf"))
      (should (equal saved (list b "/downloads/"))))))

(ert-deftest test-mu4e-attachments-selection-buffer-renders-parts ()
  "Normal: opening the selection buffer renders attachment rows with metadata."
  (let* ((a (test-mu4e-attachments--part "a.pdf" 1))
         (b (test-mu4e-attachments--part "b.pdf" 2))
         (buffer (get-buffer-create "*test-mu4e-attachments*")))
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

(ert-deftest test-mu4e-attachments-selection-toggle-current-row ()
  "Normal: RET toggles the selected state for the attachment at point."
  (let* ((a (test-mu4e-attachments--part "a.pdf" 1))
         (buffer (get-buffer-create "*test-mu4e-attachments*")))
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

(ert-deftest test-mu4e-attachments-selection-mark-all-and-unmark-all ()
  "Normal: the selection buffer supports marking and unmarking all rows."
  (let ((parts (list (test-mu4e-attachments--part "a.pdf" 1)
                     (test-mu4e-attachments--part "b.pdf" 2)))
        (buffer (get-buffer-create "*test-mu4e-attachments*")))
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

(ert-deftest test-mu4e-attachments-selection-save-marked ()
  "Normal: saving marked rows saves only the selected attachment parts."
  (let* ((a (test-mu4e-attachments--part "a.pdf" 1))
         (b (test-mu4e-attachments--part "b.pdf" 2))
         (buffer (get-buffer-create "*test-mu4e-attachments*"))
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
            (should (equal saved (list (list b) "/downloads/")))
            ;; marks clear after a successful save so `s' won't re-save them
            (should-not (seq-some (lambda (entry) (plist-get entry :selected))
                                  cj/mu4e-attachment-selection-entries))
            (should (string-match-p "\\[ \\] b\\.pdf" (buffer-string)))))
      (kill-buffer buffer))))

(ert-deftest test-mu4e-attachments-selection-save-marked-errors-when-empty ()
  "Error: saving with no marked rows fails clearly."
  (let ((buffer (get-buffer-create "*test-mu4e-attachments*")))
    (unwind-protect
        (with-current-buffer buffer
          (cj/mu4e-attachment-selection-mode)
          (cj/mu4e--attachment-selection-setup
           (list (test-mu4e-attachments--part "a.pdf" 1))
           "/downloads/")
          (should-error (cj/mu4e-attachment-selection-save-marked)
                        :type 'user-error))
      (kill-buffer buffer))))

(ert-deftest test-mu4e-attachments-save-some-opens-selection-buffer ()
  "Normal: the save-some command prompts for a directory and opens a selector."
  (let ((parts (list (test-mu4e-attachments--part "a.pdf" 1)))
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

(ert-deftest test-mu4e-attachments-default-directory-uses-target-dir ()
  "Normal: the default save directory comes from the part's :target-dir."
  (let ((parts (list (test-mu4e-attachments--part "a.pdf" 1))))
    (should (equal (cj/mu4e--attachment-default-directory parts)
                   "/tmp/mail-target/"))))

(ert-deftest test-mu4e-attachments-default-directory-falls-back-to-downloads ()
  "Boundary: with no :target-dir hint, the default is ~/Downloads/."
  (let ((parts (list (list :filename "a.pdf" :part-index 1 :attachment-like t))))
    (should (equal (cj/mu4e--attachment-default-directory parts)
                   (file-name-as-directory (expand-file-name "~/Downloads/"))))))

(ert-deftest test-mu4e-attachments-selection-entry-at-point-errors-off-row ()
  "Error: asking for the attachment at point on a header line fails clearly."
  (let* ((a (test-mu4e-attachments--part "a.pdf" 1))
         (buffer (get-buffer-create "*test-mu4e-attachments*")))
    (unwind-protect
        (with-current-buffer buffer
          (cj/mu4e-attachment-selection-mode)
          (cj/mu4e--attachment-selection-setup (list a) "/downloads/")
          (goto-char (point-min))      ; first line is the "Save attachments to:" header
          (should-error (cj/mu4e--attachment-selection-entry-at-point)
                        :type 'user-error))
      (kill-buffer buffer))))

;; ------------------------- Picker Annotations --------------------------------

(ert-deftest test-mu4e-attachments-annotator-shows-mime-and-size ()
  "Normal: the annotator yields MIME type and human-readable size."
  (let* ((part (plist-put (test-mu4e-attachments--part "invoice.pdf" 1)
                          :decoded-size-approx 2048))
         (candidates (list (cons "invoice.pdf" part)))
         (annotate (cj/mu4e--attachment-annotator candidates))
         (suffix (funcall annotate "invoice.pdf")))
    (should (stringp suffix))
    (should (string-match-p "application/pdf" suffix))
    (should (string-match-p "2k" suffix))))

(ert-deftest test-mu4e-attachments-annotator-no-size ()
  "Boundary: a part without a decoded size still annotates the MIME type."
  (let* ((candidates (list (cons "invoice.pdf"
                                 (test-mu4e-attachments--part "invoice.pdf" 1))))
         (annotate (cj/mu4e--attachment-annotator candidates))
         (suffix (funcall annotate "invoice.pdf")))
    (should (stringp suffix))
    (should (string-match-p "application/pdf" suffix))))

(ert-deftest test-mu4e-attachments-annotator-unknown-candidate-nil ()
  "Error: an unknown candidate annotates as nil (marginalia shows nothing)."
  (let ((annotate (cj/mu4e--attachment-annotator nil)))
    (should-not (funcall annotate "nope.txt"))))

(ert-deftest test-mu4e-attachments-picker-uses-annotated-category ()
  "Normal: the save-here picker's collection carries category + annotator."
  (let ((captured-collection nil))
    (cl-letf (((symbol-function 'cj/mu4e--attachment-parts)
               (lambda (&rest _) (list (test-mu4e-attachments--part "a.pdf" 1))))
              ((symbol-function 'cj/mu4e--read-attachment-directory)
               (lambda (&rest _) "/tmp/x/"))
              ((symbol-function 'cj/mu4e--save-attachment-part)
               (lambda (&rest _) "/tmp/x/a.pdf"))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (setq captured-collection collection)
                 "a.pdf")))
      (cj/mu4e-save-attachment-here)
      (let ((md (funcall captured-collection "" nil 'metadata)))
        (should (eq (alist-get 'category (cdr md)) 'mu4e-attachment))
        (should (functionp (alist-get 'annotation-function (cdr md))))))))

(provide 'test-mu4e-attachments)
;;; test-mu4e-attachments.el ends here
