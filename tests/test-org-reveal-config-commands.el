;;; test-org-reveal-config-commands.el --- Tests for org-reveal-config command wrappers -*- lexical-binding: t; -*-

;;; Commentary:
;; Sibling tests cover the header template, title-to-filename slug,
;; and the headers-remove pass.  This file fills in the command
;; wrappers and a couple of small helpers:
;;
;;   cj/--reveal-preview-export-on-save
;;   cj/--reveal-ensure-header
;;   cj/reveal-export
;;   cj/reveal-preview-start
;;   cj/reveal-preview-stop
;;   cj/reveal-present
;;   cj/reveal-insert-header
;;   cj/reveal-remove-headers
;;   cj/reveal-new
;;
;; ox-reveal and browse-url are stubbed.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'org-reveal-config)

;;; cj/--reveal-preview-export-on-save

(ert-deftest test-reveal-preview-export-on-save-in-org-mode-exports ()
  "Normal: in org-mode the helper calls `org-reveal-export-to-html'."
  (let ((called nil))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (cl-letf (((symbol-function 'org-reveal-export-to-html)
                 (lambda (&rest _) (setq called t) "/tmp/out.html")))
        (cj/--reveal-preview-export-on-save)))
    (should called)))

(ert-deftest test-reveal-preview-export-on-save-non-org-buffer-skips ()
  "Boundary: outside org-mode the helper doesn't call the exporter."
  (let ((called nil))
    (with-temp-buffer
      (cl-letf (((symbol-function 'org-reveal-export-to-html)
                 (lambda (&rest _) (setq called t) "/tmp/out.html")))
        (cj/--reveal-preview-export-on-save)))
    (should-not called)))

;;; cj/--reveal-ensure-header

(ert-deftest test-reveal-ensure-header-inserts-when-absent ()
  "Normal: with no reveal headers yet, the template is inserted at point-min."
  (with-temp-buffer
    (insert "* Slide 1\n")
    (cl-letf (((symbol-function 'read-from-minibuffer)
               (lambda (&rest _) "My Talk")))
      (cj/--reveal-ensure-header))
    (should (string-match-p "^#\\+REVEAL" (buffer-string)))))

(ert-deftest test-reveal-ensure-header-no-op-when-already-present ()
  "Boundary: when headers already present, no prompt + no re-insert."
  (with-temp-buffer
    (insert "#+REVEAL_THEME: black\n\n* Slide 1\n")
    (let ((prompted nil))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) (setq prompted t) "ignored")))
        (cj/--reveal-ensure-header))
      (should-not prompted))))

;;; cj/reveal-export

(ert-deftest test-reveal-export-error-when-not-org-mode ()
  "Error: outside org-mode the command signals user-error."
  (with-temp-buffer
    (should-error (cj/reveal-export) :type 'user-error)))

(ert-deftest test-reveal-export-opens-html-in-browser ()
  "Normal: in org-mode the exporter is called and the HTML opens in browser."
  (let ((opened nil)
        (msg nil))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (cl-letf (((symbol-function 'org-reveal-export-to-html)
                 (lambda (&rest _) "/tmp/talk.html"))
                ((symbol-function 'browse-url-of-file)
                 (lambda (f) (setq opened f)))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq msg (apply #'format fmt args)))))
        (cj/reveal-export)))
    (should (equal opened "/tmp/talk.html"))
    (should (string-match-p "Opened presentation" msg))))

;;; cj/reveal-preview-start

(ert-deftest test-reveal-preview-start-installs-hook-and-exports ()
  "Normal: preview-start adds the buffer-local hook and exports once."
  (let ((exported nil)
        (opened nil))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (cl-letf (((symbol-function 'org-reveal-export-to-html)
                 (lambda (&rest _) (setq exported t) "/tmp/preview.html"))
                ((symbol-function 'browse-url-of-file)
                 (lambda (f) (setq opened f)))
                ((symbol-function 'message) #'ignore))
        (cj/reveal-preview-start))
      (should exported)
      (should (equal opened "/tmp/preview.html"))
      (should (memq #'cj/--reveal-preview-export-on-save
                    after-save-hook)))))

(ert-deftest test-reveal-preview-start-errors-outside-org-mode ()
  "Error: outside org-mode preview-start signals user-error."
  (with-temp-buffer
    (should-error (cj/reveal-preview-start) :type 'user-error)))

;;; cj/reveal-preview-stop

(ert-deftest test-reveal-preview-stop-removes-hook-and-messages ()
  "Normal: preview-stop removes the buffer-local hook + messages."
  (let ((msg nil))
    (with-temp-buffer
      (add-hook 'after-save-hook #'cj/--reveal-preview-export-on-save nil t)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq msg (apply #'format fmt args)))))
        (cj/reveal-preview-stop))
      (should-not (memq #'cj/--reveal-preview-export-on-save
                        after-save-hook)))
    (should (string-match-p "stopped" msg))))

;;; cj/reveal-insert-header

(ert-deftest test-reveal-insert-header-errors-outside-org-mode ()
  "Error: insert-header outside org-mode signals user-error."
  (with-temp-buffer
    (should-error (cj/reveal-insert-header) :type 'user-error)))

(ert-deftest test-reveal-insert-header-errors-when-already-present ()
  "Error: insert-header when headers already present signals user-error."
  (with-temp-buffer
    (delay-mode-hooks (org-mode))
    (insert "#+REVEAL_THEME: black\n")
    (should-error (cj/reveal-insert-header) :type 'user-error)))

(ert-deftest test-reveal-insert-header-normal-inserts-and-messages ()
  "Normal: with no headers, insert-header adds them and reports."
  (let ((msg nil))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "* Slide 1\n")
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) "A Talk"))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq msg (apply #'format fmt args)))))
        (cj/reveal-insert-header))
      (should (string-match-p "^#\\+REVEAL" (buffer-string))))
    (should (string-match-p "Inserted" msg))))

;;; cj/reveal-remove-headers

(ert-deftest test-reveal-remove-headers-errors-outside-org-mode ()
  "Error: remove-headers outside org-mode signals user-error."
  (with-temp-buffer
    (should-error (cj/reveal-remove-headers) :type 'user-error)))

(ert-deftest test-reveal-remove-headers-normal-messages-count ()
  "Normal: remove-headers reports how many lines were removed."
  (let ((msg nil))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (insert "#+REVEAL_THEME: black\n"
              "#+OPTIONS: reveal_title_slide:nil\n\n"
              "* Slide 1\n")
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq msg (apply #'format fmt args)))))
        (cj/reveal-remove-headers)))
    (should (string-match-p "^Removed [0-9]+ reveal" msg))))

;;; cj/reveal-new

(ert-deftest test-reveal-new-errors-when-file-exists ()
  "Error: trying to create a file that already exists signals user-error."
  (let ((tmp (make-temp-file "test-reveal-" nil ".org")))
    (unwind-protect
        (cl-letf (((symbol-function 'read-from-minibuffer)
                   (lambda (&rest _) "Reused Title"))
                  ((symbol-function 'read-file-name)
                   (lambda (&rest _) tmp)))
          (should-error (cj/reveal-new) :type 'user-error))
      (delete-file tmp))))

(provide 'test-org-reveal-config-commands)
;;; test-org-reveal-config-commands.el ends here
