;;; test-org-roam-config-copy-and-move.el --- Tests for org-roam copy/move bodies -*- lexical-binding: t; -*-

;;; Commentary:
;; Sibling tests cover the org-roam hook wiring, slug/demote/format
;; helpers, and the link description extractor.  This file covers the
;; bodies of the two larger interactive commands:
;;
;;   cj/org-roam-copy-todo-to-today
;;   cj/move-org-branch-to-roam

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; user-constants pulls in the constants the module reads at load.
(require 'user-constants)
(require 'org)
;; `org-refile' is autoloaded; resolve it now so `cl-letf' has a real
;; function cell to replace in the test.
(require 'org-refile)
(require 'org-roam-config)

;; Top-level defvars so let-bindings reach the dynamic variable under
;; lexical scope.
(defvar org-refile-keep nil)
(defvar org-roam-dailies-capture-templates nil)
(defvar org-after-refile-insert-hook nil)
(defvar org-roam-directory nil)

;;; cj/org-roam-copy-todo-to-today

(ert-deftest test-org-roam-copy-todo-refiles-to-today-when-different-file ()
  "Normal: copy-todo-to-today calls `org-refile' targeted at today's file
when today-file differs from the current buffer file."
  (let* ((source (make-temp-file "cj-roam-source-" nil ".org"))
         (today  (make-temp-file "cj-roam-today-"  nil ".org"))
         refile-args)
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name source)
          (cl-letf (((symbol-function 'org-roam-dailies--capture)
                     (lambda (&rest _)
                       (set-buffer (find-file-noselect today))
                       (goto-char (point-max))))
                    ((symbol-function 'org-refile)
                     (lambda (&rest args) (setq refile-args args))))
            (cj/org-roam-copy-todo-to-today))
          (should refile-args)
          ;; `org-refile' was called with (nil nil ("Completed Tasks" today nil pos))
          (let ((target (nth 2 refile-args)))
            (should (equal "Completed Tasks" (nth 0 target)))
            (should (equal (file-truename today) (file-truename (nth 1 target))))))
      (when (get-file-buffer today) (kill-buffer (get-file-buffer today)))
      (delete-file source)
      (delete-file today))))

(ert-deftest test-org-roam-copy-todo-saves-target-buffer ()
  "Normal: after the refile into today's journal, the target buffer
must not be left modified.  An unsaved journal buffer is what causes
Emacs to prompt about unsaved buffers at shutdown."
  (let ((source (make-temp-file "cj-roam-source-" nil ".org"))
        (today  (make-temp-file "cj-roam-today-"  nil ".org")))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name source)
          (cl-letf (((symbol-function 'org-roam-dailies--capture)
                     (lambda (&rest _)
                       (set-buffer (find-file-noselect today))
                       (goto-char (point-max))))
                    ((symbol-function 'org-refile)
                     (lambda (&rest _)
                       ;; Simulate org-refile inserting into the
                       ;; target buffer (which marks it modified).
                       (with-current-buffer (find-file-noselect today)
                         (goto-char (point-max))
                         (insert "* refiled content\n")))))
            (cj/org-roam-copy-todo-to-today))
          (let ((target-buffer (find-buffer-visiting today)))
            (should target-buffer)
            (should-not (buffer-modified-p target-buffer))))
      (when (get-file-buffer today) (kill-buffer (get-file-buffer today)))
      (delete-file source)
      (delete-file today))))

(ert-deftest test-org-roam-copy-todo-skips-when-already-today ()
  "Boundary: when the current buffer already visits today's file, no
refile is issued (same source and target)."
  (let ((today (make-temp-file "cj-roam-same-" nil ".org"))
        called)
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name today)
          (cl-letf (((symbol-function 'org-roam-dailies--capture)
                     (lambda (&rest _)
                       (set-buffer (find-file-noselect today))
                       (goto-char (point-max))))
                    ((symbol-function 'org-refile)
                     (lambda (&rest _) (setq called t))))
            (cj/org-roam-copy-todo-to-today))
          (should-not called))
      (when (get-file-buffer today) (kill-buffer (get-file-buffer today)))
      (delete-file today))))

;;; cj/move-org-branch-to-roam

(ert-deftest test-org-roam-move-branch-creates-roam-file ()
  "Normal: move-branch writes a roam file with the demoted subtree and
syncs the roam db."
  (let* ((roam-dir (file-name-as-directory
                    (make-temp-file "cj-roam-move-dir-" t)))
         (org-roam-directory roam-dir)
         (synced nil))
    (unwind-protect
        (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
                  ((symbol-function 'org-id-new)
                   (lambda () "11111111-2222-3333-4444-555555555555"))
                  ((symbol-function 'org-roam-db-sync)
                   (lambda (&rest _) (setq synced t)))
                  ((symbol-function 'message) #'ignore))
          (with-temp-buffer
            (org-mode)
            (insert "* My Heading\n** Sub heading\nbody text\n")
            (goto-char (point-min))
            (cj/move-org-branch-to-roam))
          (should synced)
          (let* ((files (directory-files roam-dir t "\\.org\\'"))
                 (written (car files)))
            (should files)
            (should (string-match-p "-my-heading\\.org\\'" written))
            (with-temp-buffer
              (insert-file-contents written)
              (let ((text (buffer-string)))
                (should (string-match-p ":ID:" text))
                (should (string-match-p "11111111-2222-3333-4444-555555555555" text))
                (should (string-match-p "#\\+TITLE: My Heading" text))
                (should (string-match-p "#\\+FILETAGS: Topic" text))
                ;; The subtree gets demoted to level 1 -- the original
                ;; level-1 heading stays as "* My Heading" and the
                ;; level-2 child becomes "** Sub heading".
                (should (string-match-p "^\\* My Heading" text))
                (should (string-match-p "^\\*\\* Sub heading" text))))))
      (delete-directory roam-dir t))))

(ert-deftest test-org-roam-move-branch-errors-outside-heading ()
  "Error: move-branch outside an org heading signals `user-error'."
  (cl-letf (((symbol-function 'require) (lambda (&rest _) t)))
    (with-temp-buffer
      (org-mode)
      (insert "plain body text, no heading at all\n")
      (goto-char (point-min))
      (should-error (cj/move-org-branch-to-roam) :type 'user-error))))

(provide 'test-org-roam-config-copy-and-move)
;;; test-org-roam-config-copy-and-move.el ends here
