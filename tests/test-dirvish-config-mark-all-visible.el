;;; test-dirvish-config-mark-all-visible.el --- Tests for marking all visible files -*- lexical-binding: t; -*-

;;; Commentary:
;; `cj/dired-mark-all-visible-files' marks every regular file in a Dired
;; buffer and leaves directories unmarked.  The loop is exercised here against
;; a real Dired buffer over a temp directory (the line predicate has its own
;; unit tests).  The regression this pins: `dired-mark' advances point itself,
;; so an extra `forward-line' skipped every other file and only alternate files
;; got marked.

;;; Code:

(require 'ert)
(require 'package)
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "elpa/dirvish-2.3.0/extensions"
                                          user-emacs-directory))
(require 'user-constants)
(require 'keybindings)
(require 'dirvish-config)
(require 'dired)

(defun test-dirvish--marked-count ()
  "Return the number of `*'-marked lines in the current Dired buffer."
  (let ((n 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (looking-at-p "^\\*") (setq n (1+ n)))
        (forward-line 1)))
    n))

(ert-deftest test-dirvish-mark-all-visible-marks-every-file ()
  "Normal: all regular files get marked, no skips.
Three files plus a subdirectory; the count of marks must equal the file count."
  (let ((dir (make-temp-file "dirvish-mark-test-" t)))
    (unwind-protect
        (progn
          (dolist (f '("a.txt" "b.txt" "c.txt"))
            (write-region "" nil (expand-file-name f dir)))
          (make-directory (expand-file-name "subdir" dir))
          (let ((buf (dired-noselect dir)))
            (unwind-protect
                (with-current-buffer buf
                  (cj/dired-mark-all-visible-files)
                  (should (= 3 (test-dirvish--marked-count))))
              (kill-buffer buf))))
      (delete-directory dir t))))

(ert-deftest test-dirvish-mark-all-visible-leaves-directories-unmarked ()
  "Boundary: a directory line is never marked."
  (let ((dir (make-temp-file "dirvish-mark-test-" t)))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "only.txt" dir))
          (make-directory (expand-file-name "adir" dir))
          (let ((buf (dired-noselect dir)))
            (unwind-protect
                (with-current-buffer buf
                  (cj/dired-mark-all-visible-files)
                  (should (= 1 (test-dirvish--marked-count))))
              (kill-buffer buf))))
      (delete-directory dir t))))

(provide 'test-dirvish-config-mark-all-visible)
;;; test-dirvish-config-mark-all-visible.el ends here
