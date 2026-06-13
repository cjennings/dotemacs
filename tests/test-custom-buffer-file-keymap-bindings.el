;;; test-custom-buffer-file-keymap-bindings.el --- d/D bindings in the buffer-and-file keymap -*- lexical-binding: t; -*-

;;; Commentary:
;; `cj/buffer-and-file-map' should put the destructive op on the capital key and
;; the frequently-used op on the easy lowercase key: D = delete-buffer-and-file,
;; d = diff-buffer-with-file.  Guards the swap against silently reverting.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Stub dependencies before loading the module (mirrors the sibling tests).
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")
(provide 'ps-print)

(require 'custom-buffer-file)

(ert-deftest test-custom-buffer-file-keymap-diff-on-lowercase-d ()
  "Normal: lowercase d runs diff -- the frequently-used, non-destructive op."
  (should (eq (keymap-lookup cj/buffer-and-file-map "d") #'cj/diff-buffer-with-file)))

(ert-deftest test-custom-buffer-file-keymap-delete-on-capital-d ()
  "Normal: capital D runs delete -- the destructive op on the capital key."
  (should (eq (keymap-lookup cj/buffer-and-file-map "D") #'cj/delete-buffer-and-file)))

(provide 'test-custom-buffer-file-keymap-bindings)
;;; test-custom-buffer-file-keymap-bindings.el ends here
