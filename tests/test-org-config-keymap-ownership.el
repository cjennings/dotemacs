;;; test-org-config-keymap-ownership.el --- Tests for Org keymap ownership -*- lexical-binding: t; -*-

;;; Commentary:

;; Ensure org-config owns C-; O through one keymap.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(setq load-prefer-newer t)

(defun org-appear-mode (&optional _arg)
  "Stub `org-appear-mode' for loading org-config in batch tests.")
(provide 'org-appear)

(require 'org-config)

(ert-deftest test-org-config-keymap-ownership-normal-org-prefix-uses-org-map ()
  "C-; O should be mounted through `cj/org-map'."
  (should (eq (keymap-lookup cj/custom-keymap "O")
              cj/org-map)))

(ert-deftest test-org-config-keymap-ownership-normal-clear-cache-on-org-map ()
  "The Org prefix should expose the cache-clear command on capital C.

Lowercase `c' is reserved as a sub-prefix for table column operations
(`c i' insert, `c d' delete); the single-key org commands under this
menu use capitals to leave the lowercase letters free as table
sub-prefixes."
  (should (eq (keymap-lookup cj/org-map "C")
              #'cj/org-clear-element-cache)))

(ert-deftest test-org-config-keymap-ownership-table-row-bindings ()
  "Table row operations live directly under the org menu at `r i' /
`r d', no longer behind a `T' sub-prefix."
  (should (eq (keymap-lookup cj/org-map "r i") #'org-table-insert-row))
  (should (eq (keymap-lookup cj/org-map "r d") #'org-table-kill-row)))

(ert-deftest test-org-config-keymap-ownership-table-column-bindings ()
  "Table column operations live directly under the org menu at `c i' /
`c d', sharing the `c' prefix that used to host the clear-cache
command (which moved to capital `C')."
  (should (eq (keymap-lookup cj/org-map "c i") #'org-table-insert-column))
  (should (eq (keymap-lookup cj/org-map "c d") #'org-table-delete-column)))

(ert-deftest test-org-config-keymap-ownership-regression-no-duplicate-org-keymap ()
  "The old duplicate `cj/org-keymap' binding should not exist."
  (should-not (boundp 'cj/org-keymap)))

(ert-deftest test-org-config-keymap-ownership-normal-clear-cache-defaults-all ()
  "Cache clear should reset all Org buffers by default."
  (let ((current-prefix-arg nil)
        (calls nil))
    (cl-letf (((symbol-function 'org-element-cache-reset)
               (lambda (&optional arg) (push arg calls))))
      (cj/org-clear-element-cache))
    (should (equal calls '(all)))))

(ert-deftest test-org-config-keymap-ownership-normal-prefix-clears-current-buffer ()
  "Cache clear should reset only the current Org buffer with a prefix."
  (let ((current-prefix-arg '(4))
        (calls nil))
    (cl-letf (((symbol-function 'org-element-cache-reset)
               (lambda (&optional arg) (push arg calls))))
      (with-temp-buffer
        (setq major-mode 'org-mode)
        (cj/org-clear-element-cache)))
    (should (equal calls '(nil)))))

(provide 'test-org-config-keymap-ownership)
;;; test-org-config-keymap-ownership.el ends here
