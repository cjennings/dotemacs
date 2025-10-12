;;; test-custom-org-agenda-functions.el --- Tests for custom functions in org-agenda -*- lexical-binding: t; -*-

;;; Commentary:
;; This tests the custom functions created to build the main agenda in org-agenda-config.el

;;; Code:

(add-to-list 'load-path (concat user-emacs-directory "modules"))
(require 'org-agenda-config)

(ert-deftest test-cj/org-skip-subtree-if-habit-positive ()
  (with-temp-buffer
	(insert "* TODO [#A] Test task\n")
	(insert ":PROPERTIES:\n")
	(insert ":STYLE:    habit\n")
	(insert ":RESET_CHECK_BOXES: t\n")
	(insert ":END:\n")
	(org-mode)
	(goto-char (point-min))
	(should (not (eq nil (cj/org-skip-subtree-if-habit))))))

(ert-deftest test-cj/org-skip-subtree-if-habit-negative ()
  (with-temp-buffer
    (insert "* TODO [#A] Test task\n")
    (org-mode)
    (goto-char (point-min))
    (should (eq nil (cj/org-skip-subtree-if-habit)))))

(ert-deftest test-cj/org-skip-subtree-if-priority-positive ()
  (with-temp-buffer
	(insert "* TODO [#A] Test task\n")
	(org-mode)
	(goto-char (point-min))
	(should (not (eq nil (cj/org-skip-subtree-if-priority ?A))))))

(ert-deftest test-cj/org-skip-subtree-if-priority-negative ()
  (erase-buffer)
  (insert "* TODO [#B] Test task\n")
  (org-mode)
  (goto-char (point-min))
  (should (eq nil (cj/org-skip-subtree-if-priority ?A))))

(ert-deftest test-cj/org-skip-subtree-if-priority-boundary0 ()
  (erase-buffer)
  (insert "* TODO Test task\n")
  (org-mode)
  (goto-char (point-min))
  (should (eq nil (cj/org-skip-subtree-if-priority ?A))))

(ert-deftest test-cj/org-skip-subtree-if-priority-boundary1 ()
  (erase-buffer)
  (insert "* Test entry\n")
  (org-mode)
  (goto-char (point-min))
  (should (eq nil (cj/org-skip-subtree-if-priority ?A))))

(ert-deftest test-cj/org-skip-subtree-if-keyword-positive ()
  (with-temp-buffer
	(insert "* TODO [#A] Test task\n")
	(org-mode)
	(goto-char (point-min))
	(should (not (eq nil (cj/org-skip-subtree-if-keyword '("TODO")))))))

(ert-deftest test-cj/org-skip-subtree-if-keyword-positive-multiple ()
  (with-temp-buffer
	(insert "* PROJECT Test entry\n")
	(org-mode)
	(goto-char (point-min))
	(should (not (eq nil (cj/org-skip-subtree-if-keyword '("TODO" "PROJECT")))))))

(ert-deftest test-cj/org-skip-subtree-if-keyword-negative ()
  (erase-buffer)
  (insert "* PROJECT [#A] Test task\n")
  (org-mode)
  (goto-char (point-min))
  (should (eq nil (cj/org-skip-subtree-if-keyword '("TODO")))))

(ert-deftest test-cj/org-skip-subtree-if-keyword-negative-superset ()
  (erase-buffer)
  (insert "* PROJECT [#A] Test task\n")
  (org-mode)
  (goto-char (point-min))
  (should (eq nil (cj/org-skip-subtree-if-keyword '("TODOTODO")))))

(ert-deftest test-cj/org-skip-subtree-if-keyword-negative-multiple ()
  (erase-buffer)
  (insert "* PROJECT [#A] Test task\n")
  (org-mode)
  (goto-char (point-min))
  (should (eq nil (cj/org-skip-subtree-if-keyword '("TODO" "DONE")))))


(provide 'test-custom-org-agenda-functions)
;;; test-custom-org-agenda-functions.el ends here.
