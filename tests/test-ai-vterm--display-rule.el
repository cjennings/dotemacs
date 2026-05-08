;;; test-ai-vterm--display-rule.el --- Tests for the AI-vterm display-buffer rule -*- lexical-binding: t; -*-

;;; Commentary:
;; The module installs a `display-buffer-alist' entry routing buffers
;; whose names match "\\`claude \\[" to a right-side window.  These
;; tests verify the rule reaches the right side and ignores buffers
;; that don't match the prefix.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-vterm)

(defun test-ai-vterm--cleanup (name)
  "Kill buffer NAME if it exists."
  (when (get-buffer name)
    (kill-buffer name)))

(defmacro test-ai-vterm--with-clean-frame (&rest body)
  "Run BODY in a context with one window and the AI-vterm rule loaded."
  (declare (indent 0) (debug t))
  `(save-window-excursion
     (delete-other-windows)
     (let ((display-buffer-alist (cj/--ai-vterm-display-rule-list)))
       ,@body)))

(ert-deftest test-ai-vterm--display-rule-routes-claude-buffer-to-right ()
  "Normal: a buffer named \"claude [foo]\" lands in a window to the right.

The rule uses `display-buffer-in-direction' with `(direction . right)',
which splits the current window so the new window's left edge sits at
a positive column.  The buffer winds up in that new window."
  (let ((name "claude [display-rule-test]"))
    (test-ai-vterm--cleanup name)
    (unwind-protect
        (test-ai-vterm--with-clean-frame
          (let* ((buf (get-buffer-create name))
                 (win (display-buffer buf)))
            (should (windowp win))
            (should (> (window-left-column win) 0))))
      (test-ai-vterm--cleanup name))))

(ert-deftest test-ai-vterm--display-rule-skips-non-matching-buffer ()
  "Boundary: a buffer not named \"claude [...]\" does not match the rule.

The rule's regex doesn't fire, so `display-buffer' falls back to the
default action -- reuse the current window -- and no rightward split
occurs."
  (let ((name "scratch-buffer-no-match"))
    (test-ai-vterm--cleanup name)
    (unwind-protect
        (test-ai-vterm--with-clean-frame
          (let* ((buf (get-buffer-create name))
                 (win (display-buffer buf)))
            (should (windowp win))
            (should (= (window-left-column win) 0))))
      (test-ai-vterm--cleanup name))))

(ert-deftest test-ai-vterm--display-rule-prefix-not-substring ()
  "Boundary: \"foo claude [bar]\" does not match -- the rule anchors at start."
  (let ((name "foo claude [substring-test]"))
    (test-ai-vterm--cleanup name)
    (unwind-protect
        (test-ai-vterm--with-clean-frame
          (let* ((buf (get-buffer-create name))
                 (win (display-buffer buf)))
            (should (windowp win))
            (should (= (window-left-column win) 0))))
      (test-ai-vterm--cleanup name))))

(provide 'test-ai-vterm--display-rule)
;;; test-ai-vterm--display-rule.el ends here
