;;; test-ai-vterm--display-rule.el --- Tests for the AI-vterm display-buffer rule -*- lexical-binding: t; -*-

;;; Commentary:
;; The module installs a `display-buffer-alist' entry routing buffers
;; whose names match "\\`agent \\[" to a right-side window.  These
;; tests verify the rule reaches the right side and ignores buffers
;; that don't match the prefix.

;;; Code:

(require 'ert)
(require 'cl-lib)

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

(ert-deftest test-ai-vterm--display-rule-routes-agent-buffer-to-right ()
  "Normal: on a desktop, \"agent [foo]\" lands in a window to the right.

The desktop default direction is `right' (see
`cj/--ai-vterm-default-direction'), so the rule splits the current
window with `(direction . right)' and the new window's left edge
sits at a positive column.  `env-laptop-p' is stubbed nil to pin the
desktop branch; on a laptop the agent would land below instead."
  (let ((name "agent [display-rule-test]"))
    (test-ai-vterm--cleanup name)
    (unwind-protect
        (cl-letf (((symbol-function 'env-laptop-p) (lambda () nil)))
          (test-ai-vterm--with-clean-frame
            (let* ((buf (get-buffer-create name))
                   (win (display-buffer buf)))
              (should (windowp win))
              (should (> (window-left-column win) 0)))))
      (test-ai-vterm--cleanup name))))

(ert-deftest test-ai-vterm--display-rule-skips-non-matching-buffer ()
  "Boundary: a buffer not named \"agent [...]\" does not match the rule.

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
  "Boundary: \"foo agent [bar]\" does not match -- the rule anchors at start."
  (let ((name "foo agent [substring-test]"))
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
