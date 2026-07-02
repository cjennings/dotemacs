;;; test-ai-term--accent.el --- Tests for the agent-terminal accent color -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests the per-terminal accent recolor: the accent face's dupre-blue default,
;; the palette-index list, the apply helper that points a terminal's 256-color
;; palette entries at the accent face, and the show-or-create wiring.  eat and
;; its terminal API are stubbed -- no process spawning, no eat load in batch.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-term)

(declare-function cj/--ai-term-apply-accent "ai-term-backend-eat" (buffer))
(declare-function cj/--ai-term-show-or-create "ai-term-backend-eat" (dir name))

(defvar cj/ai-term-accent-color-indices)
(defvar cj/--ai-term-mru)
(defvar eat-buffer-name)
(defvar eat-terminal)

;; eat isn't loaded in batch -- provide stubs so cl-letf has overrides.
(unless (fboundp 'eat)
  (defun eat (&optional _program _arg) nil))
(unless (fboundp 'eat-term-set-parameter)
  (defun eat-term-set-parameter (_terminal _parameter _value) nil))

;;; ------------------------------ accent face ---------------------------------

(ert-deftest test-ai-term-accent-face-defaults-to-dupre-blue ()
  "Normal: the accent face exists and defaults to dupre blue (#67809c)."
  (should (facep 'cj/ai-term-accent))
  (should (string-equal-ignore-case
           (face-attribute 'cj/ai-term-accent :foreground nil t)
           "#67809c")))

(ert-deftest test-ai-term-accent-indices-default ()
  "Normal: the remapped palette indices default to Claude Code's accent (211)."
  (should (equal cj/ai-term-accent-color-indices '(211))))

;;; --------------------------- cj/--ai-term-apply-accent ----------------------

(ert-deftest test-ai-term-apply-accent-sets-palette-entries ()
  "Normal: every configured index is pointed at the accent face via the eat API."
  (let ((set-params nil))
    (cl-letf (((symbol-function 'eat-term-set-parameter)
               (lambda (_terminal parameter value)
                 (push (cons parameter value) set-params))))
      (with-temp-buffer
        (setq-local eat-terminal 'dummy-terminal)
        (cj/--ai-term-apply-accent (current-buffer))))
    (should (equal set-params '((color-211-face . cj/ai-term-accent))))))

(ert-deftest test-ai-term-apply-accent-multiple-indices ()
  "Boundary: several indices each get their own palette-entry call."
  (let ((set-params nil)
        (cj/ai-term-accent-color-indices '(211 174)))
    (cl-letf (((symbol-function 'eat-term-set-parameter)
               (lambda (_terminal parameter value)
                 (push (cons parameter value) set-params))))
      (with-temp-buffer
        (setq-local eat-terminal 'dummy-terminal)
        (cj/--ai-term-apply-accent (current-buffer))))
    (should (equal (sort (mapcar #'car set-params) #'string<)
                   '(color-174-face color-211-face)))))

(ert-deftest test-ai-term-apply-accent-no-terminal-is-noop ()
  "Error: a buffer without a live eat terminal is left alone, no API call, no error."
  (let ((set-params nil))
    (cl-letf (((symbol-function 'eat-term-set-parameter)
               (lambda (_terminal parameter value)
                 (push (cons parameter value) set-params))))
      (with-temp-buffer
        (cj/--ai-term-apply-accent (current-buffer))))
    (should-not set-params)))

;;; ------------------------- show-or-create wiring ----------------------------

(ert-deftest test-ai-term-show-or-create-applies-accent-on-create ()
  "Normal: creating a fresh agent terminal applies the accent to its buffer."
  (let ((name "agent [accent-wire-test]")
        (cj/--ai-term-mru nil)
        (applied nil))
    (when (get-buffer name) (kill-buffer name))
    (unwind-protect
        (cl-letf (((symbol-function 'eat)
                   (lambda (&optional _program _arg)
                     (get-buffer-create eat-buffer-name)))
                  ((symbol-function 'cj/--ai-term-send-string)
                   (lambda (_buf _s) nil))
                  ((symbol-function 'display-buffer)
                   (lambda (&rest _) nil))
                  ((symbol-function 'cj/--ai-term-apply-accent)
                   (lambda (buffer) (push (buffer-name buffer) applied))))
          (cj/--ai-term-show-or-create "/tmp/accent-wire-test" name)
          (should (equal applied (list name))))
      (when (get-buffer name) (kill-buffer name)))))

(provide 'test-ai-term--accent)
;;; test-ai-term--accent.el ends here
