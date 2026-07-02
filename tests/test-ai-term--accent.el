;;; test-ai-term--accent.el --- Tests for the agent-terminal accent colors -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests the per-terminal palette recolor: the dupre faces for Claude Code's
;; session colors, the palette-index-to-face alist, the apply helper that
;; points a terminal's 256-color palette entries at those faces, and the
;; show-or-create wiring.  eat and its terminal API are stubbed -- no process
;; spawning, no eat load in batch.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-term)

(declare-function cj/--ai-term-apply-accent "ai-term-backend-eat" (buffer))
(declare-function cj/--ai-term-show-or-create "ai-term-backend-eat" (dir name))

(defvar cj/ai-term-palette-faces)
(defvar cj/--ai-term-mru)
(defvar eat-buffer-name)
(defvar eat-terminal)

;; eat isn't loaded in batch -- provide stubs so cl-letf has overrides.
(unless (fboundp 'eat)
  (defun eat (&optional _program _arg) nil))
(unless (fboundp 'eat-term-set-parameter)
  (defun eat-term-set-parameter (_terminal _parameter _value) nil))

;;; ------------------------------ accent faces --------------------------------

(ert-deftest test-ai-term-accent-face-defaults-to-dupre-blue ()
  "Normal: the default accent face exists and is dupre blue (#67809c)."
  (should (facep 'cj/ai-term-accent))
  (should (string-equal-ignore-case
           (face-attribute 'cj/ai-term-accent :foreground nil t)
           "#67809c")))

(ert-deftest test-ai-term-session-color-faces-carry-dupre-hues ()
  "Normal: each of Claude Code's session colors has a face with its dupre hue."
  (dolist (pair '((cj/ai-term-color-red    . "#d47c59")
                  (cj/ai-term-color-blue   . "#67809c")
                  (cj/ai-term-color-green  . "#a4ac64")
                  (cj/ai-term-color-yellow . "#d7af5f")
                  (cj/ai-term-color-purple . "#b294bb")
                  (cj/ai-term-color-orange . "#edb08f")
                  (cj/ai-term-color-pink   . "#c397d8")
                  (cj/ai-term-color-cyan   . "#8a9496")))
    (should (facep (car pair)))
    (should (string-equal-ignore-case
             (face-attribute (car pair) :foreground nil t)
             (cdr pair)))))

(ert-deftest test-ai-term-palette-faces-cover-banner-and-session-colors ()
  "Normal: the alist pins the bypass banner (211) and all 8 session-color indices."
  (should (eq (alist-get 211 cj/ai-term-palette-faces) 'cj/ai-term-accent))
  (dolist (pair '((167 . cj/ai-term-color-red)
                  (110 . cj/ai-term-color-blue)
                  (35  . cj/ai-term-color-green)
                  (178 . cj/ai-term-color-yellow)
                  (140 . cj/ai-term-color-purple)
                  (174 . cj/ai-term-color-orange)
                  (175 . cj/ai-term-color-pink)
                  (37  . cj/ai-term-color-cyan)))
    (should (eq (alist-get (car pair) cj/ai-term-palette-faces) (cdr pair)))))

;;; --------------------------- cj/--ai-term-apply-accent ----------------------

(ert-deftest test-ai-term-apply-accent-sets-palette-entries ()
  "Normal: every alist entry is pointed at its face via the eat API."
  (let ((set-params nil)
        (cj/ai-term-palette-faces '((211 . cj/ai-term-accent)
                                    (110 . cj/ai-term-color-blue))))
    (cl-letf (((symbol-function 'eat-term-set-parameter)
               (lambda (_terminal parameter value)
                 (push (cons parameter value) set-params))))
      (with-temp-buffer
        (setq-local eat-terminal 'dummy-terminal)
        (cj/--ai-term-apply-accent (current-buffer))))
    (should (equal (nreverse set-params)
                   '((color-211-face . cj/ai-term-accent)
                     (color-110-face . cj/ai-term-color-blue))))))

(ert-deftest test-ai-term-apply-accent-full-alist-count ()
  "Boundary: the default alist yields one palette call per entry (9 total)."
  (let ((set-params nil))
    (cl-letf (((symbol-function 'eat-term-set-parameter)
               (lambda (_terminal parameter value)
                 (push (cons parameter value) set-params))))
      (with-temp-buffer
        (setq-local eat-terminal 'dummy-terminal)
        (cj/--ai-term-apply-accent (current-buffer))))
    (should (= (length set-params) (length cj/ai-term-palette-faces)))))

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
  "Normal: creating a fresh agent terminal applies the palette to its buffer."
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
