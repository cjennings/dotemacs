;;; test-modeline-config-eat-state.el --- eat input-mode/process segment -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `cj/--modeline-eat-state', the left-side segment that shows
;; an eat terminal's input mode and process state as icons with
;; explanatory hover text (replacing eat's own [semi-char]:run
;; mode-line-process).  eat isn't installed under `make test', so the
;; tests fake an eat buffer: `major-mode' set to `eat-mode' and the
;; input-mode flags defvar'd here.  Batch has no graphic display, so the
;; glyphs exercise their letter/text fallbacks.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(require 'modeline-config)

(defvar eat--semi-char-mode nil)
(defvar eat--char-mode nil)
(defvar eat--line-mode nil)

(defmacro test-eat-state--with-fake-eat-buffer (&rest body)
  "Run BODY in a temp buffer masquerading as an eat-mode buffer."
  `(with-temp-buffer
     (setq major-mode 'eat-mode)
     ,@body))

(ert-deftest test-modeline-eat-state-nil-outside-eat ()
  "Boundary: non-eat buffers get no segment."
  (with-temp-buffer
    (should-not (cj/--modeline-eat-state))))

(ert-deftest test-modeline-eat-state-semi-char-quiet-face ()
  "Normal: semi-char (the default mode) renders in the quiet shadow face."
  (test-eat-state--with-fake-eat-buffer
   (setq-local eat--semi-char-mode t)
   (let ((s (cj/--modeline-eat-state)))
     (should (stringp s))
     (should (text-property-any 0 (length s) 'face 'shadow s)))))

(ert-deftest test-modeline-eat-state-char-mode-warning-face ()
  "Normal: char mode (raw keys) renders in the warning face."
  (test-eat-state--with-fake-eat-buffer
   (setq-local eat--char-mode t)
   (let ((s (cj/--modeline-eat-state)))
     (should (stringp s))
     (should (text-property-any 0 (length s) 'face 'warning s)))))

(ert-deftest test-modeline-eat-state-input-mode-detection ()
  "Normal: the four input-mode flags map to the right symbols."
  (test-eat-state--with-fake-eat-buffer
   (setq-local eat--semi-char-mode t)
   (should (eq (cj/--modeline-eat-input-mode) 'semi-char)))
  (test-eat-state--with-fake-eat-buffer
   (setq-local eat--char-mode t)
   (should (eq (cj/--modeline-eat-input-mode) 'char)))
  (test-eat-state--with-fake-eat-buffer
   (setq-local eat--line-mode t)
   (should (eq (cj/--modeline-eat-input-mode) 'line)))
  (test-eat-state--with-fake-eat-buffer
   (should (eq (cj/--modeline-eat-input-mode) 'emacs))))

(ert-deftest test-modeline-eat-state-dead-process-error-face ()
  "Error: no live process renders the exited indicator in the error face."
  (test-eat-state--with-fake-eat-buffer
   (setq-local eat--semi-char-mode t)
   (let ((s (cj/--modeline-eat-state)))
     (should (text-property-any 0 (length s) 'face 'error s)))))

(ert-deftest test-modeline-eat-state-live-process-success-face ()
  "Normal: a live buffer process renders the running indicator, no error face."
  (test-eat-state--with-fake-eat-buffer
   (setq-local eat--semi-char-mode t)
   (let ((proc (start-process "test-eat-state" (current-buffer) "sleep" "10")))
     (unwind-protect
         (let ((s (cj/--modeline-eat-state)))
           (should (text-property-any 0 (length s) 'face 'success s))
           (should-not (text-property-any 0 (length s) 'face 'error s)))
       (set-process-query-on-exit-flag proc nil)
       (kill-process proc)))))

(ert-deftest test-modeline-eat-state-hover-text-explains ()
  "Normal: the input-mode glyph carries explanatory help-echo."
  (test-eat-state--with-fake-eat-buffer
   (setq-local eat--semi-char-mode t)
   (let* ((s (cj/--modeline-eat-state))
          (pos (text-property-not-all 0 (length s) 'help-echo nil s)))
     (should pos)
     (should (string-match-p "semi-char" (get-text-property pos 'help-echo s))))))

(provide 'test-modeline-config-eat-state)
;;; test-modeline-config-eat-state.el ends here
