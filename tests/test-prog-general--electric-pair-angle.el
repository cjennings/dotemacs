;;; test-prog-general--electric-pair-angle.el --- Angle-bracket pairing inhibit -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for cj/--electric-pair-inhibit-angle, which stops electric-pair from
;; pairing "<" into "<>".  Craig's yasnippet keys start with "<" (e.g. <cj);
;; auto-pairing the "<" strands a ">" after the expanded snippet, which broke
;; the cj-comment close fence into "#+end_src>".

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'elec-pair)
(require 'org)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'prog-general)

;;; cj/--electric-pair-inhibit-angle

(ert-deftest test-prog-general-electric-pair-inhibit-angle-open ()
  "Normal: the open angle bracket is inhibited."
  (should (cj/--electric-pair-inhibit-angle ?<)))

(ert-deftest test-prog-general-electric-pair-inhibit-angle-delegates ()
  "Boundary: any other character defers to electric-pair-default-inhibit."
  (cl-letf (((symbol-function 'electric-pair-default-inhibit)
             (lambda (_c) 'delegated)))
    (should (eq (cj/--electric-pair-inhibit-angle ?a) 'delegated))
    (should (eq (cj/--electric-pair-inhibit-angle ?\() 'delegated))))

(ert-deftest test-prog-general-electric-pair-predicate-installed ()
  "Normal: prog-general installs the predicate as the global value."
  (should (eq electric-pair-inhibit-predicate #'cj/--electric-pair-inhibit-angle)))

;;; Integration — the actual pairing behavior

(ert-deftest test-integration-prog-general-angle-not-paired-in-org ()
  "Integration: in an org buffer (where < has paren syntax), typing < with the
inhibit predicate active inserts just <, not <>.

Components integrated:
- cj/--electric-pair-inhibit-angle (real)
- electric-pair-local-mode / self-insert-command (real)
- org-mode syntax table (real — gives < paren syntax)"
  (with-temp-buffer
    (org-mode)
    (electric-pair-local-mode 1)
    (setq-local electric-pair-inhibit-predicate #'cj/--electric-pair-inhibit-angle)
    (let ((last-command-event ?<))
      (call-interactively #'self-insert-command))
    (should (equal (buffer-substring-no-properties (point-min) (point-max)) "<"))))

(provide 'test-prog-general--electric-pair-angle)
;;; test-prog-general--electric-pair-angle.el ends here
