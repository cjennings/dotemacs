;;; test-keyboard-compat-setup.el --- Tests for the keyboard-compat setup functions -*- lexical-binding: t; -*-

;;; Commentary:
;; `cj/keyboard-compat-terminal-setup' and `cj/keyboard-compat-gui-setup' each
;; gate on the display type (`env-terminal-p' / `env-gui-p') and then mutate a
;; global keymap -- `input-decode-map' (arrow escape sequences) for terminals,
;; `key-translation-map' (M-<UPPER> -> M-S-<lower>) for GUI.  These tests
;; `let'-bind those keymaps to fresh copies and stub the env predicates, so the
;; real maps are never touched.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'keyboard-compat)

;; ----------------------- cj/keyboard-compat-terminal-setup -------------------

(defmacro test-kbc--terminal (terminal-p &rest body)
  "Run BODY with `env-terminal-p' stubbed to TERMINAL-P and a fresh
`input-decode-map'."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'env-terminal-p) (lambda () ,terminal-p)))
     (let ((input-decode-map (make-sparse-keymap)))
       ,@body)))

(ert-deftest test-keyboard-compat-terminal-setup-decodes-csi-arrows ()
  "Normal: on a terminal, the ESC-[ arrow sequences decode to arrow events."
  (test-kbc--terminal t
    (cj/keyboard-compat-terminal-setup)
    (should (equal (lookup-key input-decode-map "\e[A") [up]))
    (should (equal (lookup-key input-decode-map "\e[B") [down]))
    (should (equal (lookup-key input-decode-map "\e[C") [right]))
    (should (equal (lookup-key input-decode-map "\e[D") [left]))))

(ert-deftest test-keyboard-compat-terminal-setup-decodes-application-arrows ()
  "Normal: the application-mode ESC-O arrow sequences also decode."
  (test-kbc--terminal t
    (cj/keyboard-compat-terminal-setup)
    (should (equal (lookup-key input-decode-map "\eOA") [up]))
    (should (equal (lookup-key input-decode-map "\eOB") [down]))
    (should (equal (lookup-key input-decode-map "\eOC") [right]))
    (should (equal (lookup-key input-decode-map "\eOD") [left]))))

(ert-deftest test-keyboard-compat-terminal-setup-covers-all-eight-sequences ()
  "Normal: every direction is mapped under both the ESC-[ and ESC-O prefixes."
  (test-kbc--terminal t
    (cj/keyboard-compat-terminal-setup)
    (dolist (cell '((?A . up) (?B . down) (?C . right) (?D . left)))
      (dolist (prefix '("\e[" "\eO"))
        (should (equal (lookup-key input-decode-map (concat prefix (string (car cell))))
                       (vector (cdr cell))))))))

(ert-deftest test-keyboard-compat-terminal-setup-no-op-off-terminal ()
  "Boundary: not on a terminal -> `input-decode-map' is left untouched.
\(Compared against an empty keymap because `lookup-key' on an ESC-prefixed
string can return a meta-prefix event count rather than nil.)"
  (test-kbc--terminal nil
    (cj/keyboard-compat-terminal-setup)
    (should (equal input-decode-map (make-sparse-keymap)))))

(ert-deftest test-keyboard-compat-terminal-setup-on-tty-setup-hook ()
  "Normal: terminal setup is registered on `tty-setup-hook', which runs for each
new tty frame.  `input-decode-map' is terminal-local, so `emacs-startup-hook'
\(once, at daemon start, with no tty) leaves every later `emacsclient -t' frame
without the arrow-key decodings.  The GUI half already frame-scopes itself."
  (should (memq 'cj/keyboard-compat-terminal-setup tty-setup-hook))
  (should-not (memq 'cj/keyboard-compat-terminal-setup emacs-startup-hook)))

;; -------------------------- cj/keyboard-compat-gui-setup ---------------------

(defmacro test-kbc--gui (gui-p &rest body)
  "Run BODY with `env-gui-p' stubbed to GUI-P and a fresh `key-translation-map'."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'env-gui-p) (lambda () ,gui-p)))
     (let ((key-translation-map (make-sparse-keymap)))
       ,@body)))

(defconst test-kbc--meta-shift-letters
  '(?o ?m ?y ?f ?w ?e ?l ?r ?v ?h ?t ?z ?u ?d ?i ?c ?b)
  "The 17 letters whose M-<UPPER> form is translated to M-S-<lower> in GUI mode.")

(ert-deftest test-keyboard-compat-gui-setup-translates-spot-checks ()
  "Normal: in GUI mode, M-O -> M-S-o and M-B -> M-S-b (sampled).
M-K is no longer translated: show-kill-ring, its only consumer, was retired."
  (test-kbc--gui t
    (cj/keyboard-compat-gui-setup)
    (should (equal (lookup-key key-translation-map (kbd "M-O")) (kbd "M-S-o")))
    (should (equal (lookup-key key-translation-map (kbd "M-B")) (kbd "M-S-b")))
    (should (equal (lookup-key key-translation-map (kbd "M-D")) (kbd "M-S-d")))
    (should-not (lookup-key key-translation-map (kbd "M-K")))))

(ert-deftest test-keyboard-compat-gui-setup-translates-all-seventeen ()
  "Normal: every documented M-<UPPER> maps to its M-S-<lower> form."
  (test-kbc--gui t
    (cj/keyboard-compat-gui-setup)
    (dolist (l test-kbc--meta-shift-letters)
      (should (equal (lookup-key key-translation-map (kbd (format "M-%c" (upcase l))))
                     (kbd (format "M-S-%c" l)))))))

(ert-deftest test-keyboard-compat-gui-setup-no-op-off-gui ()
  "Boundary: not in GUI mode -> `key-translation-map' is left untouched."
  (test-kbc--gui nil
    (cj/keyboard-compat-gui-setup)
    (should-not (lookup-key key-translation-map (kbd "M-O")))
    (should-not (lookup-key key-translation-map (kbd "M-K")))))

(provide 'test-keyboard-compat-setup)
;;; test-keyboard-compat-setup.el ends here
