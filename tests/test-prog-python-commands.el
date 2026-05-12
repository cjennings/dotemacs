;;; test-prog-python-commands.el --- Tests for prog-python command builders -*- lexical-binding: t; -*-

;;; Commentary:
;; Covers the Python helper commands in `modules/prog-python.el':
;;   - `cj/--python-mypy-command' / `cj/--python-debug-command' (pure builders)
;;   - `cj/python-mypy' / `cj/python-debug' (interactive wrappers, boundaries stubbed)
;;   - `cj/python-mode-keybindings' (local key wiring)
;;
;; Requiring `prog-python' pulls in its `use-package' forms, so the package
;; archive needs initialising first -- same bootstrap the formatter-wiring
;; tests use.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(require 'testutil-format-wiring)

(format-test--ensure-packages-init)
(require 'prog-python)

;; ----------------------------- command builders ------------------------------

(ert-deftest test-prog-python--python-mypy-command-normal ()
  "Normal: a plain path yields \"mypy <path>\"."
  (let ((mypy-path "mypy"))
    (should (equal "mypy /home/me/foo.py"
                   (cj/--python-mypy-command "/home/me/foo.py")))))

(ert-deftest test-prog-python--python-mypy-command-spaces-quoted ()
  "Boundary: a target with spaces is shell-quoted."
  (let ((mypy-path "mypy"))
    (should (equal (format "mypy %s" (shell-quote-argument "/home/me/my project/foo.py"))
                   (cj/--python-mypy-command "/home/me/my project/foo.py")))))

(ert-deftest test-prog-python--python-mypy-command-directory ()
  "Boundary: a directory target (trailing slash) is passed through quoted."
  (let ((mypy-path "mypy"))
    (should (equal "mypy /home/me/proj/"
                   (cj/--python-mypy-command "/home/me/proj/")))))

(ert-deftest test-prog-python--python-mypy-command-honours-path-var ()
  "Boundary: a non-default `mypy-path' is used verbatim."
  (let ((mypy-path "/opt/venv/bin/mypy"))
    (should (equal "/opt/venv/bin/mypy /home/me/foo.py"
                   (cj/--python-mypy-command "/home/me/foo.py")))))

(ert-deftest test-prog-python--python-debug-command-normal ()
  "Normal: a file yields \"python3 -m pdb <file>\"."
  (should (equal "python3 -m pdb /home/me/foo.py"
                 (cj/--python-debug-command "/home/me/foo.py"))))

(ert-deftest test-prog-python--python-debug-command-spaces-quoted ()
  "Boundary: a file with spaces is shell-quoted."
  (should (equal (format "python3 -m pdb %s" (shell-quote-argument "/home/me/my dir/foo.py"))
                 (cj/--python-debug-command "/home/me/my dir/foo.py"))))

;; ---------------------------- cj/python-mypy ---------------------------------

(ert-deftest test-prog-python-mypy-runs-compile-when-mypy-present ()
  "Normal: with mypy on PATH, `compile' gets the builder's command."
  (let ((mypy-path "mypy")
        compiled)
    (cl-letf (((symbol-function 'executable-find) (lambda (_p) "/usr/bin/mypy"))
              ((symbol-function 'compile) (lambda (cmd &rest _) (setq compiled cmd))))
      (with-temp-buffer
        (setq buffer-file-name "/home/me/foo.py")
        (cj/python-mypy)
        (setq buffer-file-name nil)))
    (should (equal "mypy /home/me/foo.py" compiled))))

(ert-deftest test-prog-python-mypy-falls-back-to-default-directory ()
  "Boundary: no file -> the command targets `default-directory'."
  (let ((mypy-path "mypy")
        compiled)
    (cl-letf (((symbol-function 'executable-find) (lambda (_p) "/usr/bin/mypy"))
              ((symbol-function 'compile) (lambda (cmd &rest _) (setq compiled cmd))))
      (with-temp-buffer
        (setq-local default-directory "/home/me/proj/")
        (cj/python-mypy)))
    (should (equal "mypy /home/me/proj/" compiled))))

(ert-deftest test-prog-python-mypy-messages-when-mypy-absent ()
  "Error: with mypy missing, nothing compiles and a message is shown."
  (let ((mypy-path "mypy")
        (compiled nil)
        (messaged nil))
    (cl-letf (((symbol-function 'executable-find) (lambda (_p) nil))
              ((symbol-function 'compile) (lambda (&rest _) (setq compiled t)))
              ((symbol-function 'message) (lambda (fmt &rest args)
                                            (setq messaged (apply #'format fmt args)))))
      (cj/python-mypy))
    (should-not compiled)
    (should (string-match-p "mypy not found" messaged))))

;; ---------------------------- cj/python-debug --------------------------------

(ert-deftest test-prog-python-debug-runs-pdb-on-current-file ()
  "Normal: with a file, `pdb' gets the builder's command."
  (let (pdb-cmd)
    (cl-letf (((symbol-function 'pdb) (lambda (cmd) (setq pdb-cmd cmd))))
      (with-temp-buffer
        (setq buffer-file-name "/home/me/foo.py")
        (cj/python-debug)
        (setq buffer-file-name nil)))
    (should (equal "python3 -m pdb /home/me/foo.py" pdb-cmd))))

(ert-deftest test-prog-python-debug-messages-without-a-file ()
  "Error: no file -> pdb is not started and a message is shown."
  (let ((started nil)
        (messaged nil))
    (cl-letf (((symbol-function 'pdb) (lambda (&rest _) (setq started t)))
              ((symbol-function 'message) (lambda (fmt &rest args)
                                            (setq messaged (apply #'format fmt args)))))
      (with-temp-buffer
        (should-not buffer-file-name)
        (cj/python-debug)))
    (should-not started)
    (should (string-match-p "No file" messaged))))

;; ------------------------- cj/python-mode-keybindings ------------------------

(ert-deftest test-prog-python-mode-keybindings-binds-the-fkeys ()
  "Normal: S-<f5> -> `cj/python-mypy', S-<f6> -> `cj/python-debug'."
  (with-temp-buffer
    (use-local-map (make-sparse-keymap))
    (cj/python-mode-keybindings)
    (should (eq #'cj/python-mypy (local-key-binding (kbd "S-<f5>"))))
    (should (eq #'cj/python-debug (local-key-binding (kbd "S-<f6>"))))))

(ert-deftest test-prog-python-mode-keybindings-is-idempotent ()
  "Boundary: calling it twice leaves the same bindings, no error."
  (with-temp-buffer
    (use-local-map (make-sparse-keymap))
    (cj/python-mode-keybindings)
    (cj/python-mode-keybindings)
    (should (eq #'cj/python-mypy (local-key-binding (kbd "S-<f5>"))))
    (should (eq #'cj/python-debug (local-key-binding (kbd "S-<f6>"))))))

(provide 'test-prog-python-commands)
;;; test-prog-python-commands.el ends here
