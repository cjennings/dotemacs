;;; test-prog-webdev-format.el --- Tests for the webdev prettier formatter -*- lexical-binding: t; -*-

;;; Commentary:
;; Covers the prettier-formatting path in `modules/prog-webdev.el':
;;   - `cj/--webdev-format-command'  (pure: file -> "prettier --stdin-filepath ...")
;;   - `cj/webdev-format-buffer'     (interactive wrapper; `executable-find' and
;;                                    `shell-command-on-region' stubbed)
;;
;; `cj/webdev-keybindings' (the C-; f mount) is already covered by
;; `test-prog-webdev--format-wiring.el'.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(require 'testutil-format-wiring)

(format-test--ensure-packages-init)
(require 'prog-webdev)

;; --------------------------- cj/--webdev-format-command ----------------------

(ert-deftest test-prog-webdev--format-command-normal ()
  "Normal: a plain .ts path yields the stdin-filepath prettier command."
  (should (equal "prettier --stdin-filepath /home/me/app.ts"
                 (cj/--webdev-format-command "/home/me/app.ts"))))

(ert-deftest test-prog-webdev--format-command-spaces-quoted ()
  "Boundary: a path with spaces is shell-quoted."
  (should (equal (format "prettier --stdin-filepath %s"
                         (shell-quote-argument "/home/me/my app/index.tsx"))
                 (cj/--webdev-format-command "/home/me/my app/index.tsx"))))

(ert-deftest test-prog-webdev--format-command-tsx-extension ()
  "Boundary: a .tsx path is passed through (prettier infers the parser)."
  (should (equal "prettier --stdin-filepath /x/Component.tsx"
                 (cj/--webdev-format-command "/x/Component.tsx"))))

;; ---------------------------- cj/webdev-format-buffer ------------------------

(ert-deftest test-prog-webdev-format-buffer-runs-prettier-on-the-file ()
  "Normal: with prettier on PATH, the region command targets `buffer-file-name'."
  (let (cmd)
    (cl-letf (((symbol-function 'executable-find) (lambda (_p) "/usr/bin/prettier"))
              ((symbol-function 'shell-command-on-region)
               (lambda (_start _end command &rest _) (setq cmd command))))
      (with-temp-buffer
        (insert "const x=1\n")
        (setq buffer-file-name "/home/me/app.ts")
        (cj/webdev-format-buffer)
        (setq buffer-file-name nil)))
    (should (equal (cj/--webdev-format-command "/home/me/app.ts") cmd))))

(ert-deftest test-prog-webdev-format-buffer-falls-back-to-file-ts ()
  "Boundary: a buffer with no file uses the \"file.ts\" filename hint."
  (let (cmd)
    (cl-letf (((symbol-function 'executable-find) (lambda (_p) "/usr/bin/prettier"))
              ((symbol-function 'shell-command-on-region)
               (lambda (_start _end command &rest _) (setq cmd command))))
      (with-temp-buffer
        (insert "const x=1\n")
        (should-not buffer-file-name)
        (cj/webdev-format-buffer)))
    (should (equal (cj/--webdev-format-command "file.ts") cmd))))

(ert-deftest test-prog-webdev-format-buffer-clamps-point-to-point-max ()
  "Boundary: after a format that shrinks the buffer, point clamps to point-max."
  (cl-letf (((symbol-function 'executable-find) (lambda (_p) "/usr/bin/prettier"))
            ((symbol-function 'shell-command-on-region)
             (lambda (_start _end _command &rest _)
               (delete-region (point-min) (point-max))
               (insert "x"))))
    (with-temp-buffer
      (insert "a really long original buffer body that prettier will shorten\n")
      (goto-char 40)
      (cj/webdev-format-buffer)
      (should (= (point) (point-max))))))

(ert-deftest test-prog-webdev-format-buffer-errors-without-prettier ()
  "Error: prettier missing -> `user-error', nothing shells out."
  (let ((ran nil))
    (cl-letf (((symbol-function 'executable-find) (lambda (_p) nil))
              ((symbol-function 'shell-command-on-region)
               (lambda (&rest _) (setq ran t))))
      (with-temp-buffer
        (should-error (cj/webdev-format-buffer) :type 'user-error)))
    (should-not ran)))

(provide 'test-prog-webdev-format)
;;; test-prog-webdev-format.el ends here
