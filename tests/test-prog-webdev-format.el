;;; test-prog-webdev-format.el --- Tests for the webdev prettier formatter -*- lexical-binding: t; -*-

;;; Commentary:
;; Covers the prettier-formatting path in `modules/prog-webdev.el':
;;   - `cj/--webdev-format-args'    (pure: file -> ("--stdin-filepath" file))
;;   - `cj/webdev-format-buffer'    (interactive wrapper; `executable-find' and
;;                                   `call-process-region' stubbed)
;;
;; The formatter now runs prettier via `call-process-region' with an
;; explicit argv list (no shell), so there is no command string to quote.
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

;; --------------------------- cj/--webdev-format-args -------------------------

(ert-deftest test-prog-webdev--format-args-normal ()
  "Normal: a plain .ts path yields the stdin-filepath argv list."
  (should (equal '("--stdin-filepath" "/home/me/app.ts")
                 (cj/--webdev-format-args "/home/me/app.ts"))))

(ert-deftest test-prog-webdev--format-args-spaces-not-quoted ()
  "Boundary: a path with spaces is passed verbatim — argv needs no quoting."
  (should (equal '("--stdin-filepath" "/home/me/my app/index.tsx")
                 (cj/--webdev-format-args "/home/me/my app/index.tsx"))))

(ert-deftest test-prog-webdev--format-args-tsx-extension ()
  "Boundary: a .tsx path is passed through (prettier infers the parser)."
  (should (equal '("--stdin-filepath" "/x/Component.tsx")
                 (cj/--webdev-format-args "/x/Component.tsx"))))

;; ---------------------------- cj/webdev-format-buffer ------------------------

(ert-deftest test-prog-webdev-format-buffer-runs-prettier-on-the-file ()
  "Normal: with prettier on PATH, the argv targets `buffer-file-name'."
  (let (program args)
    (cl-letf (((symbol-function 'executable-find) (lambda (_p) "/usr/bin/prettier"))
              ((symbol-function 'call-process-region)
               (lambda (_start _end prog &rest rest)
                 ;; rest = (DELETE BUFFER DISPLAY &rest ARGS)
                 (setq program prog
                       args (nthcdr 3 rest))
                 0)))
      (with-temp-buffer
        (insert "const x=1\n")
        (setq buffer-file-name "/home/me/app.ts")
        (cj/webdev-format-buffer)
        (setq buffer-file-name nil)))
    (should (equal "prettier" program))
    (should (equal '("--stdin-filepath" "/home/me/app.ts") args))))

(ert-deftest test-prog-webdev-format-buffer-falls-back-to-file-ts ()
  "Boundary: a buffer with no file uses the \"file.ts\" filename hint."
  (let (args)
    (cl-letf (((symbol-function 'executable-find) (lambda (_p) "/usr/bin/prettier"))
              ((symbol-function 'call-process-region)
               (lambda (_start _end _prog &rest rest)
                 (setq args (nthcdr 3 rest))
                 0)))
      (with-temp-buffer
        (insert "const x=1\n")
        (should-not buffer-file-name)
        (cj/webdev-format-buffer)))
    (should (equal '("--stdin-filepath" "file.ts") args))))

(ert-deftest test-prog-webdev-format-buffer-clamps-point-to-point-max ()
  "Boundary: after a format that shrinks the buffer, point clamps to point-max."
  (cl-letf (((symbol-function 'executable-find) (lambda (_p) "/usr/bin/prettier"))
            ((symbol-function 'call-process-region)
             (lambda (_start _end _prog _delete buffer &rest _)
               ;; Simulate prettier writing a shorter result to the output buffer.
               (with-current-buffer buffer (insert "x"))
               0)))
    (with-temp-buffer
      (insert "a really long original buffer body that prettier will shorten\n")
      (goto-char 40)
      (cj/webdev-format-buffer)
      (should (= (point) (point-max))))))

(ert-deftest test-prog-webdev-format-buffer-replaces-on-success ()
  "Normal: a zero exit replaces the buffer with the formatter's output."
  (cl-letf (((symbol-function 'executable-find) (lambda (_p) "/usr/bin/prettier"))
            ((symbol-function 'call-process-region)
             (lambda (_start _end _prog _delete buffer &rest _)
               (with-current-buffer buffer (insert "const x = 1;\n"))
               0)))
    (with-temp-buffer
      (insert "const x=1\n")
      (cj/webdev-format-buffer)
      (should (string= (buffer-string) "const x = 1;\n")))))

(ert-deftest test-prog-webdev-format-buffer-no-clobber-on-failure ()
  "Error: a non-zero exit leaves the buffer untouched and signals an error."
  (cl-letf (((symbol-function 'executable-find) (lambda (_p) "/usr/bin/prettier"))
            ((symbol-function 'call-process-region)
             (lambda (_start _end _prog _delete buffer &rest _)
               (with-current-buffer buffer (insert "[error] syntax error"))
               1)))
    (with-temp-buffer
      (insert "const x=1\n")
      (should-error (cj/webdev-format-buffer) :type 'user-error)
      ;; Buffer must still hold the original source, not the error text.
      (should (string= (buffer-string) "const x=1\n")))))

(ert-deftest test-prog-webdev-format-buffer-errors-without-prettier ()
  "Error: prettier missing -> `user-error', nothing shells out."
  (let ((ran nil))
    (cl-letf (((symbol-function 'executable-find) (lambda (_p) nil))
              ((symbol-function 'call-process-region)
               (lambda (&rest _) (setq ran t) 0)))
      (with-temp-buffer
        (should-error (cj/webdev-format-buffer) :type 'user-error)))
    (should-not ran)))

(provide 'test-prog-webdev-format)
;;; test-prog-webdev-format.el ends here
