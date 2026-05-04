;;; test-prog-c-compile-command.el --- Tests for C compile command setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Verifies single-file C compile commands use file paths rather than buffer
;; names, and quote paths safely for compile.el's shell command.

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'prog-c)

(ert-deftest test-prog-c-single-file-command-normal-path ()
  "A simple C file should keep the readable historical command shape."
  (should (equal
           (cj/c--single-file-compile-command "/tmp/project/main.c")
           "gcc -Wall -Wextra -g -o /tmp/project/main /tmp/project/main.c")))

(ert-deftest test-prog-c-single-file-command-quotes-spaces ()
  "C source and output paths with spaces should be shell-escaped."
  (should (equal
           (cj/c--single-file-compile-command "/tmp/my project/hello world.c")
           "gcc -Wall -Wextra -g -o /tmp/my\\ project/hello\\ world /tmp/my\\ project/hello\\ world.c")))

(ert-deftest test-prog-c-single-file-command-quotes-metacharacters ()
  "Shell metacharacters in C paths should be escaped."
  (should (equal
           (cj/c--single-file-compile-command "/tmp/project/weird;name.c")
           "gcc -Wall -Wextra -g -o /tmp/project/weird\\;name /tmp/project/weird\\;name.c")))

(ert-deftest test-prog-c-single-file-command-errors-without-file ()
  "Non-file buffers should fail clearly instead of compiling buffer names."
  (should-error (cj/c--single-file-compile-command nil)
                :type 'user-error))

(ert-deftest test-prog-c-compile-command-uses-buffer-file-name ()
  "The fallback compile command should ignore a renamed buffer's display name."
  (let ((source (expand-file-name "real source.c" temporary-file-directory)))
    (with-temp-buffer
      (setq buffer-file-name source)
      (rename-buffer "renamed display buffer" t)
      (cl-letf (((symbol-function 'locate-dominating-file)
                 (lambda (&rest _args) nil)))
        (cj/c-compile-command))
      (should (equal
               compile-command
               (format "gcc -Wall -Wextra -g -o %s %s"
                       (shell-quote-argument
                        (file-name-sans-extension source))
                       (shell-quote-argument source)))))))

(ert-deftest test-prog-c-compile-command-makefile-uses-make ()
  "Normal: a Makefile dominating the source dir produces a make compile command."
  (with-temp-buffer
    (cl-letf (((symbol-function 'locate-dominating-file)
               (lambda (_dir name)
                 (when (equal name "Makefile") "/tmp/projectroot/"))))
      (cj/c-compile-command))
    (should (equal compile-command
                   (format "cd %s && make -k "
                           (shell-quote-argument "/tmp/projectroot/"))))))

(ert-deftest test-prog-c-compile-command-cmake-uses-cmake-build ()
  "Normal: a CMakeLists.txt dominating the source dir produces a cmake build command."
  (with-temp-buffer
    (cl-letf (((symbol-function 'locate-dominating-file)
               (lambda (_dir name)
                 (when (equal name "CMakeLists.txt") "/tmp/cmakeroot/"))))
      (cj/c-compile-command))
    (should (equal compile-command
                   (format "cd %s && cmake --build build "
                           (shell-quote-argument "/tmp/cmakeroot/"))))))

(ert-deftest test-prog-c-compile-command-makefile-path-with-spaces-is-quoted ()
  "Boundary: a Makefile in a directory with spaces is shell-quoted in the cd target."
  (with-temp-buffer
    (cl-letf (((symbol-function 'locate-dominating-file)
               (lambda (_dir name)
                 (when (equal name "Makefile") "/tmp/my project/"))))
      (cj/c-compile-command))
    (should (equal compile-command
                   "cd /tmp/my\\ project/ && make -k "))))

(provide 'test-prog-c-compile-command)
;;; test-prog-c-compile-command.el ends here
