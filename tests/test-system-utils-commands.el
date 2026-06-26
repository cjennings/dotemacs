;;; test-system-utils-commands.el --- Tests for system-utils command wrappers -*- lexical-binding: t; -*-

;;; Commentary:
;; Sibling `test-system-utils-eval-buffer.el` covers
;; `cj/eval-buffer-with-confirmation-or-error-message'.  This batch
;; covers:
;;
;;   cj/open-file-with-command
;;   cj/server-shutdown
;;
;; Process and prompt primitives are stubbed.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'system-utils)

;;; cj/open-file-with-command

(ert-deftest test-system-utils-open-file-with-launcher-uses-call-process ()
  "Normal: a launcher command (e.g. xdg-open) routes through `call-process'."
  (let ((tmp (make-temp-file "test-su-open-" nil ".txt"))
        (call-args nil))
    (unwind-protect
        (cl-letf (((symbol-function 'cj/file-from-context)
                   (lambda () tmp))
                  ((symbol-function 'cj/external-open-launcher-p)
                   (lambda (_) t))
                  ((symbol-function 'call-process)
                   (lambda (prog _infile _buf _disp &rest args)
                     (setq call-args (cons prog args))
                     0))
                  ((symbol-function 'message) #'ignore))
          (cj/open-file-with-command "xdg-open"))
      (delete-file tmp))
    (should (equal (car call-args) "xdg-open"))
    (should (member tmp call-args))))

(ert-deftest test-system-utils-open-file-with-non-launcher-uses-shell-process ()
  "Normal: a non-launcher command goes through `start-process-shell-command'
and lands in a dedicated output buffer."
  (let ((tmp (make-temp-file "test-su-open-shell-" nil ".txt"))
        (shell-cmd nil)
        (buf-name nil))
    (unwind-protect
        (let (output-buf)
          (cl-letf (((symbol-function 'cj/file-from-context)
                     (lambda () tmp))
                    ((symbol-function 'cj/external-open-launcher-p)
                     (lambda (_) nil))
                    ((symbol-function 'start-process-shell-command)
                     (lambda (_name buf cmd)
                       (setq shell-cmd cmd)
                       (setq output-buf buf)
                       'fake-proc))
                    ((symbol-function 'message) #'ignore))
            (cj/open-file-with-command "cat"))
          ;; Capture name as a string before cleanup kills the buffer.
          (when (bufferp output-buf)
            (setq buf-name (buffer-name output-buf))
            (kill-buffer output-buf)))
      (delete-file tmp))
    (should (string-prefix-p "cat " shell-cmd))
    (should (stringp buf-name))
    (should (string-match-p "^\\*Open with cat:" buf-name))))

(ert-deftest test-system-utils-open-file-with-errors-when-no-valid-file ()
  "Error: a missing file (no context, no selection) signals an error."
  (cl-letf (((symbol-function 'cj/file-from-context) (lambda () nil))
            ((symbol-function 'read-file-name)
             (lambda (&rest _) "/no/such/path"))
            ((symbol-function 'cj/external-open-launcher-p)
             (lambda (_) t)))
    (should-error (cj/open-file-with-command "xdg-open") :type 'error)))

;;; cj/server-shutdown

(ert-deftest test-system-utils-server-shutdown-saves-then-kills ()
  "Normal: shutdown saves buffers and then calls `kill-emacs'."
  (let ((saved nil)
        (killed nil))
    (cl-letf (((symbol-function 'save-some-buffers)
               (lambda (&rest _) (setq saved t)))
              ((symbol-function 'kill-emacs)
               (lambda (&rest _) (setq killed t))))
      (cj/server-shutdown))
    (should saved)
    (should killed)))

;;; ibuffer delete/diff keybinding swap

(ert-deftest test-system-utils-ibuffer-d-diffs-D-deletes ()
  "Normal: in the ibuffer list, d diffs the buffer at point against its file and
D marks it for deletion (the swap of ibuffer's default d/= bindings)."
  (require 'ibuffer)
  (should (eq (keymap-lookup ibuffer-mode-map "d") #'ibuffer-diff-with-file))
  (should (eq (keymap-lookup ibuffer-mode-map "D") #'ibuffer-mark-for-delete)))

(provide 'test-system-utils-commands)
;;; test-system-utils-commands.el ends here
