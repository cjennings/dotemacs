;;; read_buffer.el --- Read buffer tool for GPTel -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;; Gptel tool that returns the contents of an Emacs buffer by name.

;;; Code:

(require 'gptel)

(defun cj/read-buffer--get-content (buffer)
  "Return the substring of BUFFER from `point-min' to `point-max'.
BUFFER may be a buffer object or a buffer name string.  Signal an
error when no live buffer matches."
  (unless (buffer-live-p (get-buffer buffer))
    (error "Buffer %s is not live" buffer))
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(gptel-make-tool
 :name "read_buffer"
 :function (lambda (buffer) (cj/read-buffer--get-content buffer))
 :description "return the contents of an emacs buffer"
 :args (list '(:name "buffer"
                     :type string
                     :description "the name of the buffer whose contents are to be retrieved"))
 :category "emacs")

(add-to-list 'gptel-tools (gptel-get-tool '("emacs" "read_buffer")))

(provide 'read_buffer)
;;; read_buffer.el ends here
