;;; read_buffer.el --- Read buffer tool for GPTel -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(require 'gptel)

(gptel-make-tool
 :name "read_buffer"
 :function (lambda (buffer)
			 (unless (buffer-live-p (get-buffer buffer))
			   (error "Error: buffer %s is not live" buffer))
			 (with-current-buffer  buffer
			   (buffer-substring-no-properties (point-min) (point-max))))
 :description "return the contents of an emacs buffer"
 :args (list '(:name "buffer"
					 :type string            ; :type value must be a symbol
					 :description "the name of the buffer whose contents are to be retrieved"))
 :category "emacs")

;; Automatically add to gptel-tools on load
(add-to-list 'gptel-tools (gptel-get-tool '("emacs" "read_buffer")))

(provide 'read_buffer)
;;; read_buffer.el ends here.
