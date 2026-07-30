(let ((f (make-temp-file "h-e-" nil ".org")))
  (with-temp-file f (insert "* real\n"))
  (let* ((real (find-file-noselect f))
         (temp (cdr `(buffer . ,(eval `(let ((vc-follow-symlinks t) (find-file-hook nil))
                                         (find-file-noselect ,f 'nowarn)))))))
    (message "find-file-temporarily returned the USER'S buffer: %s (same=%s) name=%s"
             (buffer-name temp) (eq real temp) (buffer-name real)))
  (delete-file f))
