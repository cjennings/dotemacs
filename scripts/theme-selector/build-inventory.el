;;; build-inventory.el --- emit package->faces inventory for theme-selector -*- lexical-binding: t -*-
;;; Commentary:
;; Loaded into a running Emacs (emacsclient -e '(load ".../build-inventory.el")')
;; to write package-inventory.json next to itself: a JSON object mapping each
;; installed (elpa/straight) package to the faces it defines, grouped by the
;; package that owns the face's definition file. Built-in faces are skipped.
;; generate.py embeds the JSON so the theme-selector dropdown can reach every
;; installed package (tier-3 phase 6, the "theme every package" path).
;;; Code:

(require 'json)

(let ((h (make-hash-table :test 'equal)))
  (dolist (f (face-list))
    (let* ((file (ignore-errors (symbol-file f 'defface)))
           (pkg (and (stringp file)
                     (string-match "/\\(?:elpa\\|straight/build\\|site-lisp\\)/\\([a-zA-Z0-9._-]+?\\)-[0-9][^/]*/" file)
                     (match-string 1 file))))
      (when pkg (push (symbol-name f) (gethash pkg h)))))
  (let (al)
    (maphash (lambda (k v) (push (cons (intern k) (sort v #'string<)) al)) h)
    (setq al (sort al (lambda (a b) (string< (symbol-name (car a)) (symbol-name (car b))))))
    (with-temp-file (expand-file-name
                     "package-inventory.json"
                     (file-name-directory (or load-file-name buffer-file-name
                                              "~/.emacs.d/scripts/theme-selector/")))
      (let ((json-encoding-pretty-print t))
        (insert (json-encode al) "\n")))))

(provide 'build-inventory)
;;; build-inventory.el ends here
