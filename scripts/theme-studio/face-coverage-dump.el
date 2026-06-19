;;; face-coverage-dump.el --- Dump face/group/package data for the coverage worklist -*- lexical-binding: t -*-

;;; Commentary:
;; Emits a JSON file that face_coverage.py consumes to build face-coverage.org.
;; For every face in `face-list' it records the name, its documentation string,
;; and the file its `defface' lives in (used to classify built-in vs package).
;; It also dumps every customization group's documentation and every elpa
;; package's summary, so the builder can describe each bucket offline.
;;
;; Run against a live daemon to capture actually-loaded packages:
;;   emacsclient -e '(progn (load ".../face-coverage-dump.el")
;;                          (face-coverage-dump "/tmp/face-coverage-data.json"))'
;; or on a clean checkout via `emacs --batch -l init.el' then the same calls
;; (lazily-loaded packages will be absent until required).

;;; Code:

(require 'json)
(require 'package)

(defun face-coverage-dump (outfile)
  "Write face, group, and package data as JSON to OUTFILE."
  (let ((faces nil)
        (groups (make-hash-table :test 'equal))
        (packages (make-hash-table :test 'equal)))
    (dolist (f (face-list))
      (push (vector (symbol-name f)
                    (or (face-documentation f) :null)
                    (or (symbol-file f 'defface) :null))
            faces))
    (mapatoms
     (lambda (s)
       (let ((d (get s 'group-documentation)))
         (when (stringp d) (puthash (symbol-name s) d groups)))))
    (when (boundp 'package-alist)
      (dolist (entry package-alist)
        (let ((sum (ignore-errors (package-desc-summary (cadr entry)))))
          (when (stringp sum) (puthash (symbol-name (car entry)) sum packages)))))
    ;; Docstrings carry curly quotes and other non-ASCII; bind the write coding
    ;; system so `with-temp-file' never drops into the interactive
    ;; select-safe-coding-system prompt (which pops in the daemon's frame).
    (let ((n (length faces))
          (coding-system-for-write 'utf-8-unix))
      (with-temp-file outfile
        (insert (json-serialize (list :faces (vconcat (nreverse faces))
                                      :groups groups
                                      :packages packages))))
      (message "face-coverage-dump: %d faces -> %s" n outfile))))

(provide 'face-coverage-dump)
;;; face-coverage-dump.el ends here
