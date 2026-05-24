;;; test-modeline-config-vc-cache-key.el --- Tests for VC modeline cache key -*- lexical-binding: t; -*-

;;; Commentary:
;; The VC modeline cache keys on the file.  A symlink whose target moves to a
;; different VC tree must invalidate the cache, so the key includes the
;; resolved `file-truename', not just the symlink path.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'modeline-config)

;;; Normal Cases

(ert-deftest test-modeline-vc-cache-key-includes-truename ()
  "Normal: the cache key includes the resolved truename of the file."
  (let ((f (make-temp-file "cj-mlkey-")))
    (unwind-protect
        (should (member (file-truename f) (cj/modeline-vc-cache-key f)))
      (delete-file f))))

;;; Boundary Cases

(ert-deftest test-modeline-vc-cache-key-changes-when-symlink-target-moves ()
  "Boundary: re-pointing a symlink to a new target changes the cache key.
The symlink path is identical both times; only its truename differs, so a
key that ignored the truename would serve a stale VC backend."
  (let* ((dir (make-temp-file "cj-mlkey-dir-" t))
         (target-a (expand-file-name "a" dir))
         (target-b (expand-file-name "b" dir))
         (link (expand-file-name "link" dir)))
    (unwind-protect
        (progn
          (write-region "" nil target-a)
          (write-region "" nil target-b)
          (make-symbolic-link target-a link)
          (let ((key-a (cj/modeline-vc-cache-key link)))
            (delete-file link)
            (make-symbolic-link target-b link)
            (let ((key-b (cj/modeline-vc-cache-key link)))
              (should-not (equal key-a key-b)))))
      (delete-directory dir t))))

(ert-deftest test-modeline-vc-cache-key-stable-for-same-file ()
  "Boundary: the key is stable across calls for an unchanged file."
  (let ((f (make-temp-file "cj-mlkey-stable-")))
    (unwind-protect
        (should (equal (cj/modeline-vc-cache-key f)
                       (cj/modeline-vc-cache-key f)))
      (delete-file f))))

(provide 'test-modeline-config-vc-cache-key)
;;; test-modeline-config-vc-cache-key.el ends here
