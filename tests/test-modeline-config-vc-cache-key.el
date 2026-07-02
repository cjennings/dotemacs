;;; test-modeline-config-vc-cache-key.el --- Tests for VC modeline cache key -*- lexical-binding: t; -*-

;;; Commentary:
;; The VC modeline cache keys on the file path and the `cj/modeline-vc-show-remote'
;; flag only.  `file-truename' is deliberately NOT in the key: it would run on
;; every redisplay (the mode-line rebuilds the key each render to check validity),
;; and a moved symlink target is picked up at the next TTL refresh anyway, since
;; `vc-backend' resolves the link fresh.  The per-render stat isn't worth it.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'modeline-config)

(ert-deftest test-modeline-vc-cache-key-is-file-and-show-remote ()
  "Normal: the key is (FILE SHOW-REMOTE), with no per-render file-truename stat."
  (let ((cj/modeline-vc-show-remote nil))
    (should (equal (cj/--modeline-vc-cache-key "/x/y.el") '("/x/y.el" nil)))))

(ert-deftest test-modeline-vc-cache-key-tracks-show-remote ()
  "Boundary: toggling show-remote yields a different key (separate cache entry)."
  (should-not (equal (let ((cj/modeline-vc-show-remote nil))
                       (cj/--modeline-vc-cache-key "/x/y.el"))
                     (let ((cj/modeline-vc-show-remote t))
                       (cj/--modeline-vc-cache-key "/x/y.el")))))

(ert-deftest test-modeline-vc-cache-key-stable-for-same-file ()
  "Boundary: the key is stable across calls for an unchanged file + show-remote."
  (let ((cj/modeline-vc-show-remote nil))
    (should (equal (cj/--modeline-vc-cache-key "/x/y.el")
                   (cj/--modeline-vc-cache-key "/x/y.el")))))

(provide 'test-modeline-config-vc-cache-key)
;;; test-modeline-config-vc-cache-key.el ends here
