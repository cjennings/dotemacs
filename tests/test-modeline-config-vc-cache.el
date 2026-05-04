;;; test-modeline-config-vc-cache.el --- Tests for modeline VC cache -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the custom modeline VC helper behavior.  The modeline evaluates
;; often, so expensive VC calls should be cached and remote files should be
;; skipped by default.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(require 'modeline-config)

(ert-deftest test-modeline-config-vc-info-caches-per-buffer ()
  "Repeated VC info reads in one buffer should reuse the cache."
  (let ((cj/modeline-vc-cache-ttl 60)
        (backend-calls 0))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/project/file.el")
      (cl-letf (((symbol-function 'float-time)
                 (lambda (&optional _time) 100.0))
                ((symbol-function 'file-remote-p)
                 (lambda (&rest _args) nil))
                ((symbol-function 'vc-backend)
                 (lambda (_file) (cl-incf backend-calls) 'Git))
                ((symbol-function 'vc-working-revision)
                 (lambda (_file _backend) "main-revision"))
                ((symbol-function 'vc-git--symbolic-ref)
                 (lambda (_file) "main"))
                ((symbol-function 'vc-state)
                 (lambda (_file _backend) 'edited)))
        (should (equal (cj/modeline-vc-info)
                       '(:branch "main" :state edited)))
        (should (equal (cj/modeline-vc-info)
                       '(:branch "main" :state edited)))
        (should (= backend-calls 1))))))

(ert-deftest test-modeline-config-vc-info-refreshes-after-ttl ()
  "Expired VC cache entries should be refreshed."
  (let ((cj/modeline-vc-cache-ttl 5)
        (backend-calls 0)
        (times '(100.0 106.0)))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/project/file.el")
      (cl-letf (((symbol-function 'float-time)
                 (lambda (&optional _time) (or (pop times) 106.0)))
                ((symbol-function 'file-remote-p)
                 (lambda (&rest _args) nil))
                ((symbol-function 'vc-backend)
                 (lambda (_file) (cl-incf backend-calls) 'Git))
                ((symbol-function 'vc-working-revision)
                 (lambda (_file _backend) "main-revision"))
                ((symbol-function 'vc-git--symbolic-ref)
                 (lambda (_file) "main"))
                ((symbol-function 'vc-state)
                 (lambda (_file _backend) 'up-to-date)))
        (should (equal (cj/modeline-vc-info)
                       '(:branch "main" :state up-to-date)))
        (should (equal (cj/modeline-vc-info)
                       '(:branch "main" :state up-to-date)))
        (should (= backend-calls 2))))))

(ert-deftest test-modeline-config-vc-info-skips-remote-files-by-default ()
  "Remote files should not call VC unless explicitly enabled."
  (let ((backend-calls 0))
    (with-temp-buffer
      (setq buffer-file-name "/ssh:host:/tmp/project/file.el")
      (cl-letf (((symbol-function 'file-remote-p)
                 (lambda (&rest _args) t))
                ((symbol-function 'vc-backend)
                 (lambda (_file) (cl-incf backend-calls) 'Git)))
        (should-not (cj/modeline-vc-info))
        (should (= backend-calls 0))))))

(ert-deftest test-modeline-config-vc-cache-clear-resets-buffer-cache ()
  "Clearing the VC cache should remove buffer-local cached values."
  (with-temp-buffer
    (setq cj/modeline-vc-cache-key '("/tmp/project/file.el")
          cj/modeline-vc-cache-time 100.0
          cj/modeline-vc-cache-value '(:branch "main" :state edited))
    (cj/modeline-vc-cache-clear)
    (should-not cj/modeline-vc-cache-key)
    (should-not cj/modeline-vc-cache-time)
    (should-not cj/modeline-vc-cache-value)))

(ert-deftest test-modeline-config-vc-render-formats-branch-and-state ()
  "VC rendering should keep the branch text and state face metadata."
  (cl-letf (((symbol-function 'cj/modeline-string-cut-middle)
             (lambda (str) str)))
    (let ((rendered (cj/modeline-vc-render '(:branch "feature/cache"
                                             :state edited))))
      (should (string-match-p "feature/cache" rendered))
      (should (text-property-any 0 (length rendered)
                                 'face 'vc-edited-state rendered))
      (should (text-property-any 0 (length rendered)
                                 'mouse-face 'mode-line-highlight rendered)))))

(provide 'test-modeline-config-vc-cache)
;;; test-modeline-config-vc-cache.el ends here
