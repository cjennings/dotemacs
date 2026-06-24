;;; test-ai-config-gptel-local-tools.el --- Tests for local GPTel tool loading -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for optional local GPTel tool loading from ai-config.el.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(setq load-prefer-newer t)
(require 'testutil-ai-config)
(require 'ai-config)

(defun test-ai-config-gptel-local-tools--write-tool (dir feature)
  "Write a temporary tool module named FEATURE into DIR."
  (let ((file (expand-file-name (format "%s.el" feature) dir)))
    (write-region
     (format ";;; %s.el --- test tool -*- lexical-binding: t; -*-\n(provide '%s)\n"
             feature feature)
     nil
     file
     nil
     'silent)))

(ert-deftest test-ai-config-gptel-local-tools-missing-directory-is-non-fatal ()
  "Missing optional tool directory should not signal or load anything."
  (let ((dir (expand-file-name "missing-gptel-tools/"
                               (make-temp-file "gptel-tools-home-" t))))
    (should-not (cj/gptel-load-local-tools dir '(test_missing_tool)))))

(ert-deftest test-ai-config-gptel-local-tools-loads-present-tools ()
  "Present tool modules should be loaded and returned in request order."
  (let ((dir (make-temp-file "gptel-tools-" t))
        (features '(test_gptel_tool_one test_gptel_tool_two)))
    (dolist (feature features)
      (test-ai-config-gptel-local-tools--write-tool dir feature))
    (should (equal (cj/gptel-load-local-tools dir features)
                   features))
    (dolist (feature features)
      (should (featurep feature)))))

(ert-deftest test-ai-config-gptel-local-tools-skips-missing-tool-files ()
  "Missing optional tool files should not prevent present tools from loading."
  (let ((dir (make-temp-file "gptel-tools-" t))
        (present 'test_gptel_present_tool)
        (missing 'test_gptel_missing_tool))
    (test-ai-config-gptel-local-tools--write-tool dir present)
    (should (equal (cj/gptel-load-local-tools dir (list present missing))
                   (list present)))
    (should (featurep present))
    (should-not (featurep missing))))

(provide 'test-ai-config-gptel-local-tools)
;;; test-ai-config-gptel-local-tools.el ends here
