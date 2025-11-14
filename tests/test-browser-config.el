;;; test-browser-config.el --- Tests for browser-config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for browser-config.el - browser selection and configuration.
;;
;; Testing approach:
;; - Tests focus on internal `cj/--do-*` functions (pure business logic)
;; - File I/O tests use temp files
;; - executable-find is stubbed to control available browsers
;; - Each test is isolated with setup/teardown
;; - Tests verify return values, not user messages

;;; Code:

(require 'ert)
(require 'testutil-general)

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Load the module with temp file to avoid polluting real config
(defvar test-browser--temp-choice-file nil
  "Temporary file for browser choice during tests.")

(defun test-browser-setup ()
  "Setup test environment before each test."
  (setq test-browser--temp-choice-file (make-temp-file "browser-choice-test" nil ".el"))
  (setq cj/browser-choice-file test-browser--temp-choice-file))

(defun test-browser-teardown ()
  "Clean up test environment after each test."
  (when (and test-browser--temp-choice-file
             (file-exists-p test-browser--temp-choice-file))
    (delete-file test-browser--temp-choice-file))
  (setq test-browser--temp-choice-file nil))

;; Now require the module
(require 'browser-config)

;;; Helper Functions

(defun test-browser-make-plist (name &optional executable path)
  "Create a test browser plist with NAME, EXECUTABLE, and PATH."
  (list :function 'eww-browse-url
        :name name
        :executable executable
        :path path
        :program-var nil))

;;; Normal Cases - Discover Browsers

(ert-deftest test-browser-discover-finds-eww ()
  "Should always find built-in EWW browser."
  (test-browser-setup)
  (let ((browsers (cj/discover-browsers)))
    (should (cl-find-if (lambda (b) (string= (plist-get b :name) "EWW (Emacs Browser)"))
                        browsers)))
  (test-browser-teardown))

(ert-deftest test-browser-discover-deduplicates-names ()
  "Should not return duplicate browser names."
  (test-browser-setup)
  (let ((browsers (cj/discover-browsers))
        (names (mapcar (lambda (b) (plist-get b :name)) (cj/discover-browsers))))
    (should (= (length names) (length (cl-remove-duplicates names :test 'string=)))))
  (test-browser-teardown))

;;; Normal Cases - Apply Browser Choice

(ert-deftest test-browser-apply-valid-browser ()
  "Should successfully apply a valid browser configuration."
  (test-browser-setup)
  (let ((browser (test-browser-make-plist "Test Browser")))
    (let ((result (cj/--do-apply-browser-choice browser)))
      (should (eq result 'success))
      (should (eq browse-url-browser-function 'eww-browse-url))))
  (test-browser-teardown))

(ert-deftest test-browser-apply-sets-program-var ()
  "Should set browser program variable if specified."
  (test-browser-setup)
  (let ((browser (list :function 'browse-url-chrome
                       :name "Chrome"
                       :executable "chrome"
                       :path "/usr/bin/chrome"
                       :program-var 'browse-url-chrome-program)))
    (cj/--do-apply-browser-choice browser)
    (should (string= browse-url-chrome-program "/usr/bin/chrome")))
  (test-browser-teardown))

;;; Normal Cases - Save and Load

(ert-deftest test-browser-save-and-load-choice ()
  "Should save and load browser choice correctly."
  (test-browser-setup)
  (let ((browser (test-browser-make-plist "Saved Browser" "firefox" "/usr/bin/firefox")))
    (cj/save-browser-choice browser)
    (let ((loaded (cj/load-browser-choice)))
      (should loaded)
      (should (string= (plist-get loaded :name) "Saved Browser"))
      (should (string= (plist-get loaded :executable) "firefox"))))
  (test-browser-teardown))

;;; Normal Cases - Choose Browser

(ert-deftest test-browser-choose-saves-and-applies ()
  "Should save and apply browser choice."
  (test-browser-setup)
  (let ((browser (test-browser-make-plist "Test")))
    (let ((result (cj/--do-choose-browser browser)))
      (should (eq result 'success))
      ;; Verify it was saved
      (let ((loaded (cj/load-browser-choice)))
        (should (string= (plist-get loaded :name) "Test")))))
  (test-browser-teardown))

;;; Normal Cases - Initialize Browser

(ert-deftest test-browser-initialize-with-saved-choice ()
  "Should load and use saved browser choice."
  (test-browser-setup)
  (let ((browser (test-browser-make-plist "Saved")))
    (cj/save-browser-choice browser)
    (let ((result (cj/--do-initialize-browser)))
      (should (eq (car result) 'loaded))
      (should (plist-get (cdr result) :name))
      (should (string= (plist-get (cdr result) :name) "Saved"))))
  (test-browser-teardown))

(ert-deftest test-browser-initialize-without-saved-choice ()
  "Should use first available browser when no saved choice."
  (test-browser-setup)
  ;; Delete any saved choice
  (when (file-exists-p cj/browser-choice-file)
    (delete-file cj/browser-choice-file))
  (let ((result (cj/--do-initialize-browser)))
    (should (eq (car result) 'first-available))
    (should (plist-get (cdr result) :name)))
  (test-browser-teardown))

;;; Boundary Cases - Apply Browser

(ert-deftest test-browser-apply-nil-plist ()
  "Should return 'invalid-plist for nil browser."
  (test-browser-setup)
  (let ((result (cj/--do-apply-browser-choice nil)))
    (should (eq result 'invalid-plist)))
  (test-browser-teardown))

(ert-deftest test-browser-apply-missing-function ()
  "Should return 'invalid-plist when :function is missing."
  (test-browser-setup)
  (let ((browser (list :name "Bad Browser" :function nil)))
    (let ((result (cj/--do-apply-browser-choice browser)))
      (should (eq result 'invalid-plist))))
  (test-browser-teardown))

(ert-deftest test-browser-apply-with-nil-path ()
  "Should handle nil path for built-in browser."
  (test-browser-setup)
  (let ((browser (test-browser-make-plist "EWW" nil nil)))
    (let ((result (cj/--do-apply-browser-choice browser)))
      (should (eq result 'success))))
  (test-browser-teardown))

;;; Boundary Cases - Save and Load

(ert-deftest test-browser-load-nonexistent-file ()
  "Should return nil when loading from nonexistent file."
  (test-browser-setup)
  (when (file-exists-p cj/browser-choice-file)
    (delete-file cj/browser-choice-file))
  (let ((result (cj/load-browser-choice)))
    (should (null result)))
  (test-browser-teardown))

(ert-deftest test-browser-load-corrupt-file ()
  "Should return nil when loading corrupt file."
  (test-browser-setup)
  (with-temp-file cj/browser-choice-file
    (insert "this is not valid elisp {{{"))
  (let ((result (cj/load-browser-choice)))
    (should (null result)))
  (test-browser-teardown))

(ert-deftest test-browser-load-file-without-variable ()
  "Should return nil when file doesn't define expected variable."
  (test-browser-setup)
  (with-temp-file cj/browser-choice-file
    (insert "(setq some-other-variable 'foo)"))
  ;; Unset any previously loaded variable
  (makunbound 'cj/saved-browser-choice)
  (let ((result (cj/load-browser-choice)))
    (should (null result)))
  (test-browser-teardown))

;;; Boundary Cases - Choose Browser

(ert-deftest test-browser-choose-empty-plist ()
  "Should handle empty plist gracefully."
  (test-browser-setup)
  (let ((result (cj/--do-choose-browser nil)))
    (should (eq result 'invalid-plist)))
  (test-browser-teardown))

;;; Error Cases - File Operations

(ert-deftest test-browser-save-to-readonly-location ()
  "Should return 'save-failed when cannot write file."
  (test-browser-setup)
  ;; Make file read-only
  (with-temp-file cj/browser-choice-file
    (insert ";; test"))
  (set-file-modes cj/browser-choice-file #o444)
  (let ((browser (test-browser-make-plist "Test"))
        (result nil))
    (setq result (cj/--do-choose-browser browser))
    ;; Restore permissions before teardown
    (set-file-modes cj/browser-choice-file #o644)
    (should (eq result 'save-failed)))
  (test-browser-teardown))

;;; Browser Discovery Tests

(ert-deftest test-browser-discover-returns-plists ()
  "Should return properly formatted browser plists."
  (test-browser-setup)
  (let ((browsers (cj/discover-browsers)))
    (should (> (length browsers) 0))
    (dolist (browser browsers)
      (should (plist-member browser :function))
      (should (plist-member browser :name))
      (should (plist-member browser :executable))
      (should (plist-member browser :path))))
  (test-browser-teardown))

(ert-deftest test-browser-format-location-keys ()
  "Should have all required keys in browser plist."
  (test-browser-setup)
  (let ((browsers (cj/discover-browsers)))
    (when browsers
      (let ((browser (car browsers)))
        (should (plist-get browser :function))
        (should (plist-get browser :name)))))
  (test-browser-teardown))

;;; Integration Tests

(ert-deftest test-browser-full-cycle ()
  "Should handle full save-load-apply cycle."
  (test-browser-setup)
  (let ((browser (test-browser-make-plist "Cycle Test" "test-browser" "/usr/bin/test")))
    ;; Choose (save and apply)
    (should (eq (cj/--do-choose-browser browser) 'success))
    ;; Verify it was saved
    (let ((loaded (cj/load-browser-choice)))
      (should loaded)
      (should (string= (plist-get loaded :name) "Cycle Test")))
    ;; Initialize should load the saved choice
    (let ((result (cj/--do-initialize-browser)))
      (should (eq (car result) 'loaded))
      (should (string= (plist-get (cdr result) :name) "Cycle Test"))))
  (test-browser-teardown))

(ert-deftest test-browser-overwrite-choice ()
  "Should overwrite previous browser choice."
  (test-browser-setup)
  (let ((browser1 (test-browser-make-plist "First"))
        (browser2 (test-browser-make-plist "Second")))
    (cj/--do-choose-browser browser1)
    (cj/--do-choose-browser browser2)
    (let ((loaded (cj/load-browser-choice)))
      (should (string= (plist-get loaded :name) "Second"))))
  (test-browser-teardown))

(provide 'test-browser-config)
;;; test-browser-config.el ends here
