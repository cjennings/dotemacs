;;; test-ui-cursor-color-integration.el --- Integration tests for cursor color -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration tests for cursor color hook behavior.
;; Tests that cursor color actually updates when switching buffers,
;; modifying files, etc.

;;; Code:

(require 'ert)
(require 'user-constants)
(require 'ui-config)

;;; Hook Integration Tests

(ert-deftest test-cursor-color-integration-post-command-hook-installed ()
  "Test that post-command-hook is installed."
  (should (member 'cj/set-cursor-color-according-to-mode post-command-hook)))

(ert-deftest test-cursor-color-integration-function-runs-without-error ()
  "Test that cursor color function runs without error in various buffers."
  (with-temp-buffer
    (should-not (condition-case err
                    (progn
                      (cj/set-cursor-color-according-to-mode)
                      nil)
                  (error err))))

  (with-temp-buffer
    (setq buffer-read-only t)
    (should-not (condition-case err
                    (progn
                      (cj/set-cursor-color-according-to-mode)
                      nil)
                  (error err)))))

(ert-deftest test-cursor-color-integration-internal-buffers-ignored ()
  "Test that internal buffers (starting with space) are ignored."
  (let ((internal-buf (get-buffer-create " *test-internal*"))
        (cj/-cursor-last-color nil)
        (cj/-cursor-last-buffer nil))
    (unwind-protect
        (with-current-buffer internal-buf
          (cj/set-cursor-color-according-to-mode)
          ;; Cursor state should not have been updated
          (should-not cj/-cursor-last-buffer))
      (kill-buffer internal-buf))))

(ert-deftest test-cursor-color-integration-normal-buffers-processed ()
  "Test that normal buffers (not starting with space) are processed."
  (let ((normal-buf (get-buffer-create "test-normal"))
        (cj/-cursor-last-color nil)
        (cj/-cursor-last-buffer nil))
    (unwind-protect
        (with-current-buffer normal-buf
          (cj/set-cursor-color-according-to-mode)
          ;; Cursor state should have been updated
          (should (equal cj/-cursor-last-buffer "test-normal")))
      (kill-buffer normal-buf))))

(ert-deftest test-cursor-color-integration-cache-prevents-redundant-updates ()
  "Test that cache prevents redundant cursor color updates."
  (let* ((normal-buf (generate-new-buffer "test-cache"))
         (call-count 0)
         (advice-fn (lambda (&rest _) (setq call-count (1+ call-count)))))
    (unwind-protect
        (progn
          (advice-add 'set-cursor-color :before advice-fn)
          (with-current-buffer normal-buf
            ;; First call - cache matches, no update
            (let ((cj/-cursor-last-color "#ffffff")
                  (cj/-cursor-last-buffer (buffer-name)))
              (cj/set-cursor-color-according-to-mode)
              (should (= call-count 0)))  ; Cached, no update needed

            ;; Modify buffer and clear cache - should update
            (insert "test")
            (let ((cj/-cursor-last-buffer nil))  ; Force update
              (cj/set-cursor-color-according-to-mode)
              (should (= call-count 1)))))  ; New state, should update
      (advice-remove 'set-cursor-color advice-fn)
      (kill-buffer normal-buf))))

(ert-deftest test-cursor-color-integration-different-buffers-different-colors ()
  "Test that switching between buffers with different states updates cursor."
  (let ((buf1 (generate-new-buffer "test1"))
        (buf2 (generate-new-buffer "test2"))
        (cj/-cursor-last-color nil)
        (cj/-cursor-last-buffer nil))
    (unwind-protect
        (progn
          ;; Set buf1 to read-only
          (with-current-buffer buf1
            (setq buffer-read-only t)
            (cj/set-cursor-color-according-to-mode)
            (should (equal cj/-cursor-last-color "#f06a3f")))  ; Red

          ;; Set buf2 to normal
          (with-current-buffer buf2
            (setq buffer-read-only nil)
            (set-buffer-modified-p nil)
            (cj/set-cursor-color-according-to-mode)
            (should (equal cj/-cursor-last-color "#ffffff"))))  ; White
      (kill-buffer buf1)
      (kill-buffer buf2))))

(ert-deftest test-cursor-color-integration-buffer-modification-changes-color ()
  "Test that modifying a buffer changes cursor from white to green."
  (let ((normal-buf (generate-new-buffer "test-mod"))
        (cj/-cursor-last-color nil)
        (cj/-cursor-last-buffer nil))
    (unwind-protect
        (with-current-buffer normal-buf
          ;; Start unmodified
          (set-buffer-modified-p nil)
          (cj/set-cursor-color-according-to-mode)
          (should (equal cj/-cursor-last-color "#ffffff"))  ; White

          ;; Modify buffer
          (insert "test")
          (should (buffer-modified-p))
          ;; Reset last buffer to force update
          (setq cj/-cursor-last-buffer nil)
          (cj/set-cursor-color-according-to-mode)
          (should (equal cj/-cursor-last-color "#64aa0f")))  ; Green
      (kill-buffer normal-buf))))

(ert-deftest test-cursor-color-integration-save-changes-color-back ()
  "Test that saving a modified buffer changes cursor from green to white."
  (let ((test-file (make-temp-file "test-cursor-"))
        (cj/-cursor-last-color nil)
        (cj/-cursor-last-buffer nil))
    (unwind-protect
        (progn
          ;; Create and modify file
          (with-current-buffer (find-file-noselect test-file)
            (insert "test")
            (should (buffer-modified-p))
            (cj/set-cursor-color-according-to-mode)
            (should (equal cj/-cursor-last-color "#64aa0f"))  ; Green

            ;; Save file
            (save-buffer)
            (should-not (buffer-modified-p))
            (cj/set-cursor-color-according-to-mode)
            (should (equal cj/-cursor-last-color "#ffffff"))  ; White
            (kill-buffer)))
      (delete-file test-file))))

;;; Performance Tests

(ert-deftest test-cursor-color-integration-multiple-calls-efficient ()
  "Test that multiple rapid calls don't cause performance issues."
  (with-temp-buffer
    (let ((start-time (current-time)))
      ;; Call 1000 times
      (dotimes (_ 1000)
        (cj/set-cursor-color-according-to-mode))
      (let ((elapsed (float-time (time-subtract (current-time) start-time))))
        ;; Should complete in less than 1 second (cache makes this very fast)
        (should (< elapsed 1.0))))))

(provide 'test-ui-cursor-color-integration)
;;; test-ui-cursor-color-integration.el ends here
