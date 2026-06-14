;;; test-ui-cursor-color-integration.el --- Integration tests for cursor color -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration tests for the cursor-color hook.  The cursor color now comes from
;; the active theme's faces (error / warning / success) via
;; `cj/buffer-status-color', not hard-coded hexes, so these tests pin those three
;; faces to known foregrounds and assert the resolved color per buffer state:
;; read-only -> error, unmodified -> success, modified/overwrite -> warning.

;;; Code:

(require 'ert)
(require 'user-constants)

;; `cj/set-cursor-color-according-to-mode' and the `post-command-hook' install
;; both gate on `display-graphic-p' -- a TTY / batch run is a no-op by design.
;; These integration tests exercise the work body, so pretend we're graphical
;; for the whole file.  Stub BEFORE loading ui-config: the hook install reads
;; `display-graphic-p' at load time.
(advice-add 'display-graphic-p :around
            (lambda (orig &rest args) (or (apply orig args) t)))

(require 'ui-config)

(defmacro test-cursor--with-status-colors (&rest body)
  "Run BODY with error/success/warning foregrounds pinned to known hexes.
read-only -> error #ff0000, unmodified -> success #00ff00,
modified/overwrite -> warning #ffaa00.  Restores the originals after."
  `(let ((oe (face-attribute 'error :foreground nil t))
         (os (face-attribute 'success :foreground nil t))
         (ow (face-attribute 'warning :foreground nil t)))
     (unwind-protect
         (progn
           (set-face-foreground 'error "#ff0000")
           (set-face-foreground 'success "#00ff00")
           (set-face-foreground 'warning "#ffaa00")
           ,@body)
       (when (stringp oe) (set-face-foreground 'error oe))
       (when (stringp os) (set-face-foreground 'success os))
       (when (stringp ow) (set-face-foreground 'warning ow)))))

;;; Hook Integration Tests

(ert-deftest test-cursor-color-integration-post-command-hook-installed ()
  "Test that post-command-hook is installed."
  (should (member 'cj/set-cursor-color-according-to-mode post-command-hook)))

(ert-deftest test-cursor-color-integration-function-runs-without-error ()
  "Test that cursor color function runs without error in various buffers."
  (test-cursor--with-status-colors
   (with-temp-buffer
     (should-not (condition-case err
                     (progn (cj/set-cursor-color-according-to-mode) nil)
                   (error err))))
   (with-temp-buffer
     (setq buffer-read-only t)
     (should-not (condition-case err
                     (progn (cj/set-cursor-color-according-to-mode) nil)
                   (error err))))))

(ert-deftest test-cursor-color-integration-internal-buffers-ignored ()
  "Test that internal buffers (starting with space) are ignored."
  (let ((internal-buf (get-buffer-create " *test-internal*"))
        (cj/-cursor-last-color nil)
        (cj/-cursor-last-buffer nil))
    (unwind-protect
        (with-current-buffer internal-buf
          (cj/set-cursor-color-according-to-mode)
          (should-not cj/-cursor-last-buffer))
      (kill-buffer internal-buf))))

(ert-deftest test-cursor-color-integration-normal-buffers-processed ()
  "Test that normal buffers (not starting with space) are processed."
  (test-cursor--with-status-colors
   (let ((normal-buf (get-buffer-create "test-normal"))
         (cj/-cursor-last-color nil)
         (cj/-cursor-last-buffer nil))
     (unwind-protect
         (with-current-buffer normal-buf
           (cj/set-cursor-color-according-to-mode)
           (should (equal cj/-cursor-last-buffer "test-normal")))
       (kill-buffer normal-buf)))))

(ert-deftest test-cursor-color-integration-cache-prevents-redundant-updates ()
  "Test that the cache prevents redundant cursor color updates."
  (test-cursor--with-status-colors
   (let* ((normal-buf (generate-new-buffer "test-cache"))
          (call-count 0)
          (advice-fn (lambda (&rest _) (setq call-count (1+ call-count)))))
     (unwind-protect
         (progn
           (advice-add 'set-cursor-color :before advice-fn)
           (with-current-buffer normal-buf
             ;; Clean buffer -> success (#00ff00); seed the cache with that color
             ;; and this buffer so the call is a no-op.
             (set-buffer-modified-p nil)
             (let ((cj/-cursor-last-color "#00ff00")
                   (cj/-cursor-last-buffer (buffer-name)))
               (cj/set-cursor-color-according-to-mode)
               (should (= call-count 0)))
             ;; Modify -> warning (#ffaa00); clear the buffer cache to force update.
             (insert "test")
             (let ((cj/-cursor-last-color "#00ff00")
                   (cj/-cursor-last-buffer nil))
               (cj/set-cursor-color-according-to-mode)
               (should (= call-count 1)))))
       (advice-remove 'set-cursor-color advice-fn)
       (kill-buffer normal-buf)))))

(ert-deftest test-cursor-color-integration-different-buffers-different-colors ()
  "Test that buffers in different states resolve to different theme colors."
  (test-cursor--with-status-colors
   (let ((buf1 (generate-new-buffer "test1"))
         (buf2 (generate-new-buffer "test2"))
         (cj/-cursor-last-color nil)
         (cj/-cursor-last-buffer nil))
     (unwind-protect
         (progn
           (with-current-buffer buf1
             (setq buffer-read-only t)
             (cj/set-cursor-color-according-to-mode)
             (should (equal cj/-cursor-last-color "#ff0000")))  ; read-only -> error
           (with-current-buffer buf2
             (setq buffer-read-only nil)
             (set-buffer-modified-p nil)
             (cj/set-cursor-color-according-to-mode)
             (should (equal cj/-cursor-last-color "#00ff00"))))  ; unmodified -> success
       (kill-buffer buf1)
       (kill-buffer buf2)))))

(ert-deftest test-cursor-color-integration-buffer-modification-changes-color ()
  "Test that modifying a buffer moves the cursor from success to warning."
  (test-cursor--with-status-colors
   (let ((normal-buf (generate-new-buffer "test-mod"))
         (cj/-cursor-last-color nil)
         (cj/-cursor-last-buffer nil))
     (unwind-protect
         (with-current-buffer normal-buf
           (set-buffer-modified-p nil)
           (cj/set-cursor-color-according-to-mode)
           (should (equal cj/-cursor-last-color "#00ff00"))  ; unmodified -> success
           (insert "test")
           (should (buffer-modified-p))
           (setq cj/-cursor-last-buffer nil)
           (cj/set-cursor-color-according-to-mode)
           (should (equal cj/-cursor-last-color "#ffaa00")))  ; modified -> warning
       (kill-buffer normal-buf)))))

(ert-deftest test-cursor-color-integration-save-changes-color-back ()
  "Test that saving a modified buffer moves the cursor from warning to success."
  (test-cursor--with-status-colors
   (let ((test-file (make-temp-file "test-cursor-"))
         (cj/-cursor-last-color nil)
         (cj/-cursor-last-buffer nil))
     (unwind-protect
         (with-current-buffer (find-file-noselect test-file)
           (insert "test")
           (should (buffer-modified-p))
           (cj/set-cursor-color-according-to-mode)
           (should (equal cj/-cursor-last-color "#ffaa00"))  ; modified -> warning
           (save-buffer)
           (should-not (buffer-modified-p))
           (setq cj/-cursor-last-buffer nil)
           (cj/set-cursor-color-according-to-mode)
           (should (equal cj/-cursor-last-color "#00ff00"))  ; unmodified -> success
           (kill-buffer))
       (delete-file test-file)))))

;;; Performance Tests

(ert-deftest test-cursor-color-integration-multiple-calls-efficient ()
  "Test that multiple rapid calls don't cause performance issues."
  (test-cursor--with-status-colors
   (with-temp-buffer
     (let ((start-time (current-time)))
       (dotimes (_ 1000)
         (cj/set-cursor-color-according-to-mode))
       (let ((elapsed (float-time (time-subtract (current-time) start-time))))
         (should (< elapsed 1.0)))))))

(provide 'test-ui-cursor-color-integration)
;;; test-ui-cursor-color-integration.el ends here
