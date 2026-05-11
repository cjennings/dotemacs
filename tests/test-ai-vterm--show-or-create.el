;;; test-ai-vterm--show-or-create.el --- Tests for cj/--ai-vterm-show-or-create -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests the show-or-create branching:
;;
;; - buffer absent          -> vterm called, agent command sent
;; - buffer present, live   -> vterm not called, buffer displayed
;; - buffer present, dead   -> old buffer killed, vterm recreates
;;
;; vterm functions are stubbed so the test does no process spawning.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-vterm)

;; vterm isn't loaded in batch -- provide stubs so cl-letf has overrides.
(unless (fboundp 'vterm)
  (defun vterm (&optional _name) nil))
(unless (fboundp 'vterm-send-string)
  (defun vterm-send-string (_s &optional _) nil))
(unless (fboundp 'vterm-send-return)
  (defun vterm-send-return () nil))

(defmacro test-ai-vterm--with-mock-vterm (vars &rest body)
  "Run BODY with vterm + send-string + send-return mocked.

VARS is a plist of capture variable names: :calls, :strings, :returns,
:default-dir.  The test references these names directly inside BODY."
  (declare (indent 1) (debug t))
  (let ((calls (plist-get vars :calls))
        (strings (plist-get vars :strings))
        (returns (plist-get vars :returns))
        (ddir (plist-get vars :default-dir)))
    `(let ((,calls '())
           (,strings '())
           (,returns 0)
           (,ddir nil))
       (cl-letf (((symbol-function 'vterm)
                  (lambda (&optional name)
                    (push name ,calls)
                    (setq ,ddir default-directory)
                    (with-current-buffer (get-buffer-create name)
                      (current-buffer))))
                 ((symbol-function 'vterm-send-string)
                  (lambda (s &optional _) (push s ,strings)))
                 ((symbol-function 'vterm-send-return)
                  (lambda () (cl-incf ,returns))))
         ,@body))))

(defun test-ai-vterm--cleanup (name)
  "Kill buffer NAME if it exists."
  (when (get-buffer name)
    (kill-buffer name)))

(ert-deftest test-ai-vterm--show-or-create-creates-when-buffer-missing ()
  "Normal: no existing buffer -> vterm called once, launch cmd sent."
  (let ((name "agent [normal-create-test]"))
    (test-ai-vterm--cleanup name)
    (unwind-protect
        (test-ai-vterm--with-mock-vterm (:calls calls :strings strings
                                         :returns returns :default-dir ddir)
          (cj/--ai-vterm-show-or-create "/tmp/some-project" name)
          (should (equal calls (list name)))
          (should (equal strings
                         (list (cj/--ai-vterm-launch-command "/tmp/some-project"))))
          (should (= returns 1))
          (should (equal ddir "/tmp/some-project")))
      (test-ai-vterm--cleanup name))))

(ert-deftest test-ai-vterm--show-or-create-displays-existing-when-process-live ()
  "Normal: buffer exists with live process -> vterm not called."
  (let ((name "agent [reuse-test]"))
    (test-ai-vterm--cleanup name)
    (unwind-protect
        (let ((buf (get-buffer-create name)))
          (cl-letf (((symbol-function 'cj/--ai-vterm-process-live-p)
                     (lambda (b) (and (eq b buf) t))))
            (test-ai-vterm--with-mock-vterm (:calls calls :strings strings
                                             :returns returns :default-dir _ddir)
              (cj/--ai-vterm-show-or-create "/tmp/reuse" name)
              (should (null calls))
              (should (null strings))
              (should (= returns 0)))))
      (test-ai-vterm--cleanup name))))

(ert-deftest test-ai-vterm--show-or-create-recreates-when-process-dead ()
  "Boundary: buffer exists with dead process -> killed and recreated."
  (let ((name "agent [dead-test]"))
    (test-ai-vterm--cleanup name)
    (unwind-protect
        (let ((stale (get-buffer-create name)))
          (cl-letf (((symbol-function 'cj/--ai-vterm-process-live-p)
                     (lambda (_b) nil)))
            (test-ai-vterm--with-mock-vterm (:calls calls :strings strings
                                             :returns returns :default-dir _ddir)
              (cj/--ai-vterm-show-or-create "/tmp/dead" name)
              (should (equal calls (list name)))
              (should (equal strings
                             (list (cj/--ai-vterm-launch-command "/tmp/dead"))))
              (should (= returns 1))
              (should-not (buffer-live-p stale)))))
      (test-ai-vterm--cleanup name))))

(ert-deftest test-ai-vterm--show-or-create-preserves-selected-window ()
  "Regression: vterm's pop-to-buffer-same-window must not bury the dashboard.

Real `vterm' replaces the selected window's buffer as a side-effect of
construction.  On a fresh-boot frame (one window showing the dashboard),
that side-effect previously left the original window pointing at the new
agent buffer; the dashboard was buried, the alist-routed split then
created a second window also showing agent.  The wrapper must restore
the original window state before `display-buffer' fires so dashboard
stays put and the alist places agent into a fresh right-side split.

This test stubs `vterm' to mimic the pop-to-buffer-same-window side-effect
and asserts the originally-selected window still shows its original buffer
after `cj/--ai-vterm-show-or-create' returns."
  (let ((agent-name "agent [preserve-window-test]")
        (orig-name "*test-original-buffer*"))
    (test-ai-vterm--cleanup agent-name)
    (when (get-buffer orig-name) (kill-buffer orig-name))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((orig-buf (get-buffer-create orig-name))
                (orig-win (selected-window)))
            (set-window-buffer orig-win orig-buf)
            (cl-letf
                (((symbol-function 'vterm)
                  (lambda (&optional name)
                    (let ((buf (get-buffer-create name)))
                      (set-window-buffer (selected-window) buf)
                      buf)))
                 ((symbol-function 'vterm-send-string)
                  (lambda (_s &optional _) nil))
                 ((symbol-function 'vterm-send-return)
                  (lambda () nil)))
              (cj/--ai-vterm-show-or-create "/tmp/preserve" agent-name)
              (should (eq (window-buffer orig-win) orig-buf)))))
      (test-ai-vterm--cleanup agent-name)
      (when (get-buffer orig-name) (kill-buffer orig-name)))))

(ert-deftest test-ai-vterm--show-or-create-returns-buffer ()
  "Normal: return value is the vterm buffer."
  (let ((name "agent [return-test]"))
    (test-ai-vterm--cleanup name)
    (unwind-protect
        (test-ai-vterm--with-mock-vterm (:calls _c :strings _s
                                         :returns _r :default-dir _d)
          (let ((result (cj/--ai-vterm-show-or-create "/tmp/return" name)))
            (should (bufferp result))
            (should (equal (buffer-name result) name))))
      (test-ai-vterm--cleanup name))))

(provide 'test-ai-vterm--show-or-create)
;;; test-ai-vterm--show-or-create.el ends here
