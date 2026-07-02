;;; test-ai-term--show-or-create.el --- Tests for cj/--ai-term-show-or-create -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests the show-or-create branching:
;;
;; - buffer absent          -> eat called, agent command + newline sent
;; - buffer present, live   -> eat not called, buffer displayed
;; - buffer present, dead   -> old buffer killed, eat recreates
;;
;; eat + the send helper are stubbed so the test does no process spawning.
;; Production calls (eat) and relies on the dynamically bound `eat-buffer-name';
;; the mock honors that.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-term)

;; eat isn't loaded in batch -- provide a stub so cl-letf has an override.
(unless (fboundp 'eat)
  (defun eat (&optional _program _arg) nil))

(defmacro test-ai-term--with-mock-eat (vars &rest body)
  "Run BODY with eat + `cj/--ai-term-send-string' mocked.

VARS is a plist of capture variable names: :calls (buffer names eat was asked
to create), :strings (sent strings), :default-dir.  The mocked `eat' creates
and returns a buffer named after the dynamically bound `eat-buffer-name',
mirroring the real entry point."
  (declare (indent 1) (debug t))
  (let ((calls (plist-get vars :calls))
        (strings (plist-get vars :strings))
        (ddir (plist-get vars :default-dir)))
    `(let ((,calls '())
           (,strings '())
           (,ddir nil))
       (cl-letf (((symbol-function 'eat)
                  (lambda (&optional _program _arg)
                    (setq ,ddir default-directory)
                    (let ((b (get-buffer-create eat-buffer-name)))
                      (push (buffer-name b) ,calls)
                      b)))
                 ((symbol-function 'cj/--ai-term-send-string)
                  (lambda (_buf s) (push s ,strings)))
                 ;; Keep the create path hermetic: no tmux subprocess for the
                 ;; fresh-session check, no real /color poll timer.
                 ((symbol-function 'cj/--ai-term-live-tmux-sessions)
                  (lambda () nil))
                 ((symbol-function 'cj/--ai-term-schedule-color)
                  (lambda (_buffer _color) nil)))
         ,@body))))

(defun test-ai-term--cleanup (name)
  "Kill buffer NAME if it exists."
  (when (get-buffer name)
    (kill-buffer name)))

(ert-deftest test-ai-term--show-or-create-creates-when-buffer-missing ()
  "Normal: no existing buffer -> eat called once, launch cmd + newline sent,
the project recorded at the front of the MRU list."
  (let ((name "agent [normal-create-test]")
        (cj/--ai-term-mru nil))
    (test-ai-term--cleanup name)
    (unwind-protect
        (test-ai-term--with-mock-eat (:calls calls :strings strings
                                      :default-dir ddir)
          (cj/--ai-term-show-or-create "/tmp/some-project" name)
          (should (equal calls (list name)))
          (should (equal strings
                         (list (concat (cj/--ai-term-launch-command "/tmp/some-project")
                                       "\n"))))
          (should (equal ddir "/tmp/some-project"))
          (should (equal (car cj/--ai-term-mru) "/tmp/some-project")))
      (test-ai-term--cleanup name))))

(ert-deftest test-ai-term--show-or-create-displays-existing-when-process-live ()
  "Normal: buffer exists with live process -> eat not called."
  (let ((name "agent [reuse-test]"))
    (test-ai-term--cleanup name)
    (unwind-protect
        (let ((buf (get-buffer-create name)))
          (cl-letf (((symbol-function 'cj/--ai-term-process-live-p)
                     (lambda (b) (and (eq b buf) t))))
            (test-ai-term--with-mock-eat (:calls calls :strings strings
                                          :default-dir _ddir)
              (cj/--ai-term-show-or-create "/tmp/reuse" name)
              (should (null calls))
              (should (null strings)))))
      (test-ai-term--cleanup name))))

(ert-deftest test-ai-term--show-or-create-recreates-when-process-dead ()
  "Boundary: buffer exists with dead process -> killed and recreated."
  (let ((name "agent [dead-test]"))
    (test-ai-term--cleanup name)
    (unwind-protect
        (let ((stale (get-buffer-create name)))
          (cl-letf (((symbol-function 'cj/--ai-term-process-live-p)
                     (lambda (_b) nil)))
            (test-ai-term--with-mock-eat (:calls calls :strings strings
                                          :default-dir _ddir)
              (cj/--ai-term-show-or-create "/tmp/dead" name)
              (should (equal calls (list name)))
              (should (equal strings
                             (list (concat (cj/--ai-term-launch-command "/tmp/dead")
                                           "\n"))))
              (should-not (buffer-live-p stale)))))
      (test-ai-term--cleanup name))))

(ert-deftest test-ai-term--show-or-create-preserves-selected-window ()
  "Regression: eat's same-window switch must not bury the dashboard.

Real `eat' switches the selected window to its buffer as a side-effect of
construction.  On a fresh-boot frame (one window showing the dashboard), that
side-effect would otherwise leave the original window pointing at the new
agent buffer.  The wrapper runs `(eat)' inside `save-window-excursion' so the
original window state is restored before `display-buffer' fires, leaving the
dashboard put and letting the alist place agent into a fresh split.

This test stubs `eat' to mimic the same-window side-effect and asserts the
originally-selected window still shows its original buffer afterward."
  (let ((agent-name "agent [preserve-window-test]")
        (orig-name "*test-original-buffer*"))
    (test-ai-term--cleanup agent-name)
    (when (get-buffer orig-name) (kill-buffer orig-name))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((orig-buf (get-buffer-create orig-name))
                (orig-win (selected-window)))
            (set-window-buffer orig-win orig-buf)
            (cl-letf
                (((symbol-function 'eat)
                  (lambda (&optional _program _arg)
                    (let ((buf (get-buffer-create eat-buffer-name)))
                      (set-window-buffer (selected-window) buf)
                      buf)))
                 ((symbol-function 'cj/--ai-term-send-string)
                  (lambda (_buf _s) nil)))
              (cj/--ai-term-show-or-create "/tmp/preserve" agent-name)
              (should (eq (window-buffer orig-win) orig-buf)))))
      (test-ai-term--cleanup agent-name)
      (when (get-buffer orig-name) (kill-buffer orig-name)))))

(ert-deftest test-ai-term--show-or-create-returns-buffer ()
  "Normal: return value is the eat buffer named after the project."
  (let ((name "agent [return-test]"))
    (test-ai-term--cleanup name)
    (unwind-protect
        (test-ai-term--with-mock-eat (:calls _c :strings _s :default-dir _d)
          (let ((result (cj/--ai-term-show-or-create "/tmp/return" name)))
            (should (bufferp result))
            (should (equal (buffer-name result) name))))
      (test-ai-term--cleanup name))))

(provide 'test-ai-term--show-or-create)
;;; test-ai-term--show-or-create.el ends here
