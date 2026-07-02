;;; test-ai-term--project-color.el --- Tests for per-project session colors -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests the per-project /color auto-assignment: the project -> color-name
;; mapping (override alist + deterministic hash fallback), the TUI-ready
;; detector, the two-step /color send, the polling scheduler, and the
;; show-or-create wiring (fresh sessions get a color; tmux reattaches are
;; never injected into).  eat, tmux discovery, and timers are stubbed.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-term)

(declare-function cj/--ai-term-project-color "ai-term" (dir))
(declare-function cj/--ai-term-color-ready-p "ai-term-backend-eat" (buffer))
(declare-function cj/--ai-term-send-color "ai-term-backend-eat" (buffer color))
(declare-function cj/--ai-term-schedule-color "ai-term-backend-eat" (buffer color))
(declare-function cj/--ai-term-show-or-create "ai-term-backend-eat" (dir name))

(defvar cj/ai-term-project-colors)
(defvar cj/--ai-term-color-names)
(defvar cj/--ai-term-mru)
(defvar eat-buffer-name)

;; eat isn't loaded in batch -- provide a stub so cl-letf has an override.
(unless (fboundp 'eat)
  (defun eat (&optional _program _arg) nil))

;;; ------------------------ cj/--ai-term-project-color ------------------------

(ert-deftest test-ai-term-project-color-is-deterministic ()
  "Normal: the same project dir always maps to the same color name."
  (let ((cj/ai-term-project-colors nil))
    (should (equal (cj/--ai-term-project-color "/home/u/code/someproj")
                   (cj/--ai-term-project-color "/home/u/code/someproj")))))

(ert-deftest test-ai-term-project-color-returns-a-known-name ()
  "Normal: the hash fallback lands on one of Claude Code's color names."
  (let ((cj/ai-term-project-colors nil))
    (should (seq-contains-p cj/--ai-term-color-names
                            (cj/--ai-term-project-color "/home/u/code/xyz")
                            #'equal))))

(ert-deftest test-ai-term-project-color-alist-override-wins ()
  "Normal: an explicit project entry beats the hash."
  (let ((cj/ai-term-project-colors '(("myproj" . "purple"))))
    (should (equal (cj/--ai-term-project-color "/home/u/code/myproj") "purple"))))

(ert-deftest test-ai-term-project-color-trailing-slash-insensitive ()
  "Boundary: a trailing slash on the dir does not change the color."
  (let ((cj/ai-term-project-colors nil))
    (should (equal (cj/--ai-term-project-color "/home/u/code/proj")
                   (cj/--ai-term-project-color "/home/u/code/proj/")))))

;;; ----------------------- cj/--ai-term-color-ready-p -------------------------

(defun test-ai-term-pc--buffer-with (content)
  "Make a temp buffer holding CONTENT and return `cj/--ai-term-color-ready-p' on it."
  (with-temp-buffer
    (insert content)
    (cj/--ai-term-color-ready-p (current-buffer))))

(ert-deftest test-ai-term-color-ready-detects-idle-tui ()
  "Normal: banner present + untouched prompt line reads as ready."
  (should (test-ai-term-pc--buffer-with
           "───────\n❯ \n───────\n  ⏵⏵ bypass permissions on (shift+tab to cycle)\n")))

(ert-deftest test-ai-term-color-ready-rejects-typed-prompt ()
  "Boundary: text already on the prompt line means never inject."
  (should-not (test-ai-term-pc--buffer-with
               "───────\n❯ fix the bug\n───────\n  ⏵⏵ bypass permissions on\n")))

(ert-deftest test-ai-term-color-ready-rejects-plain-shell ()
  "Error: a shell without the Claude banner is not ready (fail-safe: no injection)."
  (should-not (test-ai-term-pc--buffer-with "cjennings@velox ~ $ \n")))

(ert-deftest test-ai-term-color-ready-rejects-dead-buffer ()
  "Error: a killed buffer is never ready."
  (let ((buf (generate-new-buffer "pc-dead")))
    (kill-buffer buf)
    (should-not (cj/--ai-term-color-ready-p buf))))

;;; ------------------------ cj/--ai-term-send-color ---------------------------

(ert-deftest test-ai-term-send-color-two-step ()
  "Normal: the command text goes first; the CR follows via a timer (menu race guard)."
  (let ((sent nil) (timer-fns nil))
    (cl-letf (((symbol-function 'cj/--ai-term-send-string)
               (lambda (_buf s) (push s sent)))
              ((symbol-function 'run-at-time)
               (lambda (_time _repeat fn &rest args)
                 (push (cons fn args) timer-fns)
                 'fake-timer)))
      (with-temp-buffer
        (cj/--ai-term-send-color (current-buffer) "purple"))
      (should (equal sent '("/color purple")))
      (should (= (length timer-fns) 1))
      ;; Fire the deferred CR.
      (apply (caar timer-fns) (cdar timer-fns))
      (should (equal sent '("\r" "/color purple"))))))

;;; ---------------------- cj/--ai-term-schedule-color -------------------------

(ert-deftest test-ai-term-schedule-color-sends-when-ready ()
  "Normal: the poll sends once the TUI is ready, then cancels its timer."
  (let ((poll-fn nil) (cancelled nil) (sent nil))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_time _repeat fn &rest args)
                 (setq poll-fn (lambda () (apply fn args)))
                 'fake-timer))
              ((symbol-function 'cancel-timer)
               (lambda (&rest _) (setq cancelled t)))
              ((symbol-function 'cj/--ai-term-send-color)
               (lambda (_buf color) (push color sent))))
      (with-temp-buffer
        (rename-buffer "pc-sched-ready" t)
        (cj/--ai-term-schedule-color (current-buffer) "green")
        (should poll-fn)
        ;; Not ready yet: nothing sent, timer stays.
        (funcall poll-fn)
        (should-not sent)
        (should-not cancelled)
        ;; Becomes ready: sends and cancels.
        (insert "❯ \n⏵⏵ bypass permissions on\n")
        (funcall poll-fn)
        (should (equal sent '("green")))
        (should cancelled)))))

(ert-deftest test-ai-term-schedule-color-gives-up-on-dead-buffer ()
  "Error: a killed buffer cancels the poll without sending."
  (let ((poll-fn nil) (cancelled nil) (sent nil)
        (buf (generate-new-buffer "pc-sched-dead")))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_time _repeat fn &rest args)
                 (setq poll-fn (lambda () (apply fn args)))
                 'fake-timer))
              ((symbol-function 'cancel-timer)
               (lambda (&rest _) (setq cancelled t)))
              ((symbol-function 'cj/--ai-term-send-color)
               (lambda (_buf color) (push color sent))))
      (cj/--ai-term-schedule-color buf "red")
      (kill-buffer buf)
      (funcall poll-fn)
      (should-not sent)
      (should cancelled))))

;;; ------------------------- show-or-create wiring ----------------------------

(defmacro test-ai-term-pc--with-create-mocks (sessions scheduled &rest body)
  "Run BODY with the create path mocked; live tmux SESSIONS list injected.
SCHEDULED captures (BUFFER-NAME . COLOR) pairs from the schedule call."
  (declare (indent 2) (debug t))
  `(cl-letf (((symbol-function 'eat)
              (lambda (&optional _program _arg)
                (get-buffer-create eat-buffer-name)))
             ((symbol-function 'cj/--ai-term-send-string)
              (lambda (_buf _s) nil))
             ((symbol-function 'display-buffer)
              (lambda (&rest _) nil))
             ((symbol-function 'cj/--ai-term-apply-accent)
              (lambda (_buffer) nil))
             ((symbol-function 'cj/--ai-term-live-tmux-sessions)
              (lambda () ,sessions))
             ((symbol-function 'cj/--ai-term-schedule-color)
              (lambda (buffer color)
                (push (cons (buffer-name buffer) color) ,scheduled))))
     ,@body))

(ert-deftest test-ai-term-show-or-create-schedules-color-on-fresh-session ()
  "Normal: a project with no live tmux session gets a scheduled /color."
  (let ((name "agent [pc-fresh-test]")
        (cj/--ai-term-mru nil)
        (scheduled nil))
    (when (get-buffer name) (kill-buffer name))
    (unwind-protect
        (test-ai-term-pc--with-create-mocks nil scheduled
          (cj/--ai-term-show-or-create "/tmp/pc-fresh-test" name)
          (should (= (length scheduled) 1))
          (should (equal (caar scheduled) name))
          (should (equal (cdar scheduled)
                         (cj/--ai-term-project-color "/tmp/pc-fresh-test"))))
      (when (get-buffer name) (kill-buffer name)))))

(ert-deftest test-ai-term-show-or-create-skips-color-on-reattach ()
  "Boundary: a live tmux session (reattach) is never injected with /color."
  (let* ((dir "/tmp/pc-reattach-test")
         (name "agent [pc-reattach-test]")
         (cj/--ai-term-mru nil)
         (scheduled nil)
         (live (list (cj/--ai-term-tmux-session-name dir))))
    (when (get-buffer name) (kill-buffer name))
    (unwind-protect
        (test-ai-term-pc--with-create-mocks live scheduled
          (cj/--ai-term-show-or-create dir name)
          (should-not scheduled))
      (when (get-buffer name) (kill-buffer name)))))

(provide 'test-ai-term--project-color)
;;; test-ai-term--project-color.el ends here
