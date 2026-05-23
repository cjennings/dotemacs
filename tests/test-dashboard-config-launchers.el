;;; test-dashboard-config-launchers.el --- Tests for the dashboard launcher table -*- lexical-binding: t; -*-

;;; Commentary:
;; Locks the single-source launcher table that drives both the dashboard
;; navigator icon rows and the dashboard-mode-map keybindings.  The bug this
;; guards against: the icon row and the keymap were maintained as two separate
;; inline lists that could drift in order or wiring.  These tests assert the
;; table, the derived 3x4 navigator rows, and that every key binds to the
;; right command (including Music's two-call action).

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(setq load-prefer-newer t)

;; deps dashboard-config pulls transitively / actions reference
(unless (fboundp 'cj/kill-all-other-buffers-and-windows)
  (defun cj/kill-all-other-buffers-and-windows () nil))
(unless (fboundp 'cj/make-buffer-undead) (defun cj/make-buffer-undead (_n) nil))
(defvar user-home-dir "/tmp/")

(require 'dashboard-config)

(defconst test-dash--keys '("c" "d" "t" "a" "r" "b" "f" "m" "e" "i" "s" "g"))

;; ----------------------------- launcher table --------------------------------

(ert-deftest test-dashboard-launchers-keys-in-order ()
  "Normal: 12 launchers with the expected keys in display order."
  (should (= 12 (length cj/dashboard--launchers)))
  (should (equal test-dash--keys (mapcar (lambda (l) (nth 0 l)) cj/dashboard--launchers))))

(ert-deftest test-dashboard-launchers-labels-in-order ()
  "Normal: labels in display order."
  (should (equal '("Code" "Files" "Terminal" "Agenda" "Feeds" "Books"
                   "Flashcards" "Music" "Email" "IRC" "Slack" "Telegram")
                 (mapcar (lambda (l) (nth 3 l)) cj/dashboard--launchers))))

;; --------------------------- navigator rows ----------------------------------

(ert-deftest test-dashboard-navigator-rows-three-rows-of-four ()
  "Normal: navigator derives 3 rows of 4, with the right labels and button shape."
  (cl-letf (((symbol-function 'nerd-icons-faicon)  (lambda (n &rest _) (concat "I:" n)))
            ((symbol-function 'nerd-icons-devicon) (lambda (n &rest _) (concat "I:" n)))
            ((symbol-function 'nerd-icons-mdicon)  (lambda (n &rest _) (concat "I:" n))))
    (let ((rows (cj/dashboard--navigator-rows)))
      (should (= 3 (length rows)))
      (should (cl-every (lambda (r) (= 4 (length r))) rows))
      (should (equal '("Code" "Files" "Terminal" "Agenda")
                     (mapcar (lambda (b) (nth 1 b)) (nth 0 rows))))
      (let ((btn (car (car rows))))            ; (icon label tooltip action nil " " "")
        (should (string= "I:nf-fa-code" (nth 0 btn)))
        (should (string= "Code" (nth 1 btn)))
        (should (functionp (nth 3 btn)))
        (should (null (nth 4 btn)))))))

;; ---------------------------- keymap binding ---------------------------------

(ert-deftest test-dashboard-bind-launchers-binds-every-key ()
  "Normal: every launcher key binds to a command; q is left to the caller."
  (let ((map (make-sparse-keymap)))
    (cj/dashboard--bind-launchers map)
    (dolist (key test-dash--keys)
      (should (commandp (keymap-lookup map key))))
    (should-not (keymap-lookup map "q"))))

(ert-deftest test-dashboard-bind-launchers-each-key-runs-its-command ()
  "Behavior: invoking each key runs its launcher's command(s) — Music runs two."
  (let ((map (make-sparse-keymap)) (calls nil))
    (cl-letf (((symbol-function 'projectile-switch-project) (lambda (&rest _) (push 'code calls)))
              ((symbol-function 'dirvish) (lambda (&rest _) (push 'files calls)))
              ((symbol-function 'vterm) (lambda (&rest _) (push 'term calls)))
              ((symbol-function 'cj/main-agenda-display) (lambda (&rest _) (push 'agenda calls)))
              ((symbol-function 'cj/elfeed-open) (lambda (&rest _) (push 'feeds calls)))
              ((symbol-function 'calibredb) (lambda (&rest _) (push 'books calls)))
              ((symbol-function 'cj/drill-start) (lambda (&rest _) (push 'cards calls)))
              ((symbol-function 'cj/music-playlist-toggle) (lambda (&rest _) (push 'm-toggle calls)))
              ((symbol-function 'cj/music-playlist-load) (lambda (&rest _) (push 'm-load calls)))
              ((symbol-function 'mu4e) (lambda (&rest _) (push 'email calls)))
              ((symbol-function 'cj/erc-switch-to-buffer-with-completion) (lambda (&rest _) (push 'irc calls)))
              ((symbol-function 'cj/slack-start) (lambda (&rest _) (push 'slack calls)))
              ((symbol-function 'cj/telega) (lambda (&rest _) (push 'tg calls))))
      (cj/dashboard--bind-launchers map)
      (dolist (key test-dash--keys)
        (call-interactively (keymap-lookup map key)))
      (should (memq 'code calls))
      (should (memq 'tg calls))
      (should (memq 'm-toggle calls))
      (should (memq 'm-load calls))
      (should (= 13 (length calls))))))   ; 12 keys, Music fires two

(provide 'test-dashboard-config-launchers)
;;; test-dashboard-config-launchers.el ends here
