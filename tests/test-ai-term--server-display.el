;;; test-ai-term--server-display.el --- Tests for emacsclient window routing -*- lexical-binding: t; -*-

;;; Commentary:
;; `cj/--ai-term-server-display' is wired as `server-window' so a file
;; opened via `emacsclient -n' (e.g. when Craig tells the agent to open
;; something) doesn't land on top of the agent terminal.  When the selected
;; window shows an `agent [...]' buffer, the file goes to a non-agent
;; window instead -- splitting one off the agent if it is the only window.
;; `cj/--ai-term-non-agent-window' picks that window.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(require 'ai-term)
(require 'server)
(require 'testutil-ghostel-buffers)

(ert-deftest test-ai-term--non-agent-window-finds-code-window ()
  "Normal: agent on the right, code on the left -> returns the code window."
  (cj/test--kill-agent-buffers)
  (let ((agent (get-buffer-create "agent [proj]"))
        (code  (get-buffer-create "code.el")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window) code)
          (let ((right (split-window-right)))
            (set-window-buffer right agent)
            (let ((found (cj/--ai-term-non-agent-window right)))
              (should (windowp found))
              (should (eq (window-buffer found) code)))))
      (kill-buffer agent)
      (kill-buffer code))))

(ert-deftest test-ai-term--non-agent-window-none-when-only-agent ()
  "Boundary: the agent window is the only one -> nil."
  (cj/test--kill-agent-buffers)
  (let ((agent (get-buffer-create "agent [solo]")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window) agent)
          (should-not (cj/--ai-term-non-agent-window (selected-window))))
      (kill-buffer agent))))

(ert-deftest test-ai-term--non-agent-window-skips-dedicated ()
  "Boundary: a dedicated non-agent window is not a valid target."
  (cj/test--kill-agent-buffers)
  (let ((agent (get-buffer-create "agent [proj]"))
        (side  (get-buffer-create "*dedicated-side*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window) agent)
          (let ((w (split-window-right)))
            (set-window-buffer w side)
            (set-window-dedicated-p w t)
            (unwind-protect
                (should-not (cj/--ai-term-non-agent-window (selected-window)))
              (set-window-dedicated-p w nil))))
      (kill-buffer agent)
      (kill-buffer side))))

(ert-deftest test-ai-term--server-display-routes-around-agent ()
  "Normal: selected window is the agent -> the file lands in the other
window and the agent window keeps the agent buffer."
  (cj/test--kill-agent-buffers)
  (let ((agent (get-buffer-create "agent [proj]"))
        (code  (get-buffer-create "code.el"))
        (file  (get-buffer-create "opened-by-emacsclient.el")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window) code)
          (let ((agent-win (split-window-right)))
            (set-window-buffer agent-win agent)
            (select-window agent-win)
            (cj/--ai-term-server-display file)
            (should (eq (window-buffer agent-win) agent))
            (should (get-buffer-window file))
            (should-not (eq (get-buffer-window file) agent-win))))
      (kill-buffer agent)
      (kill-buffer code)
      (kill-buffer file))))

(ert-deftest test-ai-term--server-display-splits-when-agent-is-only-window ()
  "Boundary: the agent is the only window -> a window is split off for the
file; the agent window keeps the agent buffer."
  (cj/test--kill-agent-buffers)
  (let ((agent (get-buffer-create "agent [solo]"))
        (file  (get-buffer-create "opened.el")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window) agent)
          (let ((agent-win (selected-window)))
            (cj/--ai-term-server-display file)
            (should (= 2 (length (window-list (selected-frame) 'never))))
            (should (eq (window-buffer agent-win) agent))
            (should (eq (window-buffer (get-buffer-window file)) file))))
      (kill-buffer agent)
      (kill-buffer file))))

(ert-deftest test-ai-term--server-display-passthrough-when-not-agent ()
  "Normal: selected window is a regular buffer -> the file is displayed
normally and nothing special happens (no agent window to protect)."
  (cj/test--kill-agent-buffers)
  (let ((code (get-buffer-create "code.el"))
        (file (get-buffer-create "opened.el")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window) code)
          (cj/--ai-term-server-display file)
          (should (get-buffer-window file)))
      (kill-buffer code)
      (kill-buffer file))))

(ert-deftest test-ai-term--server-window-wired-to-helper ()
  "Normal: the module sets `server-window' to its display function."
  (should (eq server-window #'cj/--ai-term-server-display)))

(provide 'test-ai-term--server-display)
;;; test-ai-term--server-display.el ends here
