;;; test-ai-vterm--display-saved.el --- Tests for the display-saved action -*- lexical-binding: t; -*-

;;; Commentary:
;; The action reads `cj/--ai-vterm-last-direction' +
;; `cj/--ai-vterm-last-size' (with default fallbacks), builds an
;; alist with direction + the matching size key, strips any
;; conflicting entries that came in via the rule, and delegates to
;; `display-buffer-in-direction'.
;;
;; Tests stub `display-buffer-in-direction' to capture the alist
;; that would have reached it.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(require 'ai-vterm)
(require 'testutil-vterm-buffers)

(ert-deftest test-ai-vterm--display-saved-uses-desktop-defaults-when-state-nil ()
  "Normal: nil state on a desktop -> rightmost, size=cj/ai-vterm-desktop-width.
The cardinal `right' default maps to the frame-edge variant
`rightmost' so agent lands at the frame's right edge regardless of
which window is selected.  `env-laptop-p' is stubbed nil to pin the
desktop branch."
  (let (received-buf received-alist
        (cj/--ai-vterm-last-direction nil)
        (cj/--ai-vterm-last-size nil)
        (cj/ai-vterm-desktop-width 0.5))
    (cl-letf (((symbol-function 'env-laptop-p) (lambda () nil))
              ((symbol-function 'display-buffer-in-direction)
               (lambda (b a)
                 (setq received-buf b received-alist a)
                 'fake-window)))
      (cj/--ai-vterm-display-saved 'fake-buf '((inhibit-same-window . t))))
    (should (eq received-buf 'fake-buf))
    (should (eq (cdr (assq 'direction received-alist)) 'rightmost))
    (should (= (cdr (assq 'window-width received-alist)) 0.5))
    (should (eq (cdr (assq 'inhibit-same-window received-alist)) t))))

(ert-deftest test-ai-vterm--display-saved-uses-laptop-defaults-when-state-nil ()
  "Normal: nil state on a laptop -> bottom, size=cj/ai-vterm-laptop-height.
The cardinal `below' default maps to the frame-edge variant `bottom'
and the size lands on the `window-height' axis.  `env-laptop-p' is
stubbed t to pin the laptop branch."
  (let (received-alist
        (cj/--ai-vterm-last-direction nil)
        (cj/--ai-vterm-last-size nil)
        (cj/ai-vterm-laptop-height 0.75))
    (cl-letf (((symbol-function 'env-laptop-p) (lambda () t))
              ((symbol-function 'display-buffer-in-direction)
               (lambda (_b a) (setq received-alist a) 'fake-window)))
      (cj/--ai-vterm-display-saved 'fake-buf '((inhibit-same-window . t))))
    (should (eq (cdr (assq 'direction received-alist)) 'bottom))
    (should (= (cdr (assq 'window-height received-alist)) 0.75))
    (should-not (assq 'window-width received-alist))))

(ert-deftest test-ai-vterm--display-saved-uses-saved-direction-and-size-below ()
  "Normal: saved direction=below maps to bottom edge; size=0.4 passes through."
  (let (received-alist
        (cj/--ai-vterm-last-direction 'below)
        (cj/--ai-vterm-last-size 0.4))
    (cl-letf (((symbol-function 'display-buffer-in-direction)
               (lambda (_b a) (setq received-alist a) 'fake-window)))
      (cj/--ai-vterm-display-saved 'fake-buf nil))
    (should (eq (cdr (assq 'direction received-alist)) 'bottom))
    (should (= (cdr (assq 'window-height received-alist)) 0.4))
    (should-not (assq 'window-width received-alist))))

(ert-deftest test-ai-vterm--display-saved-uses-saved-direction-and-size-right ()
  "Normal: saved direction=right maps to rightmost edge; size=0.7 passes through."
  (let (received-alist
        (cj/--ai-vterm-last-direction 'right)
        (cj/--ai-vterm-last-size 0.7))
    (cl-letf (((symbol-function 'display-buffer-in-direction)
               (lambda (_b a) (setq received-alist a) 'fake-window)))
      (cj/--ai-vterm-display-saved 'fake-buf nil))
    (should (eq (cdr (assq 'direction received-alist)) 'rightmost))
    (should (= (cdr (assq 'window-width received-alist)) 0.7))
    (should-not (assq 'window-height received-alist))))

(ert-deftest test-ai-vterm--display-saved-strips-conflicting-alist-entries ()
  "Boundary: caller-supplied direction/size are stripped, saved values win."
  (let (received-alist
        (cj/--ai-vterm-last-direction 'right)
        (cj/--ai-vterm-last-size 0.7))
    (cl-letf (((symbol-function 'display-buffer-in-direction)
               (lambda (_b a) (setq received-alist a) 'fake-window)))
      (cj/--ai-vterm-display-saved
       'fake-buf
       '((direction . below)
         (window-width . 0.2)
         (window-height . 0.3)
         (inhibit-same-window . t))))
    (should (eq (cdr (assq 'direction received-alist)) 'rightmost))
    (should (= (cdr (assq 'window-width received-alist)) 0.7))
    (should (eq (cdr (assq 'inhibit-same-window received-alist)) t))
    ;; window-height should not be in the alist when direction is right
    ;; -- the action picks the matching size key based on direction.
    (let ((wh-cells (cl-remove-if-not
                     (lambda (cell) (eq (car-safe cell) 'window-height))
                     received-alist)))
      (should (null wh-cells)))))

(ert-deftest test-ai-vterm--display-saved-passes-buffer-through ()
  "Normal: BUFFER argument reaches display-buffer-in-direction unchanged."
  (let (received-buf
        (cj/--ai-vterm-last-direction 'right)
        (cj/--ai-vterm-last-size 0.5))
    (cl-letf (((symbol-function 'display-buffer-in-direction)
               (lambda (b _a) (setq received-buf b) 'fake-window)))
      (cj/--ai-vterm-display-saved 'sentinel-buffer nil))
    (should (eq received-buf 'sentinel-buffer))))

(ert-deftest test-ai-vterm--display-saved-3window-roundtrip-preserves-body-width ()
  "Regression: capture+delete+display in a 3-window layout preserves body-width.

Reproduces Craig's `peeking ~1 col' report from 2026-05-09: when
the new agent lands at a different position than the captured one
(rightmost vs middle), `window-total-width' differs by 1 because
of the right divider.  `window-body-width' is divider-independent
and is what the user actually sees, so the assertion locks down
the body match."
  (cj/test--kill-agent-buffers)
  (let ((agent-name "agent [3win-roundtrip]")
        (left-name "*test-3win-left*")
        (right-name "*test-3win-right*"))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((left-buf (get-buffer-create left-name))
                (right-buf (get-buffer-create right-name))
                (agent-buf (get-buffer-create agent-name)))
            ;; Build: left | agent | right.  Selected window starts as
            ;; the only window.  Split right twice to get three windows.
            (set-window-buffer (selected-window) left-buf)
            (let* ((right-win (split-window (selected-window) nil 'right))
                   (_ (set-window-buffer right-win right-buf))
                   (agent-win (split-window (selected-window) nil 'right)))
              (set-window-buffer agent-win agent-buf)
              ;; Capture agent's state.
              (cj/--ai-vterm-capture-state agent-win)
              (let ((captured-size cj/--ai-vterm-last-size)
                    (captured-direction cj/--ai-vterm-last-direction))
                ;; Simulate quit-window on agent.
                (delete-window agent-win)
                ;; Now route a fresh display through the actual rule.
                (let* ((display-buffer-alist (cj/--ai-vterm-display-rule-list))
                       (new-win (display-buffer agent-buf)))
                  (should (windowp new-win))
                  (should (eq (window-buffer new-win) agent-buf))
                  ;; The captured size should be replayed exactly.
                  (should (= (window-body-width new-win)
                             captured-size))
                  ;; Direction should also match.
                  (should (eq captured-direction 'right)))))))
      (when (get-buffer left-name) (kill-buffer left-name))
      (when (get-buffer right-name) (kill-buffer right-name))
      (cj/test--kill-agent-buffers))))

(ert-deftest test-ai-vterm--display-saved-3window-agent-rightmost-roundtrip ()
  "Round-trip when agent is the rightmost window (no right divider)."
  (cj/test--kill-agent-buffers)
  (let ((agent-name "agent [rightmost]")
        (left-name "*test-rm-left*")
        (mid-name "*test-rm-mid*"))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((left-buf (get-buffer-create left-name))
                (mid-buf (get-buffer-create mid-name))
                (agent-buf (get-buffer-create agent-name)))
            ;; Build: left | mid | agent (agent rightmost)
            (set-window-buffer (selected-window) left-buf)
            (let* ((mid-win (split-window (selected-window) nil 'right))
                   (agent-win (split-window mid-win nil 'right)))
              (set-window-buffer mid-win mid-buf)
              (set-window-buffer agent-win agent-buf)
              (cj/--ai-vterm-capture-state agent-win)
              (let ((captured-size cj/--ai-vterm-last-size))
                (delete-window agent-win)
                (let* ((display-buffer-alist (cj/--ai-vterm-display-rule-list))
                       (new-win (display-buffer agent-buf)))
                  (should (windowp new-win))
                  (should (= (window-body-width new-win) captured-size)))))))
      (when (get-buffer left-name) (kill-buffer left-name))
      (when (get-buffer mid-name) (kill-buffer mid-name))
      (cj/test--kill-agent-buffers))))

(ert-deftest test-ai-vterm--display-saved-3window-after-mouse-resize ()
  "Round-trip after a deliberate mid-window resize (mimics mouse-drag)."
  (cj/test--kill-agent-buffers)
  (let ((agent-name "agent [mouse-resize]")
        (left-name "*test-mr-left*")
        (right-name "*test-mr-right*"))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((left-buf (get-buffer-create left-name))
                (right-buf (get-buffer-create right-name))
                (agent-buf (get-buffer-create agent-name)))
            (set-window-buffer (selected-window) left-buf)
            (let* ((right-win (split-window (selected-window) nil 'right))
                   (agent-win (split-window (selected-window) nil 'right)))
              (set-window-buffer right-win right-buf)
              (set-window-buffer agent-win agent-buf)
              ;; Resize agent smaller to mimic the user dragging the
              ;; divider.  Shrink agent by 5 cols, give to left.
              (let ((delta -5))
                (when (window--resizable-p agent-win delta t)
                  (window-resize agent-win delta t)))
              (cj/--ai-vterm-capture-state agent-win)
              (let ((captured-size cj/--ai-vterm-last-size))
                (delete-window agent-win)
                (let* ((display-buffer-alist (cj/--ai-vterm-display-rule-list))
                       (new-win (display-buffer agent-buf)))
                  (should (windowp new-win))
                  (should (= (window-body-width new-win) captured-size)))))))
      (when (get-buffer left-name) (kill-buffer left-name))
      (when (get-buffer right-name) (kill-buffer right-name))
      (cj/test--kill-agent-buffers))))

(ert-deftest test-ai-vterm--display-saved-roundtrip-via-cj/ai-vterm-toggle ()
  "End-to-end: toggle-off via dispatch then redisplay -- preserves size."
  (cj/test--kill-agent-buffers)
  (let ((agent-name "agent [toggle-roundtrip]")
        (left-name "*test-tr-left*")
        (right-name "*test-tr-right*"))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((left-buf (get-buffer-create left-name))
                (right-buf (get-buffer-create right-name))
                (agent-buf (get-buffer-create agent-name)))
            (set-window-buffer (selected-window) left-buf)
            (let* ((right-win (split-window (selected-window) nil 'right))
                   (agent-win (split-window (selected-window) nil 'right)))
              (set-window-buffer right-win right-buf)
              (set-window-buffer agent-win agent-buf)
              (let ((display-buffer-alist (cj/--ai-vterm-display-rule-list)))
                ;; Focus agent (mimics `M-x cj/ai-vterm' from inside agent).
                (select-window agent-win)
                (let ((before-size (window-body-width agent-win)))
                  ;; Toggle off via the actual command -- captures + quit-window.
                  (cj/ai-vterm)
                  (should-not (cj/--ai-vterm-displayed-agent-window))
                  ;; Toggle on -- single-buffer DWIM redisplay path.
                  (cj/ai-vterm)
                  (let* ((new-win (cj/--ai-vterm-displayed-agent-window))
                         (new-size (window-body-width new-win)))
                    (should (windowp new-win))
                    (should (= new-size before-size))))))))
      (when (get-buffer left-name) (kill-buffer left-name))
      (when (get-buffer right-name) (kill-buffer right-name))
      (cj/test--kill-agent-buffers))))

(ert-deftest test-ai-vterm--display-saved-two-toggle-cycles-stable ()
  "Two consecutive toggle-off+toggle-on cycles -- no compounding error."
  (cj/test--kill-agent-buffers)
  (let ((agent-name "agent [two-cycle]")
        (left-name "*test-2c-left*")
        (right-name "*test-2c-right*"))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((left-buf (get-buffer-create left-name))
                (right-buf (get-buffer-create right-name))
                (agent-buf (get-buffer-create agent-name)))
            (set-window-buffer (selected-window) left-buf)
            (let* ((right-win (split-window (selected-window) nil 'right))
                   (agent-win (split-window (selected-window) nil 'right)))
              (set-window-buffer right-win right-buf)
              (set-window-buffer agent-win agent-buf)
              (let ((display-buffer-alist (cj/--ai-vterm-display-rule-list))
                    (initial-size (window-body-width agent-win)))
                (select-window agent-win)
                ;; Cycle 1
                (cj/ai-vterm) ; off
                (cj/ai-vterm) ; on
                (let ((cycle1-size (window-body-width
                                    (cj/--ai-vterm-displayed-agent-window))))
                  (should (= cycle1-size initial-size))
                  (select-window (cj/--ai-vterm-displayed-agent-window))
                  ;; Cycle 2
                  (cj/ai-vterm) ; off
                  (cj/ai-vterm) ; on
                  (let ((cycle2-size (window-body-width
                                      (cj/--ai-vterm-displayed-agent-window))))
                    (should (= cycle2-size initial-size))))))))
      (when (get-buffer left-name) (kill-buffer left-name))
      (when (get-buffer right-name) (kill-buffer right-name))
      (cj/test--kill-agent-buffers))))

(ert-deftest test-ai-vterm--display-saved-craig-c-x-3-roundtrip ()
  "Reproduces Craig's repro from 2026-05-09:
launch -> F9 -> dashboard splits via C-x 3 -> toggle off -> toggle on.
Expected: new agent lands at the same total-width it had before."
  (cj/test--kill-agent-buffers)
  (let ((agent-name "agent [c-x-3-repro]")
        (dash-name "*test-cx3-dashboard*"))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((dash-buf (get-buffer-create dash-name))
                (agent-buf (get-buffer-create agent-name)))
            (set-window-buffer (selected-window) dash-buf)
            (let ((display-buffer-alist (cj/--ai-vterm-display-rule-list)))
              ;; Step 1: F9 displays agent.  Layout: dashboard | agent.
              (let ((agent-win-1 (display-buffer agent-buf)))
                (should (windowp agent-win-1)))
              ;; Step 2: focus dashboard, C-x 3 (split-window-right).
              (let ((dash-win (get-buffer-window dash-buf)))
                (select-window dash-win)
                (split-window-right))
              ;; Layout now: dashboard1 | dashboard2 | agent
              ;; Capture agent's pre-toggle body width for later assertion.
              (let* ((agent-win-2 (cj/--ai-vterm-displayed-agent-window))
                     (size-before (window-body-width agent-win-2)))
                ;; Step 3: F9 toggles agent off (selected is dashboard).
                (cj/ai-vterm)
                (should-not (cj/--ai-vterm-displayed-agent-window))
                ;; Step 4: F9 toggles agent on -- redisplay-single path.
                (cj/ai-vterm)
                (let* ((agent-win-3 (cj/--ai-vterm-displayed-agent-window))
                       (size-after (window-body-width agent-win-3)))
                  (should (windowp agent-win-3))
                  (should (= size-after size-before)))))))
      (when (get-buffer dash-name) (kill-buffer dash-name))
      (cj/test--kill-agent-buffers))))

(ert-deftest test-ai-vterm--toggle-after-buffer-move-no-extra-window ()
  "Regression: toggle-off must remove agent's window even when buffer-move
has cleared its `quit-restore' parameter.

Reproduces Craig's repro from 2026-05-09: 3 windows, user uses
buffer-move (C-M-arrows) to relocate agent.  buffer-move swaps
buffers between windows and leaves the receiving window with no
record that it was created for the agent buffer.  `quit-window'
respects that history and only buries -- the window stays with
some other buffer in it.  The next toggle-on then doesn't recognize
that window as an agent home and creates a fresh one alongside,
landing the user at N+1 windows instead of N.

Assertion: after toggle-off+toggle-on, the window count is back to
its pre-cycle value, regardless of `quit-restore' state."
  (cj/test--kill-agent-buffers)
  (let ((agent-name "agent [buffer-move-toggle]")
        (left-name "*test-bm-left*")
        (right-name "*test-bm-right*"))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((left-buf (get-buffer-create left-name))
                (right-buf (get-buffer-create right-name))
                (agent-buf (get-buffer-create agent-name)))
            (set-window-buffer (selected-window) left-buf)
            (let* ((right-win (split-window (selected-window) nil 'right))
                   (agent-win (split-window (selected-window) nil 'right)))
              (set-window-buffer right-win right-buf)
              (set-window-buffer agent-win agent-buf)
              ;; Mimic buffer-move's effect: agent lives in this
              ;; window but quit-restore says nothing about it.
              (set-window-parameter agent-win 'quit-restore nil)
              (let ((display-buffer-alist (cj/--ai-vterm-display-rule-list))
                    (window-count-before (count-windows)))
                (select-window agent-win)
                (cj/ai-vterm) ; off
                (cj/ai-vterm) ; on
                (should (= (count-windows) window-count-before))
                ;; Agent must be displayed exactly once.
                (let ((agent-windows
                       (seq-filter
                        (lambda (w)
                          (eq (window-buffer w) agent-buf))
                        (window-list))))
                  (should (= (length agent-windows) 1)))))))
      (when (get-buffer left-name) (kill-buffer left-name))
      (when (get-buffer right-name) (kill-buffer right-name))
      (cj/test--kill-agent-buffers))))

(provide 'test-ai-vterm--display-saved)
;;; test-ai-vterm--display-saved.el ends here
