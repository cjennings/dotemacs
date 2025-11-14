;;; test-mousetrap-mode--build-keymap.el --- Tests for keymap building -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for mouse-trap--build-keymap function.
;; Tests keymap generation for different profiles, event categories,
;; modifiers, and edge cases.

;;; Code:

(require 'ert)
(require 'mousetrap-mode)

;;; Normal Cases

(ert-deftest test-mousetrap-mode--build-keymap-normal-disabled-profile-blocks-all-events ()
  "Test disabled profile blocks all mouse events."
  (let ((major-mode 'test-mode)
        (mouse-trap-mode-profiles '((test-mode . disabled))))
    (let ((map (mouse-trap--build-keymap)))
      (should (keymapp map))
      ;; Check various events are blocked
      (should (eq (lookup-key map (kbd "<mouse-1>")) 'ignore))
      (should (eq (lookup-key map (kbd "<wheel-up>")) 'ignore))
      (should (eq (lookup-key map (kbd "<drag-mouse-1>")) 'ignore))
      (should (eq (lookup-key map (kbd "<double-mouse-1>")) 'ignore)))))

(ert-deftest test-mousetrap-mode--build-keymap-normal-scroll-only-allows-scroll ()
  "Test scroll-only profile allows scroll, blocks clicks."
  (let ((major-mode 'test-mode)
        (mouse-trap-mode-profiles '((test-mode . scroll-only))))
    (let ((map (mouse-trap--build-keymap)))
      (should (keymapp map))
      ;; Wheel events should NOT be in map (allowed)
      (should (eq (lookup-key map (kbd "<wheel-up>")) nil))
      (should (eq (lookup-key map (kbd "<wheel-down>")) nil))
      ;; Click events should be blocked
      (should (eq (lookup-key map (kbd "<mouse-1>")) 'ignore))
      (should (eq (lookup-key map (kbd "<mouse-2>")) 'ignore)))))

(ert-deftest test-mousetrap-mode--build-keymap-normal-primary-click-allows-left-click ()
  "Test primary-click profile allows mouse-1, blocks others."
  (let ((major-mode 'test-mode)
        (mouse-trap-mode-profiles '((test-mode . primary-click))))
    (let ((map (mouse-trap--build-keymap)))
      (should (keymapp map))
      ;; mouse-1 should NOT be in map (allowed)
      (should (eq (lookup-key map (kbd "<mouse-1>")) nil))
      (should (eq (lookup-key map (kbd "<down-mouse-1>")) nil))
      ;; mouse-2/3 should be blocked
      (should (eq (lookup-key map (kbd "<mouse-2>")) 'ignore))
      (should (eq (lookup-key map (kbd "<mouse-3>")) 'ignore))
      ;; Scroll should be blocked
      (should (eq (lookup-key map (kbd "<wheel-up>")) 'ignore)))))

(ert-deftest test-mousetrap-mode--build-keymap-normal-scroll-plus-primary-allows-both ()
  "Test scroll+primary profile allows scrolling and left click."
  (let ((major-mode 'test-mode)
        (mouse-trap-mode-profiles '((test-mode . scroll+primary))))
    (let ((map (mouse-trap--build-keymap)))
      (should (keymapp map))
      ;; Allowed events
      (should (eq (lookup-key map (kbd "<mouse-1>")) nil))
      (should (eq (lookup-key map (kbd "<wheel-up>")) nil))
      ;; Blocked events
      (should (eq (lookup-key map (kbd "<mouse-2>")) 'ignore))
      (should (eq (lookup-key map (kbd "<drag-mouse-1>")) 'ignore)))))

(ert-deftest test-mousetrap-mode--build-keymap-normal-full-profile-allows-all ()
  "Test full profile allows all events."
  (let ((major-mode 'test-mode)
        (mouse-trap-mode-profiles '((test-mode . full))))
    (let ((map (mouse-trap--build-keymap)))
      (should (keymapp map))
      ;; All events should be allowed (not in map)
      (should (eq (lookup-key map (kbd "<mouse-1>")) nil))
      (should (eq (lookup-key map (kbd "<mouse-2>")) nil))
      (should (eq (lookup-key map (kbd "<wheel-up>")) nil))
      (should (eq (lookup-key map (kbd "<drag-mouse-1>")) nil))
      (should (eq (lookup-key map (kbd "<double-mouse-1>")) nil)))))

(ert-deftest test-mousetrap-mode--build-keymap-normal-read-only-profile ()
  "Test read-only profile allows scrolling and all clicks, blocks drags/multi-clicks."
  (let ((major-mode 'test-mode)
        (mouse-trap-mode-profiles '((test-mode . read-only))))
    (let ((map (mouse-trap--build-keymap)))
      (should (keymapp map))
      ;; Allowed: scroll and all clicks
      (should (eq (lookup-key map (kbd "<wheel-up>")) nil))
      (should (eq (lookup-key map (kbd "<mouse-1>")) nil))
      (should (eq (lookup-key map (kbd "<mouse-2>")) nil))
      ;; Blocked: drags and multi-clicks
      (should (eq (lookup-key map (kbd "<drag-mouse-1>")) 'ignore))
      (should (eq (lookup-key map (kbd "<double-mouse-1>")) 'ignore)))))

(ert-deftest test-mousetrap-mode--build-keymap-normal-interactive-profile ()
  "Test interactive profile allows scrolling, clicks, drags; blocks multi-clicks."
  (let ((major-mode 'test-mode)
        (mouse-trap-mode-profiles '((test-mode . interactive))))
    (let ((map (mouse-trap--build-keymap)))
      (should (keymapp map))
      ;; Allowed: scroll, clicks, drags
      (should (eq (lookup-key map (kbd "<wheel-up>")) nil))
      (should (eq (lookup-key map (kbd "<mouse-1>")) nil))
      (should (eq (lookup-key map (kbd "<drag-mouse-1>")) nil))
      ;; Blocked: multi-clicks
      (should (eq (lookup-key map (kbd "<double-mouse-1>")) 'ignore))
      (should (eq (lookup-key map (kbd "<triple-mouse-1>")) 'ignore)))))

;;; Boundary Cases

(ert-deftest test-mousetrap-mode--build-keymap-boundary-single-category-profile ()
  "Test profile with single category works correctly."
  (let ((major-mode 'test-mode)
        (mouse-trap-mode-profiles '((test-mode . primary-click))))
    (let ((map (mouse-trap--build-keymap)))
      (should (keymapp map))
      ;; Only primary-click should be allowed
      (should (eq (lookup-key map (kbd "<mouse-1>")) nil))
      ;; Everything else blocked
      (should (eq (lookup-key map (kbd "<mouse-2>")) 'ignore))
      (should (eq (lookup-key map (kbd "<wheel-up>")) 'ignore))
      (should (eq (lookup-key map (kbd "<drag-mouse-1>")) 'ignore))
      (should (eq (lookup-key map (kbd "<double-mouse-1>")) 'ignore)))))

(ert-deftest test-mousetrap-mode--build-keymap-boundary-all-modifiers-included ()
  "Test all modifier combinations are handled."
  (let ((major-mode 'test-mode)
        (mouse-trap-mode-profiles '((test-mode . scroll-only))))
    (let ((map (mouse-trap--build-keymap)))
      (should (keymapp map))
      ;; Check various modifier combinations are blocked for clicks
      (should (eq (lookup-key map (kbd "<mouse-1>")) 'ignore))
      (should (eq (lookup-key map (kbd "<C-mouse-1>")) 'ignore))
      (should (eq (lookup-key map (kbd "<M-mouse-1>")) 'ignore))
      (should (eq (lookup-key map (kbd "<S-mouse-1>")) 'ignore))
      (should (eq (lookup-key map (kbd "<C-M-mouse-1>")) 'ignore))
      (should (eq (lookup-key map (kbd "<C-S-mouse-1>")) 'ignore))
      (should (eq (lookup-key map (kbd "<M-S-mouse-1>")) 'ignore))
      (should (eq (lookup-key map (kbd "<C-M-S-mouse-1>")) 'ignore)))))

(ert-deftest test-mousetrap-mode--build-keymap-boundary-all-button-numbers ()
  "Test button numbers handled according to category definitions.
Buttons 1-3 are in click categories, buttons 1-5 are in drag/multi-click."
  (let ((major-mode 'test-mode)
        (mouse-trap-mode-profiles '((test-mode . scroll-only))))
    (let ((map (mouse-trap--build-keymap)))
      (should (keymapp map))
      ;; Primary and secondary click buttons (1-3) should be blocked
      (should (eq (lookup-key map (kbd "<mouse-1>")) 'ignore))
      (should (eq (lookup-key map (kbd "<mouse-2>")) 'ignore))
      (should (eq (lookup-key map (kbd "<mouse-3>")) 'ignore))
      ;; Drag events include all buttons (1-5)
      (should (eq (lookup-key map (kbd "<drag-mouse-1>")) 'ignore))
      (should (eq (lookup-key map (kbd "<drag-mouse-4>")) 'ignore))
      (should (eq (lookup-key map (kbd "<drag-mouse-5>")) 'ignore)))))

(ert-deftest test-mousetrap-mode--build-keymap-boundary-all-wheel-directions ()
  "Test all wheel directions are handled."
  (let ((major-mode 'test-mode)
        (mouse-trap-mode-profiles '((test-mode . primary-click))))
    (let ((map (mouse-trap--build-keymap)))
      (should (keymapp map))
      ;; All wheel directions should be blocked (not in primary-click)
      (should (eq (lookup-key map (kbd "<wheel-up>")) 'ignore))
      (should (eq (lookup-key map (kbd "<wheel-down>")) 'ignore))
      (should (eq (lookup-key map (kbd "<wheel-left>")) 'ignore))
      (should (eq (lookup-key map (kbd "<wheel-right>")) 'ignore)))))

(ert-deftest test-mousetrap-mode--build-keymap-boundary-returns-valid-keymap ()
  "Test function always returns a valid keymap."
  (let ((major-mode 'test-mode)
        (mouse-trap-mode-profiles '((test-mode . disabled))))
    (let ((map (mouse-trap--build-keymap)))
      (should (keymapp map)))))

;;; Error Cases

(ert-deftest test-mousetrap-mode--build-keymap-error-nil-profile-blocks-all ()
  "Test nil profile (unmapped) blocks all events."
  (let ((major-mode 'unmapped-mode)
        (mouse-trap-mode-profiles '((test-mode . disabled)))
        (mouse-trap-profiles '((disabled . ()))))
    ;; This mode will get nil from alist-get, treated as empty list
    (let ((map (mouse-trap--build-keymap)))
      (should (keymapp map))
      ;; With default scroll-only, clicks should be blocked
      (should (eq (lookup-key map (kbd "<mouse-1>")) 'ignore)))))

(ert-deftest test-mousetrap-mode--build-keymap-error-invalid-category-ignored ()
  "Test invalid category in profile is gracefully ignored."
  (let ((major-mode 'test-mode)
        (mouse-trap-profiles '((test-profile . (scroll invalid-category primary-click))))
        (mouse-trap-mode-profiles '((test-mode . test-profile))))
    (let ((map (mouse-trap--build-keymap)))
      (should (keymapp map))
      ;; Valid categories should work
      (should (eq (lookup-key map (kbd "<wheel-up>")) nil))  ; scroll allowed
      (should (eq (lookup-key map (kbd "<mouse-1>")) nil))   ; primary-click allowed
      ;; Other events should be blocked
      (should (eq (lookup-key map (kbd "<mouse-2>")) 'ignore)))))

(ert-deftest test-mousetrap-mode--build-keymap-error-empty-category-spec-ignored ()
  "Test empty category spec is handled gracefully."
  (let ((major-mode 'test-mode)
        (mouse-trap-mode-profiles '((test-mode . disabled)))
        (mouse-trap--event-categories
         '((primary-click . ((types . ("mouse" "down-mouse"))
                             (buttons . (1))))
           (empty-category . ())  ; Empty spec
           (scroll . ((wheel . ("wheel-up" "wheel-down")))))))
    (let ((map (mouse-trap--build-keymap)))
      (should (keymapp map))
      ;; Should still work despite empty category
      (should (eq (lookup-key map (kbd "<mouse-1>")) 'ignore)))))

;;; Edge Cases

(ert-deftest test-mousetrap-mode--build-keymap-edge-event-bound-to-ignore-function ()
  "Test blocked events are bound to ignore function, not nil."
  (let ((major-mode 'test-mode)
        (mouse-trap-mode-profiles '((test-mode . disabled))))
    (let ((map (mouse-trap--build-keymap)))
      (should (keymapp map))
      ;; Check binding is exactly 'ignore
      (should (eq (lookup-key map (kbd "<mouse-1>")) 'ignore))
      (should-not (eq (lookup-key map (kbd "<mouse-1>")) nil)))))

(ert-deftest test-mousetrap-mode--build-keymap-edge-allowed-events-not-in-keymap ()
  "Test allowed events are not present in keymap (nil lookup)."
  (let ((major-mode 'test-mode)
        (mouse-trap-mode-profiles '((test-mode . scroll-only))))
    (let ((map (mouse-trap--build-keymap)))
      (should (keymapp map))
      ;; Allowed events should return nil from lookup
      (should (eq (lookup-key map (kbd "<wheel-up>")) nil))
      (should (eq (lookup-key map (kbd "<wheel-down>")) nil)))))

(ert-deftest test-mousetrap-mode--build-keymap-edge-drag-vs-click-separation ()
  "Test drag events are independent from click events."
  (let ((major-mode 'test-mode)
        (mouse-trap-mode-profiles '((test-mode . primary-click))))
    (let ((map (mouse-trap--build-keymap)))
      (should (keymapp map))
      ;; mouse-1 allowed
      (should (eq (lookup-key map (kbd "<mouse-1>")) nil))
      ;; drag-mouse-1 blocked
      (should (eq (lookup-key map (kbd "<drag-mouse-1>")) 'ignore)))))

(ert-deftest test-mousetrap-mode--build-keymap-edge-double-vs-single-click-separation ()
  "Test multi-clicks are independent from single clicks."
  (let ((major-mode 'test-mode)
        (mouse-trap-mode-profiles '((test-mode . primary-click))))
    (let ((map (mouse-trap--build-keymap)))
      (should (keymapp map))
      ;; mouse-1 allowed
      (should (eq (lookup-key map (kbd "<mouse-1>")) nil))
      ;; double-mouse-1 blocked
      (should (eq (lookup-key map (kbd "<double-mouse-1>")) 'ignore))
      (should (eq (lookup-key map (kbd "<triple-mouse-1>")) 'ignore)))))

(provide 'test-mousetrap-mode--build-keymap)
;;; test-mousetrap-mode--build-keymap.el ends here
