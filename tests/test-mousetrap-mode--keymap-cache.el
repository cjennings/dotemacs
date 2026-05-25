;;; test-mousetrap-mode--keymap-cache.el --- Tests for keymap caching -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for the per-profile keymap cache in mouse-trap--build-keymap.
;; Repeated builds for the same profile should reuse one keymap object;
;; distinct profiles get distinct objects; clearing the cache forces a rebuild.

;;; Code:

(require 'ert)
(require 'mousetrap-mode)

;; Each test starts from a clean cache so results don't depend on order.
(defun test-mousetrap--with-clear-cache (thunk)
  "Run THUNK with the keymap cache cleared before and after."
  (mouse-trap--clear-keymap-cache)
  (unwind-protect (funcall thunk)
    (mouse-trap--clear-keymap-cache)))

;;; Cache-hit cases

(ert-deftest test-mousetrap-mode--keymap-cache-same-profile-returns-same-object ()
  "Normal: two builds for the same profile return the same keymap (eq)."
  (test-mousetrap--with-clear-cache
   (lambda ()
     (let ((major-mode 'test-mode)
           (mouse-trap-mode-profiles '((test-mode . disabled))))
       (let ((first (mouse-trap--build-keymap))
             (second (mouse-trap--build-keymap)))
         (should (eq first second)))))))

(ert-deftest test-mousetrap-mode--keymap-cache-different-profiles-differ ()
  "Normal: distinct profiles return distinct keymap objects."
  (test-mousetrap--with-clear-cache
   (lambda ()
     (let* ((mouse-trap-mode-profiles '((mode-a . disabled)
                                        (mode-b . full)))
            (map-a (let ((major-mode 'mode-a)) (mouse-trap--build-keymap)))
            (map-b (let ((major-mode 'mode-b)) (mouse-trap--build-keymap))))
       (should-not (eq map-a map-b))))))

;;; Cache-clear case

(ert-deftest test-mousetrap-mode--keymap-cache-clear-forces-fresh-object ()
  "Boundary: after clearing the cache, a rebuild yields a fresh object."
  (test-mousetrap--with-clear-cache
   (lambda ()
     (let ((major-mode 'test-mode)
           (mouse-trap-mode-profiles '((test-mode . disabled))))
       (let ((first (mouse-trap--build-keymap)))
         (mouse-trap--clear-keymap-cache)
         (let ((second (mouse-trap--build-keymap)))
           (should-not (eq first second))))))))

(ert-deftest test-mousetrap-mode--keymap-cache-edited-categories-rebuild ()
  "Boundary: editing a profile's category list at runtime changes the key,
forcing a rebuild rather than returning the stale cached keymap."
  (test-mousetrap--with-clear-cache
   (lambda ()
     (let ((major-mode 'test-mode)
           (mouse-trap-mode-profiles '((test-mode . tweakable))))
       (let* ((mouse-trap-profiles '((tweakable . (scroll))))
              (first (mouse-trap--build-keymap)))
         (let* ((mouse-trap-profiles '((tweakable . (scroll primary-click))))
                (second (mouse-trap--build-keymap)))
           (should-not (eq first second))
           ;; Behavior reflects the edited categories.
           (should (eq (lookup-key second (kbd "<mouse-1>")) nil))))))))

;;; Behavior-preservation sanity

(ert-deftest test-mousetrap-mode--keymap-cache-preserves-ignore-binding ()
  "Normal: a cached keymap still binds a disallowed event to `ignore'."
  (test-mousetrap--with-clear-cache
   (lambda ()
     (let ((major-mode 'test-mode)
           (mouse-trap-mode-profiles '((test-mode . disabled))))
       ;; Build twice; the cached object must still block the event.
       (mouse-trap--build-keymap)
       (let ((map (mouse-trap--build-keymap)))
         (should (eq (lookup-key map (kbd "<mouse-1>")) 'ignore)))))))

(provide 'test-mousetrap-mode--keymap-cache)
;;; test-mousetrap-mode--keymap-cache.el ends here
