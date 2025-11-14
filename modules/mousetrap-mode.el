;;; mousetrap-mode.el ---  -*- coding: utf-8; lexical-binding: t; -*-
;;
;;; Commentary:
;; Mouse Trap Mode is a minor mode for Emacs that disables most mouse and
;; trackpad events to prevent accidental text modifications.  Hitting the
;; trackpad and finding my text is being inserted in an unintended place is
;; quite annoying, especially when you're overcaffeinated.
;;
;; The mode uses a profile-based architecture to selectively enable/disable
;; mouse events based on the current major mode.  Profiles define which
;; event categories are allowed (scrolling, clicks, drags, etc.), and modes
;; are mapped to profiles.
;;
;; The keymap is built dynamically when the mode is toggled, so you can
;; change profiles or mode mappings and re-enable the mode without reloading
;; your Emacs configuration.
;;
;; Inspired by this blog post from Malabarba
;; https://endlessparentheses.com/disable-mouse-only-inside-emacs.html
;;
;;; Code:

(require 'cl-lib)

;; ------------------------------ Mouse Trap Mode ------------------------------

;;; Event Categories

(defvar mouse-trap--event-categories
  '((primary-click   . ((types . ("mouse" "down-mouse"))
                        (buttons . (1))))
    (secondary-click . ((types . ("mouse" "down-mouse"))
                        (buttons . (2 3))))
    (drags           . ((types . ("drag-mouse"))
                        (buttons . (1 2 3 4 5))))
    (multi-clicks    . ((types . ("double-mouse" "triple-mouse"))
                        (buttons . (1 2 3 4 5))))
    (scroll          . ((wheel . ("wheel-up" "wheel-down" "wheel-left" "wheel-right")))))
  "Event category definitions for mouse-trap-mode.

Each category maps to a set of event types and buttons (or wheel events).
Categories can be combined in profiles to allow specific interaction patterns.")

;;; Profiles

(defvar mouse-trap-profiles
  '((disabled        . ())
    (scroll-only     . (scroll))
    (primary-click   . (primary-click))
    (scroll+primary  . (scroll primary-click))
    (read-only       . (scroll primary-click secondary-click))
    (interactive     . (scroll primary-click secondary-click drags))
    (full            . (scroll primary-click secondary-click drags multi-clicks)))
  "Mouse interaction profiles for different use cases.

Each profile specifies which event categories are allowed.
Available categories: primary-click, secondary-click, drags, multi-clicks, scroll.

Profiles:
  - disabled: Block all mouse events
  - scroll-only: Only allow scrolling
  - primary-click: Only allow left click
  - scroll+primary: Allow scrolling and left click
  - read-only: Scrolling and clicking for reading/browsing
  - interactive: Add dragging for text selection
  - full: Allow all mouse events")

(defvar mouse-trap-mode-profiles
  '((dashboard-mode  . primary-click)
    (pdf-view-mode   . full)
    (nov-mode        . full))
  "Map major modes to mouse-trap profiles.

Modes not listed here will use `mouse-trap-default-profile'.
When checking, the mode hierarchy is respected via `derived-mode-p'.")

(defvar mouse-trap-default-profile 'disabled
  "Default profile to use when current major mode is not in `mouse-trap-mode-profiles'.")

;;; Keymap Builder

(defun mouse-trap--get-profile-for-mode ()
  "Return the profile for the current major mode.

Checks `mouse-trap-mode-profiles' for an exact match with `major-mode',
then checks parent modes via `derived-mode-p'.  Falls back to
`mouse-trap-default-profile' if no match."
  ;; First check for exact match with current major-mode
  (or (alist-get major-mode mouse-trap-mode-profiles)
      ;; Then check parent modes
      (cl-loop for (mode . profile) in mouse-trap-mode-profiles
               when (and (not (eq mode major-mode))
                         (derived-mode-p mode))
               return profile)
      ;; Finally use default
      mouse-trap-default-profile))

(defun mouse-trap--build-keymap ()
  "Build a keymap based on current major mode's profile.

Returns a keymap that binds mouse events to `ignore' for all events
NOT allowed by the current profile.  This function is called each time
the mode is toggled, allowing dynamic behavior without reloading config."
  (let* ((profile-name (mouse-trap--get-profile-for-mode))
         (allowed-categories (alist-get profile-name mouse-trap-profiles))
         (prefixes '("" "C-" "M-" "S-" "C-M-" "C-S-" "M-S-" "C-M-S-"))
         (map (make-sparse-keymap)))

    ;; For each event category, disable it if not in allowed list
    (dolist (category-entry mouse-trap--event-categories)
      (let ((category (car category-entry))
            (spec (cdr category-entry)))
        (unless (memq category allowed-categories)
          ;; This category is NOT allowed - bind its events to ignore
          (cond
           ;; Scroll events (wheel)
           ((alist-get 'wheel spec)
            (dolist (evt (alist-get 'wheel spec))
              (dolist (pref prefixes)
                (define-key map (kbd (format "<%s%s>" pref evt)) #'ignore))))

           ;; Click/drag events (types + buttons)
           ((and (alist-get 'types spec) (alist-get 'buttons spec))
            (dolist (type (alist-get 'types spec))
              (dolist (button (alist-get 'buttons spec))
                (dolist (pref prefixes)
                  (define-key map (kbd (format "<%s%s-%d>" pref type button)) #'ignore)))))))))
    map))

;;; Minor Mode Definition

(defvar-local mouse-trap-mode-map nil
  "Keymap for `mouse-trap-mode'.  Built dynamically per buffer.")

(defvar mouse-trap--lighter-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1]
      (lambda (event)
        (interactive "e")
        (with-selected-window (posn-window (event-start event))
          (mouse-trap-mode (if mouse-trap-mode -1 1)))))
    map)
  "Keymap for the mouse-trap-mode lighter.
Allows clicking the lighter to toggle the mode.")

(defun mouse-trap--lighter-string ()
  "Generate the mode-line lighter string for mouse-trap-mode.
Returns a propertized string that shows 🪤 when mode is on, 🐭 when off.
The string is clickable to toggle the mode."
  (propertize (if mouse-trap-mode " 🪤" " 🐭")
              'mouse-face 'mode-line-highlight
              'help-echo "mouse-1: Toggle mousetrap mode"
              'local-map mouse-trap--lighter-keymap))

(define-minor-mode mouse-trap-mode
  "Buffer-locally disable mouse and trackpad events based on major mode.

Mouse-trap-mode uses a profile-based system to selectively enable or
disable mouse events.  Each major mode can be mapped to a profile, and
profiles define which event categories are allowed.

Available event categories:
  - primary-click: Left mouse button
  - secondary-click: Middle and right mouse buttons
  - drags: Drag selections
  - multi-clicks: Double and triple clicks
  - scroll: Mouse wheel / trackpad scrolling

The keymap is built dynamically when the mode is toggled, so you can
change `mouse-trap-mode-profiles' or `mouse-trap-profiles' and re-enable
the mode without reloading your configuration.

See `mouse-trap-profiles' for available profiles and
`mouse-trap-mode-profiles' for mode mappings."
  :lighter nil  ; We use mode-line-misc-info instead
  :group 'convenience
  ;; Build keymap dynamically when mode is activated
  (if mouse-trap-mode
      (progn
        (setq mouse-trap-mode-map (mouse-trap--build-keymap))
        ;; Force the keymap to be recognized by the minor mode system
        (setq minor-mode-map-alist
              (cons (cons 'mouse-trap-mode mouse-trap-mode-map)
                    (assq-delete-all 'mouse-trap-mode minor-mode-map-alist)))
        ;; Add dynamic lighter to mode-line-misc-info (always visible)
        (unless (member '(:eval (mouse-trap--lighter-string)) mode-line-misc-info)
          (push '(:eval (mouse-trap--lighter-string)) mode-line-misc-info)))
    ;; When disabling, remove from minor-mode-map-alist
    (setq minor-mode-map-alist
          (assq-delete-all 'mouse-trap-mode minor-mode-map-alist))
    ;; Note: We keep the lighter in mode-line-misc-info so it shows 🐭 when disabled
    ))

(defvar mouse-trap-excluded-modes
  '(image-mode eww-mode Info-mode dired-mode)
  "Major modes where `mouse-trap-mode' should not be auto-enabled.

These modes are excluded from automatic activation via hooks, but you
can still manually enable mouse-trap-mode in these buffers if desired.")

(defun mouse-trap-maybe-enable ()
  "Enable `mouse-trap-mode' unless in an excluded mode."
  (unless (apply #'derived-mode-p mouse-trap-excluded-modes)
    (mouse-trap-mode 1)))

;; Enable in text, prog, and special modes
(add-hook 'text-mode-hook #'mouse-trap-maybe-enable)
(add-hook 'prog-mode-hook #'mouse-trap-maybe-enable)
(add-hook 'special-mode-hook #'mouse-trap-maybe-enable)

(keymap-global-set "C-c M" #'mouse-trap-mode)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "C-c M" "mouse trap mode"))

(provide 'mousetrap-mode)
;;; mousetrap-mode.el ends here.
