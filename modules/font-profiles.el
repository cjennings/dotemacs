;;; font-profiles.el --- Shared Workflow Font Profile Data -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 1 (Foundation).
;; Category: F/L.
;; Load shape: library.
;; Top-level side effects: none.
;; Runtime requires: host-environment.
;; Direct test load: yes.
;;
;; Owns the effective font properties shared by the global Fontaine adapter and
;; buffer-local mode adapters such as nov-reading.  Consumers can therefore use
;; the same named profile without making a global Fontaine selection.

;;; Code:

(require 'host-environment)

(declare-function face-remap-add-relative "face-remap")

(defconst cj/font-profile-shared-properties
  `(:default-family "BerkeleyMono Nerd Font"
    :default-weight regular
    :default-height ,(if (env-laptop-p) 130 140)
    :fixed-pitch-family nil
    :fixed-pitch-weight nil
    :fixed-pitch-height 1.0
    :fixed-pitch-serif-family nil
    :fixed-pitch-serif-weight nil
    :fixed-pitch-serif-height 1.0
    :variable-pitch-family "Lexend"
    :variable-pitch-weight regular
    :variable-pitch-height 1.0
    :bold-family nil
    :bold-weight bold
    :italic-family nil
    :italic-slant italic
    :line-spacing nil)
  "Properties shared by every workflow font profile unless overridden.")

(defconst cj/font-profile-definitions
  '((everyday)
    (writing
     :default-height 140
     :variable-pitch-family "Merriweather"
     :variable-pitch-weight light)
    (reading
     :default-family "Merriweather"
     :default-height 140
     :fixed-pitch-family "Merriweather"
     :fixed-pitch-serif-family "Merriweather"
     :variable-pitch-family "Merriweather")
    (coding-xs
     :default-height 110
     :variable-pitch-family "BerkeleyMono Nerd Font")
    (coding-m
     :default-height 130
     :variable-pitch-family "BerkeleyMono Nerd Font")
    (coding-l
     :default-height 140
     :variable-pitch-family "BerkeleyMono Nerd Font")
    (coding-xl
     :default-height 160
     :variable-pitch-family "BerkeleyMono Nerd Font")
    (presentation
     :default-height 200))
  "Profile-specific font properties in user-facing order.")

(defconst cj/font-profile-order
  (mapcar #'car cj/font-profile-definitions)
  "Workflow font profiles in user-facing order.")

(defun cj/font-profile-p (profile)
  "Return non-nil when PROFILE is a configured workflow font profile."
  (memq profile cj/font-profile-order))

(defun cj/font-profile-properties (profile)
  "Return effective font properties for workflow PROFILE."
  (let ((entry (assq profile cj/font-profile-definitions)))
    (unless entry
      (user-error "Unknown font profile: %s" profile))
    (append (cdr entry) cj/font-profile-shared-properties)))

(defun cj/font-profile-remap-buffer (profile &optional height)
  "Apply PROFILE's face families buffer-locally and return remap cookies.
When HEIGHT is non-nil, use it for every remapped face instead of the profile's
configured heights.  No global face or Fontaine state is changed."
  (let ((properties (cj/font-profile-properties profile))
        (cookies nil))
    (dolist (face-property '((default
                              :default-family :default-height)
                             (fixed-pitch
                              :fixed-pitch-family :fixed-pitch-height)
                             (fixed-pitch-serif
                              :fixed-pitch-serif-family
                              :fixed-pitch-serif-height)
                             (variable-pitch
                              :variable-pitch-family
                              :variable-pitch-height)))
      (pcase-let ((`(,face ,family-property ,height-property)
                   face-property))
        (let ((family (or (plist-get properties family-property)
                          (and (memq face '(fixed-pitch fixed-pitch-serif))
                               (plist-get properties :default-family))))
              (face-height (or height
                               (plist-get properties height-property))))
          (when family
            (push (face-remap-add-relative
                   face :family family :height face-height)
                  cookies)))))
    (nreverse cookies)))

(provide 'font-profiles)
;;; font-profiles.el ends here
