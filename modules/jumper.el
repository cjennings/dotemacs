;;; jumper.el --- Quick jump between locations using registers -*- lexical-binding: t -*-

;; Author: Craig Jennings
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience
;; URL: https://github.com/cjennings/jumper

;;; Commentary:
;;
;; Layer: 4 (Optional).
;; Category: O/L.
;; Load shape: eager.
;; Eager reason: none; navigation helper, a command-loaded deferral candidate.
;; Top-level side effects: defines a jumper keymap.
;; Runtime requires: cl-lib.
;; Direct test load: yes.
;;
;; Jumper provides a simple way to store and jump between locations
;; in your codebase without needing to remember register assignments.
;;
;; PURPOSE:
;;
;; When working on large codebases, you often need to jump between
;; multiple related locations: a function definition, its tests, its
;; callers, configuration files, etc. Emacs registers are perfect for
;; this, but require you to remember which register you assigned to
;; which location. Jumper automates register management, letting you
;; focus on your work instead of bookkeeping.
;;
;; WORKFLOW:
;;
;; 1. Navigate to an important location in your code
;; 2. Press M-SPC SPC to store it (automatically assigned to register 0)
;; 3. Continue working, storing more locations as needed (registers 1-9)
;; 4. Press M-SPC j to jump back to any stored location
;; 5. Select from the list using completion (shows file, line, context)
;; 6. Press M-SPC d to remove locations you no longer need
;;
;; RECOMMENDED USAGE:
;;
;; Store locations temporarily while working on a feature:
;;   - Store the main function you're implementing
;;   - Store the test file where you're writing tests
;;   - Store the caller that needs updating
;;   - Store the documentation that needs changes
;;   - Jump between them freely as you work
;;   - Clear them when done with the feature
;;
;; SPECIAL BEHAVIORS:
;;
;; - Duplicate prevention: Storing the same location twice shows a message
;;   instead of wasting a register slot.
;;
;; - Single location toggle: When only one location is stored, M-SPC j
;;   toggles between that location and your current position. Perfect for
;;   rapid back-and-forth between two related files.
;;
;; - Last location tracking: The last position before each jump is saved
;;   in register 'z', allowing quick "undo" of navigation.
;;
;; - Smart selection: With multiple locations, completing-read shows
;;   helpful context: "[0] filename.el:42 - function definition..."
;;
;; KEYBINDINGS:
;;
;; M-SPC SPC   Store current location in next available register
;; M-SPC j     Jump to a stored location (with completion)
;; M-SPC d     Delete a stored location from the list
;;
;; CONFIGURATION:
;;
;; You can customize the prefix key and maximum locations:
;;
;;   (setq jumper-prefix-key "C-c j")  ; Change prefix key
;;   (setq jumper-max-locations 20)    ; Store up to 20 locations
;;
;; Note: Changing jumper-max-locations requires restarting Emacs or
;; manually reinitializing jumper--registers.

;;; Code:

(require 'cl-lib)

(defvar jumper-prefix-key "M-SPC"
  "Prefix key for jumper commands.
Note that using M-SPC will override the default binding to just-one-space.")

(defvar jumper-max-locations 10
  "Maximum number of locations to store.")

;; Internal variables
(defvar jumper--registers (make-vector jumper-max-locations nil)
  "Vector of used registers.")

(defvar jumper--next-index 0
  "Next available index in the jumper--registers vector.")

(defvar jumper--last-location-register ?z
  "Register used to store the last location.")

(defun jumper--location-key ()
  "Generate a key to identify the current location."
  (format "%s:%d:%d"
		  (or (buffer-file-name) (buffer-name))
		  (line-number-at-pos)
		  (current-column)))

(defun jumper--with-marker-at (index fn)
  "Call FN with point at the marker stored for register INDEX.
Resolve register INDEX's marker; when it is a live marker, run FN in that
marker's buffer with point at the marker (within `save-current-buffer' and
`save-excursion') and return FN's value.  Return nil when INDEX has no valid
marker."
  (let* ((reg (aref jumper--registers index))
         (marker (get-register reg)))
    (when (and marker (markerp marker)
               (buffer-live-p (marker-buffer marker)))
      (save-current-buffer
        (set-buffer (marker-buffer marker))
        (save-excursion
          (goto-char marker)
          (funcall fn))))))

(defun jumper--location-exists-p ()
  "Check if current location is already stored."
  (let ((key (jumper--location-key)))
    (cl-loop for i from 0 below jumper--next-index
             thereis (jumper--with-marker-at
                      i (lambda () (string= key (jumper--location-key)))))))

(defun jumper--register-available-p ()
  "Check if there are registers available."
  (< jumper--next-index jumper-max-locations))

(defun jumper--format-location (index)
  "Format location at INDEX for display."
  (jumper--with-marker-at
   index
   (lambda ()
     (format "[%d] %s:%d - %s"
             index
             (buffer-name)
             (line-number-at-pos)
             (buffer-substring-no-properties
              (line-beginning-position)
              (min (+ (line-beginning-position) 40)
                   (line-end-position)))))))

(defun jumper--location-candidates ()
  "Return an alist of (DISPLAY . INDEX) for all stored locations.
Indices whose marker is no longer valid are skipped (their
`jumper--format-location' returns nil)."
  (cl-loop for i from 0 below jumper--next-index
           for fmt = (jumper--format-location i)
           when fmt collect (cons fmt i)))

(defun jumper--first-free-register ()
  "Return the lowest register char in 0..N-1 not held by a live slot.
N is `jumper-max-locations'.  Only the live slice (indices 0 through
`jumper--next-index' minus 1) is consulted, so a char freed by a removal is
reused on the next store instead of colliding with a surviving slot's
register and silently overwriting its marker."
  (let ((used (make-hash-table :test 'eql)))
    (dotimes (i jumper--next-index)
      (let ((r (aref jumper--registers i)))
        (when r (puthash r t used))))
    (cl-loop for c from ?0 below (+ ?0 jumper-max-locations)
             unless (gethash c used)
             return c)))

(defun jumper--do-store-location ()
  "Store current location in the next free register.
Returns: \\='already-exists if location is already stored,
         \\='no-space if all registers are full,
         register character if successfully stored."
  (cond
   ((jumper--location-exists-p) 'already-exists)
   ((not (jumper--register-available-p)) 'no-space)
   (t
    (let ((reg (jumper--first-free-register)))
      (point-to-register reg)
      (aset jumper--registers jumper--next-index reg)
      (setq jumper--next-index (1+ jumper--next-index))
      reg))))

(defun jumper-store-location ()
  "Store current location in the next free register."
  (interactive)
  (pcase (jumper--do-store-location)
    ('already-exists (message "Location already stored"))
    ('no-space (message "Sorry - all jump locations are filled!"))
    (reg (message "Location stored in register %c" reg))))

(defun jumper--do-jump-to-location (target-idx)
  "Jump to location at TARGET-IDX.
TARGET-IDX: -1 for last location, 0-9 for stored locations, nil for toggle.
Returns: \\='no-locations if no locations stored,
         \\='already-there if at the only location (toggle case),
         \\='jumped if successfully jumped."
  (cond
   ((= jumper--next-index 0) 'no-locations)
   ;; Toggle behavior when target-idx is nil and only 1 location
   ((and (null target-idx) (= jumper--next-index 1))
    (if (jumper--location-exists-p)
        ;; Already at the only location: toggle back to where we came from
        ;; when a last-location is recorded, otherwise report no movement.
        (if (get-register jumper--last-location-register)
            (progn
              (jump-to-register jumper--last-location-register)
              'jumped-back)
          'already-there)
      (let ((reg (aref jumper--registers 0)))
        (point-to-register jumper--last-location-register)
        (jump-to-register reg)
        'jumped)))
   ;; Jump to specific target
   (t
    (if (= target-idx -1)
        ;; Jumping to last location - don't overwrite it
        (jump-to-register jumper--last-location-register)
      ;; Jumping to stored location - save current for "last"
      (progn
        (point-to-register jumper--last-location-register)
        (jump-to-register (aref jumper--registers target-idx))))
    'jumped)))

(defun jumper-jump-to-location ()
  "Jump to a stored location."
  (interactive)
  (cond
   ;; No locations
   ((= jumper--next-index 0)
    (message "No locations stored"))
   ;; Single location - toggle
   ((= jumper--next-index 1)
    (pcase (jumper--do-jump-to-location nil)
      ('already-there (message "You're already at the stored location"))
      ('jumped-back (message "Jumped back to previous location"))
      ('jumped (message "Jumped to location"))))
   ;; Multiple locations - prompt user
   (t
    (let* ((locations
            (jumper--location-candidates))
           ;; Add last location if available
           (last-pos (get-register jumper--last-location-register))
           (locations (if last-pos
                          (cons (cons "[z] Last location" -1) locations)
                        locations))
           (choice (completing-read "Jump to: " locations nil t))
           (idx (cdr (assoc choice locations))))
      (jumper--do-jump-to-location idx)
      (message "Jumped to location")))))

(defun jumper--reorder-registers (removed-idx)
  "Reorder registers after removing the one at REMOVED-IDX.
Shift the higher registers down and clear the freed register so its marker
no longer pins its buffer."
  (let ((freed (aref jumper--registers removed-idx)))
    (when (< removed-idx (1- jumper--next-index))
      ;; Shift all higher registers down
      (cl-loop for i from removed-idx below (1- jumper--next-index)
               do (aset jumper--registers i (aref jumper--registers (1+ i)))))
    (setq jumper--next-index (1- jumper--next-index))
    (when freed (set-register freed nil))))

(defun jumper--do-remove-location (index)
  "Remove location at INDEX.
Returns: \\='no-locations if no locations stored,
         \\='cancelled if index is -1,
         t if successfully removed."
  (cond
   ((= jumper--next-index 0) 'no-locations)
   ((= index -1) 'cancelled)
   (t
    (jumper--reorder-registers index)
    t)))

(defun jumper-remove-location ()
  "Remove a stored location."
  (interactive)
  (if (= jumper--next-index 0)
      (message "No locations stored")
    (let* ((locations
            (jumper--location-candidates))
           (locations (cons (cons "Cancel" -1) locations))
           (choice (completing-read "Remove location: " locations nil t))
           (idx (cdr (assoc choice locations))))
      (pcase (jumper--do-remove-location idx)
        ('cancelled (message "Operation cancelled"))
        ('t (message "Location removed"))))))

(defvar-keymap jumper-map
  :doc "Keymap for jumper commands"
  "SPC" #'jumper-store-location
  "j"   #'jumper-jump-to-location
  "d"   #'jumper-remove-location)

(defun jumper-setup-keys ()
  "Setup default keybindings for jumper."
  (interactive)
  (keymap-global-set jumper-prefix-key jumper-map))

;; Jumper's M-SPC prefix was removed 2026-06-23 so M-SPC could go to
;; `cj/ai-term-next'.  A cleverer home for jumper (numbers or F-keys) is
;; pending review; until then its commands are reachable via M-x
;; (jumper-store-location / jumper-jump-to-location / jumper-remove-location).
;; To re-home: set `jumper-prefix-key' to the new prefix and call
;; `jumper-setup-keys' (and restore the which-key labels for that prefix).

(provide 'jumper)
;;; jumper.el ends here.
