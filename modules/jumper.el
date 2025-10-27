;;; jumper.el --- Quick jump between locations using registers -*- lexical-binding: t -*-

;; Author: Craig Jennings
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience
;; URL: https://github.com/cjennings/jumper

;;; Commentary:

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

(defun jumper--location-exists-p ()
  "Check if current location is already stored."
  (let ((key (jumper--location-key))
		(found nil))
	(dotimes (i jumper--next-index found)
	  (let* ((reg (aref jumper--registers i))
			 (marker (get-register reg)))
		(when (and marker (markerp marker))
		  (save-current-buffer
			(set-buffer (marker-buffer marker))
			(save-excursion
			  (goto-char marker)
			  (when (string= key (jumper--location-key))
				(setq found t)))))))))

(defun jumper--register-available-p ()
  "Check if there are registers available."
  (< jumper--next-index jumper-max-locations))

(defun jumper--format-location (index)
  "Format location at INDEX for display."
  (let* ((reg (aref jumper--registers index))
		 (marker (get-register reg)))
	(when (and marker (markerp marker))
	  (save-current-buffer
		(set-buffer (marker-buffer marker))
		(save-excursion
		  (goto-char marker)
		  (format "[%d] %s:%d - %s"
				  index
				  (buffer-name)
				  (line-number-at-pos)
				  (buffer-substring-no-properties
				   (line-beginning-position)
				   (min (+ (line-beginning-position) 40)
						(line-end-position)))))))))

(defun jumper--do-store-location ()
  "Store current location in the next free register.
Returns: \\='already-exists if location is already stored,
         \\='no-space if all registers are full,
         register character if successfully stored."
  (cond
   ((jumper--location-exists-p) 'already-exists)
   ((not (jumper--register-available-p)) 'no-space)
   (t
    (let ((reg (+ ?0 jumper--next-index)))
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
        'already-there
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
      ('jumped (message "Jumped to location"))))
   ;; Multiple locations - prompt user
   (t
    (let* ((locations
            (cl-loop for i from 0 below jumper--next-index
                     for fmt = (jumper--format-location i)
                     when fmt collect (cons fmt i)))
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
  "Reorder registers after removing the one at REMOVED-IDX."
  (when (< removed-idx (1- jumper--next-index))
	;; Shift all higher registers down
	(cl-loop for i from removed-idx below (1- jumper--next-index)
			 do (let ((next-reg (aref jumper--registers (1+ i))))
				  (aset jumper--registers i next-reg))))
  (setq jumper--next-index (1- jumper--next-index)))

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
            (cl-loop for i from 0 below jumper--next-index
                     for fmt = (jumper--format-location i)
                     when fmt collect (cons fmt i)))
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

;; Call jumper-setup-keys when the package is loaded
(jumper-setup-keys)

;; which-key integration
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "M-SPC" "jumper menu"
    "M-SPC SPC" "store location"
    "M-SPC j" "jump to location"
    "M-SPC d" "remove location"))

(provide 'jumper)
;;; jumper.el ends here.
