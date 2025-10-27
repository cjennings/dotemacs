;;; jumper.el --- Quick jump between locations using registers -*- lexical-binding: t -*-

;; Author: Craig Jennings
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience
;; URL: https://github.com/cjennings/jumper

;;; Commentary:

;; Jumper provides a simple way to store and jump between locations
;; in your codebase without needing to remember register assignments.

;;; Code:

(defgroup jumper nil
  "Quick navigation between stored locations."
  :group 'convenience)

(defcustom jumper-prefix-key "M-SPC"
  "Prefix key for jumper commands.

Note that using M-SPC will override the default binding to just-one-space."
  :type 'string
  :group 'jumper)

(defcustom jumper-max-locations 10
  "Maximum number of locations to store."
  :type 'integer
  :group 'jumper)

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
	(dotimes (i
			  jumper--next-index found)
	  (let* ((reg (aref jumper--registers i))
			 (pos (get-register reg))
			 (marker (and pos (registerv-data pos))))
		(when marker
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
		 (pos (get-register reg))
		 (marker (and pos (registerv-data pos))))
	(when marker
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

(defun jumper-store-location ()
  "Store current location in the next free register."
  (interactive)
  (if (jumper--location-exists-p)
	  (message "Location already stored")
	(if (jumper--register-available-p)
		(let ((reg (+ ?0 jumper--next-index)))
		  (point-to-register reg)
		  (aset jumper--registers jumper--next-index reg)
		  (setq jumper--next-index (1+ jumper--next-index))
		  (message "Location stored in register %c" reg))
	  (message "Sorry - all jump locations are filled!"))))

(defun jumper-jump-to-location ()
  "Jump to a stored location."
  (interactive)
  (if (= jumper--next-index 0)
	  (message "No locations stored")
	(if (= jumper--next-index 1)
		;; Special case for one location - toggle behavior
		(let ((reg (aref jumper--registers 0)))
		  (if (jumper--location-exists-p)
			  (message "You're already at the stored location")
			(point-to-register jumper--last-location-register)
			(jump-to-register reg)
			(message "Jumped to location")))
	  ;; Multiple locations - use completing-read
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
		(point-to-register jumper--last-location-register)
		(if (= idx -1)
			(jump-to-register jumper--last-location-register)
		  (jump-to-register (aref jumper--registers idx)))
		(message "Jumped to location")))))

(defun jumper--reorder-registers (removed-idx)
  "Reorder registers after removing the one at REMOVED-IDX."
  (when (< removed-idx (1- jumper--next-index))
	;; Shift all higher registers down
	(cl-loop for i from removed-idx below (1- jumper--next-index)
			 do (let ((next-reg (aref jumper--registers (1+ i))))
				  (aset jumper--registers i next-reg))))
  (setq jumper--next-index (1- jumper--next-index)))

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
	  (if (= idx -1)
		  (message "Operation cancelled")
		(jumper--reorder-registers idx)
		(message "Location removed")))))

(defvar jumper-map
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "SPC") #'jumper-store-location)
	(define-key map (kbd "j") #'jumper-jump-to-location)
	(define-key map (kbd "d") #'jumper-remove-location)
	map)
  "Keymap for jumper commands.")

(defun jumper-setup-keys ()
  "Setup default keybindings for jumper."
  (interactive)
  (keymap-global-set jumper-prefix-key jumper-map))

;; Call jumper-setup-keys when the package is loaded
(jumper-setup-keys)

(provide 'jumper)
;;; jumper.el ends here.
