;;; vcf-conversion-helpers.el --- vcf conversion utility functions -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(defun cj/clean-vcf-for-import (input-vcf output-vcf)
  "Clean and prepare VCF file for import to org-contacts."
  (interactive "fInput VCF file: \nFOutput VCF file: ")
  (with-temp-buffer
	(insert-file-contents input-vcf)
	(goto-char (point-min))

	;; First, clean up multi-line fields (unfold them) BEFORE processing
	;; This ensures PHOTO and other multi-line fields are on single lines
	(goto-char (point-min))
	(while (re-search-forward "\n[ \t]+" nil t)
	  (replace-match " " t t))

	;; Handle birthdays while the structure is intact
	(goto-char (point-min))
	(while (re-search-forward "^BDAY:\\(.*\\)$" nil t)
	  (let* ((bday-value (match-string 1))
			 (full-match (match-string 0))
			 (match-start (match-beginning 0))
			 (match-end (match-end 0))
			 (cleaned-bday
			  (cond
			   ;; Full date format: 19700805 -> 1970-08-05
			   ((string-match "^\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)$" bday-value)
				(format "BDAY:%s-%s-%s"
						(match-string 1 bday-value)
						(match-string 2 bday-value)
						(match-string 3 bday-value)))
			   ;; Month-day only: --0714 -> use current year or set to 1900
			   ((string-match "^--\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)$" bday-value)
				(format "BDAY:1900-%s-%s"
						(match-string 1 bday-value)
						(match-string 2 bday-value)))
			   ;; Text format incomplete: convert to note
			   ((string-match "VALUE=text" bday-value)
				nil) ; We'll add as note later
			   ;; Keep as is if already in good format
			   (t full-match))))
		(when cleaned-bday
		  (goto-char match-start)
		  (delete-region match-start match-end)
		  (insert cleaned-bday)
		  (goto-char match-end))))

	;; Convert item-prefixed TEL and EMAIL fields to standard format
	(goto-char (point-min))
	(while (re-search-forward "^item[0-9]+\\.\\(TEL\\|EMAIL\\)\\(.*?\\):\\(.+\\)$" nil t)
	  (let ((field-type (match-string 1))
			(field-params (match-string 2))
			(field-value (match-string 3)))
		(replace-match (format "%s%s:%s" field-type field-params field-value) t t)))

	;; NOW remove unwanted fields (but not the converted TEL/EMAIL fields)
	(let ((remove-patterns
		   '("^PHOTO:.*$"
			 "^X-ABRELATEDNAMES:.*$"
			 "^X-ABLabel:.*$"
			 "^X-FILE-AS:.*$"
			 "^VERSION:.*$"
			 "^item[0-9]+\\.X-.*$"  ; Only remove item-prefixed X- fields
			 "^CATEGORIES:.*Imported on.*$")))
	  (dolist (pattern remove-patterns)
		(goto-char (point-min))
		(while (re-search-forward pattern nil t)
		  (delete-region (line-beginning-position)
						 (min (1+ (line-end-position)) (point-max))))))

	;; Process each VCARD to ensure it has an FN field or remove if no identifying info
	(goto-char (point-min))
	(let ((vcards-to-remove '()))
	  (while (re-search-forward "^BEGIN:VCARD" nil t)
		(let ((vcard-start (match-beginning 0))
			  (has-fn nil)
			  (has-n nil)
			  (has-org nil)
			  (fn-value nil)
			  (n-value nil)
			  (org-value nil))
		  (when (re-search-forward "^END:VCARD" nil t)
			(let ((vcard-end (match-end 0)))
			  (save-excursion
				(save-restriction
				  (narrow-to-region vcard-start vcard-end)

				  ;; Check for FN field
				  (goto-char (point-min))
				  (when (re-search-forward "^FN:\\(.+\\)$" nil t)
					(setq has-fn t)
					(setq fn-value (match-string 1)))

				  ;; Check for N field (structured name)
				  (goto-char (point-min))
				  (when (re-search-forward "^N:\\(.+\\)$" nil t)
					(setq has-n t)
					(setq n-value (match-string 1)))

				  ;; Check for ORG field
				  (goto-char (point-min))
				  (when (re-search-forward "^ORG:\\(.+\\)$" nil t)
					(setq has-org t)
					(setq org-value (match-string 1)))

				  ;; If no FN but has N or ORG, synthesize FN
				  (when (and (not has-fn) (or has-n has-org))
					(goto-char (point-min))
					(if (re-search-forward "^BEGIN:VCARD$" nil t)
						(let ((synthesized-fn
							   (cond
								;; Try to build from N field first
								(has-n
								 (let* ((n-parts (split-string n-value ";"))
										(last-name (nth 0 n-parts))
										(first-name (nth 1 n-parts))
										(middle-name (nth 2 n-parts))
										(prefix (nth 3 n-parts))
										(suffix (nth 4 n-parts))
										(name-parts '()))
								   ;; Build name in "First Middle Last" order
								   (when (and prefix (not (string-empty-p prefix)))
									 (push prefix name-parts))
								   (when (and first-name (not (string-empty-p first-name)))
									 (push first-name name-parts))
								   (when (and middle-name (not (string-empty-p middle-name)))
									 (push middle-name name-parts))
								   (when (and last-name (not (string-empty-p last-name)))
									 (push last-name name-parts))
								   (when (and suffix (not (string-empty-p suffix)))
									 (push suffix name-parts))
								   (if name-parts
									   (string-join (reverse name-parts) " ")
									 ;; If N field exists but is empty, fall back to ORG
									 (when has-org
									   (replace-regexp-in-string ";" ", " org-value)))))
								;; Use ORG field if no N field
								(has-org
								 (replace-regexp-in-string ";" ", " org-value))
								(t nil))))
						  (when synthesized-fn
							(end-of-line)
							(insert (format "\nFN:%s" synthesized-fn))
							(setq has-fn t)))))

				  ;; Mark for removal if no identifying information
				  (when (not (or has-fn has-n has-org))
					(push (cons vcard-start vcard-end) vcards-to-remove))

				  (widen)))))))

	  ;; Remove VCARDs with no identifying information (in reverse order to preserve positions)
	  (dolist (vcard-range (reverse vcards-to-remove))
		(delete-region (car vcard-range) (cdr vcard-range))
		(message "Removed VCARD with no identifying information")))

	;; Remove empty lines within VCARDs
	(goto-char (point-min))
	(while (re-search-forward "^BEGIN:VCARD" nil t)
	  (let ((start (point)))
		(when (re-search-forward "^END:VCARD" nil t)
		  (save-restriction
			(narrow-to-region start (match-beginning 0))
			(goto-char (point-min))
			(delete-matching-lines "^$")
			(widen)))))

	;; Add back VERSION field (required for valid VCF)
	(goto-char (point-min))
	(while (re-search-forward "^BEGIN:VCARD$" nil t)
	  (end-of-line)
	  (insert "\nVERSION:3.0"))

	;; Save cleaned file
	(write-region (point-min) (point-max) output-vcf)
	(message "Cleaned VCF saved to %s" output-vcf)))


(defun split-vcf-file (vcf-file output-dir)
  "Split a combined VCF file into individual contact files."
  (interactive "fVCF file to split: \nDOutput directory: ")
  (mkdir output-dir t)
  (with-temp-buffer
	(insert-file-contents vcf-file)
	(goto-char (point-min))
	(let ((count 0)
		  (contact-name ""))
	  (while (re-search-forward "^BEGIN:VCARD" nil t)
		(let ((start (match-beginning 0)))
		  (when (re-search-forward "^END:VCARD" nil t)
			(let* ((end (match-end 0))
				   (vcard (buffer-substring start end)))
			  ;; Try to extract name for better filename
			  (with-temp-buffer
				(insert vcard)
				(goto-char (point-min))
				(if (re-search-forward "^FN:\\(.+\\)$" nil t)
					(setq contact-name
						  (replace-regexp-in-string
						   "[^A-Za-z0-9-_]" "_"
						   (match-string 1)))
				  (setq contact-name (format "contact-%04d" count))))
			  ;; Write individual VCF file
			  (let ((filename (format "%s/%s.vcf" output-dir contact-name)))
				(with-temp-file filename
				  (insert vcard))
				(cl-incf count))))))
	  (message "Split into %d contact files in %s" count output-dir))))


(defun cj/convert-cleaned-vcf-to-org-contacts (vcf-file org-file)
  "Convert cleaned VCF file to org-contacts format."
  (interactive "fCleaned VCF file: \nFOrg file to create: ")
  (with-temp-buffer
	(insert-file-contents vcf-file)
	(let ((contacts '())
		  (contact-count 0))  ; Add a counter for the final message
	  (goto-char (point-min))
	  (while (re-search-forward "^BEGIN:VCARD" nil t)
		(let ((vcard-start (point))
			  (contact (make-hash-table :test 'equal)))
		  (when (re-search-forward "^END:VCARD" nil t)
			(let ((vcard-end (match-beginning 0)))
			  (save-restriction
				(narrow-to-region vcard-start vcard-end)
				(goto-char (point-min))

				;; Extract FN (Full Name)
				(when (re-search-forward "^FN:\\(.+\\)$" nil t)
				  (puthash "name" (match-string 1) contact))

				;; Extract EMAIL (can have multiple)
				(goto-char (point-min))
				(let ((emails '()))
				  (while (re-search-forward "^EMAIL[^:]*:\\(.+\\)$" nil t)
					(push (match-string 1) emails))
				  (when emails
					(puthash "email" (string-join (reverse emails) " ") contact)))

				;; Extract PHONE (can have multiple)
				(goto-char (point-min))
				(let ((phones '()))
				  (while (re-search-forward "^TEL[^:]*:\\(.+\\)$" nil t)
					(let ((phone (match-string 1)))
					  ;; Clean up phone numbers slightly
					  (setq phone (replace-regexp-in-string "^\\+1 " "" phone))
					  (push phone phones)))
				  (when phones
					(puthash "phone" (string-join (reverse phones) ", ") contact)))

				;; Extract ORG
				(goto-char (point-min))
				(when (re-search-forward "^ORG:\\(.+\\)$" nil t)
				  (let ((org-value (match-string 1)))
					;; Handle semicolon-separated org values
					(setq org-value (replace-regexp-in-string ";" ", " org-value))
					(puthash "org" org-value contact)))

				;; Extract TITLE
				(goto-char (point-min))
				(when (re-search-forward "^TITLE:\\(.+\\)$" nil t)
				  (puthash "title" (match-string 1) contact))

				;; Extract ADDRESS
				(goto-char (point-min))
				(when (re-search-forward "^ADR[^:]*:\\(.+\\)$" nil t)
				  (let ((addr (match-string 1)))
					;; VCF address format: ;;Street;City;State;Zip;Country
					;; Clean up the semicolons
					(setq addr (replace-regexp-in-string ";" " " addr))
					(setq addr (replace-regexp-in-string "  +" " " addr))
					(puthash "address" addr contact)))

				;; Extract NOTE
				(goto-char (point-min))
				(when (re-search-forward "^NOTE:\\(.+\\)$" nil t)
				  (let ((note (match-string 1)))
					;; Unescape VCF note formatting
					(setq note (replace-regexp-in-string "\\\\n" "\n    " note))
					(setq note (replace-regexp-in-string "\\\\:" ":" note))
					(puthash "note" note contact)))

				;; Extract BDAY (should be cleaned format now)
				(goto-char (point-min))
				(when (re-search-forward "^BDAY:\\(.+\\)$" nil t)
				  (let ((bday (match-string 1)))
					;; Convert to org format
					(when (string-match "\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)" bday)
					  (let ((year (match-string 1 bday))
							(month (match-string 2 bday))
							(day (match-string 3 bday)))
						(if (string= year "1900")  ; Our placeholder for unknown years
							(puthash "birthday"
									 (format "<%%(diary-anniversary %d %d)>"
											 (string-to-number month)
											 (string-to-number day))
									 contact)
						  (puthash "birthday" (format "<%s-%s-%s>" year month day) contact))))))

				;; Extract URL
				(goto-char (point-min))
				(when (re-search-forward "^URL:\\(.+\\)$" nil t)
				  (puthash "url" (match-string 1) contact))

				;; Extract NICKNAME
				(goto-char (point-min))
				(when (re-search-forward "^NICKNAME:\\(.+\\)$" nil t)
				  (puthash "nickname" (match-string 1) contact))

				(widen))))

		  ;; Only add contact if it has a name
		  (when (gethash "name" contact)
			(push contact contacts)
			(setq contact-count (1+ contact-count)))))

	  ;; Write to org file
	  (with-temp-file org-file
		(insert "#+TITLE: Contacts\n")
		(insert "#+STARTUP: overview\n")
		(insert "#+CATEGORY: contacts\n\n")

		;; Sort contacts by name
		(setq contacts (sort (reverse contacts)
							 (lambda (a b)
							   (string< (or (gethash "name" a) "")
										(or (gethash "name" b) "")))))

		(dolist (contact contacts)
		  (insert (format "* %s\n" (gethash "name" contact "")))
		  (insert "  :PROPERTIES:\n")

		  ;; Add all properties
		  (when-let ((email (gethash "email" contact)))
			(insert (format "  :EMAIL:    %s\n" email)))
		  (when-let ((phone (gethash "phone" contact)))
			(insert (format "  :PHONE:    %s\n" phone)))
		  (when-let ((org-name (gethash "org" contact)))
			(insert (format "  :COMPANY:  %s\n" org-name)))
		  (when-let ((title (gethash "title" contact)))
			(insert (format "  :TITLE:    %s\n" title)))
		  (when-let ((addr (gethash "address" contact)))
			(insert (format "  :ADDRESS:  %s\n" addr)))
		  (when-let ((bday (gethash "birthday" contact)))
			(insert (format "  :BIRTHDAY: %s\n" bday)))
		  (when-let ((url (gethash "url" contact)))
			(insert (format "  :URL:      %s\n" url)))
		  (when-let ((nick (gethash "nickname" contact)))
			(insert (format "  :NICKNAME: %s\n" nick)))

		  (insert "  :END:\n")

		  ;; Add note as body text
		  (when-let ((note (gethash "note" contact)))
			(insert (format "\n  %s\n" note)))

		  (insert "\n")))

	  ;; Message is now inside the let block where contact-count is accessible
	  (message "Converted %d contacts to %s" contact-count org-file))))


(defun cj/delete-contact-files ()
  "Delete contact conversion files without confirmation."
  (interactive)
  (let ((files '("~/downloads/contacts-clean.vcf"
				 "~/sync/org/contacts.org")))
	(dolist (file files)
	  (let ((expanded-file (expand-file-name file)))
		(when (file-exists-p expanded-file)
		  (delete-file expanded-file)
		  (message "Deleted: %s" expanded-file))))))

(cj/delete-contact-files)

(cj/clean-vcf-for-import "~/downloads/contacts.vcf"
						 "~/downloads/contacts-clean.vcf")

;; convert it to org contacts
(cj/convert-cleaned-vcf-to-org-contacts "~/downloads/contacts-clean.vcf"
										"~/sync/org/contacts.org")

(provide 'vcf-conversion-helpers)
;;; vcf-conversion-helpers.el ends here.
