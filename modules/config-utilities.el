;;; config-utilities  --- Config Hacking Utilities -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Development and debugging utilities for Emacs configuration maintenance.
;;
;; Features include:
;; - reloading and recompiling configuration (native/byte compilation)
;; - inspecting loaded packages and features
;; - reporting on Emacs version build configuration
;; - validating org-agenda timestamp integrity
;; - debugging org-alert timers
;; - SQLite database tracing and finalizer debugging
;; - auth-source cache management
;;
;; Key commands:
;; - ~cj/reload-init-file~ to reload init.el.
;; - ~cj/recompile-emacs-home~ to recompile all Elisp files.
;; - ~cj/list-loaded-packages~ to show currently loaded packages.
;; - ~cj/check-org-agenda-invalid-timestamps~ to scan for invalid timestamps.
;; - ~cj/sqlite-tracing-enable~ to enable SQLite debugging.
;; - ~cj/emacs-build-summary~ to build a buffer containing information about the Emacs version.
;;
;;; Code:

(require 'cl-lib)

;; Declare functions from lazy-loaded packages to suppress byte-compiler warnings.
;; These packages are required at runtime when their respective functions are called.
(declare-function find-lisp-find-files "find-lisp" (directory regexp))
(declare-function org-element-parse-buffer "org-element" (&optional granularity visible-only))
(declare-function org-element-map "org-element" (data types fun &optional info first-match no-recursion with-affiliated))
(declare-function org-element-property "org-element" (property element))
(declare-function org-time-string-to-absolute "org" (s &optional daynr prefer buffer pos))
(declare-function org-alert-check "org-alert" nil)

;; Declare variables from lazy-loaded packages
(defvar org-agenda-files)
(defvar org-ts-regexp)

;; ------------------------------ Reload Init File -----------------------------
;; it does what it says it does.

(defun cj/reload-init-file ()
  "Reload the init file.  Useful when modifying Emacs config."
  (interactive)
  (load-file user-init-file))

;; ----------------------------- Reset-Auth-Sources ----------------------------

(defun cj/reset-auth-cache ()
  "Clear Emacs auth-source cache."
  (interactive)
  (auth-source-forget-all-cached)
  (message "Emacs auth-source cache cleared."))

;; ---------------------------- Emacs Build Summary ----------------------------
;; builds a buffer with information about this version of Emacs

(defun cj--yes-no (flag)
  "Return \"yes\" if FLAG is non-nil, otherwise return \"no\"."
  (if flag "yes" "no"))

(defun cj--format-build-time (tval)
  "Return a human-readable build time from TVAL."
  (cond
   ((null tval) "unknown")
   ((stringp tval) tval)
   ((and (consp tval) (integerp (car tval)))
	(format-time-string "%Y-%m-%d %H:%M:%S %Z" tval))
   ((numberp tval)
	(format-time-string "%Y-%m-%d %H:%M:%S %Z" (seconds-to-time tval)))
   (t (format "%s" tval))))

(defun cj/emacs-build-summary-string ()
  "Return a concise multi-line string describing this Emacs build."
  (let ((build-time (and (boundp 'emacs-build-time) emacs-build-time))
		(build-system (and (boundp 'emacs-build-system) emacs-build-system))
		(branch (and (boundp 'emacs-repository-branch) emacs-repository-branch))
		(commit (and (boundp 'emacs-repository-version) emacs-repository-version))
		(features (and (boundp 'system-configuration-features) system-configuration-features))
		(options (and (boundp 'system-configuration-options) system-configuration-options)))
	(concat
	 (format "Version: %s\n" emacs-version)
	 (format "System: %s\n" system-configuration)
	 (format "Build date: %s\n" (cj--format-build-time build-time))
	 (when build-system
	   (format "Build system: %s\n" build-system))
	 (when branch
	   (format "Git branch: %s\n" (or branch "n/a")))
	 (when commit
	   (format "Git commit: %s\n" (or commit "n/a")))
	 "\nCapabilities:\n"
	 (format "- Native compilation: %s\n"
			 (cj--yes-no (and (fboundp 'native-comp-available-p)
							  (native-comp-available-p))))
	 (format "- Dynamic modules: %s\n"
			 (cj--yes-no (and (boundp 'module-file-suffix)
							  module-file-suffix)))
	 (format "- GnuTLS: %s\n"
			 (cj--yes-no (and (fboundp 'gnutls-available-p)
							  (gnutls-available-p))))
	 (format "- libxml2: %s\n"
			 (cj--yes-no (fboundp 'libxml-parse-html-region)))
	 (format "- ImageMagick: %s\n"
			 (cj--yes-no (and (fboundp 'image-type-available-p)
							  (image-type-available-p 'imagemagick))))
	 (format "- SQLite: %s\n"
			 (cj--yes-no (and (fboundp 'sqlite-available-p)
							  (sqlite-available-p))))
	 (when features
	   (format "\nConfigured features:\n%s\n" features))
	 (when options
	   (format "\nConfiguration arguments:\n%s\n" options)))))

(defun cj/emacs-build-summary ()
  "Display a buffer with the Emacs build summary."
  (interactive)
  (let ((buf (get-buffer-create "*Emacs-Build-Summary*")))
	(with-current-buffer buf
	  (setq buffer-read-only nil)
	  (erase-buffer)
	  (insert (cj/emacs-build-summary-string))
	  (goto-char (point-min))
	  (help-mode)
	  (setq-local truncate-lines nil))
	(pop-to-buffer buf)))

;; ---------------------------- Recompile Emacs Home ---------------------------
;; deletes all .elc and .eln files in user-emacs-directory, then compiles
;; all emacs-lisp files natively if supported, or byte-compiles them if not.

(defvar comp-async-report-warnings-errors)

(defun cj/recompile-emacs-home()
  "Delete all compiled files in the Emacs home before recompiling.
Recompile natively when supported, otherwise fall back to byte compilation."
  (interactive)
  (let* ((native-comp-supported (boundp 'native-compile-async))
		 (elt-dir
		  (expand-file-name (if native-comp-supported "eln" "elc")
							user-emacs-directory))
		 (message-format
		  (format "Please confirm recursive %s recompilation of %%s: "
				  (if native-comp-supported "native" "byte")))
		 (compile-message (format "%scompiling all emacs-lisp files in %%s"
								  (if native-comp-supported "Natively " "Byte-"))))
	(if (yes-or-no-p (format message-format user-emacs-directory))
		(progn
		  (message "Deleting all compiled files in %s" user-emacs-directory)
		  (dolist (file (directory-files-recursively user-emacs-directory
													 "\\(\\.elc\\|\\.eln\\)$"))
			(delete-file file))
		  (when (file-directory-p elt-dir)
			(delete-directory elt-dir t t))
		  (message compile-message user-emacs-directory)
		  (if native-comp-supported
			  (progn
				(setq comp-async-report-warnings-errors nil)
				(native-compile-async user-emacs-directory 'recursively))
			(byte-recompile-directory user-emacs-directory 0)))
	  (message "Cancelled recompilation of %s" user-emacs-directory))))

;; ---------------------- Delete Emacs Home Compiled Files ---------------------
;; removes all compiled files and deletes the eln directory

(defun cj/delete-emacs-home-compiled-files ()
  "Delete all compiled files recursively in \='user-emacs-directory\='."
  (interactive)
  (message "Deleting compiled files under %s. This may take a while."
		   user-emacs-directory)
  (require 'find-lisp)    ;; make sure the package is required
  (mapc (lambda (path)
		  (when (or (string-suffix-p ".elc" path)
					(string-suffix-p ".eln" path))
			(delete-file path)))
        (find-lisp-find-files user-emacs-directory ""))
  (message "Done. Compiled files removed under %s" user-emacs-directory))

;; ---------------------- List Loaded Packages ---------------------
;; you don't really need an explanation for this function, do you?

(defvar cj--loaded-file-paths nil
  "All file paths that are loaded.")
(defvar cj--loaded-packages-buffer "*loaded-packages*"
  "Buffer name for data about loaded packages.")
(defvar cj--loaded-features-buffer "*loaded-features*"
  "Buffer name for data about loaded features.")

(defun cj/list-loaded-packages()
  "List all currently loaded packages."
  (interactive)
  (with-current-buffer (get-buffer-create cj--loaded-packages-buffer)
	(erase-buffer)
	(pop-to-buffer (current-buffer))

	(insert "* Live Packages Exploration\n\n")

	;; Extract data from builtin variable `load-history'.
	(setq cj--loaded-file-paths
		  (seq-filter #'stringp
					  (mapcar #'car load-history)))
	(setq cj--loaded-file-paths (cl-sort cj--loaded-file-paths 'string-lessp))
	(insert (format "%s total packages currently loaded\n"
					(length cj--loaded-file-paths)))
	(cl-loop for file in cj--loaded-file-paths
			 do (insert "\n" file))

	(goto-char (point-min))))

;; ---------------------------- List Loaded Features ---------------------------
;; this function's also self-explanatory

(defun cj/list-loaded-features()
  "List all currently loaded features."
  (interactive)
  (with-current-buffer (get-buffer-create cj--loaded-features-buffer)
    (erase-buffer)
    (pop-to-buffer (current-buffer))

    (insert (format "\n** %d features currently loaded\n"
                    (length features)))

    (let ((features-vec (apply 'vector features)))
      (setq features-vec (cl-sort features-vec 'string-lessp))
      (cl-loop for x across features-vec
               do (insert (format "  - %-25s: %s\n" x
								  (locate-library (symbol-name x))))))
	(goto-char (point-min))))

;; ------------------------ Validate Org Agenda Entries ------------------------

(defun cj/check-org-agenda-invalid-timestamps ()
  "Scan all files in \='org-agenda-files\=' for invalid timestamps.
Checks DEADLINE, SCHEDULED, TIMESTAMP properties and inline timestamps in
headline contents. Generates an Org-mode report buffer with links to problematic
entries, property/type, and raw timestamp string."
  (interactive)
  (require 'org)
  (require 'org-element)
  (let ((report-buffer (get-buffer-create "*Org Invalid Timestamps Report*")))
	(with-current-buffer report-buffer
	  (erase-buffer)
	  (org-mode)
	  (insert "#+TITLE: Org Invalid Timestamps Report\n\n")
	  (insert "* Overview\nScan of org-agenda-files for invalid timestamps.\n\n"))
	(dolist (file org-agenda-files)
	  (with-current-buffer (find-file-noselect file)
		(let ((invalid-entries '())
			  (props '("DEADLINE" "SCHEDULED" "TIMESTAMP"))
			  (parse-tree (org-element-parse-buffer 'headline)))
		  (org-element-map parse-tree 'headline
			(lambda (hl)
			  (let ((headline-text (org-element-property :raw-value hl))
					(begin-pos (org-element-property :begin hl)))
				(dolist (prop props)
				  (let ((timestamp (org-element-property (intern (downcase prop)) hl)))
					(when timestamp
					  (let ((time-str (org-element-property :raw-value timestamp)))
						(unless (ignore-errors (org-time-string-to-absolute time-str))
						  (push (list file begin-pos headline-text prop time-str) invalid-entries))))))
				(let ((contents-begin (org-element-property :contents-begin hl))
					  (contents-end (org-element-property :contents-end hl)))
				  (when (and contents-begin contents-end)
					(save-excursion
					  (goto-char contents-begin)
					  (while (re-search-forward org-ts-regexp contents-end t)
						(let ((ts-string (match-string 0)))
						  (unless (ignore-errors (org-time-string-to-absolute ts-string))
							(push (list file begin-pos headline-text "inline timestamp" ts-string) invalid-entries))))))))))

		  (with-current-buffer report-buffer
			(insert (format "* %s\n" file))
			(if invalid-entries
				(dolist (entry (reverse invalid-entries))
				  (cl-destructuring-bind (f pos head prop ts) entry
					(insert (format "- [[file:%s::%d][%s]]\n  - Property/Type: %s\n  - Invalid timestamp: \"%s\"\n"
									f pos head prop ts))))
			  (insert "No invalid timestamps found.\n")))
		  (with-current-buffer report-buffer (insert "\n")))))
	(pop-to-buffer report-buffer)))

;; --------------------------- Org-Alert-Check Timers --------------------------
;; Utility to list timers running org-alert-check

(defun cj/org-alert-list-timers ()
  "List all active timers running `org-alert-check' with next run time."
  (interactive)
  (let ((timers (cl-remove-if-not
				 (lambda (timer)
				   (eq (timer--function timer) #'org-alert-check))
				 timer-list)))
	(if timers
		(let ((lines
			   (mapcar
				(lambda (timer)
				  (let* ((next-run (timer--time timer))
						 (next-run-str (format-time-string "%Y-%m-%d %H:%M:%S" next-run)))
					(format "Timer next runs at: %s" next-run-str)))
				timers)))
		  (message "org-alert-check timers:\n%s" (string-join lines "\n")))
	  (message "No org-alert-check timers found."))))

;; ------------------------------- Sqlite Tracing ------------------------------

(defvar cj/sqlite-tracing-enabled nil)
(defvar cj/sqlite--db-origins (make-hash-table :test 'eq :weakness 'key))

(defun cj/capture-backtrace ()
  "Capture and return the current stack trace as a list of function names.
Returns a list containing function names from the backtrace, or a fallback
message if backtrace capture fails or is unavailable."
  (condition-case nil
	  (if (fboundp 'backtrace-frames)
		  (mapcar (lambda (fr) (car fr)) (backtrace-frames))
		(list "no-backtrace-frames"))
	(error (list "failed-to-capture-backtrace"))))

(defun cj/take (n xs)
  "Return the first N elements from list XS.
If XS has fewer than N elements, return all elements."
  (cl-subseq xs 0 (min n (length xs))))

(defun cj--ad-sqlite-open (orig file &rest opts)
  "Advice function wrapping \='sqlite-open\=' to track database origins.
ORIG is the original function, FILE is the database file path, and OPTS are
additional options. Records database handle with metadata (file, time, location,
and backtrace) in \='cj/sqlite--db-origins\=' for debugging purposes."
  (let ((db (apply orig file opts)))
	(puthash db
			 (list :file file
				   :opts opts
				   :where (or load-file-name buffer-file-name)
				   :time (current-time-string)
				   :stack (cj/capture-backtrace))
			 cj/sqlite--db-origins)
	db))

(defun cj--ad-sqlite-close (orig db &rest args)
  "Advice function wrapping \='sqlite-close\=' to log database closure.
ORIG is the original function, DB is the database handle, and ARGS are
additional arguments. Logs information about when and where the database was
originally opened before closing it."
  (let ((info (gethash db cj/sqlite--db-origins)))
	(when info
	  (message "cj/sqlite: closing %s opened at %s by %s"
			   (plist-get info :file)
			   (plist-get info :time)
			   (or (plist-get info :where) "unknown"))))
  (apply orig db args))

(defun cj--ad-set-finalizer (orig obj fn)
  "Advice function wrapping \='set-finalizer\=' to debug finalizer failures.
ORIG is the original function, OBJ is the object to finalize, and FN is the
finalizer function. Wraps the finalizer to capture and log detailed diagnostic
information (creation time, location, call stack, and SQLite database info if
applicable) when finalizers fail, then re-signals the error."
  (let* ((origin (list :time (current-time-string)
					   :where (or load-file-name buffer-file-name)
					   :stack (cj/capture-backtrace)
					   :sqlite-open (when (and (fboundp 'sqlitep)
											   (ignore-errors (sqlitep obj)))
									  (gethash obj cj/sqlite--db-origins))))
		 (wrapped
		  (lambda (&rest args)
			(condition-case err
				(apply fn args)
			  (error
			   (let* ((stack (cj/take 8 (plist-get origin :stack)))
					  (dbi   (plist-get origin :sqlite-open))
					  (extra (if dbi
								 (format " db=%s opened at %s by %s"
										 (plist-get dbi :file)
										 (plist-get dbi :time)
										 (or (plist-get dbi :where) "unknown"))
							   "")))
				 (message "cj/finalizer: failed; created at %s (%s); callers=%S;%s; error=%S"
						  (plist-get origin :time)
						  (or (plist-get origin :where) "unknown")
						  stack extra err))
			   ;; Re-signal so Emacs still shows the standard finalizer message.
			   (signal (car err) (cdr err)))))))
	(funcall orig obj wrapped)))

(defun cj/sqlite-tracing-enable ()
  "Enable tracing of sqlite opens/closes and annotate failing finalizers."
  (interactive)
  (unless cj/sqlite-tracing-enabled
	(setq cj/sqlite-tracing-enabled t)
	(advice-add 'set-finalizer :around #'cj--ad-set-finalizer)
	(when (fboundp 'sqlite-open)
	  (advice-add 'sqlite-open :around #'cj--ad-sqlite-open)
	  (advice-add 'sqlite-close :around #'cj--ad-sqlite-close))
	(message "cj/sqlite tracing enabled")))

(defun cj/sqlite-tracing-disable ()
  "Disable sqlite/finalizer tracing and clear recorded origins."
  (interactive)
  (setq cj/sqlite-tracing-enabled nil)
  (ignore-errors (advice-remove 'set-finalizer #'cj--ad-set-finalizer))
  (when (fboundp 'sqlite-open)
	(ignore-errors (advice-remove 'sqlite-open #'cj--ad-sqlite-open))
	(ignore-errors (advice-remove 'sqlite-close #'cj--ad-sqlite-close)))
  (clrhash cj/sqlite--db-origins)
  (message "cj/sqlite tracing disabled"))

(cj/sqlite-tracing-enable)
(setq debug-on-message (rx bos "finalizer failed"))

(provide 'config-utilities)
;;; config-utilities.el ends here
