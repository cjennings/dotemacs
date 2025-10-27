;;; config-utilities  --- Config Hacking Utilities -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Development and debugging utilities for Emacs configuration maintenance.
;;
;;; Code:

(require 'cl-lib)
(require 'find-lisp)
(require 'profiler)

;;; -------------------------------- Debug Keymap -------------------------------

(defvar-keymap cj/debug-config-keymap
  :doc "config debugging utilities keymap.")
(keymap-global-set "C-c d" cj/debug-config-keymap)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c d" "config debugging utils"
    "C-c d p" "profiler menu"
    "C-c d p s" "start profiler"
    "C-c d p h" "stop profiler"
    "C-c d p r" "profiler report"
    "C-c d t" "toggle debug-on-error"
    "C-c d b" "benchmark method"
    "C-c d c" "compilation menu"
    "C-c d c h" "compile home"
    "C-c d c d" "delete compiled"
    "C-c d c ." "compile buffer"
    "C-c d i" "info menu"
    "C-c d i b" "info build"
    "C-c d i p" "info packages"
    "C-c d i f" "info features"
    "C-c d r" "reload init"
    "C-c d a" "reset auth cache"))

;;; --------------------------------- Profiling ---------------------------------

(keymap-set cj/debug-config-keymap "p s" #'profiler-start)
(keymap-set cj/debug-config-keymap "p h" #'profiler-stop)
(keymap-set cj/debug-config-keymap "p r" #'profiler-report)

;;; --------------------------- Toggle Debug On Error ---------------------------

(keymap-set cj/debug-config-keymap "t" #'toggle-debug-on-error)

;;; -------------------------------- Benchmarking -------------------------------

(defmacro with-timer (title &rest forms)
  "Run the given FORMS, counting the elapsed time.
A message including the given TITLE and the corresponding elapsed
time is displayed."
  (declare (indent 1))
  (let ((nowvar (make-symbol "now"))
        (body   `(progn ,@forms)))
    `(let ((,nowvar (current-time)))
       (message "%s..." ,title)
       (prog1 ,body
         (let ((elapsed
                (float-time (time-subtract (current-time) ,nowvar))))
           (message "%s... done (%.3fs)" ,title elapsed))))))

(defun cj/benchmark-this-method ()
  "Prompt for a title and method name, then time the execution of the method."
  (interactive)
  (let ((title (read-string "Enter the title for the timing: "))
        (method-name (completing-read "Enter the method name to time: " obarray
                                      #'fboundp t)))
    (let ((method-symbol (intern-soft method-name)))
      (if (and method-symbol (fboundp method-symbol))
          (with-timer title
            (funcall method-symbol))
        (message "Invalid method name: %s" method-name)))))
(keymap-set cj/debug-config-keymap "b" #'cj/benchmark-this-method)

;;; ----------------------------- Config Compilation ----------------------------

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

(keymap-set cj/debug-config-keymap "c h" 'cj/recompile-emacs-home)

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
(keymap-set cj/debug-config-keymap "c d" 'cj/delete-emacs-home-compiled-files)

(defun cj/compile-this-elisp-buffer ()
  "Compile the current .el: prefer native (.eln), else .elc. Message if neither."
  (interactive)
  (unless (and buffer-file-name (string-match-p "\\.el\\'" buffer-file-name))
    (user-error "Not visiting a .el file"))
  (save-buffer)
  (let ((file buffer-file-name))
    (cond
     ;; Native compilation (async preferred)
     ((fboundp 'native-compile-async)
      (native-compile-async file)
      (message "Queued native compilation for %s" file))
     ;; Native compilation (sync, if async not available)
     ((fboundp 'native-compile)
      (condition-case err
          (progn
            (native-compile file)
            (message "Native-compiled %s" file))
        (error (message "Native compile failed: %s" (error-message-string err)))))
     ;; Byte-compile fallback
     ((fboundp 'byte-compile-file)
      (let ((out (byte-compile-file file)))
        (if out
            (message "Byte-compiled -> %s" out)
          (message "Byte-compilation failed for %s" file))))
     ;; Neither facility available
     (t
      (message "No compilation available (no native-compile, no byte-compile)")))))
(keymap-set cj/debug-config-keymap "c ." 'cj/compile-this-elisp-buffer)

;; --------------------------- Information Reporting ---------------------------

(defun cj/emacs-build--format-build-time (tval)
  "Return a human-readable build time from TVAL."
  (cond
   ((null tval) "unknown")
   ((stringp tval) tval)
   ((and (consp tval) (integerp (car tval)))
    (format-time-string "%Y-%m-%d %H:%M:%S %Z" tval))
   ((numberp tval)
    (format-time-string "%Y-%m-%d %H:%M:%S %Z" (seconds-to-time tval)))
   (t (format "%s" tval))))

(defun cj/emacs-build--summary-string ()
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
     (format "Location: %s\n"  (executable-find "emacs"))
     (format "Build date: %s\n" (cj/emacs-build--format-build-time build-time))
     (when build-system
       (format "Build system: %s\n" build-system))
     (when branch
       (format "Git branch: %s\n" (or branch "n/a")))
     (when commit
       (format "Git commit: %s\n" (or commit "n/a")))
     "\nCapabilities:\n"
     (format "- Native compilation: %s\n"
             (if (and (fboundp 'native-comp-available-p)
                      (native-comp-available-p))
                 "yes" "no"))
     (format "- Dynamic modules: %s\n"
             (if (and (boundp 'module-file-suffix)
                      module-file-suffix)
                 "yes" "no"))
     (format "- GnuTLS: %s\n"
             (if (and (fboundp 'gnutls-available-p)
                      (gnutls-available-p))
                 "yes" "no"))
     (format "- libxml2: %s\n"
             (if (fboundp 'libxml-parse-html-region)
                 "yes" "no"))
     (format "- ImageMagick: %s\n"
             (if (and (fboundp 'image-type-available-p)
                      (image-type-available-p 'imagemagick))
                 "yes" "no" ))
     (format "- SQLite: %s\n"
             (if (and (fboundp 'sqlite-available-p)
                      (sqlite-available-p))
                 "yes" "no"))
     (when features
       (format "\nConfigured features:\n%s\n" features))
     (when options
       (format "\nConfiguration arguments:\n%s\n" options)))))

(defun cj/info-emacs-build ()
  "Display a buffer with the Emacs build summary."
  (interactive)
  (let ((buf (get-buffer-create "*Emacs-Build-Summary*")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (cj/emacs-build--summary-string))
      (goto-char (point-min))
      (help-mode)
      (setq-local truncate-lines nil))
    (pop-to-buffer buf)))

(keymap-set cj/debug-config-keymap "i b" 'cj/info-emacs-build)

(defvar cj--loaded-file-paths nil
  "All file paths that are loaded.")
(defvar cj--loaded-packages-buffer "*loaded-packages*"
  "Buffer name for data about loaded packages.")
(defvar cj--loaded-features-buffer "*loaded-features*"
  "Buffer name for data about loaded features.")

(defun cj/info-loaded-packages()
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
(keymap-set cj/debug-config-keymap "i p" 'cj/info-loaded-packages)

(defun cj/info-loaded-features()
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
(keymap-set cj/debug-config-keymap "i f" 'cj/info-loaded-features)

;; ------------------------------ Reload Init File -----------------------------

(defun cj/reload-init-file ()
  "Reload the init file.  Useful when modifying Emacs config."
  (interactive)
  (load-file user-init-file))
(keymap-set cj/debug-config-keymap "r" 'cj/reload-init-file)

;; ----------------------------- Reset-Auth-Sources ----------------------------

(defun cj/reset-auth-cache ()
  "Clear Emacs auth-source cache."
  (interactive)
  (auth-source-forget-all-cached)
  (message "Emacs auth-source cache cleared."))
(keymap-set cj/debug-config-keymap "a" 'cj/reset-auth-cache)

;; ------------------------ Validate Org Agenda Entries ------------------------

(defun cj/validate-org-agenda-timestamps ()
  "Scan all files in `org-agenda-files' for invalid timestamps.
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


(provide 'config-utilities)
;;; config-utilities.el ends here
