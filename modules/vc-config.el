;;; vc-config.el --- Version Control Configuration -*- lexical-binding: t; -*-
;; author: Craig Jennings <c@cjennings.net>
;;; Commentary:
;; C-x g is my general entry to Magit's version control via the status page.

;; Navigating changes in file happens via git gutter
;; - C-v d will allow jumping to changes
;; - C-v n and p will bring to next/previous changes

;; Reviewing previous versions happens through git timemahine
;; - C-v t allows viewing this file by selecting a previous commit
;; - Once in timemachine, n and p will take you to previous/next commits
;; - To exit timemachine, press q in the read-only timemachine buffer

;;; Code:

;; ---------------------------- Magit Configuration ----------------------------

(use-package magit
  :defer .5
  :bind ("C-x g" . magit-status)
  :hook
  (magit-log-mode . display-line-numbers-mode)
  :custom
  (magit-define-global-key-bindings 'default)
  :config
  (setq magit-bury-buffer-function 'magit-restore-window-configuration)
  (setq git-commit-major-mode 'org-mode) ;; edit commit messages in org-mode
  (setq magit-display-buffer-function
		'magit-display-buffer-fullframe-status-topleft-v1)

  ;; CLONING
  (setq magit-clone-default-directory code-dir)  ;; cloned repositories go here by default
  (setq magit-clone-set-remote-head t)           ;; do as git does for remote heads
  (setq magit-clone-set-remote.pushDefault 'ask) ;; ask if origin is default
  ) ;; end use-package magit

;; --------------------------------- Git Gutter --------------------------------
;; mark changed lines since last commit in the margin

(use-package git-gutter
  :defer .5
  :hook (prog-mode . git-gutter-mode)
  :custom
  (git-gutter:modified-sign "~")
  (git-gutter:added-sign "+")
  (git-gutter:deleted-sign "-")
  (git-gutter:update-interval 0.05))

;; ------------------------------ Git Timemachine ------------------------------
;; walk through revisions of the current file in your buffer
;; also: https://blog.binchen.org/posts/new-git-timemachine-ui-based-on-ivy-mode/

(defun cj/git-timemachine-show-selected-revision ()
  "Show last (current) revision of file."
  (interactive)
  (let* ((collection
		  (mapcar (lambda (rev)
					;; First, let's debug the structure of a revision
					(message "Revision structure: %S" rev)
					;; re-shape list for the ivy-read
					(cons (concat (substring-no-properties (nth 0 rev) 0 7)
								 " | "
								 ;; Use the date-time string directly if available
								 (or (nth 3 rev) "No date")
								 " | "
								 (nth 5 rev)
								 " | "
								 (nth 6 rev))
						  rev))
				  (git-timemachine--revisions))))
	(ivy-read "commits:"
			  collection
			  :action (lambda (rev)
						;; compatible with ivy 9+ and ivy 8
						(unless (string-match-p "^[a-z0-9]*$" (car rev))
						  (setq rev (cdr rev)))
						(git-timemachine-show-revision rev))
			  :sort nil
			  :preselect 0)))

;; (defun cj/git-timemachine-show-selected-revision ()
;;   "Show last (current) revision of file."
;;   (interactive)
;;   (let* ((collection
;; 		  (mapcar (lambda (rev)
;; 					;; re-shape list for the ivy-read
;; 					(cons (concat (substring-no-properties (nth 0 rev) 0 7) " | " (nth 5 rev) " | " (nth 6 rev)) rev))
;; 				  (git-timemachine--revisions))))
;; 	(ivy-read "commits:"
;; 			  collection
;; 			  :action (lambda (rev)
;; 						;; compatible with ivy 9+ and ivy 8
;; 						(unless (string-match-p "^[a-z0-9]*$" (car rev))
;; 						  (setq rev (cdr rev)))
;; 						(git-timemachine-show-revision rev))
;; 			  :sort nil  ;; Disable sorting
;; 			  :preselect 0)))  ;; Preselect the most recent commit


(defun cj/git-timemachine ()
  "Open git snapshot with the selected version. Based on ivy-mode."
  (interactive)
  (unless (featurep 'git-timemachine)
	(require 'git-timemachine))
  (git-timemachine--start #'cj/git-timemachine-show-selected-revision))

(use-package git-timemachine
  :defer .5)

;; --------------------------------- VC Keymap ---------------------------------
;; version control keymap

(global-unset-key (kbd "C-v"))
(defvar cj/vc-keymap
  (let ((map (make-sparse-keymap)))
	(define-key map "t" 'cj/git-timemachine)
	(define-key map "d" 'cj/goto-git-gutter-diff-hunks)
	(define-key map "n" 'git-gutter:next-hunk)
	(define-key map "p" 'git-gutter:previous-hunk)
	map)
  "My version control key map.")
(global-set-key (kbd "C-v") cj/vc-keymap)


(provide 'vc-config)
;;; vc-config.el ends here.
