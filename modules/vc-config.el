;;; vc-config.el --- Version Control Configuration -*- lexical-binding: t; coding: utf-8; -*-
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
  :defer t
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
  :defer t
  :hook (prog-mode . git-gutter-mode)
  :custom
  (git-gutter:modified-sign "~")
  (git-gutter:added-sign "+")
  (git-gutter:deleted-sign "-")
  (git-gutter:update-interval 0.05))

;; ------------------------------ Git Timemachine ------------------------------

(defun cj/git-timemachine-show-selected-revision ()
  "Displays git revisions of file in chronological order adding metadata."
  (interactive)
  (let* ((revisions (git-timemachine--revisions))
		 (candidates (mapcar
					  (lambda (rev)
						(concat (substring-no-properties (nth 0 rev) 0 7)
								" | "
								(or (nth 3 rev) "No date")
								" | "
								(nth 5 rev)
								" | "
								(nth 6 rev)))
					  revisions))
		 ;; Create completion table with metadata to prevent sorting
		 (completion-table
		  (lambda (string pred action)
			(if (eq action 'metadata)
				;; Tell vertico not to sort these candidates
				'(metadata (display-sort-function . identity)
						   (cycle-sort-function . identity))
			  (complete-with-action action candidates string pred)))))
	(let* ((selected (completing-read "Select revision: " completion-table nil t))
		   (index (cl-position selected candidates :test #'string=)))
	  (when index
		(git-timemachine-show-revision (nth index revisions))))))

(defun cj/git-timemachine ()
  "Open git snapshot with the selected version."
  (interactive)
  (unless (featurep 'git-timemachine)
    (require 'git-timemachine))
  (git-timemachine--start #'cj/git-timemachine-show-selected-revision))

(use-package git-timemachine
  :defer t)

;; --------------------------------- VC Keymap ---------------------------------

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
