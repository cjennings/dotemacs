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
  :defer 0.5
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

(defun cj/git-timemachine ()
  "Open git snapshot with the selected version."
  (interactive)
  (unless (featurep 'git-timemachine)
	(require 'git-timemachine))
  (git-timemachine--start #'cj/git-timemachine-show-selected-revision))

(use-package git-timemachine
  :commands (git-timemachine
			 git-timemachine-show-revision
			 git-timemachine-show-selected-revision)
  :init
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

  :bind (:map cj/vc-map
			  ("t" . cj/git-timemachine)))

;; -------------------------------- Magit Forge --------------------------------
;; GitHub/GitLab/etc integration for Magit

(use-package forge
  :commands (forge-pull forge-list-notifications forge-create-issue forge-create-pullreq)
  :init
  ;; Set up forge database location
  (setq forge-database-file
		(expand-file-name "forge-database.sqlite" user-emacs-directory))
  ;; Auto-load forge when visiting magit-status in a forge-enabled repo
  (with-eval-after-load 'magit
    (define-key magit-mode-map (kbd "N") 'forge-pull))

  :config
  (setq forge-pull-notifications nil)       ;; Don't pull notifications by default
  (setq forge-topic-list-limit '(60 . 10))) ;; Show 60 open and 10 closed items

(defun cj/forge-create-issue ()
  "Create a new issue in the current repository."
  (interactive)
  (if (forge-current-repository)
	  (forge-create-issue)
	(user-error "Not in a forge repository")))

(defun cj/goto-git-gutter-diff-hunks ()
  "Jump to git-gutter diff hunks using consult.
Searches for lines starting with + or - (diff markers) and allows
interactive selection to jump to any changed line in the buffer."
  (interactive)
  (require 'git-gutter)
  (consult-line "^[+\\-]"))

;; -------------------------------- Difftastic ---------------------------------
;; Structural diffs for better git change visualization

(use-package difftastic
  :demand t
  :after magit
  :bind (:map magit-blame-read-only-mode-map
              ("D" . difftastic-magit-show)
              ("S" . difftastic-magit-show))
  :config
  (eval-after-load 'magit-diff
    '(transient-append-suffix 'magit-diff '(-1 -1)
       [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
        ("S" "Difftastic show" difftastic-magit-show)])))

;; --------------------------------- VC Keymap ---------------------------------

;; Ordering & sorting prefix and keymap
(defvar-keymap cj/vc-map
  :doc "Keymap for version control operations"
  "d" #'cj/goto-git-gutter-diff-hunks
  "c" #'cj/forge-create-issue
  "f" #'forge-pull
  "i" #'forge-list-issues
  "n" #'git-gutter:next-hunk
  "p" #'git-gutter:previous-hunk
  "r" #'forge-list-pullreqs
  "t" #'cj/git-timemachine)

(keymap-set cj/custom-keymap "v" cj/vc-map)
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-; v" "version control menu"
    "C-; v d" "goto diff hunks"
    "C-; v c" "create issue"
    "C-; v f" "forge pull"
    "C-; v i" "list issues"
    "C-; v n" "next hunk"
    "C-; v p" "previous hunk"
    "C-; v r" "list pull requests"
    "C-; v t" "git timemachine"))

(provide 'vc-config)
;;; vc-config.el ends here.
