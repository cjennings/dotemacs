;;; lorem-generator.el --- Fake Latin Text Generator -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings
;; Version: 0.5
;; Package-Requires: ((emacs "26.1") (cl-lib "0.6"))
;; Keywords: text, lorem-ipsum, dummy, filler, markov
;; URL: https://github.com/yourname/cj-lipsum

;;; Commentary:
;; Generate pseudo-Latin placeholder text using a simple order-two
;; Markov chain.  You can train the chain on region, buffer, or file.
;; By default, it learns from a bundled Latin wordlist, which you can
;; change via customization.
;;
;; Interactive commands:
;;   M-x cj/lipsum             – Return N words
;;   M-x cj/lipsum-insert      – Insert N words at point
;;   M-x cj/lipsum-title       – Generate a pseudo-Latin heading
;;   M-x cj/lipsum-paragraphs  – Insert multiple paragraphs
;;   M-x cj/lipsum-learn-*     – Train the chain
;;   M-x cj/lipsum-reset       – Clear chain

;;; Code:

(require 'cl-lib)

(cl-defstruct (cj/markov-chain
			   (:constructor cj/markov-chain-create))
  "An order-two Markov chain."
  (map (make-hash-table :test 'equal))
  (keys nil))

(defun cj/markov-tokenize (text)
  "Split TEXT into tokens: words and punctuation separately."
  (let ((case-fold-search nil))
	(split-string text "\\b" t "[[:space:]\n]+")))

(defun cj/markov-learn (chain text)
  "Add TEXT into the Markov CHAIN with tokenized input."
  (let* ((words (cj/markov-tokenize text))
		 (len (length words)))
	(cl-loop for i from 0 to (- len 3)
			 for a = (nth i words)
			 for b = (nth (1+ i) words)
			 for c = (nth (+ i 2) words)
			 do (let* ((bigram (list a b))
					   (nexts (gethash bigram (cj/markov-chain-map chain))))
				  (puthash bigram (cons c nexts)
						   (cj/markov-chain-map chain)))))
	(setf (cj/markov-chain-keys chain)
		  (cl-loop for k being the hash-keys of (cj/markov-chain-map chain)
				   collect k)))

(defun cj/markov-fix-capitalization (sentence)
  "Capitalize the first word and the first word after .!? in SENTENCE."
  (let* ((tokens (split-string sentence "\\b" t)))
	(cl-loop with capitalize-next = t
			 for i from 0 below (length tokens)
			 for tok = (nth i tokens)
			 do (when (and capitalize-next (string-match-p "^[[:alpha:]]" tok))
				  (setf (nth i tokens)
						(concat (upcase (substring tok 0 1))
								(substring tok 1)))
				  (setq capitalize-next nil))
			 do (when (string-match-p "[.!?]" tok)  ; <-- Changed: removed $ anchor
				  (setq capitalize-next t)))
	(mapconcat #'identity tokens "")))

(defun cj/markov-join-tokens (tokens)
  "Join TOKENS into a sentence with proper spacing/punctuation."
  (let ((sentence "") (need-space nil))
	(dolist (tok tokens)
	  (cond
	   ;; punctuation attaches directly
	   ((string-match-p "^[[:punct:]]+$" tok)
		(setq sentence (concat sentence tok))
		(setq need-space t))
	   ;; word
	   (t
		(when need-space
		  (setq sentence (concat sentence " ")))
		(setq sentence (concat sentence tok))
		(setq need-space t))))
	;; fix capitalization of first word only
	(when (string-match "\\`\\([[:alpha:]]\\)" sentence)
	  (setq sentence
			(replace-match (upcase (match-string 1 sentence))
						   nil nil sentence)))
	;; ensure it ends with .!?
	(unless (string-match-p "[.!?]$" sentence)
	  (setq sentence (concat (replace-regexp-in-string "[[:punct:]]+$" "" sentence) ".")))
  (setq sentence (cj/markov-fix-capitalization sentence))
		sentence))

(defun cj/markov-generate (chain n &optional start)
  "Generate a sentence of N tokens from CHAIN."
  (when (cj/markov-chain-keys chain)
	(let* ((state (or (and start
						   (gethash start (cj/markov-chain-map chain))
						   start)
					  (cj/markov-random-key chain)))
		   (w1 (car state))
		   (w2 (cadr state))
		   (tokens (list w1 w2)))
	  (dotimes (_ (- n 2))
		(let ((next (cj/markov-next-word chain state)))
		  (if next
			  (setq tokens (append tokens (list next))
					state (list w2 next)
					w1 w2
					w2 next)
			(setq state (cj/markov-random-key chain)
				  w1 (car state)
				  w2 (cadr state)
				  tokens (append tokens (list w1 w2))))))
	  (cj/markov-join-tokens tokens))))

(defun cj/markov-random-key (chain)
  (nth (random (length (cj/markov-chain-keys chain)))
	   (cj/markov-chain-keys chain)))

(defun cj/markov-next-word (chain bigram)
  (let ((candidates (gethash bigram (cj/markov-chain-map chain))))
	(when candidates
	  (nth (random (length candidates)) candidates))))

;;;###autoload
(defvar cj/lipsum-chain (cj/markov-chain-create)
  "Global Markov chain for lipsum generation.")

;;;###autoload
(defun cj/lipsum-reset ()
  "Reset the global lipsum Markov chain."
  (interactive)
  (setq cj/lipsum-chain (cj/markov-chain-create))
  (message "cj/lipsum-chain reset."))

;;;###autoload
(defun cj/lipsum-learn-region (beg end)
  "Learn text from region."
  (interactive "r")
  (cj/markov-learn cj/lipsum-chain (buffer-substring-no-properties beg end))
  (message "Learned from region."))

;;;###autoload
(defun cj/lipsum-learn-buffer ()
  "Learn from entire buffer."
  (interactive)
  (cj/markov-learn cj/lipsum-chain
				   (buffer-substring-no-properties (point-min) (point-max)))
  (message "Learned from buffer."))

;;;###autoload
(defun cj/lipsum-learn-file (file)
  "Learn from FILE containing plain text."
  (interactive "fTrain from file: ")
  (with-temp-buffer
	(insert-file-contents file)
	(cj/markov-learn cj/lipsum-chain (buffer-string)))
  (message "Learned from file: %s" file))

;;;###autoload
(defun cj/lipsum (n)
  "Return N words of lorem ipsum."
  (cj/markov-generate cj/lipsum-chain n '("Lorem" "ipsum")))

;;;###autoload
(defun cj/lipsum-insert (n)
  "Insert N words of lorem ipsum at point."
  (interactive "nNumber of words: ")
  (insert (cj/lipsum n)))

;;; Title generation

(defconst cj/lipsum-title-min 3)
(defconst cj/lipsum-title-max 8)
(defconst cj/lipsum-title-small 3)

;;;###autoload
(defun cj/lipsum-title ()
  "Generate a pseudo-Latin title."
  (interactive)
  (let* ((n (+ cj/lipsum-title-min
			   (random (1+ (- cj/lipsum-title-max cj/lipsum-title-min)))))
		 (words
		  (cl-loop with state = (cj/markov-random-key cj/lipsum-chain)
				   for i from 0 below n
				   for w = (car state)
				   do (setq state (list (cadr state)
										(or (cj/markov-next-word cj/lipsum-chain state)
											(cadr (cj/markov-random-key cj/lipsum-chain))))))
				   collect (replace-regexp-in-string "^[[:punct:]]+\\|[[:punct:]]+$" "" w))))
	(setq words (cl-remove-if #'string-empty-p words))
	(mapconcat
	 (lambda (word idx)
	   (if (or (zerop idx) (> (length word) cj/lipsum-title-small))
		   (capitalize word)
		 word))
	 words " "))

;;; Paragraphs

;;;###autoload
(defun cj/lipsum-paragraphs (count &optional min max)
  "Insert COUNT paragraphs of lipsum.

Each paragraph has a random length between MIN and MAX words.
Defaults: MIN=30, MAX=80."
  (interactive "nNumber of paragraphs: ")
  (let ((min (or min 30))
        (max (or max 80)))
    (dotimes (_ count)
      (let ((len (+ min (random (1+ (- max min))))))
		(insert (cj/lipsum len) "\n\n")))))

;;; Customization

(defgroup cj-lipsum nil
  "Pseudo-Latin lorem ipsum text generator."
  :prefix "cj/lipsum-"
  :group 'text)

(defcustom cj/lipsum-default-file
  (expand-file-name "latin.txt"
					(file-name-directory (or load-file-name buffer-file-name)))
  "Default training file for cj-lipsum.

This should be a plain UTF-8 text file with hundreds of Latin words
or sentences.  By default it points to the bundled `latin.txt`."
  :type 'file
  :group 'cj-lipsum)

;;; Initialization: train on default file
(defun cj/lipsum--init ()
  "Initialize cj-lipsum by learning from `cj/lipsum-default-file`."
  (when (and cj/lipsum-default-file
			 (file-readable-p cj/lipsum-default-file))
	(cj/lipsum-reset)
	(cj/lipsum-learn-file cj/lipsum-default-file)))

(cj/lipsum--init)

(provide 'lorem-generator)
;;; lorem-generator.el ends here.
