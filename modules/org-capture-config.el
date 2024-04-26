;;; org-capture-config.el --- Org Capture Configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;; Customizations related to org-capture and org-refile is here.
;; This includes 'cj/org-webpage-clipper' functionality.

;; To ensure the code below is only loaded after org-mode, all code is wrapped in an
;; eval-after-load function.


;;; Code:


;; ---------------------------- Org Webpage Clipper ----------------------------
;; Allows saving a copy of the page eww is visiting for offline reading.
;; In other words, it's a "Pocket/Instapaper" that keeps the article in Emacs.
;; Used in conjuction with org-capture-template "w" "Web Page Clipper" below.


(defun cj/org-webpage-clipper ()
  "Capture a web page for later viewing in an org-file.
  and Returns the yanked content as a string."
  (interactive)
  (let* ((source-buffer (org-capture-get :original-buffer))
         (source-mode (with-current-buffer source-buffer major-mode)))
    (cond
     ((eq source-mode 'w3m-mode)
      (with-current-buffer source-buffer
        (org-w3m-copy-for-org-mode)))
     ((eq source-mode 'eww-mode)
      (with-current-buffer source-buffer
        (org-eww-copy-for-org-mode)))
     (t
      (error "Not valid -- must be in w3m or eww mode")))
    ;; extract the webpage content from the kill ring
    (car kill-ring)))


;; ----------------------- Org Capture PDF Active Region -----------------------
;; allows capturing the selected region from within a PDF file.

(defun cj/org-capture-pdf-active-region ()
  "Capture the active region of the pdf-view buffer.
Intended to be called within an org capture template."
  (let* ((pdf-buf-name (plist-get org-capture-plist :original-buffer))
         (pdf-buf (get-buffer pdf-buf-name)))
    (if (buffer-live-p pdf-buf)
        (with-current-buffer pdf-buf
          (car (pdf-view-active-region-text)))
      (user-error "Buffer %S not alive" pdf-buf-name))))

;; --------------------------- Org-Capture Templates ---------------------------
;; you can bring up the org capture menu with C-c c

(use-package org-protocol
  :ensure nil ;; built-in
  :defer .5
  :after org
  :config
  ;; ORG-CAPTURE TEMPLATES
  (setq org-protocol-default-template-key "L")
  (setq org-capture-templates
        '(("t" "Task" entry (file+headline inbox-file "Inbox")
           "* TODO %?" :prepend t)

          ("e" "Event" entry (file+headline schedule-file "Scheduled Events")
           "* %?\nWHEN: %^t" :prepend t)

          ("E" "Epub Text" entry (file+headline inbox-file "Inbox")
           "* %?
#+BEGIN_QUOTE\n %i\n#+END_QUOTE
Source: [[%:link][%(buffer-name (org-capture-get :original-buffer))]]
Captured On: %U" :prepend t)

          ;; requires cj/org-capture-pdf-active-region function defined above
          ("P" "PDF Text" entry (file+headline inbox-file "Inbox")
           "* %?
#+BEGIN_QUOTE\n%(cj/org-capture-pdf-active-region)\n#+END_QUOTE
Source:[[%L][%(buffer-name (org-capture-get :original-buffer))]]
Captured On: %U" :prepend t)

          ("p" "Link with Selection" entry (file+headline inbox-file "Inbox")
           "* %?%:description
#+BEGIN_QUOTE\n%i\n#+END_QUOTE
[[%:link][%:description]]
Captured On: %U\n" :prepend t)

          ("L" "Link" entry (file+headline inbox-file "Inbox")
           "* %?%:description
[[%:link][%:description]]\nCaptured On: %U" :prepend t :immediate-finish t)

          ("m" "Mu4e Email" entry (file+headline inbox-file "Inbox")
           "* TODO %?
%(if (string= \"%i\" \"\") \"\" \"\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\")
[[%:link][%:description]]
Captured On: %U"
           :prepend t)

          ("g" "Grocery List Item" item
           (file+headline inbox-file "Grocery List") "%?")

          ("s" "Shopping List Entry" entry
           (file+headline inbox-file "Shopping List") "* %?")

          ;; requires cj/org-web-clipper function defined above
          ("w" "Web Page Clipper" entry
           (file+headline inbox-file "To Read")
           "* %a\nURL: %L\nCaptured On:%U\n%(cj/org-webpage-clipper)\n"
           :prepend t :immediate-finish t)
          )) ;; end setq
  ) ;; end use-package org-protocol

;; ---------------------------- Simple Task Capture ----------------------------
;; the simplest way to capture a task. Also a simple way to write this function.

(define-key global-map (kbd "C-S-t") (kbd "C-c c t"))

(provide 'org-capture-config)
;;; org-capture-config.el ends here.
