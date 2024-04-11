;;; calibredb-epub-config --- Functionality for Ebook Management and Display -*- lexical-binding: t; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:

;; Note: Calibre virtual library functionality works as designed, but not as I
;; want. I had hoped to simply view a virtual library defined by it's tag.
;; Instead, it searches for the library keywords within the description as well,
;; turning up many books that aren't remotely related. I've overwritten the
;; virtual-library functionality to simply filter by tag, and given that the
;; "l" keybinding in the calibredb-search-mode-map.

;;; Code:

;; -------------------------- CalibreDB Ebook Manager --------------------------

(use-package calibredb
  :defer 1
  :commands calibredb
  :bind
  ("M-B" . calibredb)
  ;; override virtual libraries to filter-by-tag
  (:map calibredb-search-mode-map
		("l" . calibredb-filter-by-tag))
  :config
  ;; basic config
  (setq calibredb-root-dir "~/sync/books/")
  (setq calibredb-library-alist '(("~/sync/books/")))
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-program "/usr/bin/calibredb")
  (setq calibredb-preferred-format "epub")

  ;; search window display
  (setq calibredb-size-show nil)
  (setq calibredb-order "asc")
  (setq calibredb-id-width 7))

;; ------------------------------ Nov Epub Reader ------------------------------

(use-package nov
  :defer .5
  :after visual-fill-column
  :mode ("\\.epub\\'" . nov-mode)
;;  :hook (nov-mode . cj/nov-apply-preferences)
  :bind
  (:map nov-mode-map
        ("m" . cj/bookmark-set-and-save)
        ("b" . bookmark-bmenu-list)
        ("r" . nov-render-document)
        ("l" . recenter-top-bottom)
		("d" . sdcv-search-input)
		("." . cj/forward-paragraph-and-center)
        ("<" . nov-history-back)
        (">" . nov-history-forward)
        ("," . backward-paragraph)
        ("z" . (lambda () (interactive) (cj/open-file-with-command "zathura")))
        ("e" . (lambda () (interactive) (cj/open-file-with-command "evince")))
		("t" . nov-goto-toc)))

(defun cj/forward-paragraph-and-center()
  "Forward one paragraph and center the page."
  (interactive)
  (forward-paragraph)
  (recenter))

(defun cj/nov-apply-preferences ()
  "Apply preferences after nove-mode has launched.
Meant to be called via the nov-mode hook to apply font and display preferences
 when displaying epub files."
  (interactive)
  (face-remap-add-relative 'variable-pitch :height 180)         ;; increase the size for both variable...
  (face-remap-add-relative 'fixed-pitch :height 180)            ;; ...and fixed-pitch fonts for readability
  (setq nov-text-width 115)                                     ;; narrow text width
  (when (require 'visual-fill-column nil t)                     ;; if visual-fill-column isn't already loaded, do it now.
    (setq-local visual-fill-column-center-text t                ;; center the text
                visual-fill-column-width (+ nov-text-width 10)) ;; helps avoid truncation of long word
    (hl-line-mode)
    (visual-fill-column-mode 1)                                 ;; wrap lines according to fill-column
    (nov-render-document)))                                     ;; re-render the epub

(defun cj/nov-center-images ()
  "Center the images in an nov document.
To be called immediately after nov renders the html via
the nov-post-html-render-hook."
  (let* ((pixel-buffer-width (shr-pixel-buffer-width))
         match)
    (save-excursion
      (goto-char (point-min))
      (while (setq match (text-property-search-forward
                          'display nil
                          (lambda (_ p) (eq (car-safe p) 'image))))
        (when-let ((size (car (image-size
                               (prop-match-value match) 'pixels)))
                   ((> size 150))
                   (center-pixel (floor (- pixel-buffer-width size) 2))
                   (center-pos (floor center-pixel (frame-char-width))))
          (beginning-of-line)
          (indent-to center-pos)
          (end-of-line))))))
(add-hook 'nov-post-html-render-hook 'cj/nov-center-images)

(provide 'calibredb-epub-config)
;;; calibredb-epub-config.el ends here
