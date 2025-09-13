;;; dwim-shell-commands.el --- DWIM Shell Commands -*- coding: utf-8; lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; This module provides a collection of DWIM (Do What I Mean) shell commands
;; for common file operations in Dired and other buffers. It leverages the
;; `dwim-shell-command' package to execute shell commands on marked files
;; with smart templating and progress tracking.
;;
;; Features:
;; - Audio/Video conversion (mp3, opus, webp, HEVC)
;; - Image manipulation (resize, flip, format conversion)
;; - PDF operations (merge, split, password protection, OCR)
;; - Archive management (zip/unzip)
;; - Document conversion (epub to org, docx to pdf, pdf to txt)
;; - Git operations (clone from clipboard)
;; - External file opening with context awareness
;;
;; Workflow:
;; 1. *Mark files in Dired/Dirvish*
;;    - Use =m= to mark individual files
;;    - Use =* .= to mark by extension
;;    - Use =% m= to mark by regexp
;;    - Or operate on the file under cursor if nothing is marked
;;
;; 2. *Execute a DWIM command*
;;    - Call the command via =M-x dwim-shell-commands-[command-name]=
;;    - Or bind frequently used commands to keys
;;
;; 3. *Command execution*
;;    - The command runs asynchronously in the background
;;    - A =*Async Shell Command*= buffer shows progress
;;    - Files are processed with smart templating (replacing =<<f>>=, =<<fne>>=, etc.)
;;
;; 4. *Results*
;;    - New files appear in the Dired/Dirvish buffer
;;    - Buffer auto-refreshes when command completes
;;    - Errors appear in the async buffer if something fails
;;
;; Requirements:
;; The commands rely on various external utilities that need to be installed:
;; - ffmpeg: Audio/video conversion
;; - imagemagick (convert): Image manipulation
;; - qpdf: PDF operations
;; - tesseract: OCR functionality
;; - pandoc: Document conversion
;; - atool: Archive extraction
;; - rsvg-convert: SVG to PNG conversion
;; - pdftotext: PDF text extraction
;; - git: Version control operations
;; - gpgconf: GPG agent management
;;
;; On Arch Linux, install the requirements with:
;; #+begin_src bash
;; sudo pacman -S --needed ffmpeg imagemagick qpdf tesseract tesseract-data-eng pandoc atool librsvg poppler git gnupg zip unzip mkvtoolnix-cli mpv ruby
;; #+end_src
;;
;; On MacOS, install the requirements with:
;; #+begin_src bash
;; brew install ffmpeg imagemagick qpdf tesseract pandoc atool librsvg poppler gnupg mkvtoolnix mpv
;; #+end_src
;;
;; Usage:
;; Commands operate on marked files in Dired or the current file in other modes.
;; The package automatically replaces standard shell commands with DWIM versions
;; for a more intuitive experience.
;;
;; Template Variables:
;; - <<f>>: Full path to file
;; - <<fne>>: File name without extension
;; - <<e>>: File extension
;; - <<td>>: Temporary directory
;; - <<cb>>: Clipboard contents
;; - <<*>>: All marked files
;;

;;; Code:

;; -------------------------- Dwim Shell Commands Menu -------------------------

(defun dwim-shell-commands-menu ()
  "Select and execute a dwim-shell-command function with prettified names."
  (interactive)
  (let* ((commands (cl-loop for symbol being the symbols
							when (and (fboundp symbol)
									  (string-prefix-p "dwim-shell-commands-" (symbol-name symbol))
									  (not (eq symbol 'dwim-shell-commands-menu)))
							collect symbol))
		 ;; Create alist of (pretty-name . command-symbol)
		 (command-alist (mapcar (lambda (cmd)
								  (cons (replace-regexp-in-string
										 "^dwim-shell-commands-"
										 ""
										 (replace-regexp-in-string "-" " " (symbol-name cmd)))
										cmd))
								commands))
         (selected (completing-read "DWIM Shell Command: "
									command-alist
									nil
									t
									nil
									'dwim-shell-command-history))
		 (command (alist-get selected command-alist nil nil #'string=)))
	(when command
	  (call-interactively command))))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "M-D") #'dwim-shell-commands-menu))
;; (with-eval-after-load 'dirvish
;;   (define-key dirvish-mode-map (kbd "M-D") #'dwim-shell-commands-menu))

;; ----------------------------- Dwim Shell Command ----------------------------

(use-package dwim-shell-command
  :defer 0.5
  :bind (([remap shell-command] . dwim-shell-command)
		 :map dired-mode-map
		 ([remap dired-do-async-shell-command] . dwim-shell-command)
		 ([remap dired-do-shell-command] . dwim-shell-command)
		 ([remap dired-smart-shell-command] . dwim-shell-command))
  :init
  (defun dwim-shell-commands-audio-to-mp3 ()
	"Convert all marked audio to mp3(s)."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Convert to mp3"
	 "ffmpeg -stats -n -i '<<f>>' -acodec libmp3lame '<<fne>>.mp3'"
	 :utils "ffmpeg"))

  (defun dwim-shell-commands-audio-to-opus ()
	"Convert all marked audio to opus(s)."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Convert to mp3"
	 "ffmpeg -stats -n -i '<<f>>' -c:a libopus -vbr on -compression_level 10 -b:a 256k '<<fne>>.opus'"
	 :utils "ffmpeg"))

  (defun dwim-shell-commands-image-exif-metadata ()
	"View EXIF metadata in image(s)."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "View EXIF"
	 "exiftool '<<f>>'"
	 :utils "exiftool"))

  (defun dwim-shell-commands-image-horizontal-flip ()
	"Horizontally flip image(s)."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Image horizontal flip"
	 "convert -verbose -flop '<<f>>' '<<fne>>_h_flipped.<<e>>'"
	 :utils "convert"))

  (defun dwim-shell-commands-image-vertical-flip ()
	"Horizontally flip image(s)."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Image vertical flip"
	 "convert -verbose -flip '<<f>>' '<<fne>>_v_flipped.<<e>>'"
	 :utils "convert"))

  (defun dwim-shell-commands-image-to-jpg ()
	"Convert all marked images to jpg(s)."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Convert to jpg"
	 "convert -verbose '<<f>>' '<<fne>>.jpg'"
	 :utils "convert"))

  (defun dwim-shell-commands-image-to-png ()
	"Convert all marked images to png(s)."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Convert to png"
	 "convert -verbose '<<f>>' '<<fne>>.png'"
	 :utils "convert"))

  (defun dwim-shell-commands-svg-to-png ()
	"Convert all marked svg(s) to png(s)."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Convert to png"
	 "rsvg-convert -b white '<<f>>' -f png -o '<<fne>>.png'"
	 :utils "rsvg-convert"))

  (defun dwim-shell-commands-join-as-pdf ()
	"Join all marked images as a single pdf."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Join as pdf"
	 (format "convert -verbose '<<*>>' '<<%s(u)>>'"
			 (dwim-shell-command-read-file-name
			  "Join as pdf named (default \"joined.pdf\"): "
			  :extension "pdf"
			  :default "joined.pdf"))
	 :utils "convert"))

  (defun dwim-shell-commands-extract-pdf-page ()
	"Keep a page from pdf."
	(interactive)
	(let ((page-num (read-number "Keep page number: " 1)))
	  (dwim-shell-command-on-marked-files
	   "Keep pdf page"
	   (format "qpdf '<<f>>' --pages . %d -- '<<fne>>_%d.<<e>>'" page-num page-num)
	   :utils "qpdf")))

  (defun dwim-shell-commands-tesseract-ocr-text-from-image ()
	"Extract text from image via tesseract."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Extract text from image via tesseract."
	 "tesseract '<<f>>' -"
	 :utils "tesseract"))

  (defun dwim-shell-commands-video-to-webp ()
	"Convert all marked videos to webp(s)."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Convert to webp"
	 "ffmpeg -i '<<f>>' -vcodec libwebp -filter:v fps=fps=10 -compression_level 3 -loop 0 -preset default -an -vsync 0 '<<fne>>'.webp"
	 :utils "ffmpeg"))

  (defun dwim-shell-commands-webp-to-video ()
	"Convert all marked webp(s) to video(s)."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Convert webp to video"
	 "convert '<<f>>' '<<td>>/<<bne>>.gif'
	ffmpeg -i '<<td>>/<<bne>>.gif' -movflags faststart -pix_fmt yuv420p -vf 'scale=trunc(iw/2)*2:trunc(ih/2)*2' '<<fne>>.mp4'"
	 :utils '("ffmpeg" "convert")
	 :extensions "webp"))

  (defun dwim-shell-commands-video-to-hevc-mkv ()
	"Convert all marked videos to hevc mkv."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Convert video to h265 "
	 "REPO_DIR=/tmp/other_video_transcoding
	if ! [ -d \"$REPO_DIR\" ]
	then
	  git clone https://github.com/donmelton/other_video_transcoding.git $REPO_DIR
	fi
	pushd $REPO_DIR
	git pull origin master || echo \"skipping repo update...\"
	popd
	ruby $REPO_DIR/bin/other-transcode --hevc '<<f>>'"
	 :utils '("git" "ffmpeg" "mkvtoolnix" "mpv")))

  (defun dwim-shell-commands-unzip ()
	"Unzip all marked archives (of any kind) using `atool'."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Unzip" "atool --extract --explain '<<f>>'"
	 :utils "atool"))

  (defun dwim-shell-commands-zip ()
	"Zip all marked files into archive.zip."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Zip" (if (eq 1 (seq-length (dwim-shell-command--files)))
			   "zip -r '<<fne>>.<<e>>' '<<f>>'"
			 "zip -r '<<archive.zip(u)>>' '<<*>>'")
	 :utils "zip"))

  (defun dwim-shell-commands-epub-to-org ()
	"Convert epub(s) to org."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "epub to org"
	 "pandoc --from=epub --to=org '<<f>>' > '<<fne>>.org'"
	 :extensions "epub"
	 :utils "pandoc"))

  (defun dwim-shell-commands-docx-to-pdf ()
	"Convert docx(s) to pdf (via latex)."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "docx to pdf (via latex)"
	 "pandoc -t latex '<<f>>' -o '<<fne>>.pdf'"
	 :extensions "docx" ;; brew install mactex
	 :utils "pdflatex"))

  (defun dwim-shell-commands-pdf-to-txt ()
	"Convert pdf to txt."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "pdf to txt"
	 "pdftotext -layout '<<f>>' '<<fne>>.txt'"
	 :utils "pdftotext"))

  (defun dwim-shell-commands-resize-image-by-factor ()
	"Resize marked image(s) by factor."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Resize image"
	 (let ((factor (read-number "Resize scaling factor: " 0.5)))
	   (format "convert -resize %%%d '<<f>>' '<<fne>>_x%.2f.<<e>>'"
			   (* 100 factor) factor))
	 :utils "convert"))

  (defun dwim-shell-commands-resize-image-in-pixels ()
	"Resize marked image(s) in pixels."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Resize image"
	 (let ((width (read-number "Resize width (pixels): " 500)))
	   (format "convert -resize %dx '<<f>>' '<<fne>>_x%d.<<e>>'" width width))
	 :utils "convert"))

  (defun dwim-shell-commands-pdf-password-protect ()
	"Add a password to pdf(s)."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Password protect pdf"
	 (format "qpdf --verbose --encrypt '%s' '%s' 256 -- '<<f>>' '<<fne>>_protected.<<e>>'"
			 (read-passwd "user-password: ")
			 (read-passwd "owner-password: "))
	 :utils "qpdf"
	 :extensions "pdf"))

  (defun dwim-shell-commands-pdf-password-unprotect ()
	"Remove a password from pdf(s)."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Remove protection from pdf"
	 (format "qpdf --verbose --decrypt --password='%s' -- '<<f>>' '<<fne>>_unprotected.<<e>>'"
			 (read-passwd "password: "))
	 :utils "qpdf"
	 :extensions "pdf"))

  (defun dwim-shell-commands-video-trim-beginning ()
	"Trim seconds off the beginning of videos."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Trim beginning"
	 "ffmpeg -i '<<f>>' -y -ss <<Seconds:5>> -c:v copy -c:a copy '<<fne>>_trimmed.<<e>>'"
	 :silent-success t
	 :utils "ffmpeg"))

  (defun dwim-shell-commands-video-trim-end ()
	"Trim seconds off the end of videos."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Trim beginning"
	 "ffmpeg -sseof -<<Seconds:5>> -i '<<f>>' -y -c:v copy -c:a copy '<<fne>>_trimmed.<<e>>'"
	 :silent-success t
	 :utils "ffmpeg"))

  (defun dwim-shell-commands-drop-audio-from-video ()
	"Drop audio from all marked videos."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Drop audio"
	 "ffmpeg -i '<<f>>' -c copy -an '<<fne>>_no_audio.<<e>>'"
	 :utils "ffmpeg"))

  (defun dwim-shell-commands-open-externally ()
	"Open file(s) externally."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Open externally"
	 (if (eq system-type 'darwin)
		 (if (derived-mode-p 'prog-mode)
			 (format "xed --line %d '<<f>>'"
					 (line-number-at-pos (point)))
		   "open '<<f>>'")
	   "xdg-open '<<f>>'")
	 :shell-util "zsh"
	 :shell-args '("-x" "-c")
	 :silent-success t
	 :utils (if (eq system-type 'darwin)
				"open"
			  "xdg-open")))

  (defun dwim-shell-commands-git-clone-clipboard-url ()
	"Clone git URL in clipboard to `default-directory'."
	(interactive)
	(dwim-shell-command-on-marked-files
	 (format "Clone %s" (file-name-base (current-kill 0)))
	 "git clone <<cb>>"
	 :utils "git"))

  (defun dwim-shell-commands-open-file-manager ()
  "Open the default file manager in the current directory."
  (interactive)
  (dwim-shell-command-on-marked-files
   "Open file manager"
   (cond ((eq system-type 'darwin)
		  "open . ")
		 ((eq system-type 'windows-nt)
		  "explorer . ")
		 (t  ;; Linux/Unix
		  "thunar . ")) ;; hardcoded: xdg-open failed silently
   :silent-success t
   :no-progress t))

  (defun dwim-shell-commands-kill-gpg-agent ()
	"Kill (thus restart) gpg agent.
Useful for when you get this error:
gpg: public key decryption failed: No pinentry
gpg: decryption failed: No pinentry"
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Kill gpg agent"
	 "gpgconf --kill gpg-agent"
	 :utils "gpgconf"
	 :silent-success t)))

(provide 'dwim-shell-commands)
;;; dwim-shell-commands.el ends here.
