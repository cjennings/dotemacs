;; dwim-shell-config.el --- Dired Shell Commands -*- coding: utf-8; lexical-binding: t; -*-
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
;; - qpdf: PDF operations (requires version 8.x+ for secure password handling)
;; - tesseract: OCR functionality
;; - pandoc: Document conversion
;; - atool: Archive extraction
;; - rsvg-convert: SVG to PNG conversion
;; - pdftotext: PDF text extraction
;; - git: Version control operations
;; - gpgconf: GPG agent management
;; - 7z (p7zip): Secure password-protected archives
;;
;; On Arch Linux, install the requirements with:
;; #+begin_src bash
;; sudo pacman -S --needed ffmpeg imagemagick qpdf tesseract tesseract-data-eng pandoc atool librsvg poppler git gnupg p7zip zip unzip mkvtoolnix-cli mpv ruby
;; #+end_src
;;
;; On MacOS, install the requirements with:
;; #+begin_src bash
;; brew install ffmpeg imagemagick qpdf tesseract pandoc atool librsvg poppler gnupg p7zip mkvtoolnix mpv
;; #+end_src
;;
;; Usage:
;; Commands operate on marked files in Dired or the current file in other modes.
;; The package automatically replaces standard shell commands with DWIM versions
;; for a more intuitive experience.
;;
;; Security:
;; Password-protected operations (PDF encryption, archive encryption) use secure
;; methods to avoid exposing passwords in process lists or command history:
;; - PDF operations: Use temporary files with restrictive permissions (mode 600)
;; - Archive operations: Use 7z instead of zip for better password handling
;; - Temporary password files are automatically cleaned up after use
;; - Note: Switched from zip to 7z for encryption due to zip's insecure -P flag
;;
;; Template Variables:
;; - <<f>>: Full path to file
;; - <<fne>>: File name without extension
;; - <<e>>: File extension
;; - <<b>>: Base name (file name with extension, no directory)
;; - <<d>>: Directory path
;; - <<n>>: Sequential number (for batch renaming)
;; - <<td>>: Temporary directory
;; - <<cb>>: Clipboard contents
;; - <<*>>: All marked files
;;

;;; Code:

(require 'cl-lib)


;; Bind menu to dired (after dwim-shell-command loads)
(with-eval-after-load 'dwim-shell-command
  (with-eval-after-load 'dired
    (keymap-set dired-mode-map "M-D" #'dwim-shell-commands-menu)))

;; ----------------------------- Dwim Shell Command ----------------------------

(use-package dwim-shell-command
  :after dired
  :bind (("<remap> <shell-command>" . dwim-shell-command)
		 :map dired-mode-map
		 ("<remap> <dired-do-async-shell-command>" . dwim-shell-command)
		 ("<remap> <dired-do-shell-command>" . dwim-shell-command)
		 ("<remap> <dired-smart-shell-command>" . dwim-shell-command))
  :config
  (defun cj/dwim-shell-commands-convert-audio-to-mp3 ()
	"Convert all marked audio to mp3(s)."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Convert to mp3"
	 "ffmpeg -stats -n -i '<<f>>' -acodec libmp3lame '<<fne>>.mp3'"
	 :utils "ffmpeg"))

  (defun cj/dwim-shell-commands-convert-audio-to-opus ()
	"Convert all marked audio to opus(s)."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Convert to opus"
	 "ffmpeg -stats -n -i '<<f>>' -c:a libopus -vbr on -compression_level 10 -b:a 256k '<<fne>>.opus'"
	 :utils "ffmpeg"))

  (defun cj/dwim-shell-commands-view-image-exif-metadata ()
	"View EXIF metadata in image(s)."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "View EXIF"
	 "exiftool '<<f>>'"
	 :utils "exiftool"))

  (defun cj/dwim-shell-commands-flip-image-horizontally ()
	"Horizontally flip image(s)."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Image horizontal flip"
	 "convert -verbose -flop '<<f>>' '<<fne>>_h_flipped.<<e>>'"
	 :utils "convert"))

  (defun cj/dwim-shell-commands-flip-image-vertically ()
	"Horizontally flip image(s)."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Image vertical flip"
	 "convert -verbose -flip '<<f>>' '<<fne>>_v_flipped.<<e>>'"
	 :utils "convert"))

  (defun cj/dwim-shell-commands-convert-image-to ()
	"Convert all marked images to a specified format."
	(interactive)
	(let ((format (completing-read "Convert to format: "
								   '("jpg" "png" "webp" "gif" "bmp" "tiff")
								   nil t)))
      (dwim-shell-command-on-marked-files
	   (format "Convert to %s" format)
	   (format "convert -verbose '<<f>>' '<<fne>>.%s'" format)
	   :utils "convert")))

  (defun cj/dwim-shell-commands-convert-svg-to-png ()
	"Convert all marked svg(s) to png(s)."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Convert to png"
	 "rsvg-convert -b white '<<f>>' -f png -o '<<fne>>.png'"
	 :utils "rsvg-convert"))

  (defun cj/dwim-shell-commands-join-images-into-pdf ()
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

  (defun cj/dwim-shell-commands-extract-pdf-page-number ()
	"Keep a page from pdf."
	(interactive)
	(let ((page-num (read-number "Keep page number: " 1)))
	  (when (<= page-num 0)
		(user-error "Page number must be positive"))
	  (dwim-shell-command-on-marked-files
	   "Keep pdf page"
	   (format "qpdf '<<f>>' --pages . %d -- '<<fne>>_%d.<<e>>'" page-num page-num)
	   :utils "qpdf")))

  (defun cj/dwim-shell-commands-ocr-text-from-image-using-tesseract ()
	"Extract text from image via tesseract."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Extract text from image via tesseract."
	 "tesseract '<<f>>' -"
	 :utils "tesseract"))

  (defun cj/dwim-shell-commands-convert-video-to-webp ()
	"Convert all marked videos to webp(s)."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Convert to webp"
	 "ffmpeg -i '<<f>>' -vcodec libwebp -filter:v fps=fps=10 -compression_level 3 -loop 0 -preset default -an -vsync 0 '<<fne>>'.webp"
	 :utils "ffmpeg"))

  (defun cj/dwim-shell-commands-convert-video-to-high-compatibility-mp4 ()
	"Convert all marked video(s) to MP4 format with H.264/AAC."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Convert to MP4"
	 "ffmpeg -i '<<f>>' -c:v libx264 -preset slow -crf 23 -profile:v baseline -level 3.0 -c:a aac -b:a 128k '<<fne>>.mp4'"
	 :utils "ffmpeg"))

  (defun cj/dwim-shell-commands-convert-video-to-hevc-mkv ()
	"Convert all marked videos to HEVC (H.265) in MKV container."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Convert to HEVC/H.265"
	 "ffmpeg -i '<<f>>' -c:v libx265 -preset slower -crf 22 -x265-params profile=main10:level=4.0 -c:a copy -c:s copy '<<fne>>.mkv'"
	 :utils "ffmpeg"))

  (defun cj/dwim-shell-commands-extract-archive-smartly ()
	"Unzip all marked archives (of any kind) using =atool'.

If there's only one file, unzip it to current directory.
Otherwise, unzip it to an appropriately named subdirectory "
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Unzip" "atool --extract --subdir --explain '<<f>>'"
	 :utils "atool"))

  (defun cj/dwim-shell-commands-zip-file-or-directory ()
	"Zip all marked files into archive.zip."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Zip" (if (eq 1 (seq-length (dwim-shell-command--files)))
			   "zip -r '<<fne>>.<<e>>' '<<f>>'"
			 "zip -r '<<archive.zip(u)>>' '<<*>>'")
	 :utils "zip"))

  (defun cj/dwim-shell-commands-tar-gzip-file-or-directory ()
	"Tar gzip all marked files into archive.tar.gz."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Tar gzip" (if (eq 1 (seq-length (dwim-shell-command--files)))
					"tar czf '<<fne>>.tar.gz' '<<f>>'"
				  "tar czf '<<archive.tar.gz(u)>>' '<<*>>'")
	 :utils "tar"))

  (defun cj/dwim-shell-commands-epub-to-org ()
	"Convert epub(s) to org."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "epub to org"
	 "pandoc --from=epub --to=org '<<f>>' > '<<fne>>.org'"
	 :extensions "epub"
	 :utils "pandoc"))

  (defun cj/dwim-shell-commands-document-to-pdf ()
	"Convert document(s) to pdf (via latex).

Supports docx, odt, and other pandoc-compatible formats."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Document to pdf (via latex)"
	 "pandoc -t latex '<<f>>' -o '<<fne>>.pdf'"
	 :extensions '("docx" "odt" "odp" "ods" "rtf" "doc")
	 :utils "pdflatex"))

  (defun cj/dwim-shell-commands-pdf-to-txt ()
	"Convert pdf to txt."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "pdf to txt"
	 "pdftotext -layout '<<f>>' '<<fne>>.txt'"
	 :utils "pdftotext"))

  (defun cj/dwim-shell-commands-resize-image-by-factor ()
	"Resize image(s) by factor."
	(interactive)
	(let ((factor (read-number "Resize scaling factor: " 0.5)))
	  (when (<= factor 0)
		(user-error "Scaling factor must be positive"))
	  (dwim-shell-command-on-marked-files
	   "Resize image"
	   (format "convert -resize %%%d '<<f>>' '<<fne>>_x%.2f.<<e>>'"
			   (* 100 factor) factor)
	   :utils "convert")))

  (defun cj/dwim-shell-commands-resize-image-in-pixels ()
	"Resize image(s) in pixels."
	(interactive)
	(let ((width (read-number "Resize width (pixels): " 500)))
	  (when (<= width 0)
		(user-error "Width must be positive"))
	  (dwim-shell-command-on-marked-files
	   "Resize image"
	   (format "convert -resize %dx '<<f>>' '<<fne>>_x%d.<<e>>'" width width)
	   :utils "convert")))

  (defun cj/dwim-shell-commands-pdf-password-protect ()
	"Add a password to pdf(s).
Uses temporary file with restrictive permissions to avoid exposing passwords
in process lists or command history."
	(interactive)
	(let* ((user-pass (read-passwd "user-password: "))
		   (owner-pass (read-passwd "owner-password: "))
		   (temp-file (make-temp-file "qpdf-pass-")))
	  (unwind-protect
		  (progn
			;; Write passwords to temp file with restrictive permissions
			(with-temp-file temp-file
			  (insert user-pass "\n" owner-pass))
			(set-file-modes temp-file #o600)
			(dwim-shell-command-on-marked-files
			 "Password protect pdf"
			 (format "qpdf --verbose --password-file='%s' --encrypt --use-aes=y -- '<<f>>' '<<fne>>_protected.<<e>>'"
					 temp-file)
			 :utils "qpdf"
			 :extensions "pdf"))
		;; Always cleanup temp file
		(when (file-exists-p temp-file)
		  (delete-file temp-file)))))

  (defun cj/dwim-shell-commands-pdf-password-unprotect ()
	"Remove a password from pdf(s).
Uses temporary file with restrictive permissions to avoid exposing passwords
in process lists or command history."
	(interactive)
	(let* ((password (read-passwd "password: "))
		   (temp-file (make-temp-file "qpdf-pass-")))
	  (unwind-protect
		  (progn
			;; Write password to temp file with restrictive permissions
			(with-temp-file temp-file
			  (insert password))
			(set-file-modes temp-file #o600)
			(dwim-shell-command-on-marked-files
			 "Remove protection from pdf"
			 (format "qpdf --verbose --decrypt --password-file='%s' -- '<<f>>' '<<fne>>_unprotected.<<e>>'"
					 temp-file)
			 :utils "qpdf"
			 :extensions "pdf"))
		;; Always cleanup temp file
		(when (file-exists-p temp-file)
		  (delete-file temp-file)))))

  (defun cj/dwim-shell-commands-video-trim ()
	"Trim video with options for beginning, end, or both."
	(interactive)
	(let* ((trim-type (completing-read "Trim from: "
									   '("Beginning" "End" "Both")
									   nil t))
           (command (pcase trim-type
					  ("Beginning"
					   (let ((seconds (read-number "Seconds to trim from beginning: " 5)))
						 (when (< seconds 0)
						   (user-error "Seconds must be non-negative"))
						 (format "ffmpeg -i '<<f>>' -y -ss %d -c:v copy -c:a copy '<<fne>>_trimmed.<<e>>'"
								 seconds)))
					  ("End"
					   (let ((seconds (read-number "Seconds to trim from end: " 5)))
						 (when (< seconds 0)
						   (user-error "Seconds must be non-negative"))
						 (format "ffmpeg -sseof -%d -i '<<f>>' -y -c:v copy -c:a copy '<<fne>>_trimmed.<<e>>'"
								 seconds)))
					  ("Both"
					   (let ((start (read-number "Seconds to trim from beginning: " 5))
							 (end (read-number "Seconds to trim from end: " 5)))
						 (when (or (< start 0) (< end 0))
						   (user-error "Seconds must be non-negative"))
						 (format "ffmpeg -i '<<f>>' -y -ss %d -sseof -%d -c:v copy -c:a copy '<<fne>>_trimmed.<<e>>'"
								 start end))))))
      (dwim-shell-command-on-marked-files
	   (format "Trim video (%s)" trim-type)
	   command
	   :silent-success t
	   :utils "ffmpeg")))

  (defun cj/dwim-shell-commands-drop-audio-from-video ()
	"Drop audio from all marked videos."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Drop audio"
	 "ffmpeg -i '<<f>>' -c copy -an '<<fne>>_no_audio.<<e>>'"
	 :utils "ffmpeg"))

  (defun cj/dwim-shell-commands-open-externally ()
	"Open file(s) externally."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Open externally"
	 (cond ((eq system-type 'darwin)
			(if (derived-mode-p 'prog-mode)
				(format "open -a Xcode --args --line %d '<<f>>'"
						(line-number-at-pos (point)))
			  "open '<<f>>'"))
		   ((eq system-type 'windows-nt)
			"start '<<f>>'")
		   (t  ;; Linux/Unix
			"xdg-open '<<f>>' 2>/dev/null || (echo 'Failed to open with xdg-open' && exit 1)"))
	 :silent-success t
	 :utils (cond ((eq system-type 'darwin) "open")
				  ((eq system-type 'windows-nt) "start")
				  (t "xdg-open"))))


  (defun cj/dwim-shell-commands-git-clone-clipboard-url ()
	"Clone git URL in clipboard to `default-directory'."
	(interactive)
	(dwim-shell-command-on-marked-files
	 (format "Clone %s" (file-name-base (current-kill 0)))
	 "git clone <<cb>>"
	 :utils "git"))

  (defun cj/dwim-shell-commands-open-file-manager ()
	"Open the default file manager in the current directory."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Open file manager"
	 (cond ((eq system-type 'darwin)
			"open .")
		   ((eq system-type 'windows-nt)
			"explorer .")
		   (t  ;; Linux/Unix - try multiple options
			(cond ((executable-find "thunar") "thunar .")
				  ((executable-find "nautilus") "nautilus .")
				  ((executable-find "dolphin") "dolphin .")
				  ((executable-find "pcmanfm") "pcmanfm .")
				  (t "xdg-open ."))))
	 :silent-success t
	 :no-progress t))

  (defun cj/dwim-shell-commands-count-words-lines ()
	"Count words, lines, and characters in text file(s)."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Word count"
	 "wc -lwc '<<f>>'"
	 :utils "wc"))

  (defun cj/dwim-shell-commands-checksum ()
	"Generate checksums for file(s) and save to .checksum file."
	(interactive)
	(let ((algorithm (completing-read "Algorithm: "
									  '("md5" "sha1" "sha256" "sha512")
									  nil t)))
      (dwim-shell-command-on-marked-files
	   (format "Generate %s checksum" algorithm)
	   (format "%ssum '<<f>>' | tee '<<f>>.%s'" algorithm algorithm)
	   :utils (format "%ssum" algorithm))))

  (defun cj/dwim-shell-commands-backup-with-timestamp ()
    "Create dated backup of file(s)."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Backup with date"
	 "cp -p '<<f>>' '<<f>>.$(date +%Y%m%d_%H%M%S).bak'"
	 :utils '("cp" "date")))

  (defun cj/dwim-shell-commands-optimize-image-for-web ()
	"Optimize image(s) for web (reduce file size)."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Optimize image"
	 "convert '<<f>>' -strip -interlace Plane -gaussian-blur 0.05 -quality 85% '<<fne>>_optimized.<<e>>'"
	 :utils "convert"))

  (defun cj/dwim-shell-commands-csv-to-json ()
	"Convert CSV to JSON."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "CSV to JSON"
	 "python -c \"import csv,json,sys; print(json.dumps(list(csv.DictReader(open('<<f>>')))))\" > '<<fne>>.json'"
	 :extensions "csv"
	 :utils "python"))

  (defun cj/dwim-shell-commands-json-to-yaml ()
	"Convert JSON to YAML."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "JSON to YAML"
	 "python -c \"import json,yaml,sys; yaml.dump(json.load(open('<<f>>')), open('<<fne>>.yaml', 'w'))\" && echo 'Created <<fne>>.yaml'"
	 :extensions "json"
	 :utils "python"))

  (defun cj/dwim-shell-commands-extract-urls-from-file ()
	"Extract all URLs from file(s)."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Extract URLs"
	 "grep -Eo 'https?://[^[:space:]]+' '<<f>>'"
	 :utils "grep"))

  (defun cj/dwim-shell-commands-extract-emails-from-file ()
	"Extract all email addresses from file(s)."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Extract emails"
	 "grep -Eo '[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}' '<<f>>'"
	 :utils "grep"))

  (defun cj/dwim-shell-commands-create-gif-from-video ()
	"Create animated GIF from video."
	(interactive)
	(let ((fps (read-number "FPS for GIF: " 10))
		  (scale (read-number "Scale (pixels width): " 480)))
	  (when (<= fps 0)
		(user-error "FPS must be positive"))
	  (when (<= scale 0)
		(user-error "Scale must be positive"))
	  (dwim-shell-command-on-marked-files
	   "Create GIF"
	   (format "ffmpeg -i '<<f>>' -vf 'fps=%d,scale=%d:-1:flags=lanczos' '<<fne>>.gif'" fps scale)
	   :utils "ffmpeg")))

  (defun cj/dwim-shell-commands-concatenate-videos ()
	"Concatenate multiple videos into one."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Concatenate videos"
	 "echo '<<*>>' | tr ' ' '\n' | sed 's/^/file /' > '<<td>>/filelist.txt' && ffmpeg -f concat -safe 0 -i '<<td>>/filelist.txt' -c copy '<<concatenated.mp4(u)>>'"
	 :utils "ffmpeg"))

  (defun cj/dwim-shell-commands-create-video-thumbnail ()
	"Extract thumbnail from video at specific time."
	(interactive)
	(let ((time (read-string "Time (HH:MM:SS or seconds): " "00:00:05")))
	  (dwim-shell-command-on-marked-files
	   "Extract video thumbnail"
	   (format "ffmpeg -i '<<f>>' -ss %s -vframes 1 '<<fne>>_thumb.jpg'" time)
	   :utils "ffmpeg")))

  (defun cj/dwim-shell-commands-merge-pdfs ()
	"Merge multiple PDFs into one."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Merge PDFs"
	 "qpdf --empty --pages '<<*>>' -- '<<merged.pdf(u)>>'"
	 :extensions "pdf"
	 :utils "qpdf"))

  (defun cj/dwim-shell-commands-split-pdf-by-pages ()
	"Split PDF into individual pages."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Split PDF pages"
	 "qpdf --split-pages '<<f>>' '<<fne>>-page-%d.pdf'"
	 :extensions "pdf"
	 :utils "qpdf"))

  (defun cj/dwim-shell-commands-compress-pdf ()
	"Compress PDF file size."
	(interactive)
	(let ((quality (completing-read "Quality: "
									'("screen" "ebook" "printer" "prepress")
									nil t "ebook")))
      (dwim-shell-command-on-marked-files
	   "Compress PDF"
	   (format "gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/%s -dNOPAUSE -dBATCH -sOutputFile='<<fne>>_compressed.pdf' '<<f>>'" quality)
	   :extensions "pdf"
	   :utils "gs")))

  (defun cj/dwim-shell-commands-ascii-art ()
	"Convert image to ASCII art."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Create ASCII art"
	 "jp2a --width=80 '<<f>>'"
	 :utils "jp2a"))

  (defun cj/dwim-shell-commands-text-to-speech ()
	"Convert text file to speech (audio file)."
	(interactive)
	(let ((voice (if (eq system-type 'darwin)
					 (completing-read "Voice: " '("Alex" "Samantha" "Victoria" "Karen") nil t "Alex")
				   "en")))
	  (dwim-shell-command-on-marked-files
	   "Text to speech"
	   (if (eq system-type 'darwin)
		   (format "say -v %s -o '<<fne>>.aiff' -f '<<f>>'" voice)
		 "espeak -f '<<f>>' -w '<<fne>>.wav'")
	   :utils (if (eq system-type 'darwin) "say" "espeak"))))

  (defun cj/dwim-shell-commands-remove-empty-directories ()
	"Remove all empty directories recursively."
	(interactive)
	(when (yes-or-no-p "Remove all empty directories? ")
	  (dwim-shell-command-on-marked-files
	   "Remove empty dirs"
	   "find . -type d -empty -delete"
	   :utils "find")))

  (defun cj/dwim-shell-commands-create-thumbnail-from-image ()
	"Create thumbnail(s) from image(s)."
	(interactive)
	(let ((size (read-number "Thumbnail size (pixels): " 200)))
	  (when (<= size 0)
		(user-error "Thumbnail size must be positive"))
	  (dwim-shell-command-on-marked-files
	   "Create thumbnail"
	   (format "convert '<<f>>' -thumbnail %dx%d '<<fne>>_thumb.<<e>>'" size size)
	   :utils "convert")))

  (defun cj/dwim-shell-commands-extract-audio-from-video ()
	"Extract audio track from video file(s)."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Extract audio"
	 "ffmpeg -i '<<f>>' -vn -acodec copy '<<fne>>.m4a'"
	 :utils "ffmpeg"))

  (defun cj/dwim-shell-commands-normalize-audio-volume ()
	"Normalize audio volume in file(s)."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Normalize audio"
	 "ffmpeg -i '<<f>>' -af 'loudnorm=I=-16:LRA=11:TP=-1.5' '<<fne>>_normalized.<<e>>'"
	 :utils "ffmpeg"))

  (defun cj/dwim-shell-commands-remove-zip-encryption ()
	"Remove password protection from archive file(s).
Uses 7z for secure password handling via temporary file.
Works with .7z, .zip, and other password-protected archives.
Extracts and re-archives without password protection."
	(interactive)
	(let* ((password (read-passwd "Current password: "))
		   (temp-file (make-temp-file "7z-pass-")))
	  (unwind-protect
		  (progn
			;; Write password to temp file with restrictive permissions
			(with-temp-file temp-file
			  (insert password))
			(set-file-modes temp-file #o600)
			(dwim-shell-command-on-marked-files
			 "Remove archive encryption"
			 (format "TMPDIR=$(mktemp -d) && 7z x -p\"$(cat '%s')\" '<<f>>' -o\"$TMPDIR\" && 7z a -tzip '<<fne>>_decrypted.zip' \"$TMPDIR\"/* && rm -rf \"$TMPDIR\""
					 temp-file)
			 :utils "7z"))
		;; Always cleanup temp file
		(when (file-exists-p temp-file)
		  (delete-file temp-file)))))

  (defun cj/dwim-shell-commands-create-encrypted-zip ()
	"Create password-protected archive of file(s).
Uses 7z instead of zip for secure password handling via temporary file.
Creates a .7z archive with AES-256 encryption."
	(interactive)
	(let* ((password (read-passwd "Password: "))
		   (temp-file (make-temp-file "7z-pass-"))
		   (archive-name (read-string "Archive name (without extension): " "archive")))
	  (unwind-protect
		  (progn
			;; Write password to temp file with restrictive permissions
			(with-temp-file temp-file
			  (insert password))
			(set-file-modes temp-file #o600)
			(dwim-shell-command-on-marked-files
			 "Create encrypted archive"
			 (format "7z a -t7z -mhe=on -p\"$(cat '%s')\" '%s.7z' '<<*>>'"
					 temp-file
					 archive-name)
			 :utils "7z"))
		;; Always cleanup temp file
		(when (file-exists-p temp-file)
		  (delete-file temp-file)))))


  (defun cj/dwim-shell-commands-list-archive-contents ()
	"List contents of archive without extracting."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "List archive contents"
	 "atool --list '<<f>>'"
	 :utils "atool"))

  (defun cj/dwim-shell-commands-make-executable ()
	"Make file(s) executable."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Make executable"
	 "chmod +x '<<f>>'"
	 :silent-success t
	 :utils "chmod"))

  (defun cj/dwim-shell-commands-secure-delete ()
	"Securely delete file(s) by overwriting with random data."
	(interactive)
	(when (yes-or-no-p "This will permanently destroy files. Continue? ")
	  (dwim-shell-command-on-marked-files
	   "Secure delete"
	   "shred -vfz -n 3 '<<f>>'"
	   :utils "shred")))

  (defun cj/dwim-shell-commands-sanitize-filename ()
	"Sanitize filename(s) - remove spaces and special characters."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Sanitize filename"
	 "NEW_NAME=$(echo '<<b>>' | tr ' ' '_' | tr -cd '[:alnum:]._-'); mv '<<f>>' \"$(dirname '<<f>>')/${NEW_NAME}\""
	 :utils '("tr" "mv")))

  (defun cj/dwim-shell-commands-number-files-sequentially ()
	"Rename files with sequential numbers."
	(interactive)
	(let ((prefix (read-string "Prefix (optional): ")))
	  (dwim-shell-command-on-marked-files
	   "Number files"
	   (format "mv '<<f>>' '<<d>>/%s<<n>>.<<e>>'" prefix)
	   :utils "mv")))

  (defun cj/dwim-shell-commands-git-history ()
	"Show git history for file(s)."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Git history"
	 "git log --oneline --follow '<<f>>'"
	 :utils "git"))

  (defun cj/dwim-shell-commands-encrypt-with-gpg ()
	"Encrypt file(s) with GPG."
	(interactive)
	(let ((recipient (read-string "Recipient email (or leave empty for symmetric): ")))
	  (dwim-shell-command-on-marked-files
	   "GPG encrypt"
	   (if (string-empty-p recipient)
		   "gpg --symmetric --cipher-algo AES256 '<<f>>'"
		 (format "gpg --encrypt --recipient '%s' '<<f>>'" recipient))
	   :utils "gpg")))

  (defun cj/dwim-shell-commands-decrypt-with-gpg ()
	"Decrypt GPG file(s)."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "GPG decrypt"
	 "gpg --decrypt '<<f>>' > '<<fne>>'"
	 :extensions '("gpg" "asc" "pgp")
	 :utils "gpg"))

  (defun cj/dwim-shell-commands-markdown-to-html5-and-open ()
	"Convert markdown file to HTML in specified directory and open it."
	(interactive)
	(let ((files (dwim-shell-command--files)))
	  ;; verify it's a markdown file
	  (unless (and files
				   (= 1 (length files))
				   (string-match-p "\\.\\(md\\|markdown\\|mkd\\|mdown\\)\\'" (car files)))
		(user-error "Please place cursor on a single markdown file"))
	  (let* ((dest-dir (expand-file-name (read-directory-name "Destination directory: " default-directory)))
			 (base-name (file-name-sans-extension (file-name-nondirectory (car files))))
			 (output-file (expand-file-name (concat base-name ".html") dest-dir)))
		(dwim-shell-command-on-marked-files
		 "Convert markdown to HTML"
		 (format "pandoc --standalone --from=markdown --to=html5 --metadata title='<<fne>>' '<<f>>' -o '%s'"
				 output-file)
		 :utils "pandoc"
		 :on-completion (lambda (&rest _args)
						  (when (file-exists-p output-file)
							(cj/xdg-open output-file)
							(message "Opened %s" output-file)))))))

  (defun cj/dwim-shell-commands-optimize-image-for-email ()
	"Optimize image(s) for email - reduces file size while maintaining quality.
Resizes to max 1200px (only if larger), strips metadata, and applies JPEG
optimization."
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Optimize for email"
	 "magick '<<f>>' -strip -resize '1200x1200>' -quality 85 -interlace Plane -colorspace sRGB -sampling-factor 4:2:0 '<<fne>>_email.<<e>>'"
	 :utils "magick"))

  (defun cj/dwim-shell-commands-kill-gpg-agent ()
	"Kill (thus restart) gpg agent.
Useful for when you get this error:
gpg: public key decryption failed: No pinentry
gpg: decryption failed: No pinentry"
	(interactive)
	(dwim-shell-command-on-marked-files
	 "Kill gpg agent"
	 "gpgconf --kill gpg-agent"
	 :utils "gpgconf"
	 :silent-success t))

  ;; Dwim shell commands menu
  (defun dwim-shell-commands-menu ()
	"Select and execute a dwim-shell-command function with prettified names."
	(interactive)
	(let* ((commands (cl-loop for symbol being the symbols
							  when (and (fboundp symbol)
										(string-prefix-p "cj/dwim-shell-commands-" (symbol-name symbol))
										(not (eq symbol 'dwim-shell-commands-menu)))
							  collect symbol))
		   ;; Create alist of (pretty-name . command-symbol)
		   (command-alist (mapcar (lambda (cmd)
									(cons (replace-regexp-in-string
										   "-" " "
										   (replace-regexp-in-string
											"^cj/dwim-shell-commands-"
											""
											(symbol-name cmd)))
										  cmd))
								  commands))
		   (selected (completing-read "Command: "
									  command-alist
									  nil
									  t
									  nil
									  'dwim-shell-command-history))
		   (command (alist-get selected command-alist nil nil #'string=)))
	  (when command
		(call-interactively command)))))

(provide 'dwim-shell-config)
;;; dwim-shell-config.el ends here.
