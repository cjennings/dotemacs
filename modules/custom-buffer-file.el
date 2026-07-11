;;; custom-buffer-file.el --- Custom Buffer and File Operations -*- coding: utf-8; lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Layer: 2 (Core UX).
;; Category: L/C.
;; Load shape: eager.
;; Eager reason: registers its C-; b buffer/file submap at load. Currently eager
;;   by init order; a deferral candidate for Phase 3/4.
;; Top-level side effects: defines cj/copy-buffer-content-map and
;;   cj/buffer-and-file-map; conditionally registers the latter under C-; b.
;; Runtime requires: keybindings, external-open, mm-decode, system-lib.
;; Direct test load: yes (requires keybindings explicitly).
;;
;; This module provides custom buffer and file operations including PostScript
;; printing capabilities.
;;
;; Functions include:
;; - printing buffers or regions as PostScript to the default printer (with color support)
;; - moving/renaming/deleting buffer files
;; - diffing buffer contents with saved file version
;; - copying file paths and file:// links to the kill ring
;; - copying buffer contents (whole buffer, to top of buffer, to bottom of buffer)
;; - clearing buffer contents from point to top or bottom.
;;
;; The PostScript printing auto-detects the system print spooler (lpr or lp)
;; and prints with face/syntax highlighting.
;;
;; Destructive-operation policy (2026-07-01): delete file (D) always
;; confirms, naming the file.  Erase (x), clear to top/bottom (t/b), and
;; revert (g) confirm only when a file-visiting buffer has unsaved edits;
;; unmodified and non-file buffers stay fast.  See
;; cj/--destructive-op-allowed-p.
;;
;; Keybindings under ~C-; b~:
;; - ~C-; b k~ kill buffer and window (delete window, kill/bury buffer)
;; - ~C-; b K~ kill the other window's buffer, keeping that window/split
;;   (cj/kill-other-window-buffer in undead-buffers.el)
;; - ~C-; b <arrow>~ move the active window's divider that way (via windsize);
;;   bare arrows keep nudging until any other key (cj/window-resize-sticky in
;;   ui-navigation.el)
;; - Copy buffer content submenu at ~C-; b c~
;;   - ~C-; b c w~ copy whole buffer
;;   - ~C-; b c t~ copy from beginning to point
;;   - ~C-; b c b~ copy from point to end
;;
;;; Code:

(require 'keybindings) ;; provides cj/custom-keymap
(eval-when-compile (require 'ps-print)) ;; for ps-print variables
(declare-function ps-print-buffer-with-faces "ps-print")
(declare-function ps-print-region-with-faces "ps-print")

;; mm-handle-type is a macro used in `cj/--email-handle-is-type-p', so mm-decode
;; is only needed at compile time here; `cj/view-email-in-buffer' requires it at
;; runtime before any mm-* call, so the eager startup require is unnecessary.
(eval-when-compile (require 'mm-decode))
(declare-function mm-dissect-buffer "mm-decode")
(declare-function mm-insert-part "mm-decode")
(declare-function mm-destroy-parts "mm-decode")
(require 'external-open) ;; for cj/xdg-open, cj/open-this-file-with
(require 'system-lib)   ;; cj/confirm-strong (overwrite confirms), used below

;; cj/kill-buffer-and-window and cj/kill-other-window-buffer defined in undead-buffers.el
(declare-function cj/kill-buffer-and-window "undead-buffers")
(declare-function cj/kill-other-window-buffer "undead-buffers")

;; cj/window-resize-sticky (C-; b <arrow>) defined in ui-navigation.el
(declare-function cj/window-resize-sticky "ui-navigation")

;; ------------------------- Print Buffer As Postscript ------------------------

(defvar cj/print-spooler-command 'auto
  "Command used to send PostScript to the system print spooler.
Set to a string to force a specific command (e.g., lpr or lp). Set to `auto' to
auto-detect once per session.")

(defvar cj/print--spooler-cache nil
  "Cached spooler command detected for the current Emacs session.")

(defun cj/print--resolve-spooler ()
  "Return the spooler command to use, auto-detecting and caching if needed."
  (cond
   ;; User-specified command
   ((and (stringp cj/print-spooler-command)
         (> (length cj/print-spooler-command) 0))
    (or (executable-find cj/print-spooler-command)
        (user-error "Cannot print: spooler command '%s' not found in PATH"
                    cj/print-spooler-command))
    cj/print-spooler-command)
   ;; Auto-detect once per session
   ((eq cj/print-spooler-command 'auto)
    (or cj/print--spooler-cache
        (let ((cmd (or (and (executable-find "lpr") "lpr")
                       (and (executable-find "lp")  "lp"))))
          (unless cmd
            (user-error "Cannot print: neither 'lpr' nor 'lp' found in PATH"))
          (setq cj/print--spooler-cache cmd)
          cmd)))
   (t
    (user-error "Invalid value for cj/print-spooler-command: %S"
                cj/print-spooler-command))))

(defun cj/print-buffer-ps (&optional color)
  "Print the buffer (or active region) as PostScript to the default printer.
With prefix argument COLOR, print in color and skip confirmation; otherwise
print in monochrome with confirmation prompt.
Sends directly to the system spooler with no header."
  (interactive "P")
  (unless (require 'ps-print nil t)
    (user-error "Cannot print: ps-print library not found"))
  (let* ((spooler (cj/print--resolve-spooler))
         (want-color (not (null color)))
         (have-region (use-region-p))
         (skip-confirm color))  ; C-u skips confirmation
    ;; Confirm unless C-u was used
    (when (and (not skip-confirm)
               (not (y-or-n-p (format "Send %s to printer? "
                                     (if have-region "region" "buffer")))))
      (user-error "Printing cancelled"))
    (let ((ps-lpr-command spooler)
          (ps-printer-name nil)      ; default system printer
          (ps-lpr-switches nil)
          (ps-print-color-p want-color)
          (ps-use-face-background want-color)
          (ps-print-header nil))     ; no headers
      (if have-region
          (ps-print-region-with-faces (region-beginning) (region-end))
        (ps-print-buffer-with-faces)))
    (message "Sent %s to default printer via %s (%s)"
             (if have-region "region" "buffer")
             spooler
             (if want-color "color" "monochrome"))))

;; ------------------------- Buffer And File Operations ------------------------

(defun cj/--move-buffer-and-file (dir &optional ok-if-exists)
  "Internal implementation: Move buffer and file to DIR.
If OK-IF-EXISTS is nil and target exists, signal an error.
If OK-IF-EXISTS is non-nil, overwrite existing file.
Returns t on success, nil if buffer not visiting a file."
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (basename (and filename (file-name-nondirectory filename)))
         (dir (expand-file-name dir))
         (dir
          (if (string-match "[/\\\\]$" dir)
              (substring dir 0 -1) dir))
         (newname (and basename (expand-file-name basename dir))))
    (if (not filename)
        (progn
          (message "Buffer '%s' is not visiting a file!" name)
          nil)
      (progn  (copy-file filename newname ok-if-exists)
              (delete-file filename)
              (set-visited-file-name newname)
              (set-buffer-modified-p nil)
              t))))

(defun cj/move-buffer-and-file (dir)
  "Move both current buffer and the file it visits to DIR.
When called interactively, prompts for confirmation if target file exists."
  (interactive (list (read-directory-name "Move buffer and file (to new directory): ")))
  (let* ((filename (buffer-file-name))
         (target (expand-file-name
                  (file-name-nondirectory (or filename (buffer-name)))
                  (expand-file-name dir))))
    (condition-case _
        (cj/--move-buffer-and-file dir nil)
      (file-already-exists
       (if (cj/confirm-strong (format "File %s exists; overwrite? " target))
           (cj/--move-buffer-and-file dir t)
         (message "File not moved"))))))

(defun cj/--rename-buffer-and-file (new-name &optional ok-if-exists)
  "Internal implementation: Rename buffer and file to NEW-NAME.
NEW-NAME can be just a basename or a full path to move to different directory.
If OK-IF-EXISTS is nil and target exists, signal an error.
If OK-IF-EXISTS is non-nil, overwrite existing file.
Returns t on success, nil if buffer not visiting a file."
  (let ((filename (buffer-file-name))
        (new-basename (file-name-nondirectory new-name)))
    (if (not filename)
        (progn
          (message "Buffer '%s' is not visiting a file!" (buffer-name))
          nil)
      ;; Check if a buffer with the new name already exists
      (when (and (get-buffer new-basename)
                 (not (eq (get-buffer new-basename) (current-buffer))))
        (error "A buffer named '%s' already exists" new-basename))
      ;; Expand new-name to absolute path (preserves directory if just basename)
      (let ((expanded-name (expand-file-name new-name
                                              (file-name-directory filename))))
        (rename-file filename expanded-name ok-if-exists)
        (rename-buffer new-basename)
        (set-visited-file-name expanded-name)
        (set-buffer-modified-p nil)
        t))))

(defun cj/rename-buffer-and-file (new-name)
  "Rename both current buffer and the file it visits to NEW-NAME.
When called interactively, prompts for confirmation if target file exists."
  (interactive
   (list (if (not (buffer-file-name))
             (user-error "Buffer '%s' is not visiting a file!" (buffer-name))
           (read-string "Rename buffer and file (to new name): "
                        (file-name-nondirectory (buffer-file-name))))))
  (condition-case err
      (cj/--rename-buffer-and-file new-name nil)
    (file-already-exists
     (if (cj/confirm-strong (format "File %s exists; overwrite? " new-name))
         (cj/--rename-buffer-and-file new-name t)
       (message "File not renamed")))
    (error
     ;; Handle buffer-already-exists and other errors
     (message "%s" (error-message-string err)))))

(defun cj/--delete-buffer-and-file ()
  "Kill the current buffer and delete the file it visits.
The unconfirmed workhorse behind `cj/delete-buffer-and-file'."
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename t)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defun cj/delete-buffer-and-file ()
  "Kill the current buffer and delete the file it visits.
Always confirms, naming the file -- deleting a file is the highest
blast-radius operation on this map.  The VC path is not double-prompted:
`vc-delete-file' asks on its own."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (cj/--delete-buffer-and-file)
        (when (yes-or-no-p (format "Delete file %s? " filename))
          (cj/--delete-buffer-and-file))))))

(defun cj/copy-link-to-buffer-file ()
  "Copy the full file:// path of the current buffer's source file to the kill ring."
  (interactive)
  (let ((file-path (buffer-file-name)))
    (when file-path
      (setq file-path (concat "file://" file-path))
      (kill-new file-path)
      (message "Copied file link to kill ring: %s" file-path))))

(defvar cj/buffer-source-functions
  '((eww-mode         . (lambda () (eww-current-url)))
    (elfeed-show-mode . (lambda () (elfeed-entry-link elfeed-show-entry)))
    (dired-mode       . (lambda () (dired-get-filename nil t)))
    (dirvish-mode     . (lambda () (dired-get-filename nil t)))
    (mu4e-view-mode   . (lambda ()
                          (when-let* ((msg (mu4e-message-at-point))
                                      (id (plist-get msg :message-id)))
                            (format "mu4e:msgid:%s" id))))
    (Info-mode        . (lambda ()
                          (when (and (boundp 'Info-current-file)
                                     (boundp 'Info-current-node)
                                     Info-current-file
                                     Info-current-node)
                            ;; Strip the compression suffix (via
                            ;; file-name-base) AND the .info suffix.
                            ;; "emacs.info.gz" -> base "emacs.info" ->
                            ;; manual "emacs".
                            (let* ((base (file-name-base Info-current-file))
                                   (manual (if (string-suffix-p ".info" base)
                                               (substring base 0 -5)
                                             base))
                                   (node Info-current-node))
                              (when (and (not (string-empty-p manual))
                                         (not (string-empty-p node)))
                                ;; Return the bracketed org link form so a
                                ;; paste into notes lands as a labeled,
                                ;; clickable link.  Label uses "(manual) Node"
                                ;; for grep-friendliness.
                                (format "[[info:(%s)%s][(%s) %s]]"
                                        manual node manual node)))))))
  "Alist mapping major-mode -> thunk returning the buffer's \"source\".

Each thunk is called with no arguments and should return a string
to be copied to the kill ring, or nil to fall through to
`buffer-file-name'.  Modes not listed here also fall through to
`buffer-file-name'.

Used by `cj/copy-buffer-source-as-kill' (`C-; b p').  Doc-view and
PDF-view modes intentionally aren't listed -- their
`buffer-file-name' already points at the underlying file, so the
fallback handles them.")

(defun cj/copy-buffer-source-as-kill ()
  "Copy the current buffer's \"source\" to the kill ring.

Source means the URL, file path, or other clickable reference that
identifies what the buffer represents.  Dispatches by `major-mode'
via `cj/buffer-source-functions'; falls back to `buffer-file-name'
for modes without a dispatch entry.

Signals `user-error' when no source can be determined."
  (interactive)
  (let* ((handler (alist-get major-mode cj/buffer-source-functions))
         (source (or (and handler (funcall handler))
                     (buffer-file-name))))
    (unless source
      (user-error "Buffer has no copyable source"))
    (kill-new source)
    (message "Copied: %s" source)
    source))

;; Backwards-compat alias.  The old name predates the dispatch
;; extension and several test files still reference it; keep the
;; alias so external callers and existing tests continue to work.
(defalias 'cj/copy-path-to-buffer-file-as-kill 'cj/copy-buffer-source-as-kill)

(defun cj/copy-whole-buffer ()
  "Copy the entire contents of the current buffer to the kill ring.
Point and mark are left exactly where they were.  No transient region
is created.  A message is displayed when done."
  (interactive)
  (let ((contents (buffer-substring-no-properties (point-min) (point-max))))
    (kill-new contents)
    (message "Buffer contents copied to kill ring")))

(defun cj/copy-to-bottom-of-buffer ()
  "Copy text from point to the end of the buffer to the kill ring.
Point and mark are left exactly where they were.  No transient region
is created.  A message is displayed when done."
  (interactive)
  (let ((contents (buffer-substring-no-properties (point) (point-max))))
    (kill-new contents)
    (message "Copied from point to end of buffer")))

(defun cj/copy-to-top-of-buffer ()
  "Copy text from the beginning of the buffer to point to the kill ring.
Point and mark are left exactly where they were.  No transient region
is created.  A message is displayed when done."
  (interactive)
  (let ((contents (buffer-substring-no-properties (point-min) (point))))
    (kill-new contents)
    (message "Copied from beginning of buffer to point")))

;; Confirmation policy for the destructive C-; b operations (erase, clear
;; to top/bottom, revert): confirm only when a file-visiting buffer has
;; unsaved edits -- destroying unsaved work is the hazard; an unmodified
;; buffer rereads from disk and a non-file buffer has nothing at stake
;; that undo can't restore.  Delete-file (D) always confirms.

(defun cj/--destructive-op-allowed-p (operation)
  "Return non-nil when the destructive OPERATION may proceed.
Prompts via `yes-or-no-p' only when the current buffer visits a file
and carries unsaved edits; otherwise allows silently.  OPERATION is a
short verb phrase for the prompt (e.g. \"erase it\")."
  (or (not (and buffer-file-name (buffer-modified-p)))
      (yes-or-no-p (format "Buffer %s has unsaved edits; %s anyway? "
                           (buffer-name) operation))))

(defun cj/clear-to-bottom-of-buffer ()
  "Delete all text from point to the end of the current buffer.
This does not save the deleted text in the kill ring.  Confirms first
when the buffer has unsaved edits."
  (interactive)
  (when (cj/--destructive-op-allowed-p "clear to the end")
    (delete-region (point) (point-max))
    (message "Buffer contents removed to the end of the buffer.")))

(defun cj/clear-to-top-of-buffer ()
  "Delete all text from point to the beginning of the current buffer.
Do not save the deleted text in the kill ring.  Confirms first when
the buffer has unsaved edits."
  (interactive)
  (when (cj/--destructive-op-allowed-p "clear to the beginning")
    (delete-region (point) (point-min))
    (message "Buffer contents removed to the beginning of the buffer.")))

(defun cj/erase-buffer ()
  "Erase the whole buffer, confirming first when it has unsaved edits."
  (interactive)
  (when (cj/--destructive-op-allowed-p "erase it")
    (erase-buffer)
    (message "Buffer erased.")))

(defun cj/revert-buffer ()
  "Revert the buffer from disk, confirming only when edits would be lost.
An unmodified buffer rereads silently (no data at risk), replacing the
stock `revert-buffer' prompt-every-time behavior on this map."
  (interactive)
  (unless buffer-file-name
    (user-error "Buffer %s is not visiting a file" (buffer-name)))
  (when (cj/--destructive-op-allowed-p "discard them and revert")
    (revert-buffer :ignore-auto :noconfirm)
    (message "Reverted %s" (buffer-name))))

(defun cj/copy-buffer-name ()
  "Copy current buffer name to kill ring."
  (interactive)
  (kill-new (buffer-name))
  (message "Copied: %s" (buffer-name)))

(declare-function ansi-color-apply-on-region "ansi-color")

(defvar cj/diff-context-lines 1
  "Lines of unchanged context shown around each change in buffer/file diffs.
Passed to difftastic's --context and diff's -U, so the diff view shows what
differs rather than pages of identical text.")

(defun cj/--difft-args (file1 file2 context width)
  "Build the difftastic argument list for FILE1 vs FILE2.
CONTEXT is the number of unchanged lines shown around each change; WIDTH is the
column budget for the side-by-side layout (difftastic cannot detect a terminal
when run as a subprocess and would otherwise wrap at 80)."
  (list "--color" "always"
        "--display" "side-by-side-show-both"
        "--context" (number-to-string context)
        "--width" (number-to-string width)
        file1 file2))

(defun cj/--unified-diff-args (file1 file2 context)
  "Build the diff(1) argument list for a unified diff of FILE1 vs FILE2.
CONTEXT is the number of unchanged lines shown around each change (-U)."
  (list (format "-U%d" context) file1 file2))

(defun cj/--diff-with-difftastic (file1 file2 buffer)
  "Run difftastic on FILE1 and FILE2, output to BUFFER.
Applies ANSI color, sets up special-mode for navigation, and leaves point on
the first hunk's content (past this header and difftastic's file header) so the
window opens on the first difference."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "Difftastic diff: %s (saved) vs buffer (modified)\n\n"
                      (file-name-nondirectory file1)))
      (apply #'call-process "difft" nil t nil
             (cj/--difft-args file1 file2 cj/diff-context-lines (frame-width)))
      (require 'ansi-color)
      (ansi-color-apply-on-region (point-min) (point-max))
      (special-mode)
      (goto-char (point-min))
      (forward-line 2)                       ; past this function's header
      (when (looking-at-p ".* --- ")         ; difftastic's own file header
        (forward-line 1))
      (while (and (eolp) (not (eobp)))       ; any blank separator lines
        (forward-line 1)))))

(defun cj/--diff-with-regular-diff (file1 file2 buffer)
  "Run regular unified diff on FILE1 and FILE2, output to BUFFER.
Sets up diff-mode for navigation and leaves point on the first @@ hunk so the
window opens on the first difference."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "Unified diff: %s (saved) vs buffer (modified)\n\n"
                      (file-name-nondirectory file1)))
      (apply #'call-process "diff" nil t nil
             (cj/--unified-diff-args file1 file2 cj/diff-context-lines))
      (diff-mode)
      (goto-char (point-min))
      (when (re-search-forward "^@@" nil t)
        (goto-char (match-beginning 0))))))

(defun cj/--diff-buffer-renderer (ws-only difft-available)
  "Choose the diff renderer symbol from WS-ONLY and DIFFT-AVAILABLE.
`whitespace' for a whitespace-only diff (a plain unified diff with trailing
whitespace highlighted, because difftastic treats it as no change and renders it
blank); otherwise `difftastic' when available, else `regular'."
  (cond (ws-only 'whitespace)
        (difft-available 'difftastic)
        (t 'regular)))

(defun cj/--diff-whitespace-only-p (file-a file-b)
  "Return non-nil if FILE-A and FILE-B differ ONLY in whitespace.
Route-1 detection via diff(1): true when a plain `diff' reports a difference but
`diff -w' (ignore all whitespace) reports none.  Identical files differ in
nothing, so they are not whitespace-only."
  (and (not (zerop (call-process "diff" nil nil nil "-q" file-a file-b)))
       (zerop (call-process "diff" nil nil nil "-q" "-w" file-a file-b))))

(defun cj/--buffer-differs-prompt-string (name ws-only-p)
  "Build the buffer-differs prompt question for buffer NAME.
When WS-ONLY-P is non-nil, fold a terse \"(whitespace only)\" parenthetical into
the question so the reader knows the difference is whitespace before choosing."
  (format "%s changed on disk%s"
          name (if ws-only-p " (whitespace only)" "")))

(defun cj/--buffer-differs-choices ()
  "Return the terse `read-multiple-choice' menu for the disk-changed save prompt.
Inline names are single words so the menu fits at a glance; the full meaning is
in each description (the ? help).  s overwrites the file with the buffer; r
discards the buffer's edits and rereads from disk; m resolves the two versions
side-by-side in ediff."
  '((?s "save"   "overwrite the file with this buffer")
    (?d "diff"   "review the diff; navigate and act from there")
    (?w "clean"  "clean whitespace and save")
    (?r "revert" "discard edits and reread from disk")
    (?m "merge"  "resolve side-by-side in ediff, then save from there")
    (?c "cancel" "leave the buffer as is")))

(defun cj/--buffer-changed-on-disk-p (buffer)
  "Return non-nil if BUFFER is modified AND its file changed on disk since visited.
This is the disk-changed conflict: there are unsaved edits to lose AND the file
underneath has moved, so a plain save would silently overwrite the disk version."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (and (buffer-modified-p)
           buffer-file-name
           (file-exists-p buffer-file-name)
           (not (verify-visited-file-modtime buffer))))))

(defun cj/--buffer-differs-action (key)
  "Map a disk-changed-prompt KEY to an action symbol, or nil when unmapped.
`save' overwrites the file, `clean-save' cleans whitespace then saves, `revert'
rereads from disk, `merge' resolves in ediff, `cancel' does nothing, and `diff'
peeks (the caller re-prompts)."
  (pcase key
    (?s 'save)
    (?w 'clean-save)
    (?r 'revert)
    (?m 'merge)
    (?d 'diff)
    (?c 'cancel)))

(declare-function ediff-current-file "ediff")

(defun cj/--buffer-differs-dispatch (buffer action)
  "Carry out ACTION for BUFFER after a disk-changed prompt.
`save' overwrites the file with the buffer; `clean-save' strips trailing
whitespace first; `revert' discards the buffer's edits and rereads the disk;
`merge' launches `ediff-current-file' to resolve the buffer against the disk
version hunk-by-hunk (the file's modtime is deliberately NOT marked as seen, so
the save after merging re-asks once -- an abandoned merge can never silently
overwrite the disk); `cancel' leaves the buffer untouched.  Save updates the
recorded modtime first so the stock `save-buffer' does not re-ask its own
\"changed on disk\" question."
  (with-current-buffer buffer
    (pcase action
      ('save (set-visited-file-modtime) (save-buffer))
      ('clean-save (delete-trailing-whitespace) (set-visited-file-modtime) (save-buffer))
      ('revert (revert-buffer t t))
      ('merge (ediff-current-file)
              (message "Merge in ediff, quit it, then C-x C-s and choose save"))
      ('cancel (message "Save cancelled; buffer left as is"))
      (_ nil))))

(defvar cj/--diff-review-choice nil
  "Key chosen inside the diff review, or nil when the review went back to the menu.")

(declare-function diff-hunk-next "diff-mode")
(declare-function diff-hunk-prev "diff-mode")

(defun cj/--diff-section-next ()
  "Move to the next hunk in the diff buffer.
Uses diff-mode's hunk motion when available; otherwise the next
blank-line-separated section (difftastic output)."
  (interactive)
  (if (derived-mode-p 'diff-mode)
      (diff-hunk-next)
    (forward-paragraph)
    (skip-chars-forward "\n")))

(defun cj/--diff-section-prev ()
  "Move to the previous hunk in the diff buffer.
Uses diff-mode's hunk motion when available; otherwise the previous
blank-line-separated section (difftastic output)."
  (interactive)
  (if (derived-mode-p 'diff-mode)
      (diff-hunk-prev)
    (skip-chars-backward "\n")
    (backward-paragraph)
    (skip-chars-forward "\n")))

(defun cj/--diff-review-choose (key)
  "Record KEY as the review's choice and exit the review."
  (setq cj/--diff-review-choice key)
  (exit-recursive-edit))

(defun cj/--diff-review-back ()
  "Leave the diff review without a choice; the menu prompt returns."
  (interactive)
  (exit-recursive-edit))

(defun cj/--diff-review-keymap (choices)
  "Build the diff-review keymap from CHOICES (a `read-multiple-choice' list).
Every menu key except d acts directly from the review.  TAB / S-TAB (and n / p
when the menu doesn't claim them) move between hunks.  ESC always goes back to
the menu prompt; q does too unless the menu claims q (the save-some loop)."
  (let ((map (make-sparse-keymap)))
    (dolist (entry choices)
      (let ((key (car entry)))
        (unless (eq key ?d)
          (define-key map (vector key)
                      (lambda () (interactive) (cj/--diff-review-choose key))))))
    (unless (assq ?q choices)
      (define-key map "q" #'cj/--diff-review-back))
    (unless (assq ?n choices)
      (define-key map "n" #'cj/--diff-section-next))
    (unless (assq ?p choices)
      (define-key map "p" #'cj/--diff-section-prev))
    (define-key map (kbd "<escape>") #'cj/--diff-review-back)
    (define-key map (kbd "TAB") #'cj/--diff-section-next)
    (define-key map (kbd "<backtab>") #'cj/--diff-section-prev)
    map))

(defun cj/--diff-review-hint (choices)
  "One-line echo hint for the review built from CHOICES: actions, motion, exit."
  (concat
   (mapconcat (lambda (e) (format "%c:%s" (car e) (nth 1 e)))
              (seq-remove (lambda (e) (eq (car e) ?d)) choices)
              "  ")
   "  TAB:hunks"
   (if (assq ?q choices) "  ESC:menu" "  q:menu")))

(defun cj/--diff-review (diff-buffer choices)
  "Modal review of DIFF-BUFFER: navigate the diff, act with the menu keys.
Selects DIFF-BUFFER's window and enters a recursive edit with the keymap from
CHOICES layered on top, so point motion (arrows, scrolling, search) works while
every menu key acts immediately.  Returns the chosen key, or nil when the user
went back to the menu (q, ESC, or an aborted recursive edit).  Returns nil
untouched when the buffer has no window to review in."
  (let ((win (and (buffer-live-p diff-buffer) (get-buffer-window diff-buffer))))
    (when win
      (setq cj/--diff-review-choice nil)
      (with-selected-window win
        (let ((exit-fn (set-transient-map (cj/--diff-review-keymap choices)
                                          (lambda () t))))
          (unwind-protect
              (progn
                (message "%s" (cj/--diff-review-hint choices))
                (condition-case nil (recursive-edit) (quit nil)))
            (funcall exit-fn))))
      cj/--diff-review-choice)))

(defun cj/--read-choice-with-diff (prompt choices show-diff-fn)
  "Read a `read-multiple-choice' key for PROMPT and CHOICES; d reviews the diff.
SHOW-DIFF-FN displays the buffer/file diff and returns its buffer.  d shows the
diff and enters a navigable review (`cj/--diff-review') where every other menu
key acts directly; leaving the review (q, ESC) returns to this prompt.  A
terminating choice -- made at the prompt or inside the review -- closes a
still-open diff window before it is returned, so the diff never lingers after
the decision is made."
  (let ((key nil) (diff-buf nil))
    (while (not key)
      (let ((k (car (read-multiple-choice prompt choices))))
        (if (not (eq k ?d))
            (setq key k)
          (unless (and (buffer-live-p diff-buf) (get-buffer-window diff-buf))
            (setq diff-buf (funcall show-diff-fn)))
          (when diff-buf
            (setq key (cj/--diff-review diff-buf choices))))))
    (let ((win (and (buffer-live-p diff-buf) (get-buffer-window diff-buf))))
      (when win (quit-window nil win)))
    key))

(defun cj/--buffer-differs-read-key (buffer ws-only)
  "Read a disk-changed-prompt key for BUFFER via `read-multiple-choice'.
WS-ONLY non-nil folds a terse \"(whitespace only)\" note into the prompt.  d
opens the buffer/file diff in a navigable review where the menu keys act
directly; a terminating choice closes a still-open diff."
  (cj/--read-choice-with-diff
   (cj/--buffer-differs-prompt-string (buffer-name buffer) ws-only)
   (cj/--buffer-differs-choices)
   (lambda () (with-current-buffer buffer (cj/diff-buffer-with-file)))))

(defun cj/save-buffer (&optional arg)
  "Save the current buffer; show a legible menu when the file changed on disk.
A normal save falls straight through to `save-buffer' (ARG, the prefix argument,
is passed along so \\[universal-argument] \\[save-buffer] still marks for backup).
When the buffer has unsaved edits AND the file changed on disk since it was
visited, offer a terse labeled menu -- save / diff / clean / revert / merge /
cancel -- instead of the stock yes/no \"Save anyway?\" prompt.  d opens a
navigable diff review (arrows and TAB move, the menu keys act from inside); m
resolves the two versions in ediff.  Bound to \\`C-x C-s'."
  (interactive "P")
  (if (not (cj/--buffer-changed-on-disk-p (current-buffer)))
      (save-buffer arg)
    (let* ((buf (current-buffer))
           (ws-only (cj/--buffer-file-whitespace-only-p buf))
           (key (cj/--buffer-differs-read-key buf ws-only)))
      (cj/--buffer-differs-dispatch buf (cj/--buffer-differs-action key)))))

(defun cj/--save-some-buffers-action (key)
  "Map a save-loop KEY to (THIS-ACTION . LOOP-EFFECT), or nil when unmapped.
THIS-ACTION is `save', `clean-save', `skip', or `diff'.  LOOP-EFFECT is
`continue' (keep prompting), `save-rest' (save this and all remaining without
asking), `stop' (act on this, skip the rest), or `reprompt' (peek, then ask the
same buffer again)."
  (pcase key
    (?y '(save . continue))
    (?n '(skip . continue))
    (?w '(clean-save . continue))
    (?! '(save . save-rest))
    (?. '(save . stop))
    (?q '(skip . stop))
    (?d '(diff . reprompt))))

(defun cj/--save-some-buffers-choices ()
  "Return the terse `read-multiple-choice' choices for the save loop.
Single-word inline names keep the menu to the minimum space; the full meaning is
in each description (the ? help)."
  '((?y "save"   "save this buffer")
    (?n "skip"   "do not save this buffer")
    (?w "clean"  "clean whitespace and save this buffer")
    (?d "diff"   "review the diff; navigate and act from there")
    (?! "all"    "save this and all remaining buffers")
    (?. "only"   "save this buffer, then skip the rest")
    (?q "none"   "stop; save no more buffers")))

(defun cj/--buffer-file-whitespace-only-p (buffer)
  "Return non-nil if BUFFER's text differs from its visited file ONLY in whitespace.
Writes the buffer to a temp file and reuses `cj/--diff-whitespace-only-p'.  Nil
when BUFFER visits no file or the file is gone."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((file (buffer-file-name)))
        (when (and file (file-exists-p file))
          (let ((temp (make-temp-file "cbf-ws-buf-" nil
                                      (or (file-name-extension file t) "")))
                (content (buffer-string)))
            (unwind-protect
                (progn (with-temp-file temp (insert content))
                       (cj/--diff-whitespace-only-p file temp))
              (when (file-exists-p temp) (delete-file temp)))))))))

(defun cj/--save-some-buffers-plan (buffers key-fn)
  "Resolve each buffer in BUFFERS to a per-buffer action using KEY-FN.
KEY-FN is called with a buffer and returns a `read-multiple-choice' key; the diff
re-prompt is the caller's concern, so KEY-FN never returns ?d.  Returns a list of
\(BUFFER . ACTION) where ACTION is `save', `clean-save', or `skip', honoring
`save-rest' (! saves this and all remaining) and `stop' (./q act on this, then
skip the rest).  KEY-FN is not consulted once a buffer triggers save-rest or stop."
  (let ((plan nil) (mode 'ask))
    (dolist (buf buffers (nreverse plan))
      (pcase mode
        ('save-all (push (cons buf 'save) plan))
        ('done (push (cons buf 'skip) plan))
        ('ask
         (pcase (cj/--save-some-buffers-action (funcall key-fn buf))
           (`(,act . save-rest) (push (cons buf act) plan) (setq mode 'save-all))
           (`(,act . stop)      (push (cons buf act) plan) (setq mode 'done))
           (`(,act . ,_)        (push (cons buf act) plan))
           (_                   (push (cons buf 'skip) plan))))))))

(declare-function files--buffers-needing-to-be-saved "files" (pred))

(defun cj/--save-some-buffers-read-key (buffer ws-only)
  "Read a save-loop key for BUFFER via `read-multiple-choice'.
WS-ONLY non-nil folds a terse \"(whitespace only)\" note into the prompt.  d
opens the buffer/file diff in a navigable review where the menu keys act
directly; a terminating choice closes a still-open diff."
  (cj/--read-choice-with-diff
   (format "Save %s%s"
           (if (buffer-file-name buffer)
               (file-name-nondirectory (buffer-file-name buffer))
             (buffer-name buffer))
           (if ws-only " (whitespace only)" ""))
   (cj/--save-some-buffers-choices)
   (lambda () (with-current-buffer buffer (cj/diff-buffer-with-file)))))

(defun cj/--save-some-buffers-execute (plan)
  "Carry out PLAN, a list of (BUFFER . ACTION); return the number saved.
ACTION `clean-save' deletes trailing whitespace before saving; `save' saves as-is;
`skip' leaves the buffer alone."
  (let ((n 0))
    (dolist (entry plan n)
      (let ((buffer (car entry)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (pcase (cdr entry)
              ('clean-save (delete-trailing-whitespace) (save-buffer) (setq n (1+ n)))
              ('save (save-buffer) (setq n (1+ n)))
              (_ nil))))))))

(defun cj/save-some-buffers (&optional arg pred)
  "Save modified buffers with a legible, labeled prompt per buffer.
A `read-multiple-choice' replacement for `save-some-buffers': the options are
shown on screen rather than recalled as keys, with an added clean-whitespace-and-
save action and a per-buffer \"(whitespace only)\" note.  ARG and PRED match
`save-some-buffers' -- ARG non-nil saves all without asking; PRED selects which
buffers are considered.  Installed over `save-some-buffers' by advice, so \\[save-some-buffers]
and the save-on-exit prompt both use it."
  (interactive "P")
  (unless pred
    (setq pred
          (if (and (symbolp save-some-buffers-default-predicate)
                   (get save-some-buffers-default-predicate
                        'save-some-buffers-function))
              (funcall save-some-buffers-default-predicate)
            save-some-buffers-default-predicate)))
  (let (queried autosaved-buffers files-done inhibit-message)
    (save-window-excursion
      ;; Save buffers flagged for unconditional save first (mirrors the original).
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (and buffer-save-without-query (buffer-modified-p))
            (push (buffer-name) autosaved-buffers)
            (save-buffer))))
      (let* ((candidates (files--buffers-needing-to-be-saved pred))
             (plan (if arg
                       (mapcar (lambda (b) (cons b 'save)) candidates)
                     (when candidates (setq queried t))
                     (cj/--save-some-buffers-plan
                      candidates
                      (lambda (b)
                        (cj/--save-some-buffers-read-key
                         b (cj/--buffer-file-whitespace-only-p b)))))))
        (setq files-done (cj/--save-some-buffers-execute plan)))
      ;; Let other things (abbrevs, etc.) save at this point.
      (dolist (func save-some-buffers-functions)
        (setq inhibit-message (or (funcall func nil arg) inhibit-message)))
      (or queried (> files-done 0) inhibit-message
          (cond
           ((null autosaved-buffers)
            (when (called-interactively-p 'any)
              (message "(No files need saving)")))
           ((= (length autosaved-buffers) 1)
            (message "(Saved %s)" (car autosaved-buffers)))
           (t (message "(Saved %d files: %s)" (length autosaved-buffers)
                       (mapconcat #'identity autosaved-buffers ", "))))))
    files-done))

(advice-add 'save-some-buffers :override #'cj/save-some-buffers)
(keymap-global-set "C-x C-s" #'cj/save-buffer)

(defun cj/diff-buffer-with-file ()
  "Compare the current modified buffer with the saved version.
Uses difftastic if available for syntax-aware diffing, otherwise
falls back to regular unified diff.  Shows output in a separate buffer.
Signals an error if the buffer is not visiting a file."
  (interactive)
  (unless (buffer-file-name)
    (user-error "Current buffer is not visiting a file"))
  (let* ((file (buffer-file-name))
         (file-ext (file-name-extension file t))  ; includes the dot
         (temp-file (make-temp-file "buffer-diff-" nil file-ext))
         (buffer-content (buffer-string)))  ; Capture BEFORE with-temp-file!
    (unwind-protect
        (progn
          ;; Write current buffer content to temp file
          (with-temp-file temp-file
            (insert buffer-content))
          ;; Check if there are any differences first
          (if (zerop (call-process "diff" nil nil nil "-q" file temp-file))
              (progn (message "No differences between buffer and file") nil)
            ;; Pick a renderer: difftastic for content diffs, but a plain unified
            ;; diff with trailing whitespace highlighted for whitespace-only ones
            ;; (difftastic treats trailing whitespace as no change and hides it).
            (let* ((renderer (cj/--diff-buffer-renderer
                              (cj/--diff-whitespace-only-p file temp-file)
                              (cj/executable-exists-p "difft")))
                   (buffer-name (if (eq renderer 'difftastic)
                                    "*Diff (difftastic)*"
                                  "*Diff (unified)*"))
                   (diff-buffer (get-buffer-create buffer-name)))
              (if (eq renderer 'difftastic)
                  (cj/--diff-with-difftastic file temp-file diff-buffer)
                (cj/--diff-with-regular-diff file temp-file diff-buffer)
                (when (eq renderer 'whitespace)
                  (with-current-buffer diff-buffer
                    (setq-local show-trailing-whitespace t))))
              (display-buffer diff-buffer)
              ;; Return the diff buffer so callers (the save prompts) can toggle
              ;; and auto-close its window.
              diff-buffer)))
      ;; Clean up temp file
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(defun cj/view-buffer-in-eww ()
  "Render the current buffer's file in EWW (Emacs Web Wowser).

Opens the file associated with the current buffer in EWW for rendered
viewing.  Useful for previewing HTML, XML, or other markup files with
proper formatting instead of viewing raw source.

Bound to \\`C-; b w'.

Signals an error if the buffer is not visiting a file."
  (interactive)
  (if buffer-file-name
      (eww-open-file buffer-file-name)
    (user-error "Buffer is not visiting a file")))

(defun cj/--email-handle-is-type-p (handle type)
  "Return non-nil if HANDLE is a MIME part of TYPE (e.g., \"text/html\").
TYPE matching is a prefix match, so \"text/html\" matches
\"text/html; charset=utf-8\"."
  (when (and handle (listp handle))
    (let ((content-type (mm-handle-type handle)))
      (and content-type
           (listp content-type)
           (stringp (car content-type))
           (string-prefix-p type (car content-type))))))

(defun cj/--email-find-displayable-part (handle)
  "Find a displayable part (text/html or text/plain) in HANDLE.
Prefers text/html over text/plain.  HANDLE can be a leaf handle or
a multipart structure.  Returns the handle for the displayable part, or nil."
  (cond
   ;; Leaf handle that's HTML
   ((cj/--email-handle-is-type-p handle "text/html")
    handle)
   ;; Leaf handle that's plain text - save it but keep looking for HTML
   ((cj/--email-handle-is-type-p handle "text/plain")
    handle)
   ;; Multipart - search children
   ((and (listp handle) (listp (car handle)))
    (let ((html-part nil)
          (text-part nil))
      (dolist (part handle)
        (when (listp part)
          (let ((found (cj/--email-find-displayable-part part)))
            (when found
              (if (cj/--email-handle-is-type-p found "text/html")
                  (setq html-part found)
                (unless html-part
                  (setq text-part found)))))))
      (or html-part text-part)))
   ;; Multipart container (string content-type as car)
   ((and (listp handle) (stringp (car handle)))
    ;; This is a multipart with type info - search the cdr
    (cj/--email-find-displayable-part (cdr handle)))
   (t nil)))

(defun cj/view-email-in-buffer ()
  "Render an .eml email file with proper MIME decoding.

Parses the MIME structure of the current buffer's .eml file, extracts
the text/html part (falling back to text/plain if no HTML), and renders
it using shr (Simple HTML Renderer) in a dedicated buffer.

The rendered email is displayed in a buffer named \"*Email: <filename>*\"
in `special-mode' for easy navigation and dismissal with \\`q'.

Bound to \\`C-; b E'.

Signals an error if:
- The buffer is not visiting a file
- No displayable content (text/html or text/plain) is found"
  (interactive)
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  (require 'mm-decode)
  (require 'shr)
  (let* ((handle (mm-dissect-buffer t))
         (displayable-part (cj/--email-find-displayable-part handle))
         (buffer-name (format "*Email: %s*" (file-name-nondirectory buffer-file-name))))
    ;; `mm-dissect-buffer' allocates handles that must be freed even when we
    ;; bail out early (no displayable part), so destroy them from the cleanup
    ;; form rather than after the body.
    (unwind-protect
        (progn
          (unless displayable-part
            (user-error "No displayable content found in email"))
          (with-current-buffer (get-buffer-create buffer-name)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (mm-insert-part displayable-part)
              (goto-char (point-min))
              (when (cj/--email-handle-is-type-p displayable-part "text/html")
                (shr-render-region (point-min) (point-max)))
              (goto-char (point-min))
              (special-mode)))
          (switch-to-buffer buffer-name))
      (mm-destroy-parts handle))))

;; --------------------------- Buffer And File Keymap --------------------------

;; Copy buffer content sub-keymap
(defvar-keymap cj/copy-buffer-content-map
  :doc "Keymap for copy buffer content operations."
  "w" #'cj/copy-whole-buffer
  "b" #'cj/copy-to-bottom-of-buffer
  "t" #'cj/copy-to-top-of-buffer)

;; Buffer & file operations prefix and keymap
(defvar-keymap cj/buffer-and-file-map
  :doc "Keymap for buffer and file operations."
  "m" #'cj/move-buffer-and-file
  "r" #'cj/rename-buffer-and-file
  "p" #'cj/copy-buffer-source-as-kill
  "d" #'cj/diff-buffer-with-file
  "D" #'cj/delete-buffer-and-file
  "c" cj/copy-buffer-content-map
  "n" #'cj/copy-buffer-name
  "l" #'cj/copy-link-to-buffer-file
  "k" #'cj/kill-buffer-and-window
  "K" #'cj/kill-other-window-buffer
  "P" #'cj/print-buffer-ps
  "t" #'cj/clear-to-top-of-buffer
  "b" #'cj/clear-to-bottom-of-buffer
  "x" #'cj/erase-buffer
  "s" #'mark-whole-buffer
  "S" #'write-file ;; save as
  "g" #'cj/revert-buffer
  "e" #'eval-buffer
  "w" #'cj/view-buffer-in-eww
  "E" #'cj/view-email-in-buffer
  "o" #'cj/xdg-open
  "O" #'cj/open-this-file-with
  ;; Window resize (cj/window-resize-sticky in ui-navigation.el, on `windsize'):
  ;; the arrow moves the active window's divider that way, then bare arrows
  ;; keep nudging until any other key.
  "<left>"  #'cj/window-resize-sticky
  "<right>" #'cj/window-resize-sticky
  "<up>"    #'cj/window-resize-sticky
  "<down>"  #'cj/window-resize-sticky)
(cj/register-prefix-map "b" cj/buffer-and-file-map)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-; b" "buffer and file menu"
    "C-; b m" "move file"
    "C-; b r" "rename file"
    "C-; b p" "copy buffer source"
    "C-; b d" "diff buffer with file"
    "C-; b D" "delete file"
    "C-; b c" "buffer copy menu"
    "C-; b c w" "copy whole buffer"
    "C-; b c b" "copy to bottom"
    "C-; b c t" "copy to top"
    "C-; b n" "copy buffer name"
    "C-; b l" "copy file link"
    "C-; b k" "kill buffer and window"
    "C-; b K" "kill other window's buffer"
    "C-; b P" "print to PS"
    "C-; b t" "clear to top"
    "C-; b b" "clear to bottom"
    "C-; b x" "erase buffer"
    "C-; b s" "select whole buffer"
    "C-; b S" "save as"
    "C-; b g" "revert buffer"
    "C-; b e" "eval buffer"
    "C-; b w" "view in EWW"
    "C-; b E" "view email"
    "C-; b o" "open with default app"
    "C-; b O" "open with program..."
    "C-; b <left>"  "resize divider left"
    "C-; b <right>" "resize divider right"
    "C-; b <up>"    "resize divider up"
    "C-; b <down>"  "resize divider down"))


;; --- previous-buffer toggle (formerly in custom-misc.el) ---
(defun cj/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(cj/register-command "SPC" #'cj/switch-to-previous-buffer "prev buffer")

(provide 'custom-buffer-file)
;;; custom-buffer-file.el ends here.
