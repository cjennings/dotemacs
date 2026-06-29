;;; video-audio-recording-devices.el --- PulseAudio device discovery for recording -*- lexical-binding: t; coding: utf-8; -*-

;; Author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 4 (Optional).
;; Category: D.
;; Load shape: library.
;; Top-level side effects: none (defuns only).
;; Runtime requires: subr-x, seq.
;; Direct test load: yes.
;;
;; Base layer of video-audio-recording: PulseAudio source and sink
;; discovery, the pactl output parsers, device labeling and sort/status
;; helpers for completing-read, and the lookup predicates used to validate
;; a configured device.  Pure string and shell-query helpers with no
;; dependency on recording state, configuration, or the capture engine, so
;; the engine and command layers build on it.

;;; Code:

(require 'subr-x)
(require 'seq)

;;; PulseAudio Source/Sink Parsing

(defun cj/recording--parse-pactl-output (output)
  "Parse pactl sources OUTPUT into structured list.
Returns list of (device-name driver state) tuples.
Extracted as a separate function for testability."
  (let ((sources nil))
    (dolist (line (split-string output "\n" t))
      (when (string-match "^[0-9]+\t\\([^\t]+\\)\t\\([^\t]+\\)\t\\([^\t]+\\)\t\\([^\t]+\\)" line)
        (let ((device (match-string 1 line))
              (driver (match-string 2 line))
              (state (match-string 4 line)))
          (push (list device driver state) sources))))
    (nreverse sources)))

(defun cj/recording-parse-sources ()
  "Parse pactl sources output into structured list.
Returns list of (device-name driver state) tuples."
  (cj/recording--parse-pactl-output
   (shell-command-to-string "pactl list sources short 2>/dev/null")))

(defun cj/recording-friendly-state (state)
  "Convert technical STATE name to user-friendly label.
STATE is the raw state from pactl (SUSPENDED, RUNNING, IDLE, etc.)."
  (pcase state
    ("SUSPENDED" "Ready")
    ("RUNNING" "Active")
    ("IDLE" "Ready")
    (_ state)))

(defun cj/recording--get-default-sink-monitor ()
  "Return the PulseAudio monitor source for the default audio output.
The monitor source captures whatever is playing through the default sink
(music, calls, system sounds, etc.).  This is the correct device
for capturing \"what I hear\" regardless of which output hardware is active."
  (let ((default-sink (string-trim
                       (shell-command-to-string
                        "pactl get-default-sink 2>/dev/null"))))
    (if (string-empty-p default-sink)
        (user-error "No default audio output found.  Is PulseAudio/PipeWire running?")
      (concat default-sink ".monitor"))))

(defun cj/recording--parse-pactl-verbose (output record-type)
  "Parse verbose pactl OUTPUT into structured list.
RECORD-TYPE is \"Source\" or \"Sink\" — the record header in pactl output.
Returns list of (name description mute state) tuples."
  (let ((entries nil)
        (header-re (concat "^" record-type " #"))
        (current-name nil)
        (current-desc nil)
        (current-mute nil)
        (current-state nil))
    (dolist (line (split-string output "\n"))
      (cond
       ((string-match-p header-re line)
        (when current-name
          (push (list current-name current-desc current-mute current-state)
                entries))
        (setq current-name nil current-desc nil
              current-mute nil current-state nil))
       ((string-match "^\\s-+Name:\\s-+\\(.+\\)" line)
        (setq current-name (match-string 1 line)))
       ((string-match "^\\s-+Description:\\s-+\\(.+\\)" line)
        (setq current-desc (match-string 1 line)))
       ((string-match "^\\s-+Mute:\\s-+\\(.+\\)" line)
        (setq current-mute (match-string 1 line)))
       ((string-match "^\\s-+State:\\s-+\\(.+\\)" line)
        (setq current-state (match-string 1 line)))))
    (when current-name
      (push (list current-name current-desc current-mute current-state)
            entries))
    (nreverse entries)))

(defun cj/recording--get-available-mics ()
  "Return available microphone sources as list of (name description state mute).
Filters out monitor sources but includes muted devices (shown with
a [muted] label in the UI).  Uses the friendly description from
PulseAudio (e.g. \"Jabra SPEAK 510 Mono\") rather than the raw
device name.  State is the PulseAudio state string (RUNNING, IDLE,
or SUSPENDED).  Mute is \"yes\" or \"no\"."
  (let* ((output (shell-command-to-string "pactl list sources 2>/dev/null"))
         (sources (cj/recording--parse-pactl-verbose output "Source"))
         (mics nil))
    (dolist (source sources)
      (let ((name (nth 0 source))
            (desc (nth 1 source))
            (mute (nth 2 source))
            (state (nth 3 source)))
        (when (not (string-match-p "\\.monitor$" name))
          (push (list name (or desc name) state mute) mics))))
    (nreverse mics)))

(defun cj/recording--get-available-sinks ()
  "Return available audio sinks as list of (name description state mute).
Includes muted sinks (shown with a [muted] label in the UI).  Uses
the friendly description from PulseAudio (e.g. \"JDS Labs Element IV
Analog Stereo\").  State is the PulseAudio state string (RUNNING,
IDLE, or SUSPENDED).  Mute is \"yes\" or \"no\"."
  (let* ((output (shell-command-to-string "pactl list sinks 2>/dev/null"))
         (sinks (cj/recording--parse-pactl-verbose output "Sink"))
         (result nil))
    (dolist (sink sinks)
      (let ((name (nth 0 sink))
            (desc (nth 1 sink))
            (mute (nth 2 sink))
            (state (nth 3 sink)))
        (push (list name (or desc name) state mute) result)))
    (nreverse result)))

(defun cj/recording--get-sink-apps ()
  "Return alist mapping sink index to list of application names.
Parses `pactl list sink-inputs' to find which apps are playing
audio through each sink."
  (let ((output (shell-command-to-string "pactl list sink-inputs 2>/dev/null"))
        (apps (make-hash-table :test 'equal))
        (current-sink nil))
    (dolist (line (split-string output "\n"))
      (cond
       ((string-match "^Sink Input #" line)
        (setq current-sink nil))
       ((string-match "^[ \t]+Sink:[ \t]+\\([0-9]+\\)" line)
        (setq current-sink (match-string 1 line)))
       ((and current-sink
             (string-match "application\\.name = \"\\([^\"]+\\)\"" line))
        (let ((existing (gethash current-sink apps)))
          (unless (member (match-string 1 line) existing)
            (puthash current-sink
                     (append existing (list (match-string 1 line)))
                     apps))))))
    ;; Convert hash to alist
    (let ((result nil))
      (maphash (lambda (k v) (push (cons k v) result)) apps)
      result)))

;;; Device Lookups

(defun cj/recording--get-sink-index (sink-name sinks-output)
  "Return the numeric index of SINK-NAME from SINKS-OUTPUT.
SINKS-OUTPUT should be the output of `pactl list sinks short'.
Returns the index as a string, or nil if not found."
  (let ((index nil))
    (dolist (line (split-string sinks-output "\n" t))
      (when (string-match "^\\([0-9]+\\)\t\\([^\t]+\\)\t" line)
        (when (equal sink-name (match-string 2 line))
          (setq index (match-string 1 line)))))
    index))

(defun cj/recording--source-exists-p (source-name pactl-output)
  "Return non-nil if SOURCE-NAME exists in PACTL-OUTPUT.
PACTL-OUTPUT should be the output of `pactl list sources short'."
  (let ((found nil))
    (dolist (line (split-string pactl-output "\n" t))
      (when (string-match "^[0-9]+\t\\([^\t]+\\)\t" line)
        (when (equal source-name (match-string 1 line))
          (setq found t))))
    found))

(defun cj/recording--sink-has-active-audio-p (sink-index pactl-output)
  "Return non-nil if SINK-INDEX has active audio streams.
PACTL-OUTPUT should be the output of `pactl list sink-inputs'.
SINK-INDEX is the numeric sink index as a string."
  (let ((found nil)
        (lines (split-string pactl-output "\n")))
    (dolist (line lines)
      (when (string-match "^[ \t]+Sink:[ \t]+\\([0-9]+\\)" line)
        (when (equal sink-index (match-string 1 line))
          (setq found t))))
    found))

;;; Device Labeling and Selection Primitives

(defun cj/recording--device-sort-key (state muted)
  "Return a numeric sort key for a device with STATE and MUTED flag.
Lower values sort first: RUNNING (0) → IDLE (1) → SUSPENDED (2) → muted (3)."
  (if (equal muted "yes")
      3
    (pcase (upcase (or state ""))
      ("RUNNING" 0)
      ("IDLE" 1)
      (_ 2))))

(defun cj/recording--device-status-label (state muted)
  "Return a human-readable status label for a device.
MUTED is \"yes\" or \"no\".  STATE is the PulseAudio state string."
  (if (equal muted "yes")
      "[muted]"
    (pcase (upcase (or state ""))
      ("RUNNING" "[in use]")
      ("IDLE"    "[ready]")
      (_         "[available]"))))

(defun cj/recording--label-devices (devices)
  "Build labeled (label . name) alist from DEVICES for `completing-read'.
DEVICES is a list of (name description state mute) as returned by
`cj/recording--get-available-mics' or `cj/recording--get-available-sinks'.
Labels are formatted as \"Description [in use]\" etc.
Sorted: in use → ready → available → muted."
  (let* ((labeled (mapcar
                   (lambda (dev)
                     (let* ((name  (nth 0 dev))
                            (desc  (nth 1 dev))
                            (state (nth 2 dev))
                            (muted (nth 3 dev))
                            (label (concat desc " "
                                           (cj/recording--device-status-label state muted))))
                       (list label name (cj/recording--device-sort-key state muted))))
                   devices))
         (sorted (sort labeled (lambda (a b) (< (nth 2 a) (nth 2 b))))))
    (mapcar (lambda (entry) (cons (nth 0 entry) (nth 1 entry))) sorted)))

(defun cj/recording--label-sinks (sinks)
  "Build labeled (label . name) alist from SINKS for `completing-read'.
Like `cj/recording--label-devices' but also appends application names
for sinks with active audio streams.  E.g. \"JDS Labs [in use] (Firefox)\"."
  (let* ((sink-apps (cj/recording--get-sink-apps))
         (sinks-short (shell-command-to-string "pactl list sinks short 2>/dev/null"))
         (labeled
          (mapcar
           (lambda (dev)
             (let* ((name  (nth 0 dev))
                    (desc  (nth 1 dev))
                    (state (nth 2 dev))
                    (muted (nth 3 dev))
                    (index (cj/recording--get-sink-index name sinks-short))
                    (apps  (and index (cdr (assoc index sink-apps))))
                    (status (cj/recording--device-status-label state muted))
                    (app-str (if apps (concat " (" (string-join apps ", ") ")") ""))
                    (label (concat desc " " status app-str)))
               (list label name (cj/recording--device-sort-key state muted))))
           sinks))
         (sorted (sort labeled (lambda (a b) (< (nth 2 a) (nth 2 b))))))
    (mapcar (lambda (entry) (cons (nth 0 entry) (nth 1 entry))) sorted)))

(defun cj/recording--select-from-labeled (prompt entries)
  "Prompt user with PROMPT to select from labeled ENTRIES.
ENTRIES is an alist of (label . device-name).  Appends a Cancel option.
Returns the selected device name, or signals user-error if cancelled."
  (let* ((alist (append entries '(("Cancel" . nil))))
         (choice (completing-read prompt
                                  (lambda (string pred action)
                                    (if (eq action 'metadata)
                                        '(metadata (display-sort-function . identity))
                                      (complete-with-action action alist string pred)))
                                  nil t))
         (device (cdr (assoc choice alist))))
    (unless device
      (user-error "Device setup cancelled"))
    device))

(defun cj/recording-group-devices-by-hardware ()
  "Group audio sources by physical hardware device.
Returns alist of (friendly-name . (mic-source . monitor-source)).
Only includes devices that have BOTH a mic and a monitor source,
since recording needs both to capture your voice and system audio."
  (let ((sources (cj/recording-parse-sources))
        (devices (make-hash-table :test 'equal))
        (result nil))
    ;; Group sources by base device name (hardware identifier)
    (dolist (source sources)
      (let* ((device (nth 0 source))
             ;; Extract hardware ID — the unique part identifying the physical device.
             ;; Different device types use different naming conventions in PulseAudio.
             (base-name (cond
                         ;; USB devices: extract usb-XXXXX-XX part
                         ((string-match "\\.\\(usb-[^.]+\\-[0-9]+\\)\\." device)
                          (match-string 1 device))
                         ;; Built-in (PCI) devices: extract pci-XXXXX part
                         ((string-match "\\.\\(pci-[^.]+\\)\\." device)
                          (match-string 1 device))
                         ;; Bluetooth devices: extract and normalize MAC address
                         ;; (input uses colons, output uses underscores)
                         ((string-match "bluez_\\(?:input\\|output\\)\\.\\([^.]+\\)" device)
                          (replace-regexp-in-string "_" ":" (match-string 1 device)))
                         (t device)))
             (is-monitor (string-match-p "\\.monitor$" device))
             (device-entry (gethash base-name devices)))
        (unless device-entry
          (setf device-entry (cons nil nil))
          (puthash base-name device-entry devices))
        (if is-monitor
            (setcdr device-entry device)
          (setcar device-entry device))))

    ;; Convert hash table to alist with user-friendly names
    (maphash (lambda (base-name pair)
               (when (and (car pair) (cdr pair))
                 (let ((friendly-name
                        (cond
                         ((string-match-p "usb.*[Jj]abra" base-name) "Jabra SPEAK 510 USB")
                         ((string-match-p "^usb-" base-name) "USB Audio Device")
                         ((string-match-p "^pci-" base-name) "Built-in Audio")
                         ((string-match-p "^[0-9A-Fa-f:]+$" base-name) "Bluetooth Headset")
                         (t base-name))))
                   (push (cons friendly-name pair) result))))
             devices)
    (nreverse result)))

(defun cj/recording-select-device (prompt device-type)
  "Interactively select an audio device.
PROMPT is shown to user.  DEVICE-TYPE is \\='mic or \\='monitor for filtering.
Monitor devices end in .monitor (they tap system audio output).
Returns selected device name or nil."
  (let* ((sources (cj/recording-parse-sources))
         (filtered (if (eq device-type 'monitor)
                       (seq-filter (lambda (s) (string-match-p "\\.monitor$" (car s))) sources)
                     (seq-filter (lambda (s) (not (string-match-p "\\.monitor$" (car s)))) sources)))
         (choices (mapcar (lambda (s)
                           (let ((device (nth 0 s))
                                 (_driver (nth 1 s))
                                 (_state (nth 2 s))
                                 (friendly-state (cj/recording-friendly-state (nth 2 s))))
                             (cons (format "%-10s %s" friendly-state device) device)))
                         filtered)))
    (if choices
        (cdr (assoc (completing-read prompt choices nil t) choices))
      (user-error "No %s devices found" (if (eq device-type 'monitor) "monitor" "input")))))

(provide 'video-audio-recording-devices)
;;; video-audio-recording-devices.el ends here
