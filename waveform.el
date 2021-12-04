;;; waveform.el --- Display and work with waveforms  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Sacha Chua

;; Author: Sacha Chua <sacha@sachachua.com>
;; Keywords: multimedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; You will also need the mpv package for Emacs and the MPV client.
;;
;; M-x waveform-show to show the waveform for a file
;; left-click to copy timestamp and play
;; q to quit

;;; Code:

(defcustom waveform-ffmpeg-executable "ffmpeg"
  "Path to the FFMPEG executable used for generating waveforms."
  :type 'file
  :group 'waveform)

(defcustom waveform-height 40
  "Height of waveform in pixels."
  :type 'integer
  :group 'waveform)

(defcustom waveform-pixels-per-second 75
  "Number of pixels used for displaying one second."
  :type 'integer
  :group 'waveform)

(defcustom waveform-sample-msecs 2000
  "Number of milliseconds to play when jumping around a waveform.
0 means don't play a sample.
nil means keep playing."
  :type 'integer
  :group 'waveform)

(defcustom waveform-ffmpeg-filter-args ":colors=white" ;; 'waveform-fancy-filter ;; ":colors=white" ;; 'waveform-fancy-filter
  "Additional arguments for the showwavespic filter.
To change the foreground color, use something like
\":colors=white\".  You can also set it to a function.  The
function will be called with WIDTH and HEIGHT as parameters, and
should return a string to include in the filter.  See
`waveform-fancy-filter' for an example."
  :type '(choice
          (string :tag "Extra arguments to include")
          (function :tag "Function to call with the width and height"))
  :group 'waveform)

(defun waveform-fancy-filter (width height)
  "Displays green waveforms on a dark green background with a grid.
WIDTH and HEIGHT are given in pixels."
  (concat
   ":colors=#9cf42f[fg];"
   (format "color=s=%dx%d:color=#44582c,drawgrid=width=iw/10:height=ih/5:color=#9cf42f@0.1[bg];"
           width height)
   "[bg][fg]overlay=format=auto,drawbox=x=(iw-w)/2:y=(ih-h)/2:w=iw:h=1:color=#9cf42f"))

(defvar-local waveform--ffmpeg-process nil)

(defun waveform-for-file (filename width height &optional callback)
  "Returns a string representing the image data in JPEG format.
FILENAME is the input file. The result can be used in `create-image'."
  (let* ((args
          (append
           (list "-i" filename)
           (list
            "-loglevel"
            "0"
            "-filter_complex"
            (format "showwavespic=s=%dx%d%s"
                    width height
                    (cond
                     ((functionp waveform-ffmpeg-filter-args)
                      (funcall waveform-ffmpeg-filter-args width height))
                     ((stringp waveform-ffmpeg-filter-args)
                      waveform-ffmpeg-filter-args)
                     (t "")))
            "-frames:v" "1"
            "-f" "image2" "-"))))
    (if (functionp callback)
        (let* ((buffer (generate-new-buffer " *temp*")))
          (when (process-live-p waveform--ffmpeg-process)
            (quit-process waveform--ffmpeg-process))
          (setq waveform--ffmpeg-process
                (apply 'start-process "ffmpeg" buffer
                       waveform-ffmpeg-executable args))
          (set-process-sentinel
           waveform--ffmpeg-process
           (lambda (process event)
             (when (save-match-data (string-match "finished" event))
               (with-current-buffer (process-buffer process)
                 (funcall callback (encode-coding-string (buffer-string) 'binary)))))))
      (with-temp-buffer
        (apply 'call-process waveform-ffmpeg-executable nil t nil args)
        (encode-coding-string (buffer-string) 'binary)))))


(defvar waveform-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'waveform-select)
    ;; (define-key map [mouse-3] #'waveform-sample)
    (define-key map [down-mouse-3] #'waveform-sample)
    (define-key map "q" #'waveform-quit)
    (define-key map " " #'waveform-mpv-position)
    (define-key map [left] #'waveform-mpv-seek-backward)
    (define-key map [right] #'waveform-mpv-seek-forward)
    (define-key map [S-left] #'waveform-mpv-seek-backward-small)
    (define-key map [S-right] #'waveform-mpv-seek-forward-small)
    (define-key map "j" #'waveform-mpv-seek-to)
    (define-key map (kbd "DEL") #'waveform-mpv-return-to-mark)
    (define-key map ">" #'mpv-speed-increase)
    (define-key map "<" #'mpv-speed-decrease)
    map))

(defvar waveform--sample-timer nil)

(defun waveform--restore-mpv-position (msecs)
  "Stop playing and seek to MSECS."
  (mpv--enqueue '("pause") #'ignore)
  (mpv-seek (/ msecs 1000.0)))

(defun waveform-play-sample (file ms &optional end-ms)
  (mpv-seek (/ ms 1000.0))
  (when (or end-ms waveform-sample-msecs)
    (when (timerp waveform--sample-timer) (cancel-timer waveform--sample-timer))
    (setq waveform--sample-timer
          (run-at-time (/ (if end-ms (- end-ms ms) waveform-sample-msecs) 1000.0) nil
                       #'waveform--restore-mpv-position
                       ms))))

(defun waveform-file-duration-ms (filename &optional type)
  (* 1000
     (string-to-number
      (shell-command-to-string
       (concat "ffprobe -v error -show_entries format=duration -of default=noprint_wrappers=1:nokey=1 " (shell-quote-argument (expand-file-name filename)))))))

(defun waveform-msecs-to-timestamp (msecs)
  "Convert MSECS to string in the format HH:MM:SS,MS."
  ;; We need to wrap format-seconds in save-match-data because it does regexp
  ;; stuff and we need to preserve our own match-data.
  (concat (save-match-data (format-seconds "%02h:%02m:%02s" (floor (/ msecs 1000))))
          "." (format "%03d" (mod msecs 1000))))

(defun waveform-mpv-position ()
  (interactive)
  (waveform--mark (mpv-get-playback-position)))

(defun waveform--update-position (marker)
  (let* ((buf (marker-buffer marker))
         (win (get-buffer-window buf)))
    (if (buffer-live-p buf)
        (when (pos-visible-in-window-p marker win t)
          (with-current-buffer buf
            (when (mpv-live-p)
              (condition-case _
                  (let* ((pos (mpv-get-playback-position))
                         (x (and pos (waveform-ms-to-x (* pos 1000))))
                         (inhibit-read-only t))
                    (when x
                      (svg-line waveform--svg x 0 x waveform--height :id "position" :stroke-color "blue")))
                (error nil)))))
      (dolist (timer timer-list)
        (if (and (eq (timer--function timer) #'waveform--update-position)
                 (equal (car (timer--args timer)) marker))
            (cancel-timer timer))))))

(defun waveform--mark (secs)
  (let ((inhibit-read-only t)
        (x (waveform-ms-to-x (* secs 1000))))
    (setq waveform-mark-msecs (* 1000 secs))
    (svg-line waveform--svg x 0 x waveform--height :id "mark" :stroke-color "green")
    (message "%s" (waveform-msecs-to-timestamp (* 1000 secs)))
    (kill-new (waveform-msecs-to-timestamp (* 1000 secs)))))

(defun waveform--string-to-secs (secs)
  (cond ((numberp secs) secs)
        ((string-match ":" secs) (org-timer-hms-to-secs secs))
        (t (string-to-number secs))))

(defun waveform-sample-at (secs)
  (interactive "MPosition (seconds or timestamp): ")
  (setq secs (waveform--string-to-secs secs))
  (waveform--mark secs)
  (waveform-play-sample waveform--media-file (* secs 1000)
                        (+ (* secs 1000) waveform-sample-msecs)))
(defun waveform-mpv-seek-to (secs)
  (interactive "MPosition (seconds or timestamp): ")
  (setq secs (waveform--string-to-secs secs))
  (waveform--mark secs)
  (mpv-seek secs))

(defun waveform-mpv-return-to-mark ()
  (interactive)
  (waveform-mpv-seek-to (/ waveform-mark-msecs 1000.0)))

(defun waveform-mpv-seek-backward (arg)
  "Seek backward ARG seconds.
If ARG is numeric, it is used as the number of seconds.  Else each use
of \\[universal-argument] will add another `mpv-seek-step' seconds."
  (interactive "P")
  (waveform-mpv-seek-to (- (mpv-get-playback-position) (mpv--raw-prefix-to-seconds arg))))

(defun waveform-mpv-seek-backward-small ()
  "Seek backward 1 second."
  (interactive)
  (waveform-mpv-seek-to (- (mpv-get-playback-position) 1)))

(defun waveform-mpv-seek-forward-small ()
  "Seek backward 1 second."
  (interactive)
  (waveform-mpv-seek-to (+ (mpv-get-playback-position) 1)))

(defun waveform-mpv-seek-forward (arg)
  "Seek forward ARG seconds.
If ARG is numeric, it is used as the number of seconds.  Else each use
of \\[universal-argument] will add another `mpv-seek-step' seconds."
  (interactive "P")
  (waveform-mpv-seek-to (+ (mpv-get-playback-position) (mpv--raw-prefix-to-seconds arg))))

(defun waveform-select (event)
  "Copy the time at point and seek to it."
  (interactive "e")
  (waveform-mpv-seek-to (/ (waveform-mouse-event-to-ms event) 1000.0)))

(defun waveform-sample (event)
  "Copy the time at point and seek to it."
  (interactive "e")
  (waveform-sample-at (/ (waveform-mouse-event-to-ms event) 1000.0)))

(define-derived-mode waveform-mode fundamental-mode "Waveform"
  "Display a waveform.")

(defun waveform-quit ()
  (interactive)
  (kill-buffer)
  (mpv-kill))

(defvar-local waveform-mark-msecs nil)
(defvar-local waveform--position-indicator nil)
(defvar-local waveform--svg nil)
(defvar-local waveform--start-ms nil)
(defvar-local waveform--stop-ms nil)
(defvar-local waveform--height nil)
(defvar-local waveform--width nil)
(defvar-local waveform--media-file nil)
(defvar-local waveform--duration nil)
(defvar-local waveform--position-timer nil)
;; (waveform-show (expand-file-name "~/vendor/emacsconf-2021-private/captions/emacsconf-2021-news--emacs-news-highlights--sacha-chua--main.webm"))
(defun waveform-show (file)
  (interactive "FMedia file: ")
  (setq file (expand-file-name file))
  (let* ((image-string (waveform-for-file file (window-pixel-width) (window-pixel-height)))
         (end-ms (waveform-file-duration-ms file))
         (inhibit-read-only t)
         (height (window-pixel-height))
         (width (window-pixel-width))
         (svg (svg-create width height)))
    (svg-embed svg image-string "image/jpeg" t :x 0 :y 0 :width (window-pixel-width) :height (window-pixel-height))
    (svg-line svg 0 (/ height 2) width (/ height 2) :stroke-color "gray")
    (svg-line svg 20 0 20 height :stroke-color "green" :id "mark")
    (svg-line svg 20 0 20 height :stroke-color "blue" :id "position")
    (switch-to-buffer (get-buffer-create "*Waveform*"))
    (erase-buffer)
    (waveform-mode)
    (setq waveform--svg svg)
    (svg-insert-image waveform--svg)
    (read-only-mode 1)
    (setq waveform--start-ms 0
          waveform--stop-ms end-ms
          waveform--media-file file
          waveform--duration end-ms
          waveform--height height
          waveform--width width)
    (when (timerp waveform--position-timer) (cancel-timer waveform--position-timer))
    (setq waveform--position-timer (run-at-time 0 0.2 #'waveform--update-position (point-marker)))
    (mpv-start file)))

(defun waveform-ms-to-x (msecs)
  "Return X position of MSECS."
  (/ (* waveform--width (- msecs waveform--start-ms)) waveform--duration))

(defun waveform-mouse-event-to-ms (event)
  "Return the millisecond position of EVENT."
  (let* ((x (car (elt (cadr event) 8)))
         (width (car (elt (cadr event) 9))))
    (floor (+ (* (/ (* 1.0 x)
                    width)
                 waveform--duration)
              waveform--start-ms))))

;; (create-image (waveform-for-file (expand-file-name "~/code/emacsconf-2021-emacs-news-highlights/output.webm") (window-pixel-width) (window-pixel-height)) nil t)
(provide 'waveform)
;;; waveform.el ends here
