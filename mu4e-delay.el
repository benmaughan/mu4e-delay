;;; mu4e-delay.el --- delay sending emails with mu4e -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Ben Maughan <benmaughan@gmail.com>

;; Author: Ben Maughan <benmaughan@gmail.com>
;; URL: http://www.pragmaticemacs.com
;; Package-Version: 20160205
;; Version: 0.1.1
;; Keywords: email

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; These functions are intended mainly to provide the functionality of
;; adding a delay to the sending of an email so that the email can be
;; retrieved and edited before it is really sent. This is equivalent
;; to the "send later" extension in thunderbird, or the undo send
;; option in gmail. The functions also support scheduling of emails to
;; be send at a specific date and time.
;;
;; Code is based closely on `gnus-delay' to add a header specifying a
;; time at which to send the email. The mail is then saved to the
;; draft folder and a scheduled job checks all mails in the draft
;; folder and sends any that are due.
;;
;; The function `mu4e-delay-add-delay-header' adds the header and can
;; be called interactively to add delay times in different formats.
;; More usually, one would replace the normal send function in mu4e
;; with `mu4e-delay-send' which adds the default delay of 2 minutes
;; and saves the message to the draft folder, giving the user the
;; impression of having sent the message.
;;
;; The function `mu4e-delay-send-delayed-mails' loops over all mails
;; in the draft folder looking for the delay header. If any messages
;; are due to be sent then it sends them. The messages are checked
;; with the function `mu4e-delay-check-file-and-send-if-due' which
;; sends any due mail. Mail can be sent with `smtpmail-send-it' or
;; `sendmail-sent-it' (currently by commenting the code). Sendmail is
;; much preferred as `smtpmail-send-it' runs synchronously so will
;; interrupt (briefly) whatever you are doing when the timer runs and
;; it finds mail to send. For more invisible use, configure a version
;; of sendmail on your system and set up mu4e to use that.
;;
;; A delayed email is easily recovered by finding it in the draft
;; folder and then editing it and removing the delay header and saving
;; it. This will prevent it from being sent until you re-send (or
;; delay) it.
;;
;; A known current limitation is that attachments are not sent
;; properly when the mail is delayed. I've not worked out why this is
;; yet!
;;
;;; Code:

(defgroup mu4e-delay nil
  "Arrange for sending mails later."
  :group 'mu4e)

(defcustom mu4e-delay-header "X-Mu4e-Delayed"
  "Header name for storing info about delayed mails."
  :type 'string
  :group 'mu4e-delay)

(defcustom mu4e-delay-default-delay "2m"
  "*Default length of delay."
  :type 'string
  :group 'mu4e-delay)

(defcustom mu4e-delay-default-hour 8
  "*If deadline is given as date, then assume this time of day."
  :type 'integer
  :group 'mu4e-delay)

(defcustom mu4e-delay-default-timer 60
  "Default number of seconds between checks for delayed mail to send."
  :type 'integer
  :group 'mu4e-delay)

;;taken from gnus-delay
(defun mu4e-delay-add-delay-header (delay)
  "Delay this article by some time.
  DELAY is a string, giving the length of the time.  Possible values are:

  ,* <digits><units> for <units> in minutes (`m'), hours (`h'), days (`d'),
    weeks (`w'), months (`M'), or years (`Y');

  ,* YYYY-MM-DD for a specific date.  The time of day is given by the
    variable `mu4e-delay-default-hour', minute and second are zero.

  ,* hh:mm for a specific time.  Use 24h format.  If it is later than this
    time, then the deadline is tomorrow, else today."
  (interactive
   (list (read-string
          "Target date (YYYY-MM-DD), time (hh:mm), or length of delay (units in [mhdwMY]): "
          mu4e-delay-default-delay)))
  ;; Allow spell checking etc.
  (run-hooks 'message-send-hook)
  (let (num unit days year month day hour minute deadline)
    (cond ((string-match
            "\\([0-9][0-9][0-9]?[0-9]?\\)-\\([0-9]+\\)-\\([0-9]+\\)"
            delay)
           (setq year  (string-to-number (match-string 1 delay))
                 month (string-to-number (match-string 2 delay))
                 day   (string-to-number (match-string 3 delay)))
           (setq deadline
                 (message-make-date
                  (encode-time 0 0      ; second and minute
                               mu4e-delay-default-hour
                               day month year))))
          ((string-match "\\([0-9]+\\):\\([0-9]+\\)" delay)
           (setq hour   (string-to-number (match-string 1 delay))
                 minute (string-to-number (match-string 2 delay)))
           ;; Use current time, except...
           (setq deadline (apply 'vector (decode-time (current-time))))
           ;; ... for minute and hour.
           (aset deadline 1 minute)
           (aset deadline 2 hour)
           ;; Convert to seconds.
           (setq deadline (gnus-float-time (apply 'encode-time
                                                  (append deadline nil))))
           ;; If this time has passed already, add a day.
           (when (< deadline (gnus-float-time))
             (setq deadline (+ 86400 deadline))) ; 86400 secs/day
           ;; Convert seconds to date header.
           (setq deadline (message-make-date
                           (seconds-to-time deadline))))
          ((string-match "\\([0-9]+\\)\\s-*\\([mhdwMY]\\)" delay)
           (setq num (match-string 1 delay))
           (setq unit (match-string 2 delay))
           ;; Start from seconds, then multiply into needed units.
           (setq num (string-to-number num))
           (cond ((string= unit "Y")
                  (setq delay (* num 60 60 24 365)))
                 ((string= unit "M")
                  (setq delay (* num 60 60 24 30)))
                 ((string= unit "w")
                  (setq delay (* num 60 60 24 7)))
                 ((string= unit "d")
                  (setq delay (* num 60 60 24)))
                 ((string= unit "h")
                  (setq delay (* num 60 60)))
                 (t
                  (setq delay (* num 60))))
           (setq deadline (message-make-date
                           (seconds-to-time (+ (gnus-float-time) delay)))))
          (t (error "Malformed delay `%s'" delay)))
    (message-add-header (format "%s: %s" mu4e-delay-header deadline))))

;;helper functions to check for attachments
;;code adapted from John Kitchin's comment at
;;http://pragmaticemacs.com/emacs/email-attachment-reminders-in-mu4e/
(defun mu4e-delay-email-says-attach-p ()
  "Return t if email suggests there could be an attachment."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward   "attach\\|\Wfiles?\W\\|\Wphoto\\|\Where\s-+is\\|\Where\s-+are\\|\Where\s-+it\s-+is\\|enclose\\|\Wdraft\\|pdf\\|\Wversion" nil t)))

(defun mu4e-delay-email-has-attachment-p ()
  "Return t if the currently open email has an attachment"
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "<#part" nil t)))

;;use this function to "send" the mail with a delay
(defun mu4e-delay-send (arg)
  "Add delay header and save to drafts.

  If used with a prefix argument then send without delay."
  (interactive "P")

  ;;check and warn about attachments
  (when (or (mu4e-delay-email-says-attach-p)
             (mu4e-delay-email-has-attachment-p))
    (unless
        (y-or-n-p "Attachment detected, or you have mentioned an attachment. mu4e-delay-send does not currently support attachments. Send with delay anyway?")
      (error "Aborting send.")))

  (if arg
      ;;just send
      (message-send-and-exit)
    ;;send with delay
    (progn (mu4e-delay-add-delay-header mu4e-delay-default-delay)
           (message-dont-send))))

;; check if a given message is due to be sent and send it if needed
(defun mu4e-delay-check-file-and-send-if-due (f)
  (save-excursion
    (with-temp-buffer
      (insert-file-contents f)
      (goto-char (point-min))
      (if (re-search-forward
           (concat "^" (regexp-quote mu4e-delay-header) ":\\s-+")
           nil t)

          ;; delayed header is there
          (progn
            (setq deadline (nnheader-header-value))
            (setq deadline (apply 'encode-time
                                  (parse-time-string deadline)))
            (setq deadline (time-since deadline))
            (if (and (>= (nth 0 deadline) 0)
                     (>= (nth 1 deadline) 0))
                ;;message is ready to go
                (progn (message "Sending delayed article %s..." f)

                       ;;uncomment to use smtpmail
                       ;;(smtpmail-send-it)
                       (sendmail-send-it)

                       ;;uncomment to backup messages for testing
                       ;;make sure directory exists
                       ;; (rename-file f
                       ;;              (expand-file-name
                       ;;               (concat "~/tmp/delayed-sent-mail/"
                       ;;                       (file-name-nondirectory f))) t)

                       ;;comment out next line if backing up file above
                       (delete-file f)

                       ;;kill tmp buffer
                       (kill-buffer (current-buffer))
                       (message "...done"))
              ;;message not ready yet
              (progn
                ;;(message "Pending delayed article %s" f)
                (kill-buffer (current-buffer)))))

        ;; no delayed header
        (progn
          ;;(message "Delay header missing for article %s" f)
          (kill-buffer (current-buffer)))))))

;;process mails
(defun mu4e-delay-send-delayed-mails ()
  "Loop over all files in the drafts maildir and send those that are due.

  Usually this would be run on a timer using `mu4e-delay-initialise-send-delay'."
  (interactive)
  ;; loop over all files in delayed dir
  (save-excursion
    (let ((dir (expand-file-name (concat mu4e-maildir mu4e-drafts-folder "/cur"))))
      (mapc 'mu4e-delay-check-file-and-send-if-due
            (directory-files dir t "^[^\.]")))
    ;;update index
    (mu4e-update-index)))

;;set it up
(defvar mu4e-delay-send-timer nil
  "Timer to run `mu4e-delay-send-delayed-mails'")

(defun mu4e-delay-initialise-send-delay ()
  "Set up `mu4e-delay-send-delayed-mails' to run on a timer."
  (interactive)
  (setq mu4e-delay-send-timer
        (run-with-timer 0 mu4e-delay-default-timer 'mu4e-delay-send-delayed-mails)))

;;bind it
(define-key mu4e-compose-mode-map (kbd "C-c C-l") 'mu4e-delay-send)

;;bcc everything to me for testing - uncomment to use
;; (add-hook 'mu4e-compose-mode-hook
;;           (defun my-add-bcc ()
;;             "Add a Bcc: header."
;;             (save-excursion (message-add-header "Bcc: my-bcc-address@gmail.com\n"))))

;;initialise
(mu4e-delay-initialise-send-delay)

(provide 'mu4e-delay)
;;; mu4e-delay.el ends here
