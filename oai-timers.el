;;; oai-timers.el --- Request Timers and notifications for oai -*- lexical-binding: t; -*-

;; Copyright (C) 2025 github.com/Anoncheg1,codeberg.org/Anoncheg
;; Author: <github.com/Anoncheg1,codeberg.org/Anoncheg>
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; License

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Licensed under the GNU Affero General Public License, version 3 (AGPLv3)
;; <https://www.gnu.org/licenses/agpl-3.0.en.html>


;;; Commentary:
;; `interrupt-request-func' is for implementation of interrupt that
;; `oai-restapi--interrupt-url-request'

;;; Code:
;; -=-= variables
(defcustom oai-timers-echo-gap 0.2
  "Echo update interval for notification about waiting."
  :type 'float
  :group 'oai)

(defcustom oai-timers-duration most-positive-fixnum
  "The total duration in seconds for which the timer should run.
Delay after which it will be killed."
  :type 'integer
  :group 'oai)

(defcustom oai-timers-retries 3
  "Amount of request attemts before give up.
Used for `oai-restapi-request-llm-retries' calling in `oai-prompt'."
  :type 'integer
  :group 'oai)

(defvar oai-timers--global-progress-reporter nil
  "Progress-reporter for request response to indical waiting.")

(defvar oai-timers--global-progress-timer nil
  "Timer for updating the progress reporter.")

(defvar oai-timers--global-progress-timer-remaining-ticks 0
  "The time when the timer started.")

(defvar-local oai-timers--current-timer nil
  "Timer for waiting for url buffer.")

(defvar-local oai-timers--current-timer-remaining-ticks 0
  "The time when the timer started.")

(defvar oai-timers--global-progress-reporter-waiting-string "Waiting for a response")

(defvar oai-timers--element-marker-variable-dict nil
  "Allow to store url buffer per block.
Pairs of url-buffer (key) -> Header-marker (variable).
So ai block may have several url-buffer running at the same time.
Intented for usage with `oai-block--copy-header-marker' and keep pairs of
\(url-retrieve buffero -> header marker).
Should be used for interactive interrup of request only.
`eq' is good for buffers, for markers we should use `equal'")

;; -=-= variable-dict
(defun oai-timers--get-variable (key)
  "Get variable (one or first) for KEY.
Get header-marker (variable) for url-buffer (key).
Key is Indented for usage with `oai-block-get-header-marker'.
Use ELEMENT only in current moment.
We use `eq' to find key which is buffer."
    (alist-get key oai-timers--element-marker-variable-dict nil nil #'eq))


(defun oai-timers--get-keys-for-variable (variable)
  "Return a list of keys.
VARIABLE is header-marker or ai block.
Return list of url-buffers.
use `oai-timers--element-marker-variable-dict'."
  (seq-uniq (mapcar #'car
                    (seq-filter (lambda (entry)
                                  (equal (cdr entry) variable)
                                       ;; (buffer-live-p (car entry))
                                       )
                                oai-timers--element-marker-variable-dict))))

;; oai-timers--set-variable
(defun oai-timers--set (key value)
  "Assign value to key.
KEY: url-buffer, VALUE: header marker.
Indented for usage with `oai-block-get-header-marker'.
Used in
- `oai-restapi-request-prepare'
- `oai-restapi-request-llm-retries'."
    (if (not value)
        (setf (alist-get key oai-timers--element-marker-variable-dict nil 'remove) nil)
      ;; else
      (setf (alist-get key oai-timers--element-marker-variable-dict) value)))

(defun oai-timers--rassq-delete-all-equal (value alist)
  "Delete from ALIST all elements whose cdr is `equal' to VALUE.
Return the modified alist.  Elements of ALIST that are not conses are ignored.
We need this,  because `rassq-delete-all' remove by `eq'  only which not
suitable for  markers which should  be compared by buffer  and position,
not by object itself."
  (delq nil
        (mapcar (lambda (elt)
                  (and (consp elt)
                       (not (equal (cdr elt) value))
                       elt))
                alist)))
;; (setq mylist '((a . 1) (b . 2) (c . (1 2)) (d . 2)))
;; (rassq-delete-all 2 mylist) ;; removes (b . 2) and (d . 2), only if value is `eq` to 2
;; (setq mylist '((a . 1) (b . 2) (c . (1 2)) (d . 2)))
;; (rassq-delete-all-equal 2 mylist) ;; removes (b . 2) and (d . 2), works for numerics
;; (rassq-delete-all-equal '(1 2) mylist) ;; removes (c . (1 2)), matches by content

(defun oai-timers--remove-variable (value)
  "Remove marker.
`equal' for markers compare buffer and positon, `eq' compare objects itself.
We use `eq' here.
Argument VALUE is Header-marker."
  (setq oai-timers--element-marker-variable-dict
        ;; eq compare objects itself
        (oai-timers--rassq-delete-all-equal value oai-timers--element-marker-variable-dict)))

;; (setq a (copy-marker (point)))
;; (setq b (copy-marker (point)))
;; (setq c (copy-marker a))
;; (eq a c)


(defun oai-timers--remove-key (key)
  "Remove buffer.  Use `eq' to find KEY, for buffer eq is ok."
  (setq oai-timers--element-marker-variable-dict
        (assq-delete-all key oai-timers--element-marker-variable-dict)))

;; (setq oai-timers--element-marker-variable-dict nil)
;; (oai-timers--set 1 'aa)
;; (oai-timers--set 2 'cc)
;; (oai-timers--set 3 'bb)
;; (oai-timers--get-variable 2)
;; (oai-timers--set (list 3 2) 'bb)
;; ;; (oai-timers--remove-key 1)
;; (print oai-timers--element-marker-variable-dict)
;; (oai-timers--get-keys-for-variable 'bb)

;; (oai-timers--get-all-variables)
;; (oai-timers--get-all-keys)
;;
;; (oai-timers--remove-variable 'aa)

;; (defun oai-timers--get-all-variables () ; not used
;;   "Get all header-makers."
;;   (seq-uniq (mapcar #'cdr oai-timers--element-marker-variable-dict)))

(defun oai-timers--get-all-keys ()
  "Get all url-buffers."
  (seq-uniq (mapcar #'car oai-timers--element-marker-variable-dict)))
  ;; (seq-uniq
  ;;  (mapcar #'cdr (seq-filter (lambda (entry)
  ;;                              (buffer-live-p (car entry)))
  ;;                            oai-timers--element-marker-variable-dict))))

;; (defun oai-timers--clear-variables () ; too simple
;;   (setq oai-timers--element-marker-variable-dict nil))

;; -=-= Timers Global
(defun oai-timers--stop-global-progress-reporter (&optional failed)
  "Stop global timer of progress reporter for restart or at success.
Don't clear list of url-buffers.
Called in
`oai-timers--progress-reporter-run' for restart,
`oai-timers--interrupt-all-requests' for full stop.
If Optional argument FAILED is non-nil, then explicitly notify user
about failure."
  (oai--debug "oai-timers--stop-global-progress-reporter1 %s %s %s"
              (current-buffer)
              oai-timers--global-progress-reporter
              oai-timers--global-progress-timer)
  ;; finish notifications
  (when oai-timers--global-progress-reporter
    (if failed ; timeout
        (progn ; from `url-queue-kill-job'
          ;; (progress-reporter-done oai-timers--global-progress-reporter)
          (progress-reporter-update oai-timers--global-progress-reporter nil "- Connection failed")
          (message (concat oai-timers--global-progress-reporter-waiting-string "- Connection failed")))
      ;; else - echo success
      (progress-reporter-done oai-timers--global-progress-reporter))
    ;; when
    (setq oai-timers--global-progress-reporter nil))

  ;; clear time
  (when oai-timers--global-progress-timer
    (oai--debug "oai-timers--stop-global-progress-reporter2"
    (cancel-timer oai-timers--global-progress-timer)
    (setq oai-timers--global-progress-timer nil)
    (setq oai-timers--global-progress-timer-remaining-ticks 0)
    (oai--debug "oai-timers--stop-global-progress-reporter3 ticks: %s" oai-timers--global-progress-timer-remaining-ticks))))

(defvar oai-timers--oai-update-mode-line (intern "oai-update-mode-line")
  "Dependency injection from in oai.el.")

(defun oai-timers--update-global-progress-reporter (&optional failed)
  "Count url-buffers and stop reporter if it is empty.
Called from
`oai-restapi-request-llm-retries'
`oai-timers--interrupt-current-request'
`oai-timers--interrupt-all-requests'.
If Optional argument FAILED is non-nil, then explicitly notify user
about failure."
  (oai--debug "oai-timers--update-global-progress-reporter1, dict: %s" oai-timers--element-marker-variable-dict)
  (let* ((buffers (oai-timers--get-all-keys))
         (count (length buffers))
         (count-live (length (delq nil (mapcar #'buffer-live-p buffers)))))
    (oai--debug "oai-timers--update-global-progress-reporter2, count: %s count-live: %s" count count-live)
    (let ((count (length (oai-timers--get-all-keys))))
      (unless oai-timers--oai-update-mode-line
        (error "Library oai.el should be loaded to use oai-timers--update-global-progress-reporter function"))
      (funcall oai-timers--oai-update-mode-line count)
      (when (eql count 0)
        (oai-timers--stop-global-progress-reporter failed)))))

(defun oai-timers--interrupt-all-requests (interrupt-request-func &optional failed)
  "Interrup all url requests and stop global timer.
INTERRUPT-REQUEST-FUNC may be `oai-restapi--interrupt-url-request' or
`oai-restapi--stop-tracking-url-request'.
Called from
`oai-restapi-stop-all-url-requests' by C\\-g
`oai-timers--progress-reporter-run' by global timer.
If Optional argument FAILED is non-nil, then explicitly notify user
about failure."
  (oai--debug "oai-timers--interrupt-all-requests1 %s %s" oai-timers--element-marker-variable-dict failed)
  (when-let ((buffers (oai-timers--get-all-keys)))
    (oai--debug "oai-timers--interrupt-all-requests2 %s" buffers)
    ;; stop requests
    ;; (unwind-protect
    (mapc (lambda (url-buffer)
            (funcall interrupt-request-func url-buffer))
          buffers))
  (oai--debug "oai-timers--interrupt-all-requests3")
  ;; clear list
  (setq oai-timers--element-marker-variable-dict nil)

  ;; stop global timer
  (oai--debug "oai-timers--interrupt-all-requests4")
  (oai-timers--update-global-progress-reporter failed)
  ;; (oai--debug "oai-timers--interrupt-all-requests5")
  ;; (oai-timers--stop-global-progress-reporter failed)
  )

;; (defun oai-timers--stop-current-timer (url-buffer &optional failed)
;;   (oai--debug "oai-timers--stop-current-timer")
;;   ;; - Remove variable
;;   (oai-timers--remove-variable url-buffer)

;;   (if (eq (current-buffer) url-buffer)
;;       (progn
;;         (oai--debug "oai-timers--stop-current-timer if1")
;;         (when oai-timers--current-timer
;;           (cancel-timer oai-timers--current-timer)
;;           (setq oai-timers--current-timer nil)
;;           (setq oai-timers--current-timer-remaining-ticks 0)))
;;     ;; else
;;     (oai--debug "oai-timers--stop-current-timer if2")
;;     ;; - Clear time and kill buffer
;;     (when (and url-buffer (buffer-live-p url-buffer))
;;       (with-current-buffer url-buffer
;;         ;; - Stop url-buffer timer
;;         (when oai-timers--current-timer
;;           (cancel-timer oai-timers--current-timer)
;;           (setq oai-timers--current-timer nil)
;;           (setq oai-timers--current-timer-remaining-ticks 0)))))
;;   (oai-timers--update-global-progress-reporter failed))

;; -=-= Timers Local
(defun oai-timers--interrupt-current-request (url-buffer interrupt-request-func)
  "Interrupt every buffer, remove buffer from list, update global timer.
URL-BUFFER one or several buffers.
Should be called in target buffer with global timer.
INTERRUPT-REQUEST-FUNC may be `oai-restapi--stop-tracking-url-request'
or `oai-restapi--interrupt-url-request'
Called from
`oai-restapi--insert-stream-response' after receiving first chunk,
`oai-restapi--url-request-on-change-function' for  not stream after  reply or
\"DONE\" string found for stream.
`oai-restapi-stop-url-request'."
  (oai--debug "oai-timers--interrupt-current-request %s %s %s" (buffer-live-p url-buffer) interrupt-request-func url-buffer)

  (if (sequencep url-buffer) ;; if several
      (mapc (lambda (b)
              (oai-timers--remove-key b)
              (oai--debug "oai-timers--interrupt-current-request lambda")
              (funcall interrupt-request-func b))
            url-buffer)
    ;; else - if one
    ;; - Remove variable
    (oai-timers--remove-key url-buffer)
    ;; ;; else
    ;; (oai-timers--set-variable nil :element element))
    ;; - Clear time and kill buffer
    (funcall interrupt-request-func url-buffer))
  ;; (if (or (eq (current-buffer) url-buffer)
  ;;         (not (buffer-live-p url-buffer)))
  ;;     (progn
  ;;       (oai--debug "oai-timers--interrupt-current-request if1" oai-timers--current-timer)
  ;;       ;; (when oai-timers--current-timer
  ;;       ;;   (cancel-timer oai-timers--current-timer)
  ;;       ;;   (setq oai-timers--current-timer nil)
  ;;       ;;   (setq oai-timers--current-timer-remaining-ticks 0))
  ;;       (when interrupt-request-func
  ;;         (funcall interrupt-request-func url-buffer)))
  ;;   ;; - else

  ;;   (oai--debug "oai-timers--interrupt-current-request if2")
  ;;   (when (and url-buffer (buffer-live-p url-buffer))
  ;;     (with-current-buffer url-buffer
  ;;       ;; - Stop url-buffer timer
  ;;       ;; (when oai-timers--current-timer
  ;;       ;;   (cancel-timer oai-timers--current-timer)
  ;;       ;;   (setq oai-timers--current-timer nil)
  ;;       ;;   (setq oai-timers--current-timer-remaining-ticks 0))
  ;;       )
  ;;       ;; - Kill buffer
  ;;       (when interrupt-request-func
  ;;         (funcall interrupt-request-func url-buffer))
  ;;       ))
    ;; - Update global timer
    (oai-timers--update-global-progress-reporter))



;; -=-= Main - constructor
(defun oai-timers--progress-reporter-run (interrupt-request-func &optional duration)
  "Start or update progress notification.
1) Save pair (HEADER-MARKER->URL-BUFFER)
2) INTERRUPT-REQUEST-FUNC - When timer expired kill all by calling for
every buffer.
Require that url-buffer was saved with `oai-timers--set', to count them.
Called from `oai-restapi-request'.
Set:
- `oai-timers--global-progress-reporter' - lambda that return a string,
- `oai-timers--global-progress-timer' - timer that output /-\ to echo area.
- `oai-timers--global-progress-timer-remaining-ticks'.
- `oai-timers--current-timer' - count life of url buffer,
- `oai-timers--current-timer-remaining-ticks'.
Optional argument DURATION may be used to replace `oai-timers-duration'
value."
  (oai--debug "oai-timers--progress-reporter-run %s %s" (length (oai-timers--get-all-keys)) oai-timers--element-marker-variable-dict)
  ;; - update mode-line
  ;; (oai-timers--update-global-progress-reporter)
  ;; We make delay because this function run after url-retrieve and url-buffer may be not saved.
  (run-with-timer 1.0 nil (lambda () (funcall oai-timers--oai-update-mode-line (length (oai-timers--get-all-keys)))))

  ;; - precalculate ticks based on duration, 25/ 0.2 = 125 ticks
  (setq oai-timers--global-progress-timer-remaining-ticks
        (fround (/ (or duration oai-timers-duration) oai-timers-echo-gap)))

  (oai--debug "oai-timers--progress-reporter-run1")
  ;; - if exist, add remaining ticks
  (when (not oai-timers--global-progress-reporter)
    ;; else - create new - reporter
    (setq oai-timers--global-progress-reporter (make-progress-reporter oai-timers--global-progress-reporter-waiting-string)))

  (oai--debug "oai-timers--progress-reporter-run2 %s" oai-timers--global-progress-timer)
  (when (not oai-timers--global-progress-timer)
    (oai--debug "oai-timers--progress-reporter-run3")
    ;; timer1
    (setq oai-timers--global-progress-timer
          (run-with-timer ; do not respect `with-current-buffer'
           1.0 oai-timers-echo-gap ; start after 1 sec
           (lambda ()
             "timer1 in current buffer"
             ;; expired or closed?
             (if (or (<= oai-timers--global-progress-timer-remaining-ticks 0)
                     (not oai-timers--global-progress-reporter))
                 (progn
                   (oai--debug "oai-timers--progress-reporter-run expired")
                   ;; - stop timer:
                   (oai-timers--interrupt-all-requests interrupt-request-func 'failed))
               ;; else -  ticks -= 1
               (setq oai-timers--global-progress-timer-remaining-ticks
                     (1- oai-timers--global-progress-timer-remaining-ticks))
               (progress-reporter-update oai-timers--global-progress-reporter)))))
    (oai--debug "oai-timers--progress-reporter-run4"))

  ;; timer2 - request killer
  ;; (with-current-buffer url-buffer
  ;;   (setq-local oai-timers--current-timer-remaining-ticks
  ;;               oai-timers--global-progress-timer-remaining-ticks)
  ;;   (oai--debug "oai-timers--progress-reporter-run timer2 oai-timers--current-timer-remaining-ticks" (current-buffer) oai-timers--current-timer-remaining-ticks)
  ;;   (setq-local oai-timers--current-timer
  ;;         (run-with-timer
  ;;          1.0 oai-timers-echo-gap ; start after 1 sec
  ;;          (lambda ()
  ;;            "timer2 in current buffer"
  ;;            ;; expired?
  ;;            (if (<= oai-timers--current-timer-remaining-ticks 0)
  ;;                (progn
  ;;                  (oai--debug "oai-timers--progress-reporter-run timer2 is expired" (current-buffer) oai-timers--current-timer-remaining-ticks)
  ;;                  (oai-timers--interrupt-current-request url-buffer interrupt-request-func 'failed))

  ;;              ;; else -  ticks -= 1
  ;;              (setq oai-timers--current-timer-remaining-ticks
  ;;                    (1- oai-timers--current-timer-remaining-ticks))))
  ;;          )))
  )

;; (defun oai-timers--with-retry-run (url-buffer recreate-func interrupt-func)
;;   "Create a timer in the current Emacs buffer.
;; Kill URL-BUFFER after timeout, recreate request?."
;;   ;; - precalculate ticks based on duration, 25/ 0.2 = 125 ticks
;;   (setq oai-timers--current-timer-remaining-ticks
;;               (fround (/ oai-timers-duration oai-timers-echo-gap)))

;;   ;; - killer timer
;;   (setq-local oai-timers--current-timer
;;               (run-with-timer
;;            1.0 oai-timers-echo-gap ; start after 1 sec
;;            (lambda ()
;;              "timer2 in current buffer"
;;              ;; expired?
;;              (if (<= oai-timers--current-timer-remaining-ticks 0)
;;                  (progn
;;                    (oai--debug "oai-timers--progress-reporter-run timer2 is expired" (current-buffer) oai-timers--current-timer-remaining-ticks)
;;                    (oai-timers--interrupt-current-request url-buffer interrupt-request-func 'failed))

;;                ;; else -  ticks -= 1
;;                (setq oai-timers--current-timer-remaining-ticks
;;                      (1- oai-timers--current-timer-remaining-ticks))))
;;            )

;;   )

(provide 'oai-timers)
;;; oai-timers.el ends here
