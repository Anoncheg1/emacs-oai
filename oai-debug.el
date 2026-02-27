;;; oai-debug.el --- Logging for oai in separate buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 github.com/Anoncheg1,codeberg.org/Anoncheg
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Author: <github.com/Anoncheg1,codeberg.org/Anoncheg>

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
;; Used as a help function `oai--debug' for conditional output of
;; debug messages.

;;; Code:

(require 'backtrace) ; for `oai-debug--get-caller' (not used now)

;; -=-= customization and function
(defcustom oai-debug-buffer nil
  "If non-nil, enable debuging to a new buffer with such name.
Set to something like *debug-oai*.  to enable debugging."
  :type '(choice (const :tag "No debugging" nil)
                 (string :tag "Name of buffer"))
  :group 'oai)

(defcustom oai-debug-timestamp-flag t
  "Non-nil means add timestamp to every debug message."
  :type 'boolean
  :group 'oai)


(defun oai-debug--get-caller ()
  "Return string with name of function of caller function.
Heavy to execute."
  (let* ((backtrace-line-length 20) ; used by `backtrace-get-frames'
         (print-level 3)
         (print-length 10)
         (bt
          ;; (with-output-to-string (backtrace))
          (backtrace-to-string (backtrace-get-frames 'backtrace)))
         (caller))
         ;; (print bt)
         (seq-find
          ; - predicate
          (lambda (line)
            (let* ( (mpos (string-match "(" line))
                   (sline (substring line 0 mpos))
                   (tline (string-trim-right (string-trim-left sline))))
                   (if (and (not (string-empty-p tline))
                            (not (member tline '("oai-debug--get-caller" "oai--debug" ) )))
                       (setq caller tline)
                     nil ; else
                     )))
          ;; - lines
          (cdr (string-split bt "\n" t)))
         caller))

(defcustom oai-debug-filter nil
  "If non-nil output only strings that contains this string."
  :type '(choice (const :tag "No filter" nil)
                 (string :tag "Regex string for filter"))
  :group 'oai)

(defun oai-debug--format-argument (args)
  "Convert ARGS to a string.
ARGS may be any Elisp object.
Used to prepare arguments of `oai--debug' for output by converting to a
string.
Always return string."
  (if (equal (type-of args) 'string)
      (format "%s\n" args)
    (concat (prin1-to-string args) "\n")))

(defun oai-debug--safe-format (fmt &rest args)
  "Formats by removing all '%s' from FMT and appending ' %s' for each ARG."
  ;; Remove all "%s" from fmt
  (let* ((fmt (replace-regexp-in-string " ?%s" "" fmt))
         (num-args (length args))
         (fmt (concat fmt " "
                   ;; (apply #'concat (make-list num-args " %s"))
                   ;; (mapconcat #'identity (make-list num-args " %s") "")
                   (string-join (make-list num-args "%s") " ")
                   "\n")))
    (apply 'format fmt args)))


;; -=-= Main
(defun oai--debug (&rest args)
  "If firt argument of ARGS is a stringwith %s than behave like format.
Otherwise format every to string and concatenate.
Return last argument, but should not be used for return value."
  (when (and (or oai-debug-buffer
                 (bound-and-true-p ert-enabled))
             args)

    (save-excursion
      (let* ((buf-exist (and oai-debug-buffer (get-buffer oai-debug-buffer)))
             (bu (or buf-exist
                     (and (bound-and-true-p ert-enabled) (current-buffer))
                     (get-buffer-create oai-debug-buffer)))
             (current-window (selected-window))
             (bu-window (or (get-buffer-window bu)
                            (when (not (eq last-input-event 7)) ; not C-g exit - too much verbose
                              (if (>= (count-windows) 2)
                                  (display-buffer-in-direction ; exist but hidden
                                   bu
                                   '((direction . left)
                                     (window . new)
                                     (window-width . 0.2)))
                                ;; else
                                (display-buffer-in-direction ; exist but hidden
                                 bu
                                 '((direction . left)
                                   (window . new)))))
                            (when (not (eq last-input-event 7)) ; not C-g exit - too much verbose
                              (select-window current-window))))
             result-string)

        (with-current-buffer bu
          ;; - move point to  to bottom
          (when buf-exist ; was not created
              (goto-char (point-max))
            ;; else buffer just created
            (local-set-key "q" #'quit-window))
          ;; ;; - scroll debug buffer down
          (when (and bu-window (not (bound-and-true-p ert-enabled)))
              (with-selected-window (get-buffer-window bu)
                ;; (with-no-warnings
                   (goto-char (point-max))
                   ;; (end-of-buffer nil)
                  ;; )
                ;; (recenter '(t))
                ))
          ;; ;; - output caller function ( working, but too heavy)
          ;; (let ((caller
          ;;        (oai-debug--get-caller)))
          ;;   (when caller
          ;;     (insert "Din ")
          ;;     (insert caller)
          ;;     (insert " :")))
          ;; - output args
          (save-match-data
            ;; if first line is a string with %s we output all at one line
            (if (and (equal (type-of (car args)) 'string)
                     (string-match "%s" (car args)))
                ;; format %s
                ;; (setq result-string (concat (apply #'format (car args) (cdr args)) "\n"))
                (setq result-string (apply #'oai-debug--safe-format args)) ; (concat (apply #'format (car args) (cdr args)) "\n"))

              ;; else - output arguments line by line
              (setq result-string (concat (oai-debug--format-argument (car args))
                                          (when (cdr args)
                                            (concat
                                             "```debug\n" (apply #'concat (mapcar #'oai-debug--format-argument
                                                                               (cdr args)))
                                             "```\n")))))
            (when (and oai-debug-filter
                       (not (string-match-p (regexp-quote oai-debug-filter) result-string)))
                    (setq result-string nil))
            (when result-string
              ;; - add timestamp
              (when oai-debug-timestamp-flag
                (let* ((time (current-time))
                       (time-mili (format "%s.%03d"
                                          (format-time-string "%M:%S" time)
                                          (/ (nth 2 time) 1000))))
                  (setq result-string (concat time-mili " " result-string))))
              ;; - two ways to output: for ert.el and to debug buffer.
              (if (bound-and-true-p ert-enabled)
                  (princ (concat result-string "\n"))
                ;; else
                (insert result-string))))))))
  (car (reverse args)))

;; -=-= Helping function
(defun oai-debug--prettify-json-string (json-string)
  "Convert a compact JSON string to a prettified JSON string.
This function uses a temporary buffer to perform the prettification.
Returns the prettified JSON string.
Argument JSON-STRING string with json."
  (condition-case err
      (let* ((parsed-json (json-read-from-string json-string))
             ;; 1. First, encode the JSON object. This will be compact with your json-encode.
             (compact-json (json-encode parsed-json)))
        (with-temp-buffer
           (insert compact-json)
           (json-pretty-print-buffer)
           (buffer-string)))
    (error
        (message "Error formatting JSON: %S" err)
        (message "Input JSON: %S" json-string))))


;; (oai--debug "test %s" 2)
;; (oai--debug "test" 2 3 "sd")

(provide 'oai-debug)
;;; oai-debug.el ends here
