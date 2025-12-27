;;; oai-tests-integ.el --- Tests for oai-restapi-request-prepare. -*- lexical-binding: t; -*-

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

;; (add-to-list 'load-path (expand-file-name "./"))
;; (eval-buffer)
;; (ert t)
;; emacs -Q --batch -l ert.el -l oai-debug.el -l oai-block.el -l oai-tests-block.el -l oai-tests-integ.el -f ert-run-tests-batch-and-exit


(require 'oai-tests-block) ; for `oai-test-setup-buffer'
(require 'oai)
(require 'ert)


;;; Commentary:
;;

;;; Code:

;;; - Help functions
(defun oai-tests--my-http-server-handler (proc _string)
  "Used for HTTP server as callback.
PROC is process object.  _STRING is data received."
  (ignore _string) ; noqa Unused lexical argument
  (process-send-string
   proc
   "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n{\"choices\":[{\"finish_reason\":\"length\",\"message\":{\"role\":\"assistant\",\"content\":\"Your question needs clarification.\"}}]}\n")
  (delete-process proc))

(defun oai-tests--my-http-server-handler-stream (proc _string)
  "Used for HTTP server as callback.
PROC is process object.  _STRING is data received."
  (ignore _string) ; noqa Unused lexical argument
  (process-send-string
   proc
   "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\ndata: {\"choices\":[{\"content_filter_results\":{},\"delta\":{\"content\":\"\",\"refusal\":null,\"role\":\"assistant\"},\"finish_reason\":null,\"index\":0,\"logprobs\":null}],\"created\":1765253459,\"id\":\"chatcmpl-CkjLvJKSZym3rh3hBEeJKaGclbAQ6\",\"model\":\"gpt-4.1-2025-04-14\",\"obfuscation\":\"nZ2\",\"object\":\"chat.completion.chunk\",\"system_fingerprint\":\"fp_f99638a8d7\"}\n")
  (process-send-string
   proc
   "\ndata: {\"choices\":[{\"content_filter_results\":{\"hate\":{\"filtered\":false,\"severity\":\"safe\"},\"self_harm\":{\"filtered\":false,\"severity\":\"safe\"},\"sexual\":{\"filtered\":false,\"severity\":\"safe\"},\"violence\":{\"filtered\":false,\"severity\":\"safe\"}},\"delta\":{\"content\":\"It\"},\"finish_reason\":null,\"index\":0,\"logprobs\":null}],\"created\":1765253459,\"id\":\"chatcmpl-CkjLvJKSZym3rh3hBEeJKaGclbAQ6\",\"model\":\"gpt-4.1-2025-04-14\",\"obfuscation\":\"Bd4\",\"object\":\"chat.completion.chunk\",\"system_fingerprint\":\"fp_f99638a8d7\"}\n")
  (process-send-string
   proc
   "\ndata: {\"choices\":[{\"content_filter_results\":{},\"delta\":{},\"finish_reason\":\"stop\",\"index\":0,\"logprobs\":null}],\"created\":1765253489,\"id\":\"chatcmpl-CkjMPSMz0QAsT689A3eSGFsTWsgwf\",\"model\":\"gpt-4.1-2025-04-14\",\"obfuscation\":\"NqUvAU8sOf0FBAY\",\"object\":\"chat.completion.chunk\",\"system_fingerprint\":\"fp_f99638a8d7\"}\n")
  (process-send-string
   proc
   "\ndata: [DONE]\n")
  (delete-process proc))


(defun oai-tests--create-http-service ()
  "To test: curl -v 127.0.0.1:9239."
  (make-network-process
  :name "my-http-server"
  :buffer "*my-http-server*"
  :family 'ipv4
  :service 9239
  ;; Try :host nil or "127.0.0.1" for clarity
  :host "127.0.0.1"
  :server t
  :filter 'oai-tests--my-http-server-handler))

(defun oai-tests--create-http-service-stream ()
  "To test: curl -v 127.0.0.1:9239."
  (make-network-process
  :name "my-http-server"
  :buffer "*my-http-server*"
  :family 'ipv4
  :service 9239
  ;; Try :host nil or "127.0.0.1" for clarity
  :host "127.0.0.1"
  :server t
  :filter 'oai-tests--my-http-server-handler-stream))



  ;; - test:
  ;; (delete-process "my-http-server")
  ;; (advice-remove 'make-network-process #'my-make-network-process-advice)
  ;; (oai-tests--create-http-service)
;; (make-network-process :name "localhost" :host "127.0.0.1" :service 9239 :nowait t)



  ;; (with-current-buffer (url-retrieve-synchronously "http://localhost:9239/")
  ;;   (prog1 (buffer-string)(kill-buffer)))

  ;; (with-current-buffer (url-retrieve-synchronously "http://127.0.0.1:9239/")
  ;;   (prog1 (buffer-string) (kill-buffer)))

  ;; (display-buffer (url-retrieve-synchronously "http://localhost:9239/"))


  ;; (url-retrieve
  ;;  "http://localhost:9239/"
  ;;  (lambda (status)
  ;;    (goto-char (point-min))
  ;;    ;; (re-search-forward "\r\n\r\n")
  ;;    (message "Server replied: %s" (buffer-substring (point-min) (point-max)))))
  ;; )


;;; - Integration test: oai-restapi-request-prepare

(ert-deftest oai-tests-integ-nostream-test ()
  "`oai-restapi-request-prepare'."
  (condition-case nil
      (delete-process "my-http-server")
    (error nil))
  (oai-tests--create-http-service)
  ;; (sleep-for 1)
  (let ((temp-buffer (generate-new-buffer " *temp*" t)))
    ;; (let ((temp-buffer (get-buffer-create "tt" t)))
    ;; (let ((temp-buffer (current-buffer)))
    (with-current-buffer temp-buffer
      ;; (goto-char 1498)
      (org-mode)
      (oai-mode)
      (oai-test-setup-buffer "#+begin_ai :stream nil :service test :model none\nTest content\n#+end_ai")
      ;; (print (point))
      (let ((oai-restapi-con-endpoints (list :test "http://localhost:9239/v1/chat/completions"))
            (oai-restapi-con-token "test"))
        ;; (plist-put oai-restapi-con-endpoints :test "http://localhost:9239/v1/chat/completions")
                                        ; delete http service if error, but not suppress
        (condition-case err
            (progn
              (sleep-for 0.5) ; required
              (org-ctrl-c-ctrl-c))
          (error
           ;; (print (list "error! delete-process" (buffer-substring-no-properties (line-beginning-position) (line-end-position) )))
           (delete-process "my-http-server")   ; run your code
           (oai-timers--interrupt-current-request (current-buffer) #'oai-restapi--interrupt-url-request)
           (signal (car err) (cdr err)))) ; re-signal error (does not suppress)
        ))
    (run-at-time 1 nil (lambda (buf) (with-current-buffer buf
                                       (should (eq 112 (point)))
                                       (should (string-equal
                                                (buffer-substring-no-properties (point-min) (point-max) )
                                                "#+begin_ai :stream nil :service test :model none\nTest content\n\n[AI]: Your question needs clarification.\n\n[ME]: \n#+end_ai"
                                                )
                                               ))
                         (delete-process "my-http-server")
                         ;; (oai-timers--interrupt-current-request (current-buffer) #'oai-restapi--interrupt-url-request)
                         )
                 temp-buffer)))

;;; - Integration test: oai-restapi-request-llm-retries

(ert-deftest oai-tests-integ-stream-test ()
  "Test `oai-restapi-request-llm-retries'."
  (condition-case nil
      (delete-process "my-http-server")
    (error nil))
  (oai-tests--create-http-service-stream)
  ;; (sleep-for 1)
  (let ;;((temp-buffer (generate-new-buffer " *temp*" t)))
      ((temp-buffer (generate-new-buffer "ttemp" t)))
    ;; (let ((temp-buffer (get-buffer-create "tt" t)))
    ;; (let ((temp-buffer (current-buffer)))
    (with-current-buffer temp-buffer
      ;; (goto-char 1498)
      (org-mode)
      (oai-mode)
      (oai-test-setup-buffer "#+begin_ai :stream t :service test :model none\nTest content\n#+end_ai")
      ;; (print (point))
      (let ((oai-restapi-con-endpoints (list :test "http://localhost:9239/v1/chat/completions"))
            (oai-restapi-con-token "test")
            (oai-timers-duration 10))
        ;; (oai-restapi--get-headers "test"))
        ;; (print (list "oai-timers-duration" oai-timers-duration (current-buffer))))))

        ;; (plist-put oai-restapi-con-endpoints :test "http://localhost:9239/v1/chat/completions")
                                        ; delete http service if error, but not suppress
        (condition-case err
            (progn
              (sleep-for 0.5) ; required
              ;; (print (list "oai-timers-duration" oai-timers-duration))
              (org-ctrl-c-ctrl-c))
          (error
           ;; (print (list "error! delete-process" (buffer-substring-no-properties (line-beginning-position) (line-end-position) )))
           (delete-process "my-http-server")   ; run your code
           (signal (car err) (cdr err)))) ; re-signal error (does not suppress)
        ))
    (run-at-time 1 nil (lambda (buf) (with-current-buffer buf
                                       ;; (print (list "POINT" (point) (buffer-substring-no-properties (point) (point-max))))
                                       (should (eq 79 (point)))
                                       (should (string-equal
                                                (buffer-substring-no-properties (point-min) (point-max) )
                                                "#+begin_ai :stream t :service test :model none\nTest content\n\n[AI]: \nIt\n\n[ME]: \n#+end_ai")
                                               ))
                         (delete-process "my-http-server"))
                 temp-buffer)
    ))

(provide 'oai-tests-integ)

;;; oai-tests-integ.el ends here
