;;; oai-tests-integration.el --- AI blocks for org-mode. -*- lexical-binding: t; -*-

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


;; (eval-buffer)
;; (ert t)
;; emacs -Q --batch -l ert.el -l oai-debug.el -l oai-block.el -l oai-tests-block.el -l oai-tests-integration.el -f ert-run-tests-batch-and-exit

(require 'oai-tests-block)
(require 'oai)
(require 'ert)


;;; Commentary:
;;

;;; Code:

(defun oai-tests--my-http-server-handler (proc string)
  "(message \"in my-http-server-handler: %s\" string)"
  (setq string string) ; noqa Unused lexical argument
  (process-send-string
   proc
   "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n{\"choices\":[{\"finish_reason\":\"length\",\"message\":{\"role\":\"assistant\",\"content\":\"Your question needs clarification.\"}}]}\n")
  (delete-process proc))


(defun oai-tests--create-http-service ()
  "to test: curl -v 127.0.0.1:9239"
  (make-network-process
  :name "my-http-server"
  :buffer "*my-http-server*"
  :family 'ipv4
  :service 9239
  ;; Try :host nil or "127.0.0.1" for clarity
  :host "127.0.0.1"
  :server t
  :filter 'oai-tests--my-http-server-handler)

;; (make-network-process :name "localhost" :host "127.0.0.1" :service 9239 :nowait t)

;; (delete-process "my-http-server")

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
  )


(ert-deftest oai-tests-integration-test ()
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
           (signal (car err) (cdr err)))) ; re-signal error (does not suppress)
        ))
    (run-at-time 1 nil (lambda (buf) (with-current-buffer buf
                                       ;; (print "#+begin_ai :stream nil :service test :model none\nTest content\n\n[AI]: Your question needs clarification.\n\n[ME]:\n#+end_ai")
                                       ;; (print (list "wtf" (buffer-substring-no-properties (point-min) (point-max) )
                                       ;; (message "A:%S\nB:%S"
                                       ;; (print (list "oai-restapi-after-chat-insertion-hook" oai-restapi-after-chat-insertion-hook))
                                       ;; (message "A:%S\nB:%S" (buffer-substring-no-properties (point-min) (point-max) )
                                       ;;          "#+begin_ai :stream nil :service test :model none\nTest content\n\n[AI]: Your question needs clarification.\n\n[ME]: \n#+end_ai")
                                       (should (string-equal
                                                (buffer-substring-no-properties (point-min) (point-max) )
                                                "#+begin_ai :stream nil :service test :model none\nTest content\n\n[AI]: Your question needs clarification.\n\n[ME]: \n#+end_ai")
                                               ))
                         (delete-process "my-http-server"))
                 temp-buffer)))

(provide 'oai-tests-integration)

;;; oai-tests-integration.el ends here
