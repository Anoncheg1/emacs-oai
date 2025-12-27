;;; oai-tests-restapi.el --- Tests. -*- lexical-binding: t; -*-

;; Copyright (c) 2025 github.com/Anoncheg1,codeberg.org/Anoncheg
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Author: github.com/Anoncheg1,codeberg.org/Anoncheg

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
;;

(require 'ert)
(require 'oai-restapi)
(defvar ert-enabled nil)
;; (eval-buffer)
;; (ert t)
;;

;;; Code:

;; -=-= For `oai-restapi--get-token' (old)

;; (require 'oai) ;; Assuming the function is defined in oai.el

(ert-deftest oai-tests-restapi--get-token-string ()
  "Test when oai-restapi-con-token is a non-empty string."
  (let ((oai-restapi-con-token "test-token-123"))
    (should (equal (oai-restapi--get-token 'openai) "test-token-123")))) ; ignored

(ert-deftest oai-tests-restapi--get-token-plist-valid-test ()
  "Test when oai-restapi-con-token is a plist with valid service token."
  (let ((oai-restapi-con-token '(:openai "test-token-openai" :anthropic "test-token-anthropic")))
    (should (equal (oai-restapi--get-token :openai) "test-token-openai"))))

;; (let ((oai-restapi-con-token '(:openai "test-token-openai" :anthropic "test-token-anthropic")))
;;     (oai-restapi--get-token :openai))

(ert-deftest oai-tests-restapi--get-token-plist-invalid-test ()
  "Test when oai-restapi-con-token is a plist without the service token."
  (let ((oai-restapi-con-token '(:anthropic "test-token-anthropic")))
    (let ((err (cadr
                (should-error (oai-restapi--get-token :openai) :type 'error))))
      (should (eql 0 (string-match "Token not found" err))))))

;; (let ((oai-restapi-con-token '(:anthropic "test-token-anthropic")))
;;   (oai-restapi--get-token :openai))

;; (ert-deftest oai-tests-restapi--get-token-auth-source-test ()
;;   "Test when token is retrieved from auth-source."
;;   (let ((oai-restapi-con-token "")
;;         (auth-sources '((:host "api.openai.com" :user "user" :secret "auth-token-123"))))
;;     (fset 'oai-restapi--get-token-auth-source (lambda (service) "auth-token-123"))
;;     (should (equal (oai-restapi--get-token 'openai) "auth-token-123"))
;;     (fmakunbound 'oai-restapi--get-token-auth-source)))


(ert-deftest oai-tests-restapi--get-token-auth-source-test ()
  "Test when token is retrieved from auth-source."
  (let* ((oai-restapi-con-token "")
         (auth-sources '((:host "api.openai.com" :user "user" :secret "auth-token-123")))
         (orig-fn (symbol-function 'oai-restapi--get-token-auth-source)))
    (unwind-protect
        (progn
          (fset 'oai-restapi--get-token-auth-source (lambda (service) (setq service service) "auth-token-123"))
          (should (equal (oai-restapi--get-token 'openai) "auth-token-123")))
      (fset 'oai-restapi--get-token-auth-source orig-fn))))

;; (ert-deftest oai-tests-restapi--get-token-no-valid-token-test ()
;;   "Test when no valid token is provided."
;;   (let ((oai-restapi-con-token "")
;;         (auth-sources nil))
;;     (fset 'oai-restapi--get-token-auth-source (lambda (service) nil))
;;     (let ((err (cadr
;;                 (should-error (oai-restapi--get-token :openai) :type 'error))))
;;       ;; (print err)
;;       (should (eql 0 (string-match "Please set" err))))

;;     )
;;     (fmakunbound 'oai-restapi--get-token-auth-source))

(ert-deftest oai-tests-restapi--get-token-no-valid-token-test ()
  "Test when no valid token is provided."
  (let ((oai-restapi-con-token "")
        (auth-sources nil)
        (orig-fn (symbol-function 'oai-restapi--get-token-auth-source)))
    (unwind-protect
        (progn
          (fset 'oai-restapi--get-token-auth-source (lambda (service) (setq service service) nil))

          (let ((err (cadr
                      (should-error (oai-restapi--get-token :openai) :type 'error))))
            ;; (print err)))))
            (should (string-match "Please set" err)))
          (setq oai-restapi-con-token '(:asd nil))
          (let ((err (cadr
                      (should-error (oai-restapi--get-token :openai) :type 'error))))
            ;; (print err)))))
            (should (string-match "ot found" err))))
      (fset 'oai-restapi--get-token-auth-source orig-fn))))
;;;
;; -=-= For `oai-restapi--get-token'
;; Dummy function for auth-source behavior
;; (defun oai-restapi--get-token-auth-source (service) nil)

(ert-deftest oai-tests-restapi--get-token/string ()
  "Single string in `oai-restapi-con-token` returns value."
  (let ((oai-restapi-con-token "tok123"))
    (should (equal (oai-restapi--get-token "foo") "tok123"))))

;; (ert-deftest oai-tests-restapi--get-token/empty-string-error ()
;;   "Empty string errors out."
;;   (let ((oai-restapi-con-token ""))
;;     (let ((err (cadr
;;                 (should-error (oai-restapi--get-token :openai) :type 'error))))
;;       ;; (print err)
;;       (should (eql 0 (string-match "Please set" err)))
;;       )))

(ert-deftest oai-tests-restapi--get-token/plist-string ()
  "Plist with symbol key, single string."
  (let ((oai-restapi-con-token '(:foo "tokfoo")))
    (should (equal (oai-restapi--get-token "foo") "tokfoo"))))

(ert-deftest oai-tests-restapi--get-token/plist-list-by-index ()
  "Plist with key and list of strings, access by index."
  (cl-labels ((oai-restapi--split-dash-number (s) (setq s s) (cons "foo" 1))) ;; fake service splitting
    (let ((oai-restapi-con-token '(:foo ("tok0" "tok1"))))
      (should (equal (oai-restapi--get-token "foo--1") "tok1")))))

(ert-deftest oai-tests-restapi--get-token/plist-list-car ()
  "Plist with key and list of strings, no index (get car)."
  (let ((oai-restapi-con-token '(:foo ("tok0" "tok1"))))
    (should (equal (oai-restapi--get-token "foo") "tok0"))))

(ert-deftest oai-tests-restapi--get-token/plist-error-when-key-not-found ()
  "Plist with missing key errors."
  (let ((oai-restapi-con-token '(:foo "tokfoo")))
    (let ((err (cadr
                (should-error (oai-restapi--get-token "bar") :type 'error))))
      ;; (print err)
      (should (eql 0 (string-match "Token not found" err))))))


(ert-deftest oai-tests-restapi--get-token/plist-bad-config ()
  "Plist with invalid structure signals error."
  (let ((oai-restapi-con-token '(:foo 1234)))
    (should-error (oai-restapi--get-token "foo")
                  :type 'error)))


(ert-deftest oai-tests-restapi--get-token/missing-errors ()
  "Neither string, plist nor auth-source: signals error."
  (let ((oai-restapi-con-token nil))
    (should-error (oai-restapi--get-token "foo")
                  :type 'user-error)))


;; -=-= For `oai-restapi--get-headers'
(ert-deftest oai-tests-restapi--get-headers()
  (let ((oai-restapi-con-token '(:local1
                                 :github ("token1" "token2" "token3")
                                 :some "vv"
                                 :local2 nil)))

    (should (equal (oai-restapi--get-values oai-restapi-con-token "local1") '(nil)))
    (should (equal (oai-restapi--get-values oai-restapi-con-token "local2") '(nil)))
    (should (equal (oai-restapi--get-values-enhanced oai-restapi-con-token "local1") '(nil)))
    (should (equal (oai-restapi--get-values-enhanced oai-restapi-con-token "local2") '(nil)))
    (should (equal (oai-restapi--get-values-enhanced oai-restapi-con-token "github--0") '("token1")))
    (should (equal (oai-restapi--get-values-enhanced oai-restapi-con-token "github--1") '("token2")))
    (should (equal (oai-restapi--get-values-enhanced oai-restapi-con-token "github--3") nil))
    (should-error (oai-restapi--get-token "github--3")
                  :type 'user-error)
    (should (string-equal (oai-restapi--get-token "github--1") "token2"))
    (should (string-equal (oai-restapi--get-token :some) "vv"))
    (should (equal (oai-restapi--get-token :local1) nil))
    (should (equal (oai-restapi--get-headers "local2") '(("Content-Type" . "application/json"))))
    (should (equal (oai-restapi--get-headers "github--1") '(("Content-Type" . "application/json") ("Authorization" . "Bearer token2"))))
    (should-error (oai-restapi--get-headers "local3")
                  :type 'user-error)
    ))
;; -=-= For `oai-restapi--get-values'
(ert-deftest oai-tests-restapi--oai-restapi--get-values ()
  ;; Example variables
  (defvar my-plist '(:foo "bar" :baz "qux" :bavv nil ))
  (defvar my-string "hello")

  ;; Using oai-restapi--get-value-or-string
  (should (equal (oai-restapi--get-values my-plist "foo") '("bar")))
  (should (equal (oai-restapi--get-values my-string "foo") '("hello")))
  (should (equal (oai-restapi--get-values my-plist "foo1")  nil))
  (should (equal (oai-restapi--get-values my-plist "bavv")  '(nil)))
)
;;         (oai-block--set-variable

;; ;;     (with-current-buffer buf
;; ;;       (org-mode)
;;   (let ((buf (generate-new-buffer "*oai-test-temp*")))
;;     ))

;; (defun oai-tests--progress-reporter-start-two-and-stop-one ()
;;   "."
;;   (let ((buf (generate-new-buffer "*oai-test-temp*")))
;;     (with-current-buffer buf
;;       (org-mode)
;;       (setq-local org-export-with-properties t) ; Ensure properties are considered
;;       (when properties-alist
;;         (dolist (prop properties-alist)
;;           (insert (format "#+PROPERTY: %s %s\n" (car prop) (cdr prop)))))
;;       (insert block-content)
;;       (goto-char (point-min))
;;       ;; Move point to the start of the AI block to ensure `org-element-at-point` works
;;       ;; and `org-entry-get-with-inheritance` can find properties.
;;       (search-forward "#+begin_ai")
;;       (let* ((element (org-element-at-point))
;;              ;; org-element-property :parameters returns a plist, which alist-get works on.
;;              (info-alist (org-element-property :parameters element)))
;;         element))))


;; -=-= Handling non-unicode characters at input in url-buffer
;; (progn
;;   (let (
;;         (json-object-type 'plist)
;;         (json-key-type 'symbol)
;;         (json-array-type 'vector)
;;         (garbage-str (concat (string ?\x81 ?\xA0 ?\xFF)))
;;          data)
;;     (setq data (with-temp-buffer
;;       ;; (insert "{\"choices\":[{\"message\":{\"annotations\":[],\"content\":\"How can I perform a test?\\n\\n\",\"refusal\":null,\"role\":\"assistant\"}}]}")
;;       (insert (concat "{\"choices\":[{\"message\":{\"annotations\":[],\"content\":\"How can I perform a test?"
;;                       garbage-str
;;                       "\\n\\n\",\"refusal\":null,\"role\":\"assistant\"}}]}"))
;;       (goto-char (point-min))
;;       (json-read)))
;;     (setq data (aref (plist-get data 'choices) 0))
;;     (print (list "data1" data))
;;     (setq data (plist-get (plist-get data 'message) 'content))
;;     (print (list "data2" data))
;;     ;; (print (alist-get 'choices data))
;; ))


;; (let ((json-object-type 'plist)
;;       (json-key-type 'symbol)
;;       (json-array-type 'vector))
;;   (condition-case _err
;;       (json-read-from-string (concat
;;                               (string-as-unibyte (string ?\x81 ?\xA0 ?\xFF )) ; garbage-str
;;                               (string ?\x10) ; garbage
;;                               "\\n\\n\",\"refusal\":null,\"role\":\"assistant\"}}]}"))
;;   (error
;;    nil
;;    )))

;; -=-= For: `oai-restapi--normalize-response'
(ert-deftest oai-tests-restapi--normalize-response ()
  (should
   (equal
    (let ((test-val '(id "o3fA4D4-62bZhn-9617f44f6d399d91" object "chat.completion" created 1752904364 model "meta-llama/Llama-3.3-70B-Instruct-Turbo-Free" prompt [] choices [(finish_reason "stop" seed 819567834314233700 logprobs nil index 0 message (role "assistant" content "It works: `(2 3 1)` is returned." tool_calls []))] usage (prompt_tokens 131 completion_tokens 14 total_tokens 145 cached_tokens 0))))
      (oai-restapi--normalize-response test-val))
    '(#s(oai-restapi--response role "assistant") #s(oai-restapi--response text "It works: `(2 3 1)` is returned.") #s(oai-restapi--response stop "stop")))))

;; -=-= For: `oai-restapi--response-payload'

(ert-deftest oai-tests-restapi--response-payload ()
  (let* ((test-val '(#s(oai-restapi--response role "assistant") #s(oai-restapi--response text "It seems ") #s(oai-restapi--response stop "length")))
       (test-val0 (nth 0 test-val))
       (test-val1 (nth 1 test-val)))
   (should (equal (length test-val) 3))
   (should (equal (oai-restapi--response-type test-val0) 'role))
   (should (string-equal (decode-coding-string (oai-restapi--response-payload test-val0) 'utf-8) "assistant"))
   (should (equal (oai-restapi--response-type test-val1) 'text))
   (should (string-equal (decode-coding-string (oai-restapi--response-payload test-val1) 'utf-8) "It seems "))))

;; -=-= For: `oai-restapi--url-request-on-change-function'
(defvar callback-n-test 0)
(defvar callback-test nil)

(defun oai-tests-restapi--callback (data)
  (when (= callback-n-test 0)
    (setq callback-n-test (1+ callback-n-test))
    (setq callback-test data)))


;; (defun unicode-string-filter (str)
;;   "Return a string with only valid Unicode code points from STR."
;;   (apply 'string
;;          (seq-filter
;;           (lambda (ch)
;;             (and (<= 0 ch #x10FFFF)       ;; unicode range
;;                  (not (and (>= ch #xD800) (<= ch #xDFFF))) ;; skip surrogates
;;                  (characterp ch)))        ;; ensure is a char
;;           (string-to-list str))))

;; (defun text-clean-unicode-string (str)
;;   "Return STR with only printable, standard Unicode text.
;; Removes ASCII control chars (except tab/newline/CR), surrogates, private use, noncharacters,
;; and other non-text code points. Suitable for JSON parsing."
;;   (apply 'string
;;          (seq-filter
;;           (lambda (ch)
;;             (and
;;              (characterp ch)
;;              (<= 0 ch #x10FFFF)
;;              (let* ((cat (get-char-code-property ch 'general-category))
;;                     (cat-str (if (symbolp cat) (symbol-name cat) cat)))
;;                (or
;;                  ;; Allow typical printable categories
;;                  (member (substring cat-str 0 1) '("L" "M" "N" "P" "S"))
;;                  ;; Allow space and separators
;;                  (member cat-str '("Zs" "Zl" "Zp"))
;;                  ;; Allow tab, newline, carriage return explicitly
;;                  (member ch '(?\t ?\n ?\r))))
;;              ;; Exclude surrogate range
;;              (not (and (>= ch #xD800) (<= ch #xDFFF)))
;;              ;; Remove Unicode noncharacters
;;              (not (or
;;                    (and (>= ch #xFDD0) (<= ch #xFDEF))
;;                    (= (mod ch #x10000) #xFFFE)
;;                    (= (mod ch #x10000) #xFFFF)))))
;;           (string-to-list str))))

;; (defun ultra-aggressive-ascii-filter (str)
;;   "Return STR containing only safe printable ASCII characters and essential whitespace.
;; Filters out all control/non-ASCII/format/non-printable/surrogate/private-use/noncharacters."
;;   (apply 'string
;;          (seq-filter
;;           (lambda (ch)
;;             (cond
;;              ;; Printable ASCII (codes 32-126: ' ', '!'..'~')
;;              ((and (>= ch 32) (<= ch 126)) t)
;;              ;; Essential whitespace: tab, newline, carriage return
;;              ((member ch '(?\t ?\n ?\r)) t)
;;              ;; Everything else excluded!
;;              (t nil)))
;;           (string-to-list str))))
;; (defun aggressive-unicode-filter (str)
;;   "Return STR with only Unicode letters, numbers, plus essential whitespace.
;; Removes all control, format, separator, symbol, private, and noncharacter code points."
;;   (apply 'string
;;          (seq-filter
;;           (lambda (ch)
;;             (let* ((cat (get-char-code-property ch 'general-category))
;;                    (cat-str (if (symbolp cat) (symbol-name cat) cat))
;;                    (cat1 (substring cat-str 0 1)))
;;               (or
;;                (member cat1 '("L" "N")) ; letters and numbers
;;                (member ch '(?\t ?\n ?\r ?\x20))))) ; whitespace
;;           (string-to-list str))))

(ert-deftest oai-tests-restapi--url-request-on-change-function-not-streamed()
  (with-temp-buffer
    ;; set vars,functions used in `oai-restapi--url-request-on-change-function'
    (let ((oai-restapi--current-url-request-callback 'oai-tests-restapi--callback)
          oai-restapi--current-request-is-streamed
          ;; oai-debug-buffer
          (callback-n-test 0)
          (payload-str (concat "{\"choices\":[{\"message\":{\"annotations\":[],\"content\":\"How can \\tI perform a test 再次?"
                               (concat
                                (string-as-unibyte (string ?\x81 ?\xA0 ?\xFF )) ; garbage-str
                                (string ?\x05) ; garbage
                                "\\n\\n\",\"refusal\":null,\"role\":\"assistant\"}}]}"))))
      ;; (setq payload-str (clean-unicode-text payload-str ))
      ;; (setq payload-str (decode-coding-string (encode-coding-string payload-str 'utf-8 't) 'utf-8))
      (insert payload-str)
      (goto-char (point-min))
      (setq url-http-end-of-headers (point-min)) ; should set globally, checked by `boundp'
      ;; (print (list (boundp 'url-http-end-of-headers) url-http-end-of-headers))
      ;; (funcall oai-restapi--current-url-request-callback "data")
      (oai-restapi--url-request-on-change-function nil nil nil)
      ;; (print (list "wtf" callback-test))
      ;; (print (list "wtf" callback-test))
      (let* ((data (aref (plist-get callback-test 'choices) 0))
             (data (plist-get (plist-get data 'message) 'content))
             (length (length data) ))
        (should (> length 25))
        ))))

(ert-deftest oai-tests-restapi--url-request-on-change-function-streamed()
  (with-temp-buffer
    ;; set vars,functions used in `oai-restapi--url-request-on-change-function'
    (let ((oai-restapi--current-url-request-callback 'oai-tests-restapi--callback)
          (oai-restapi--current-request-is-streamed t)
          (callback-test nil)
          ;; oai-debug-buffer
          (callback-n-test 0)
          (payload-str (concat "data: {\"choices\":[{\"finish_reason\":\"stop\",\"index\":0,\"delta\":{\"content\":\"Text"
                               (concat
                                (string-as-unibyte (string ?\x81 ?\xA0 ?\xFF )) ; garbage-str
                                (string ?\x05) ; garbage
                                "\"}}]}")))
          data)
      ;; (setq payload-str (clean-unicode-text payload-str ))
      ;; (setq payload-str (decode-coding-string (encode-coding-string payload-str 'utf-8 't) 'utf-8))
      (insert payload-str)
      (insert "\n\n")
      (insert "data: [DONE]")
      (insert "\n\n")
      (goto-char (point-min))
      (setq url-http-end-of-headers (point-min)) ; should set globally, checked by `boundp'
      (oai-restapi--url-request-on-change-function nil nil nil)
      ;; (print callback-test)))
      ;; (print (plist-get callback-test 'choices))))
      ;; (print (list "aa" (plist-get (plist-get (aref (plist-get callback-test 'choices) 0) 'message) 'content) "bb"))))
      ;; (print (oai-restapi--normalize-response callback-test))))
      ;; (print (oai-restapi--normalize-response callback-test))))
      (setq data (decode-coding-string (oai-restapi--response-payload (nth 0 (oai-restapi--normalize-response callback-test))) 'utf-8))
      ;; (print (list (length data) data))))
      ;; ;; (print (list "data2" (length data) data ))
      ;; (should (string-equal "Text ÿ"  data)
      (should (= (length data) 7))
      )))
    ;; (let ((json-object-type 'plist)
    ;;                 (json-key-type 'symbol)
    ;;                 (json-array-type 'vector))
    ;;                 (let ( ; error
    ;;                       (data (json-read-from-string
    ;;                              (buffer-substring-no-properties (point) (point-max))))
    ;;                       ;; (data (json-read))  ; problem: with codepage, becaseu url buffer not utf-8
    ;;                       )
    ;;                   (when data
    ;;                     (print data))
    ;; ))


;; -=-= For: `oai-restapi--strip-api-url'
(ert-deftest oai-tests-restapi--strip-api-url-test ()
  "Runs tests for `oai-restapi--strip-api-url` explicitly for each case,
   without using a loop or an explicit assert function."

  (should (string= (oai-restapi--strip-api-url "https://api.perplexity.ai/chat/completions") "api.perplexity.ai"))

  (should (string= (oai-restapi--strip-api-url "http://www.example.com/path/to/file") "www.example.com"))

  ;; (should (string= (oai-restapi--strip-api-url "ftp://some.server.org") "some.server.org")
  ;;   (error "Test 3 Failed: ftp://some.server.org"))

  (should (string= (oai-restapi--strip-api-url "no-protocol.com/stuff") "no-protocol.com"))

  (should (string= (oai-restapi--strip-api-url "http://www.google.com/search?q=elisp") "www.google.com"))

  (should (string= (oai-restapi--strip-api-url "localhost:8080/app") "localhost:8080"))

  (should (string= (oai-restapi--strip-api-url "example.com") "example.com"))

  (should (string= (oai-restapi--strip-api-url "https://sub.domain.co.uk") "sub.domain.co.uk"))

  (should (string= (oai-restapi--strip-api-url "domain.com/") "domain.com"))

  (should (string= (oai-restapi--strip-api-url "localhost") "localhost"))

  ;; (should (string= (oai-restapi--strip-api-url "") "")
  ;;   (error "Test 11 Failed: empty string"))

  ;; (message "All individual tests passed for oai-restapi--strip-api-url!")
  t) ; Return t for success


;; -=-= For: `oai-restapi--get-values-enhanced'

(ert-deftest oai-tests-restapi--get-values-enhanced ()
  (should (equal (oai-async1-plist-get '(:zaza :foo 1 :bar nil) :zaza) nil))
  (should (equal (oai-restapi--get-values '(:foo 1 :bar nil) :foo)	'(1)))
  (should (equal (oai-restapi--get-values '(:foo 1 :bar nil) :bar)	'(nil))) ; value is nil
  (should (equal (oai-restapi--get-values '(:foo 1 :bar nil) :baz)	nil)) ; not exist
  (should (equal (oai-restapi--get-values '(:foo 1 :bar nil) :zaza)	nil)) ; not exist
  (should (equal (oai-restapi--get-values '(:only) :only)		'(nil)))  ; no value
  (should (equal (oai-restapi--get-values "something" "vvv")		'("something")))
  (should (equal (oai-restapi--get-values '(:foo (1 2) :bar nil) :foo)	'(1 2))) ; list of values
  (should (equal (oai-restapi--get-values nil "vvv")		nil))
  (should (equal (oai-restapi--get-values '(:zaza :foo 1 :bar nil) :zaza)	'(nil))) ; value is null
  (should (equal
            (let ((oai-restapi-con-token '(:local1
                                           :github ("token1" "token2" "token3")
                                           :some "vv"
                                           :local2 nil)))
              (oai-restapi--get-values-enhanced oai-restapi-con-token "github--3")) nil))
  )
;; -=-= For: `oai-restapi--split-dash-number'

(ert-deftest oai-tests-restapi--split-dash-number-test ()
  (should-error (oai-restapi--split-dash-number nil))
  (should (equal (oai-restapi--split-dash-number "foo")
                                                 nil))
  (should (equal (oai-restapi--split-dash-number "foo--")
                                                 nil))
  (should (equal (oai-restapi--split-dash-number "foo--23")
                                                 '("foo" . 23)))
  (should (equal (oai-restapi--split-dash-number "--1")
                                                 '("" . 1)))
  (should (equal (oai-restapi--split-dash-number "a--b")
                                                 nil))
  (should (equal (oai-restapi--split-dash-number "foo--2.4")
                                                 nil)))


;; -=-= For: `oai-restapi--get-single-response-text'

(ert-deftest oai-tests-restapi--get-single-response-text ()
  (should
   (string-equal
    (let ((test-val
           '(id "nz7KyaB-3NKUce-9539d1912ce8b148" object "chat.completion" created 1750575101 model "meta-llama/Llama-3.3-70B-Instruct-Turbo-Free" prompt []
                choices [(finish_reason "length" seed 3309196889559996400 logprobs nil index 0
                                        message (role "assistant" content " The answer is simple: live a long time. But how do you do that? Well, itâs not as simple as it sounds." tool_calls []))] usage (prompt_tokens 5 completion_tokens 150 total_tokens 155 cached_tokens 0))))
      (oai-restapi--get-single-response-text test-val))
    " The answer is simple: live a long time. But how do you do that? Well, itâs not as simple as it sounds.")))


(provide 'oai-tests-restapi)

;; -=-= For: `oai-restapi--collect-chat-messages' (old)
;; (ert-deftest oai-tests-restapi--collect-chat-messages ()
;;   ;; deal with unspecified prefix
;;   (should
;;    (equal
;;     (let ((test-string "\ntesting\n  [ME]: foo bar baz zorrk\nfoo\n[AI]: hello hello[ME]: "))
;;       (oai-restapi--collect-chat-messages test-string))
;;     '[(:role user :content "testing\nfoo bar baz zorrk\nfoo")
;;       (:role assistant :content "hello hello")]))

;;   ;; sys prompt
;;   (should
;;    (equal
;;     (let ((test-string "[SYS]: system\n[ME]: user\n[AI]: assistant"))
;;       (oai-restapi--collect-chat-messages test-string))
;;     '[(:role system :content "system")
;;       (:role user :content "user")
;;       (:role assistant :content "assistant")]))

;;   ;; sys prompt intercalated
;;   (should
;;    (equal
;;     (let ((test-string "[SYS]: system\n[ME]: user\n[AI]: assistant\n[ME]: user"))
;;       (oai-restapi--collect-chat-messages test-string nil t))
;;     '[(:role system :content "system")
;;       (:role user :content "user")
;;       (:role assistant :content "assistant")
;;       (:role system :content "system")
;;       (:role user :content "user")]))

;;   ;; merge messages with same role
;;   (should
;;    (equal
;;     (let ((test-string "[ME]: hello [ME]: world")) (oai-restapi--collect-chat-messages test-string))
;;     '[(:role user :content "hello\nworld")]))

;;   (should
;;    (equal
;;     (let ((test-string "[ME:] hello world")) (oai-restapi--collect-chat-messages test-string))
;;     '[(:role user :content "hello world")]))

;;   (should
;;    (equal
;;     (let ((test-string "[ME]: hello [ME:] world")) (oai-restapi--collect-chat-messages test-string))
;;     '[(:role user :content "hello\nworld")]))

;;   (should
;;    (equal
;;     (let ((test-string "  [ME]: hello [ME]: world")) (oai-restapi--collect-chat-messages test-string))
;;     '[(:role user :content "hello\nworld")]))

;;   )

;; -=-= For: `oai-restapi--stringify-chat-messages' (old)
;; (ert-deftest oai-tests-restapi--stringify-chat-messages ()
;;   (should
;;    (string-equal
;;     (oai-restapi--stringify-chat-messages '[(:role system :content "system")
;;                                             (:role user :content "user")
;;                                             (:role assistant :content "assistant")])
;;     "[SYS]: system\n\n[ME]: user\n\n[AI]: assistant"))

;;   (should
;;    (string-equal
;;     (oai-restapi--stringify-chat-messages '[(:role user :content "user")
;;                                             (:role assistant :content "assistant")]
;;                                           :default-system-prompt "system")
;;     "[SYS]: system\n\n[ME]: user\n\n[AI]: assistant"))

;;   (should
;;    (string-equal
;;     (oai-restapi--stringify-chat-messages '[(:role user :content "user")
;;                                             (:role assistant :content "assistant")]
;;                                           :user-prefix "You: "
;;                                           :assistant-prefix "Assistant: ")
;;     "You: user\n\nAssistant: assistant")))

;; -=-= For: `oai-restapi--modify-last-user-content'
(ert-deftest oai-tests-restapi--modify-last-user-content ()
  (should
   (equal (oai-restapi--modify-last-user-content
           (vector (list :role 'system :content "foo")
                   (list :role 'user :content "How to make coffe1?")
                   (list :role 'assistant :content "IDK.")
                   (list :role 'user :content "How to make coffe2?")
                   (list :role 'system :content "other"))
           (lambda (x) (concat x " w11")))
          '[(:role system :content "foo")
            (:role user :content "How to make coffe1?")
            (:role assistant :content "IDK.")
            (:role user :content "How to make coffe2? w11")
            (:role system :content "other")])))

(provide 'oai-tests-restapi)
;;; oai-tests-restapi.el ends here
