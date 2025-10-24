;;; oai-tests-restapi.el --- Tests. -*- lexical-binding: t; -*-

;; Copyright (c) 2025 github.com/Anoncheg1,codeberg.org/Anoncheg
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Author: github.com/Anoncheg1,codeberg.org/Anoncheg
;; Keywords: tools, async, callback
;; URL: https://github.com/Anoncheg1/emacs-async1
;; Version: 0.1
;; Created: 25 Aug 2025
;; Package-Requires: ((emacs "24.1"))


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

;; (eval-buffer)
;; (ert t)
;;

;;; Code:

(defun oai-tests--progress-reporter-stop-one ()
  "Start one request.
Stop it with `oai-restapi-stop-url-request'."

  (let ((buf (generate-new-buffer "*oai-test-temp*")))
    (with-current-buffer buf
      (org-mode)
      (insert "#+begin_ai\n#+end_ai")
      (goto-char (point-min))
      (oai-block-p)
      ))
  )

;;; - For `oai-restapi--get-token' (old)

;; (require 'oai) ;; Assuming the function is defined in oai.el

(ert-deftest oai-restapi--get-token-string-test ()
  "Test when oai-restapi-con-token is a non-empty string."
  (let ((oai-restapi-con-token "test-token-123"))
    (should (equal (oai-restapi--get-token 'openai) "test-token-123")))) ; ignored

(ert-deftest oai-restapi--get-token-plist-valid-test ()
  "Test when oai-restapi-con-token is a plist with valid service token."
  (let ((oai-restapi-con-token '(:openai "test-token-openai" :anthropic "test-token-anthropic")))
    (should (equal (oai-restapi--get-token :openai) "test-token-openai"))))

;; (let ((oai-restapi-con-token '(:openai "test-token-openai" :anthropic "test-token-anthropic")))
;;     (oai-restapi--get-token :openai))

(ert-deftest oai-restapi--get-token-plist-invalid-test ()
  "Test when oai-restapi-con-token is a plist without the service token."
  (let ((oai-restapi-con-token '(:anthropic "test-token-anthropic")))
    (let ((err (cadr
                (should-error (oai-restapi--get-token :openai) :type 'error))))
      (should (eql 0 (string-match "Token not found" err))))))

;; (let ((oai-restapi-con-token '(:anthropic "test-token-anthropic")))
;;   (oai-restapi--get-token :openai))

;; (ert-deftest oai-restapi--get-token-auth-source-test ()
;;   "Test when token is retrieved from auth-source."
;;   (let ((oai-restapi-con-token "")
;;         (auth-sources '((:host "api.openai.com" :user "user" :secret "auth-token-123"))))
;;     (fset 'oai-restapi--get-token-auth-source (lambda (service) "auth-token-123"))
;;     (should (equal (oai-restapi--get-token 'openai) "auth-token-123"))
;;     (fmakunbound 'oai-restapi--get-token-auth-source)))


(ert-deftest oai-restapi--get-token-auth-source-test ()
  "Test when token is retrieved from auth-source."
  (let* ((oai-restapi-con-token "")
         (auth-sources '((:host "api.openai.com" :user "user" :secret "auth-token-123")))
         (orig-fn (symbol-function 'oai-restapi--get-token-auth-source)))
    (unwind-protect
        (progn
          (fset 'oai-restapi--get-token-auth-source (lambda (service) (setq service service) "auth-token-123"))
          (should (equal (oai-restapi--get-token 'openai) "auth-token-123")))
      (fset 'oai-restapi--get-token-auth-source orig-fn))))

;; (ert-deftest oai-restapi--get-token-no-valid-token-test ()
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

(ert-deftest oai-restapi--get-token-no-valid-token-test ()
  "Test when no valid token is provided."
  (let ((oai-restapi-con-token "")
        (auth-sources nil)
        (orig-fn (symbol-function 'oai-restapi--get-token-auth-source)))
    (unwind-protect
        (progn
          (fset 'oai-restapi--get-token-auth-source (lambda (service) (setq service service) nil))
          (let ((err (cadr
                      (should-error (oai-restapi--get-token :openai) :type 'error))))
            (should (eql 0 (string-match "Please set" err)))))
      (fset 'oai-restapi--get-token-auth-source orig-fn))))
;;;
;;; - For `oai-restapi--get-token'
;; Dummy function for auth-source behavior
;; (defun oai-restapi--get-token-auth-source (service) nil)

(ert-deftest oai-restapi--get-token/string ()
  "Single string in `oai-restapi-con-token` returns value."
  (let ((oai-restapi-con-token "tok123"))
    (should (equal (oai-restapi--get-token "foo") "tok123"))))

;; (ert-deftest oai-restapi--get-token/empty-string-error ()
;;   "Empty string errors out."
;;   (let ((oai-restapi-con-token ""))
;;     (let ((err (cadr
;;                 (should-error (oai-restapi--get-token :openai) :type 'error))))
;;       ;; (print err)
;;       (should (eql 0 (string-match "Please set" err)))
;;       )))

(ert-deftest oai-restapi--get-token/plist-string ()
  "Plist with symbol key, single string."
  (let ((oai-restapi-con-token '(:foo "tokfoo")))
    (should (equal (oai-restapi--get-token "foo") "tokfoo"))))

(ert-deftest oai-restapi--get-token/plist-list-by-index ()
  "Plist with key and list of strings, access by index."
  (cl-labels ((oai-restapi--split-dash-number (s) (setq s s) (cons "foo" 1))) ;; fake service splitting
    (let ((oai-restapi-con-token '(:foo ("tok0" "tok1"))))
      (should (equal (oai-restapi--get-token "foo--1") "tok1")))))

(ert-deftest oai-restapi--get-token/plist-list-car ()
  "Plist with key and list of strings, no index (get car)."
  (let ((oai-restapi-con-token '(:foo ("tok0" "tok1"))))
    (should (equal (oai-restapi--get-token "foo") "tok0"))))

(ert-deftest oai-restapi--get-token/plist-error-when-key-not-found ()
  "Plist with missing key errors."
  (let ((oai-restapi-con-token '(:foo "tokfoo")))
    (let ((err (cadr
                (should-error (oai-restapi--get-token "bar") :type 'error))))
      ;; (print err)
      (should (eql 0 (string-match "Token not found" err))))))


(ert-deftest oai-restapi--get-token/plist-bad-config ()
  "Plist with invalid structure signals error."
  (let ((oai-restapi-con-token '(:foo 1234)))
    (should-error (oai-restapi--get-token "foo")
                  :type 'error)))


(ert-deftest oai-restapi--get-token/missing-errors ()
  "Neither string, plist nor auth-source: signals error."
  (let ((oai-restapi-con-token nil))
    (should-error (oai-restapi--get-token "foo")
                  :type 'user-error)))


;;; - For `oai-restapi--get-headers'
(ert-deftest oai-tests-oai-restapi--get-headers()
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
;;; - oai-restapi--get-values
(ert-deftest oai-tests--oai-restapi--get-values ()
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
;;; others
(ert-deftest test-oai-restapi--strip-api-url-test ()
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


(ert-deftest oai-tests-restapi-get-set-test ()
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

(ert-deftest oai-tests-restapi-split-dash-number-test ()
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

(provide 'oai-tests-restapi)

;;; oai-tests-restapi.el ends here
