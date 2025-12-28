;;; oai-tests-block.el --- test  -*- lexical-binding: t -*-

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

;; ## To run these tests:
;; 1. Save the code to an .el file (e.g., `oai-params-test.el`).
;; 2. Open Emacs and load the file: `M-x load-file RET oai-tests2.el RET`.
;; 3. Run all tests: `M-x ert RET t RET`.
;;    Or run specific tests: `M-x ert RET oai-block--let-params-all-from-info RET`.
;; OR
;; to run: emacs -Q --batch -l ert.el -l oai-debug.el -l oai-block.el -l ./tests/oai-tests-block.el -f ert-run-tests-batch-and-exit
;; OR
;; M-x ert RET t RET
;; OR
;; (eval-buffer)
;; (ert t)

;;; Code:

(require 'oai-block)
(require 'ert)             ; Testing framework
(defvar ert-enabled nil)
;; -=-= Helper function to set up a temporary Org buffer for testing.
;; It inserts content and optional Org properties, then returns the
;; parsed Oai block element and its parameters alist.
;; (defun oai-test-setup-buffer (block-content &optional properties-alist)
;;   "Create a temporary Org buffer with BLOCK-CONTENT and optional PROPERTIES-ALIST.
;; PROPERTIES-ALIST should be an alist like '((property-name . \"value\")).
;; Returns a list (ELEMENT INFO-ALIST), where ELEMENT is the parsed Oai block
;; and INFO-ALIST is the parameters from its header."
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

(defun oai-test-setup-buffer (block-content &optional buf properties-alist)
  "Create ai BLOCK-CONTENT and optional PROPERTIES-ALIST.
In current buffer or in BUF.
PROPERTIES-ALIST should be an alist like ((property-name . \"value\")).
Set cursor at begining of buffer.
Returns a list (ELEMENT INFO-ALIST), where ELEMENT is the parsed Oai block
and INFO-ALIST is the parameters from its header."
  (with-current-buffer (or buf (current-buffer))
    (setq-local org-export-with-properties t) ; Ensure properties are considered
    (when properties-alist
      (dolist (prop properties-alist)
        (insert (format "#+PROPERTY: %s %s\n" (car prop) (cdr prop)))))
    (insert block-content)
    (goto-char (point-min))
    ;; Check if #+begin_ai exists to avoid search failure
    (unless (string-match-p "#\\+begin_ai" block-content)
      (error "Test setup failed: block-content does not contain '#+begin_ai'"))
    ;; Move point to the start of the AI block
    (unless (search-forward "#+begin_ai" nil t)
      (error "Failed to find '#+begin_ai' in buffer"))
    (beginning-of-line) ; Ensure point is at the start of the block
    (when (derived-mode-p 'org-mode)
      (let* ((element (org-element-at-point)))
        (unless (eq (org-element-type element) 'special-block)
          (error "No valid Oai block found at point"))
        element)) ; return
  ))


;; (oai-test-setup-buffer "#+begin_ai\nTest content\n#+end_ai")


;; -=-= test for test

(ert-deftest oai-tests-block--setup-buffer-basic-test ()
  "Test that `oai-test-setup-buffer' sets up a buffer correctly."
  (with-temp-buffer
    (org-mode)
    (let* ((block-content "#+begin_ai\nTest content\n#+end_ai")
           (element (oai-test-setup-buffer block-content)))
      (should (eq (org-element-type element) 'special-block))
      (should (equal (org-element-property :type element) "ai")))))

;; -=-= oai-block--let-params

(ert-deftest oai-tests-block--let-params-all-from-info-test1 ()
  "Test when all parameters are provided in the block header (info alist)."
  (with-temp-buffer
    (org-mode)
    (let* ((test-block "#+begin_ai :stream t :sys \"A helpful LLM.\" :stream2 :max-tokens 50 :max-tokens2 :model \"gpt-3.5-turbo\" :model1 :model2 t :model3 :temperature 0.7\n#+end_ai\n")
           (element (oai-test-setup-buffer test-block))
           (info (progn (goto-char (org-element-property :begin element)) (oai-block-get-info))))
      ;; (unwind-protect
      ;; Position point inside the block for correct context, though not strictly needed for info directly.


      (oai-block--let-params info ((stream) (stream2 0 :type number) (stream3 1 :type number) (sys) (max-tokens :type integer) (max-tokens2 10 :type integer) (model) (model1 nil :type string) (model2 10 :type number) (model4 nil :type number) (model3) (temperature :type float) (unknown "s"))
                             ;; (print (list max-tokens (type-of max-tokens)))
                             ;; (print (list temperature (type-of temperature)))
                             ;; (print (list unknown (type-of unknown)))
                             (should (= stream3 1))
                             (should (eq stream2 t))
                             (should (eq max-tokens2 t))
                             (should (string-equal stream "t"))
                             (should (= max-tokens 50))
                             (should (string-equal sys "A helpful LLM."))
                             (should (string-equal model "gpt-3.5-turbo"))
                             (should (= temperature 0.7))
                             (should (string-equal unknown "s"))
                             (should (string-equal model1 nil))
                             (should (= model2 0))
                             (should (string-equal model4 nil))
                             (should (string-equal model3 t))))))


;; Test for `oai-block--let-params':
(ert-deftest oai-tests-block--let-params-all-from-info-test2 ()
  (cl-letf (((symbol-function 'org-entry-get-with-inheritance)
             (lambda (_) nil)))
    (let ((info '((:model))))
      (oai-block--let-params info
                             ((model nil :type string))
                             (should (equal model nil))))))

(ert-deftest oai-tests-block--let-params-all-from-info-test3 ()
  (cl-letf (((symbol-function 'org-entry-get-with-inheritance)
             (lambda (_) nil)))
    (let ((info '((:model)
                  (:model1 . "nil")
                  (:stream1 . "nil")
                  (:stream2 . t)
                  ;; (:stream3)
                  (:stream4)
                  )))
      (oai-block--let-params info
                             ((model nil :type string)
                              (model1 nil :type string)
                              (model2 nil :type string)
                              (stream nil :type bool)
                              (stream1 nil :type bool)
                              (stream2 nil :type bool)
                              (stream3 t :type bool)
                              (stream4 nil :type bool)
                              )
                             ;; (print (list "stream4" stream4)))))
                             ;; (print (list "model1" model1)) => ("nil" "nil")
                             ;; (print (list "model" model))
                             (should (string-equal model nil))
                             (should (equal model1 nil))
                             (should (equal model2 nil))
                             (should (equal stream nil))
                             (should (string-equal stream1 nil))
                             (should (equal stream2 t))
                             (should (equal stream3 t))
                             (should (equal stream4 t))
                             ))))

;; (defun oai-block--oai-restapi-request-prepare (req-type content element sys-prompt sys-prompt-for-all-messages model max-tokens top-p temperature frequency-penalty presence-penalty service stream)
;;   )
;; -=-= oai-agent-call
;; (ert-deftest oai-tests-block--oai-agent-call-test ()

;;   (let* ((test-block "#+begin_ai :stream t :sys \"A helpful LLM.\" :max-tokens 50 :model \"gpt-3.5-turbo\" :temperature 0.7\n\n#+end_ai\n")
;;          (oai-agent-call #'oai-block--oai-restapi-request-prepare)
;;          ;; - setup test buffer
;;          (element (oai-test-setup-buffer test-block))
;;          (info)
;;          (marker (copy-marker (org-element-property :contents-end element)))
;;          (buffer (org-element-property :buffer element))
;;          evaluated-result)
;;     ;; (unwind-protect
;;         (with-current-buffer buffer
;;           ;; - set cursor
;;           (goto-char (org-element-property :begin element))
;;           ;; (print (list "element" (org-element-property :contents-begin element)))

;;           (let ((oai-agent-call (lambda (req-type element sys-prompt sys-prompt-for-all-messages model max-tokens top-p temperature frequency-penalty presence-penalty service stream)
;;                                      ;; (print (list 'req-type (type-of req-type) req-type))
;;                                      ;; (print (list 'element (type-of element) element))
;;                                      ;; (print (list 'sys-prompt (type-of sys-prompt) sys-prompt))
;;                                      ;; (print (list 'sys-prompt-for-all-messages (type-of sys-prompt-for-all-messages) sys-prompt-for-all-messages))
;;                                      ;; (print (list 'model (type-of model) model))
;;                                      ;; (print (list 'max-tokens (type-of max-tokens) max-tokens))
;;                                      ;; (print (list 'top-p (type-of top-p) top-p))
;;                                      ;; (print (list 'temperature (type-of temperature) temperature))
;;                                      ;; (print (list 'frequency-penalty (type-of frequency-penalty) frequency-penalty))
;;                                      ;; (print (list 'presence-penalty (type-of presence-penalty) presence-penalty))
;;                                      ;; ;; (print (list 'service (type-of service) service))
;;                                      ;; (print (list 'stream (type-of stream) stream))
;;                                      ;; (should (and (eql req-type 'chat) (eql (type-of req-type) 'symbol) ))
;;                                      ;; (should (org-element-type element 'special-block))
;;                                      ;; (should (eql (type-of element) 'cons))
;;                                      ;; (should (eql (type-of sys-prompt) 'string))
;;                                      ;; (should (string= sys-prompt "A helpful LLM."))
;;                                      ;; (should (and (eql (type-of sys-prompt-for-all-messages) 'symbol) (null sys-prompt-for-all-messages)))
;;                                      ;; (should (and (eql (type-of model) 'string) (string= model "gpt-3.5-turbo")))
;;                                      ;; (should (and (eql (type-of max-tokens) 'integer) (= max-tokens 50)))
;;                                      ;; (should (and (eql (type-of top-p) 'symbol) (null top-p)))
;;                                      ;; (should (and (eql (type-of temperature) 'float) (= temperature 0.7)))
;;                                      ;; (should (and (eql (type-of frequency-penalty) 'symbol) (null frequency-penalty)))
;;                                      ;; (should (and (eql (type-of presence-penalty) 'symbol) (null presence-penalty)))
;;                                      ;; ;; (should (and (eql (type-of service) 'symbol)
;;                                      ;; ;;              (= service 'openai)))
;;                                      ;; (should (and (eql (type-of stream) 'symbol) (eql stream t)))
;;                                                   ;; (string-equal stream "t"))
;;                                      ;; (print (list req-type content element sys-prompt sys-prompt-for-all-messages model max-tokens top-p temperature frequency-penalty presence-penalty service stream))
;;                                      )))
;;             (oai-ctrl-c-ctrl-c)
;;             )
;;           ;; (oai-interface-step1)

;;       (kill-buffer buffer)
;;       )
;;       ;; )
;;     )
;;   (should t)
;;   )


;; (ert-deftest oai-tests-block--let-params-inherited-properties ()
;;   "Test when parameters are sourced from inherited Org properties."
;;   (let* ((test-block "#+begin_ai\n#+end_ai\n") ; No parameters in block header
;;          (setup-result (oai-test-setup-buffer test-block
;;                                                  '((model . "text-davinci-003")
;;                                                    (max-tokens . "100")
;;                                                    (temperature . "0.5")
;;                                                    (sys . "Inherited system prompt"))))
;;          (element (car setup-result))
;;          (info (cadr setup-result)) ; Empty info from block header
;;          evaluated-result)
;;     (unwind-protect
;;         (with-current-buffer (marker-buffer (org-element-property :begin element))
;;           ;; Position point inside the block for `org-entry-get-with-inheritance`
;;           (goto-char (org-element-property :begin element))
;;           (setq evaluated-result
;;                 (oai-test-eval-macro
;;                  '(oai-block--let-params info
;;                                             ((stream nil) ; default `nil`
;;                                              (sys nil)    ; no default, inherited from property
;;                                              (max-tokens nil :type number)
;;                                              (model nil)
;;                                              (temperature nil :type number))
;;                                             (list stream sys max-tokens model temperature))
;;                  element info)))
;;       (kill-buffer (marker-buffer (org-element-property :begin element))))
;;     (should (equal (car evaluated-result) nil)) ; No stream property or default
;;     (should (equal (cadr evaluated-result) "Inherited system prompt")) ; From inherited property
;;     (should (equal (caddr evaluated-result) 100)) ; From inherited property, converted to number
;;     (should (equal (nth 3 evaluated-result) "text-davinci-003")) ; From inherited property, no conversion (model is special)
;;     (should (equal (nth 4 evaluated-result) 0.5))) ; From inherited property, converted to number
;;   )

;; (ert-deftest oai-tests-block--let-params-default-form ()
;;   "Test when parameters fall back to default forms."
;;   (let* ((test-block "#+begin_ai\n#+end_ai\n") ; No block params, no inherited props
;;          (setup-result (oai-test-setup-buffer test-block))
;;          (element (car setup-result))
;;          (info (cadr setup-result)) ; Empty info
;;          evaluated-result)

;;     (unwind-protect
;;         (with-current-buffer (marker-buffer (org-element-property :begin element))
;;           (goto-char (org-element-property :begin element))
;;           (setq evaluated-result
;;                 (oai-test-eval-macro
;;                  '(oai-block--let-params info
;;                     ((stream t) ; Default true
;;                      (sys "Default system prompt")
;;                      (max-tokens 200 :type number)
;;                      (model "default-model-name")
;; (temperature 0.8 :type number)
;;                      (non-existent-param "fallback-value")) ; Test a parameter not in definitions
;;                     (list stream sys max-tokens model temperature non-existent-param))
;;                  element info)))
;;       (kill-buffer (marker-buffer (org-element-property :begin element))))
;;     (should (equal (car evaluated-result) t))
;;     (should (equal (cadr evaluated-result) "Default system prompt"))
;;     (should (equal (caddr evaluated-result) 200))
;;     (should (equal (nth 3 evaluated-result) "default-model-name"))
;;     (should (equal (nth 4 evaluated-result) 0.8))
;;     ;; Note: `non-existent-param` is not in definitions, so it won't be bound by `let-params`.
;;     ;; This `list` will cause an error because `non-existent-param` is not defined.
;;     ;; The macro itself only binds variables listed in `definitions`.
;;     ;; Removing `non-existent-param` from the test list.
;;     ))

;; -=-= Test: oai-block-fill-region-as-paragraph
(ert-deftest oai-tests-block--oai-block-fill-region-as-paragraph ()
  (should (with-temp-buffer
            (progn
              (org-mode)
              (setq fill-column 10)
              (insert "Some.\n")
              (insert "Some text here asdasdasdasd asda asd asd asd asd asd asd as d\n")
              (insert "Some.\n")
              (goto-char 1)
              (oai-block--apply-to-region-lines #'oai-block-fill-region-as-paragraph (point-min) (point-max) nil)
              (let ((strings (string-split (buffer-substring-no-properties (point-min) (point-max)) "\n")))
                (< (length (nth 1 strings)) 10))))))
;; -=-= Test: parse-part
(ert-deftest oai-tests-block--parse-part ()
  (should (equal (with-temp-buffer
                   (insert "ss")
                   (oai-block--parse-part 1 (point)))
                 '(:role user :content "ss")))
  (should (not (with-temp-buffer
                 (insert "")
                 (oai-block--parse-part 1 (point)))))
  (should (not (with-temp-buffer
                 (insert "[AI:] ")
                 (oai-block--parse-part 1 (point)))))
  (should-error (with-temp-buffer
                  (insert "[AI:] vv\n[ME:] zz\n")
                  (oai-block--parse-part 1 (point)))
                :type 'error)
  (should (with-temp-buffer
            (insert "[AI:] vv\n")
            (let ((p (point))
                  res)
              (insert "[ME:] zz\n")
              (setq res (oai-block--parse-part 1 p))
              (equal res
                     '(:role assistant :content "vv"))
              (setq res (oai-block--parse-part p (point)))
              (equal res
                     '(:role user :content "zz"))))))

;; -=-= Test: oai-block--get-chat-messages-positions, oai-block--collect-chat-messages-from-string
(ert-deftest oai-tests-block--chat-messages-tests ()
  (let ((payload "text before
as
[AI]: some1
[AI]:
[AI]: some2[ME:]")
        ;; (correct-sep '((:role 'user :content "text before\nas") (:role 'assistant :content "some1") (:role 'assistant :content "some2"))
        (correct-sep '((:role user :content "text before\nas") (:role assistant :content "some1") (:role assistant :content "some2[ME:]")))
        (correct-merged '[(:role user :content "text before\nas") (:role assistant :content "some1\nsome2[ME:]")])
        res)
    (with-temp-buffer
      (insert payload)
      (setq res (oai-block--parse-part (point-min) 10))
      (should (equal res '(:role user :content "text befo")))
      (setq res (let ((lst (oai-block--get-chat-messages-positions (point-min) (point-max) oai-block--chat-prefixes-re))
                      (results '()))
                  (while (and lst (cdr lst))
                    (push (oai-block--parse-part (car lst) (cadr lst)) results) ; parse current block
                    (setq lst (cdr lst)))
                  (nreverse (remove nil results))))
      (should (equal correct-sep res))
      (setq res (oai-block--collect-chat-messages-from-string payload))
      (should (equal correct-merged res)))))

;; -=-= Test: oai-block--merge-by-role
(ert-deftest oai-tests-block--oai-block--merge-consecutive-messages-by-role1()
  (should (equal (let ((parts
         (list
          (list :role 'system :content nil )
          (list :role 'user :content "Hi." )
          (list :role 'user :content "How are you?" )
          (list :role 'assistant :content nil)
          (list :role 'assistant :content "I'm fine.")
          (list :role 'user :content "Hi." )
          (list :role 'user :content nil ))))
    (oai-block--merge-by-role parts "::" ))
                 '((:role user :content "Hi.::How are you?") (:role assistant :content "I'm fine.") (:role user :content "Hi.")))))

(ert-deftest oai-tests-block--oai-block--merge-consecutive-messages-by-role2()
  (should (equal (let ((parts
         (list
          (list :role 'system :content "Hi." )
          (list :role 'user :content "How are you?" )
          (list :role 'assistant :content nil)
          (list :role 'assistant :content "I'm fine.")
          (list :role 'user :content "Hi." )
          (list :role 'user :content nil ))))
  (oai-block--merge-by-role parts "::"))
                 '((:role system :content "Hi.") (:role user :content "How are you?") (:role assistant :content "I'm fine.") (:role user :content "Hi.")))))

;; -=-= Test: oai-block--stringify-chat-messages
(ert-deftest oai-tests-block--stringify-chat-messages1()
  (let ((parts
         (list
          (list :role 'user :content "Hi." )
          (list :role 'user :content "How are you?" )
          (list :role 'assistant :content "I'm fine.")
          (list :role 'user :content "Hi." )))
        res)
    (setq res (oai-block--stringify-chat-messages (apply #'vector parts)))
    (should (string-equal res
                          "[ME]: Hi.

[ME]: How are you?

[AI]: I'm fine.

[ME]: Hi."))))


(ert-deftest oai-tests-block--stringify-chat-messages2 ()
  (should
   (string-equal
    (oai-block--stringify-chat-messages '[(:role system :content "system")
                                            (:role user :content "user")
                                            (:role assistant :content "assistant")])
    "[SYS]: system\n\n[ME]: user\n\n[AI]: assistant"))
  (let ((oai-block-roles '(("SYS1"		system)
                           ("ME2"		user)
                           ("AI3"		assistant))))
    (should
     (string-equal
      (oai-block--stringify-chat-messages '[(:role user :content "user")
                                            (:role assistant :content "assistant")]
                                          "system1")
      "[SYS1]: system1\n\n[ME2]: user\n\n[AI3]: assistant"))))

;; -=-= Test: oai-block--collect-chat-messages
(ert-deftest oai-tests-block--collect-chat-messages()
  (let ((parts
         (list
          (list :role 'user :content "Hi." )
          (list :role 'user :content "How are you?" )
          (list :role 'assistant :content "I'm fine.")
          (list :role 'user :content "Hi." )))
        res)
    (oai-block--collect-chat-messages-from-string (oai-block--stringify-chat-messages (apply #'vector parts)))))

;; -=-= Test: oai-block--collect-chat-messages-from-string
(ert-deftest oai-tests-block--collect-chat-messages-from-string ()
  ;; deal with unspecified prefix
  ;; (should
  ;;  (equal
  ;;   (let ((test-string "\ntesting\n  [ME]: foo bar baz zorrk\nfoo\n[AI]: hello hello[ME]: "))
  ;;     ;; (oai-restapi--collect-chat-messages test-string))
  ;;     (oai-block--collect-chat-messages-from-string test-string))

  ;;   '[(:role user :content "testing\nfoo bar baz zorrk\nfoo")
  ;;     (:role assistant :content "hello hello")]))

  ;; sys prompt
  (should
   (equal
    (let ((test-string "[SYS]: system\n[ME]: user\n[AI]: assistant"))
      (oai-block--collect-chat-messages-from-string test-string))
    '[(:role system :content "system")
      (:role user :content "user")
      (:role assistant :content "assistant")]))

  ;; sys prompt intercalated
  (should
   (equal
    (let ((test-string "[SYS]: system\n[ME]: user\n[AI]: assistant\n[ME]: user"))
      (oai-block--collect-chat-messages-from-string test-string "system"))
    '[(:role system :content "system")
      (:role user :content "user")
      (:role assistant :content "assistant")
      (:role user :content "user")]))

  (should
   (equal
    (let ((test-string "[SYS]: system\n[ME]: user\n[AI]: assistant\n[ME]: user"))
      (oai-block--collect-chat-messages-from-string test-string nil "pers-system1" nil " "))
    '[(:role system :content "system")
      (:role user :content "pers-system1 user")
      (:role assistant :content "assistant")
      (:role user :content "pers-system1 user")]))

  (should
   (equal
    (let ((test-string "[SYS]: system\n[ME]: user\n[AI]: assistant\n[ME]: user"))
      (oai-block--collect-chat-messages-from-string test-string "def-system1" nil "maxt-system2" " "))
    '[(:role system :content "system maxt-system2")
      (:role user :content "user")
      (:role assistant :content "assistant")
      (:role user :content "user")]))

  ;; merge messages with same role
  (should
   (equal
    (let ((test-string "[ME]: hello\n[ME]: world")) (oai-block--collect-chat-messages-from-string test-string))
    '[(:role user :content "hello\nworld")]))

  (should
   (equal
    (let ((test-string "[ME:] hello world")) (oai-block--collect-chat-messages-from-string test-string))
    '[(:role user :content "hello world")])))

;; -=-= provide
(provide 'oai-tests-block)

;;; oai-tests-block.el ends here
