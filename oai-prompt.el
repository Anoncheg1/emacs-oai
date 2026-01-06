;;; oai-prompt.el --- Chains of requests to LLM -*- lexical-binding: t; -*-

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
;;
;; `oai-agent-call-function' -> `oai-prompt-request-switch' -> `oai-prompt-request-chain'
;;
;; Re1
;; Sys: You a helpful.  Give plan of 3 parts to research for answer
;; and do only first part.  user: How to make it?
;;
;; "choices": ["message": {"role": "assistant", "content": "To..."}}]
;;
;; Re2
;; Sys: You a helpful.  Give plan of 3 parts to research for answer
;;      and do only first part.
;; user: How to make it?
;; Assist: Plan and solution for 1) step.
;; user: Research 2-th part and what was missed before.
;;
;; Re3
;; Sys: You a helpful.  Give plan of 3 parts to research for answer
;;      and do only first part with summary.
;; user: How to make it?
;; Assist: Plan and solution for 1) step.
;; user: Research 2-th part and what was missed before.
;; Assist: sum for 1), new plan, 2) step.
;; user: Research 3-th part and what was missed before, summarize
;;       results give final answer.

;; -=-= includes
(require 'oai-block)
(require 'oai-restapi)
(require 'oai-async1)
(require 'oai-timers)

;;; Code:
;; -=-= all
(defvar oai-prompt-chain-list
  (list "Make plan of three steps; do only the first part of the plan. Provide small seed answer and enhance it at next steps."
        "Complete the second part of plan only."
        "Do the third part; provide a final full answer."))


(defun oai-prompt-collect-chat-research-steps-prompt (commands ind messages &optional default-system-prompt max-tokens)
  "Compose messages for LLM for IND step of COMMANDS.
Add to result of `oai-restapi--collect-chat-messages' CoT prompts.
Compose IND request for COMMANDS and ind-1 response.
MESSAGES is result of `oai-block-get-content'.
IND count from 0.  RESP-QUEST  is list of string  of lengh IND+1  - raw
content of ai block or answer from  LLM.  We assume that commands and AI
answers except of the first one are already in MESSAGES."
  (let* ((recom (if (and oai-restapi-add-max-tokens-recommendation max-tokens)
                    (oai-restapi--get-lenght-recommendation max-tokens)))
         (comm0 (nth 0 commands))
         (comm0 (if (and (= ind 0) recom)
                    (concat comm0 " " recom)
                  comm0))
         (comm0 (if (and default-system-prompt (not (string-empty-p default-system-prompt)))
                         (concat default-system-prompt " " comm0)
                       ;; else
                       comm0))
         (comm (nth ind commands))
         (comm (if recom (concat comm " " recom) comm))
         (sys0 (list :role 'system :content
                     comm0)))
    (apply #'vector sys0 (append messages
                                ;; command after AI answer
                                (when (> ind 0)
                                  (list (list :role 'system :content comm)))))))



(defun oai-prompt-request-prepare-chain (&rest args)
  "Check if there is :chain at ai block parameters and call chain function.
For assiging to `oai-agent-call-function' with all normal ARGS.
Return t if we replace default call implementation
`oai-restapi-request-prepare'."
  ;; element = (nth 1 args)
  (when (not (eql 'x (alist-get :chain (oai-block-get-info (nth 1 args)) 'x)))
      (apply #'oai-prompt-request-chain args)
      t))

(defun oai-prompt-request-chain (req-type element sys-prompt sys-prompt-for-all-messages model max-tokens top-p temperature frequency-penalty presence-penalty service stream &optional info)
  "Use :chain parameter to activate and use :step to execute chain of prompt.
Aspects:
1) start and stop reporter at begining and at the end (final callback).
2) error handling: kill reporter, kill tmp buffer, kill timers
Execution Chain:
`oai-restapi-request-llm-retries'
`oai-restapi-request-llm'
Modeline notification:
1) `oai-timers--set' used in `oai-restapi-request-llm-retries'.
2) `oai-timers--set' here
3) `oai-timers--progress-reporter-run' - here
For REQ-TYPE, ELEMENT, SYS-PROMPT, SYS-PROMPT-FOR-ALL-MESSAGES, MODEL,
MAX-TOKENS, TOP-P, TEMPERATURE, FREQUENCY-PENALTY, PRESENCE-PENALTY,
SERVICE, STREAM, INFO see `oai-restapi-request-prepare'."
  (oai--debug "oai-prompt-request-chain service, model, buf: %s %s %s" service model (current-buffer))
  ;; noqa Unused lexical argument
  (ignore req-type sys-prompt-for-all-messages stream info)
  ;; (setq req-type req-type
  ;;       sys-prompt-for-all-messages sys-prompt-for-all-messages
  ;;       stream stream)

  ;; (if (not (eql 'x (alist-get :chain (oai-block-get-info element) 'x))) ; check if :my exist
  ;; - My request
  (let ((service (or service 'github))
        (end-marker (oai-block--get-content-end-marker element))
        (header-marker (oai-block-get-header-marker element))
        ;; (gap-between-requests 3) ; TODO
        ;; (step (alist-get :step (oai-block-get-info element))) ; Works? not tested TODO
        (oai-timers-duration-copy oai-timers-duration)
        (oai-timers-retries-copy oai-timers-retries))

    (let (
          (call (lambda (step) ; called 3 times
                  (lambda (data callback)
                    (ignore data)
                    (oai--debug "oai-prompt-request-chain1 step %s" step) ; 0, 1, 2
                    (oai--debug "oai-prompt-request-chain1 buffer %s" (current-buffer))
                    (oai--debug "oai-prompt-request-chain1 max-tokens %s header-marker %s sys-prompt %s" max-tokens header-marker sys-prompt)

                    ;; also save request for timer
                    ;; (condition-case err ; for `oai-block-tags-replace'
                        (oai-restapi-request-llm-retries service
                                                         model
                                                         oai-timers-duration-copy ; use current-buffer
                                                         callback
                                                         :retries oai-timers-retries-copy ; use current-buffer
                                                         :messages
                                                         (oai-prompt-collect-chat-research-steps-prompt oai-prompt-chain-list
                                                                                                        step
                                                                                                        (with-current-buffer (marker-buffer header-marker)
                                                                                                          ;; get messages vector
                                                                                                          (oai-restapi--collect-chat-messages-at-point (oai-block-element-by-marker header-marker)))
                                                                                                                                                            ;; (oai-block-get-content (oai-block-element-by-marker header-marker)))
                                                                                                        sys-prompt
                                                                                                        max-tokens)
                                                         :max-tokens max-tokens
                                                         :header-marker header-marker
                                                         :temperature temperature
                                                         :top-p top-p
                                                         :frequency-penalty frequency-penalty
                                                         :presence-penalty presence-penalty)
                      ;; (user-error
                      ;;  (funcall oai-restapi-show-error-function (error-message-string err)
                      ;;           header-marker)))
                  )))
          (callbackmy (lambda (data callback)
                        "Called in (current-buffer)."
                        (when data ; if not data it is fail
                          (oai--debug "calbackmy %s %s %s" oai-timers--element-marker-variable-dict (current-buffer) data)
                          (oai-block--insert-single-response end-marker data nil)
                          (run-at-time 0 nil callback data))))
          (calbafin (lambda (data callback)
                      (ignore callback)
                      ;; (setq _callback _callback)
                      (when data ; if not data it is fail
                        (oai--debug "calbafin")
                        (oai-block--insert-single-response end-marker data nil 'final)
                        (oai-block--insert-single-response end-marker nil 'insertrole 'final) ; finalize
                        (oai-timers--interrupt-current-request (oai-timers--get-keys-for-variable header-marker) #'oai-restapi--stop-tracking-url-request)))))

      (oai--debug "oai-prompt-request-chain2 %s %s %s %s" header-marker service model oai-timers-duration)
      (condition-case err
          (progn
            (oai-timers--progress-reporter-run #'oai-restapi--stop-tracking-url-request (* oai-timers-duration oai-timers-retries-copy) )
            (oai--debug "oai-prompt-request-chain3")

            ;; There is a problem that we handle error in callback before timer may be run.
            ;; And we can't run timer before.
            (oai-async1-start nil
                              (list (funcall call 0)
                                    callbackmy
                                    (funcall call 1)
                                    callbackmy
                                    (funcall call 2)
                                    calbafin))
            (oai--debug "oai-prompt-request-chain4"))
        (user-error
         (funcall oai-restapi-show-error-function (error-message-string err)
                  header-marker)
         (oai-timers--interrupt-current-request (oai-timers--get-keys-for-variable header-marker) #'oai-restapi--stop-tracking-url-request))))))


;;; provide
(provide 'oai-prompt)
;;; oai-prompt.el ends here
