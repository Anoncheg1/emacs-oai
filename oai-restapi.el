;;; oai-restapi.el --- OpenAI REST API related functions  -*- lexical-binding: t; -*-

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

;; Get info block from #begin_ai and call url-retrieve.  Asynchronous
;; but only one call per buffer.
;;
;;
;; Main variables: (old)
;; URL = oai--get-endpoint()  or oai-restapi-con-chat-endpoint,
;;     oai-restapi-con-completion-endpoint
;; Headers = oai--get-headers
;; Token = oai-api-creds-token
;;
;; When we create request we count requests, create two timers global
;;   one and local inside url buffer.
;; When we receive error or final answer we stop local, recount
;;   requests and update global.
;;
;; Chat mode (old)
;; - :message (oai-restapi--collect-chat-messages ...)
;; - (oai-restapi--normalize-response response) -> (cl-loop for response in normalized
;;   - (setq role (oai--response-type response))
;;   - (setq text (decode-coding-string (oai--response-payload response)) 'utf-8)
;; Completion mode
;; - :prompt content-string
;; - (setq text  (decode-coding-string (oai-restapi--get-single-response-text result)
;;                                     'utf-8))

;; How requests forced to stop with C-g?

;; We save url-buffer with header marker with
;; `oai-timers--progress-reporter-run' function.  that call:
;; (oai-timers--set-variable url-buffer header-marker) in
;; `oai-timers--interrupt-current-request' we remove buffer from
;; saved and call (oai-restapi--interrupt-url-request url-buffer).

;;; TODO:
;; - BUG: shut network process when timer expire - kill buffer, remove callback.
;; - escape #+end_ai after insert of text in block
;; - add support for several backends, curl, request.el

;; -=-= includes
(require 'org)
(require 'org-element)
(require 'url)
(require 'url-http)
(require 'cl-lib)
(require 'gv) ; for setf
(require 'json) ; json-read, json-read-from-string, json-encode
(require 'oai-debug)
(require 'oai-block-tags)
(require 'oai-block)
(require 'oai-timers)
(require 'oai-async1) ; for `oai-async1-plist-get'

;;; Code:
;; -=-= Constants, variables
(defcustom oai-restapi-con-token nil
  "This is your OpenAI API token.
If not-nil, store token as a string or may be as a list of key-value:
\='(:openai token).

You can retrieve it at
https://platform.openai.com/account/api-keys.
If  nil, `auth-sources'  file  (with encryption  support)  used to  read
token.  In such  case the secret should be stored  in the format:
machine openai password <your token>
or
machine openai--0 password <your token>
machine openai--1 password <your token>"
  :type '(choice (string :tag "String value")
                 (plist :tag "Property list (symbol => token string or list of token strings)"
                        :key-type symbol
                        :value-type (choice (string :tag "token string")
                                            (repeat :tag "or list of token strings" string )))
                 (const :tag "Use auth-source." nil))
  :group 'oai)

(defcustom oai-restapi-con-service 'openai
  "Service to use if not specified."
  :type '(choice (const :tag "OpenAI" openai)
                 (const :tag "Azure-OpenAI" azure-openai)
                 (const :tag "perplexity.ai" perplexity.ai)
                 (const :tag "anthropic" anthropic)
                 (const :tag "DeepSeek" deepseek)
                 (const :tag "google" google)
                 (const :tag "Together" together)
                 (const :tag "Github" github))
  :group 'oai)

(defcustom oai-restapi-con-endpoints
  '(:openai		"https://api.openai.com/v1/chat/completions"
    :openai-completion	"https://api.openai.com/v1/completions"
    :perplexity.ai	"https://api.perplexity.ai/chat/completions"
    :deepseek		"https://api.deepseek.com/v1/chat/completions"
    :anthropic		"https://api.anthropic.com/v1/messages"
    :google		"https://generativelanguage.googleapis.com/v1beta/openai/chat/completions"
    :together		"https://api.together.xyz/v1/chat/completions"
    :github		"https://models.github.ai/inference/chat/completions")
  "Endpoints for services.
This is a not ordered list of key-value pairs in format of List of
  lists: (SYMBOL VALUE-STRING).  Used for POST HTTP request to service.
To add service use: (plist-put oai-restapi-con-endpoints :myservice \"http\")."
  :type '(plist :key-type symbol :value-type string
                :tag "Plist with Service as a symbol key and Endpoint URL as a string value")
         :group 'oai)


(defcustom oai-restapi-con-model '(:openai "gpt-4o-mini"
                                   :github "openai/gpt-4.1")
  "The default model to use.
See https://platform.openai.com/docs/models for other options.
If mode is not chat but completion, appropriate model should be set."
  :type '(choice (string :tag "String value")
                  (plist :key-type symbol :value-type string :tag "Plist with symbol key and string value"))
  :group 'oai)


(defcustom oai-restapi-openai-known-chat-models '("gpt-5-chat-latest"
                                                  "gpt-4o-mini"
                                                  "gpt-4"
                                                  "gpt-4-32k"
                                                  "gpt-4-turbo"
                                                  "gpt-4o"
                                                  "gpt-4o-mini"
                                                  "gpt-4o-realtime-preview"
                                                  "gpt-4o-search-preview"
                                                  "gpt-4o-mini-search-preview"
                                                  "gpt-4.1"
                                                  "gpt-4.1-nano"
                                                  "gpt-4.1-mini"
                                                  "gpt-3.5-turbo"
                                                  "o1"
                                                  "o1-pro"
                                                  "o1-preview"
                                                  "o1-mini"
                                                  "o3"
                                                  "o3-mini"
                                                  "o3-pro"
                                                  "o4-mini"
                                                  "chatgpt-4o-latest")
  "Alist of OpenAI chat models from https://platform.openai.com/docs/models."
  :type '(alist :key-type string :value-type string)
  :group 'oai)

(defcustom oai-restapi-default-max-tokens nil
  "The default maximum number of tokens to generate.
This is what costs money."
  :type '(choice (integer :tag "Integer value")
                 (string :tag "String value")
                  (const :tag "None" nil))
  :group 'oai)

(defcustom oai-restapi-add-max-tokens-recommendation t
  "If non-nil, add to system recomendation about max-token.
It is recommeded to explicitly repeat to LLM information, such as
max-token limit.
Function `oai-restapi--get-lenght-recommendation' is used to create
prompt."
  :type 'boolean
  :group 'oai)

(defcustom oai-restapi-default-chat-system-prompt "Be helpful."
  "The system message helps set the behavior of the assistant:
https://platform.openai.com/docs/guides/chat/introduction.  This
default prompt is send as the first message before any user (ME)
or assistant (AI) messages.  Inside a +#begin_ai...#+end_ai block
you can override it with: '[SYS]: <your prompt>'."
  :type 'string
  :group 'oai)

(defcustom oai-restapi-default-inject-sys-prompt-for-all-messages nil
  "Wether to add the system prompt before every user message.
By default the system prompt is only added before the first
message.
Get prompt from `oai-restapi-default-chat-system-prompt'.
You can set this to true for a single block using the
:sys-everywhere option on the #+begin_ai block.
This can be useful to enforce the behavior specified by this
messages."
  :type '(choice (const :tag "Before every message" all)
                 (const :tag "Before first" first)
                 (const :tag "Before last" last)
                 (const :tag "Don't add" nil))
  :group 'oai)

;; (make-obsolete-variable 'oai-restapi-default-inject-sys-prompt-for-all-messages
;;                         "With newer ChatGPT versions this is no longer necessary."
;;                         "2023-12-26")


;; Azure-Openai specific variables

(defcustom oai-restapi-azure-openai-api-base "https://your-instance.openai.azure.com"
  "Base API URL for Azure-OpenAI."
  :type 'string
  :group 'oai)

;; Additional Azure-Openai specific variables
(defcustom oai-restapi-azure-openai-deployment "azure-openai-deployment-name"
  "Deployment name for Azure-OpenAI API."
  :type 'string
  :group 'oai)

(defcustom oai-restapi-azure-openai-api-version "2023-07-01-preview"
  "API version for Azure-OpenAI."
  :type 'string
  :group 'oai)

(defcustom oai-restapi-anthropic-api-version "2023-06-01"
  "API version for api.anthropic.com."
  :type 'string
  :group 'oai)

(defcustom oai-restapi-after-prepare-messages-hook nil
  "List of functions that called with one argument messages vector.
Executed at step of sending request to LLM after all preparations for
messages was done.  Ever function called with one argument from left to
right and pass result to each other."
  :type 'hook
  :group 'oai)

(defvar-local oai-restapi--current-url-request-callback nil
  "Internal var that stores the current request callback.
Called within url request buffer, should know about target position,
that is why defined as lambda with marker.")

(defvar-local oai-restapi--current-request-is-streamed nil
  "Whether we expect a streamed response or a single completion payload.")

(defvar oai-restapi-after-chat-insertion-hook nil
  "Hook that is called when a chat response is inserted.
Note this is called for every stream response so it will typically only
contain fragments.
For STREAM executed for every word and one time with TYPE=\'end.
For non-STREAM executed one time with TYPE=\'end.
Arguments: type, role-text, pos, buffer
- TYPE - simbol \='role, \='text or'end,
- ROLE-TEXT - text or role name,
- POS - position before text insertion
- STREAM - stream mode or a single insertion.")

;; (defvar-local oai-restapi--currently-reasoning nil)

(defvar-local oai-restapi--url-buffer-last-position-marker nil
  "Local buffer var to store url-buffer read position.")
;; (make-variable-buffer-local 'oai-restapi--url-buffer-last-position-marker)
;; (makunbound 'oai-restapi--url-buffer-last-position-marker)


;; -=-= Debugging

(defun oai-restapi--debug-urllib (source-buf)
  "Copy `url-http' buffer with response to our debugging buffer.
Argument SOURCE-BUF url-http response buffer."
  (when (and source-buf (bound-and-true-p oai-debug-buffer))
    (save-excursion
      (let* ((buf-exist (get-buffer oai-debug-buffer))
             (bu (or buf-exist (get-buffer-create oai-debug-buffer))))
        (with-current-buffer bu
          (let ((stri (with-current-buffer source-buf
                        ;; (save-excursion
                          (buffer-substring-no-properties (or oai-restapi--url-buffer-last-position-marker
                                                              (point-min))
                                                          (point-max)))))
            (goto-char (point-max))
            (insert "oai-restapi--debug-urllib response:\n")
            (insert stri)
            (newline)))))))

;; -=-= Show error
(defcustom oai-restapi-show-error-function 'oai-block-insert-result-message
  "Function to display error in oai-restapi about internal and remote errors.
Available choices include:
- `oai-block-insert-result-message' accept message and header-marker parameters.
To  use  url-buffer,  get header-marker  with  (oai-timers--get-variable
url-buffer), hence every url-buffer key bound to some ai block variable.
- `oai-restapi--show-error' ignore header-marker parameter.
Or provide your own function."
  :type 'function
  :options '(oai-block-insert-result-message oai-restapi--show-error)
  :group 'oai)

;; (defun oai-restapi-insert-result-error (message url-buffer)
;;     (oai-block-insert-result-message message
;;                                      (oai-timers--get-variable url-buffer)))

;; (defcustom oai-restapi-show-error-in-result t
;;   "If non-nil, show error in #+RESULTS of block, otherwise in a buffer."
;;   :type 'boolean
;;   :group 'oai)


(defun oai-restapi--show-error (error-message &optional header-marker)
  "Show an error message in a buffer.
ERROR-MESSAGE is the error message to show.
Argument _HEADER-MARKER not used."
  (ignore header-marker)
  ;; (setq _header-marker _header-marker) ; for melpazoid
  (condition-case nil
      (let ((buf (get-buffer-create "*oai error*")))
        (with-current-buffer buf
          (read-only-mode -1)
          (erase-buffer)
          (insert "Error from the service API:\n\n")
          (insert error-message)
          (display-buffer buf)
          (goto-char (point-min))
          (toggle-truncate-lines -1)
          (read-only-mode 1)
          ;; close buffer when q is pressed
          (local-set-key (kbd "q") (lambda () (interactive) (kill-buffer)))
          t))
    (error nil)))

;; -=-= Get constant functions
(defun oai-restapi--check-model (model endpoint)
  "Check if the model name is somehow mistyped.
MODEL is the model name.  ENDPOINT is the API endpoint."
  (unless model
    (error "No oai model specified"))

  (when (or (string-match-p "api.openai.com" endpoint)
            (string-match-p "openai.azure.com" endpoint))

    (let ((lowercased (downcase model)))
      (when (and (string-prefix-p "gpt-" model) (not (string-equal lowercased model)))
        (warn "Model name '%s' should be lowercase. Use '%s' instead." model lowercased)))

    (unless (member model oai-restapi-openai-known-chat-models)
      (message "Model '%s' is not in the list of available models. Maybe this is because of a typo or maybe we haven't yet added it to the list. To disable this message add (add-to-list 'oai-restapi-openai-known-chat-models \"%s\") to your init file." model model))))

(defun oai-restapi--split-dash-number (str)
  "Split STR and return list of main string and number after dashes.
Used to for simple numbering of instances in config."
  (pcase-let ((`(,a ,b) (string-split str "--")))
    (and b (string-match-p "\\`[0-9]+\\'" b)
         (cons a (string-to-number b)))))


(defun oai-restapi--openai-service-clear-dashes (service)
  "Remove --N part from SERVICE if it have such.
If SERVICE is not a string, just return it."
  (if (stringp service)
      (let ((spl (oai-restapi--split-dash-number service)))
        (if spl (car spl)
          service))
    ;; else
    service))

;; (defmacro oai-restapi--get-value-or-string (var key) ; old
;;   "Retrieve value from VAR using KEY if VAR is a plist, or return VAR if it's a string.
;; VAR is the variable name to evaluate.
;; KEY is a string (without :) or keyword (leading with :) used as a key if
;; VAR is a plist."
;;   `(cond ((plistp ,var)
;;           (plist-get ,var (if (stringp ,key)
;;                               (intern (concat ":" ,key))
;;                             ,key)))
;;           ((stringp ,var)
;;            ,var)
;;           (t nil)))

;; ;; ;; (oai-restapi--get-value-or-string oai-restapi-con-endpoints "openai")
;; ;; ;; (oai-restapi--get-value-or-string oai-restapi-con-endpoints :openai)
;; ;; (oai-restapi--get-value-or-string oai-restapi-con-token "local")
;; ;; (oai-restapi--get-value-or-string oai-restapi-con-token "github")

(defun oai-restapi--ensure-keyword (sym)
  "Return keyword version of SYM suitable for plist keys.
SYM may be a string, symbol or keywordp."
  (cond
   ((keywordp sym) sym)
   ((symbolp sym) (intern (concat ":" (symbol-name sym))))
   ((stringp sym)
    (intern (if (not (string-prefix-p ":" sym))
                (concat ":" sym)
              sym)))
   (t (error "Argument not a symbol or string: %S" sym))))

;; (oai-restapi--ensure-keyword "sym") ; => :sym
;; (oai-restapi--ensure-keyword ":sym") ; => :sym
;; (oai-restapi--ensure-keyword :sym) ; => :sym
;; (oai-restapi--ensure-keyword 'sym) ; => :sym

(defun oai-restapi--get-values (plist key)
  "KEY can be a keyword or string.
Return:
- list with its value (even if nil) - If KEY exists in PLIST.
- (list nil) - If KEY exists with nil or without value.
- nil - If KEY does NOT exist."
  (if (stringp plist)
      (list plist)
    ;; else
    (let* ((search-key (oai-restapi--ensure-keyword key))
           (marker (make-symbol "oai-not-found"))
           (val (oai-async1-plist-get plist search-key marker)))
      (cond ((eq val marker)
             '())
          ((and val (listp val))
           val)
          (val
           (list val))
          (t
           (list nil))))))


(defun oai-restapi--get-values-enhanced (keeper key)
  "KEY string may have postfix --N.
Return nil if key not found, else list with value if found.
Argument KEEPER is a variable with list of key-values."
  (if (and (stringp keeper) (string-empty-p keeper))
      nil
    ;; else
    (let* ((spl (if (stringp key) (oai-restapi--split-dash-number key))) ; nil or ("github" . 1)
           (key-number (if spl (cdr spl)))
           (key (if spl (car spl) key))
           ;; find key in plist
           (values (oai-restapi--get-values keeper key))) ; list or one string or ( nil not exist) or list with nil if exist
      (if (and spl (listp values) key-number)
          (if (>= key-number (length values))
              nil
            ;; else
            (list (nth key-number values))) ; list with one value
        ;; else
        values))))


(defun oai-restapi--get-token (service)
  "Get token or errored.
Get token from `oai-restapi-con-token' or from auth-source.
Return nil if SERVICE exist without token, signal error if SERVICE
not found in tokens."
  (let ((token (oai-restapi--get-values-enhanced oai-restapi-con-token service)))
    (oai--debug "oai-restapi--get-token %s %s" service token)
    (if token
        (prog1 (setq token (car token)) ; nil or value
          (when (and token (not (stringp token))) ; check that token is string, not number
            (user-error "Token in `oai-restapi-con-token' is not string but something other, please check")))
      ;; else - nil not found or `oai-restapi-con-token' is not defined
      (prog1 (setq token (oai-restapi--get-token-auth-source service))
        (when (not token)
          ;; else - not found in auth-sources and not found or `oai-restapi-con-token' is not defined
          (cond
           ((and oai-restapi-con-token (proper-list-p oai-restapi-con-token))
            (user-error "Token not found in defined plist `oai-restapi-con-token' and in auth sources"))
           ((and oai-restapi-con-token
                 (stringp oai-restapi-con-token)
                 (string-empty-p oai-restapi-con-token))
            (user-error "`oai-restapi-con-token' is an empty string.  Please set"))
           (t
            ;; else no `oai-restapi-con-token'
            (user-error "Please set `oai-restapi-con-token' to your OpenAI API token or setup auth-source (see oai readme)"))))))))


;; (oai-restapi--get-token "github") => first token from list
;; (oai-restapi--get-token "github--0") => first token from list
;; (oai-restapi--get-token "local") => nil, exist
;; (oai-restapi--get-token "local") => error!


(defun oai-restapi--strip-api-url (url)
  "Strip the leading https:// and trailing / from an URL."
  (let* ((stripped-url
          ;; Remove "https://" or "http://" if present
          (cond
           ((string-prefix-p "https://" url) (substring url 8))
           ((string-prefix-p "http://" url) (substring url 7))
           (t url)))
         (parts (string-split stripped-url "/" t))) ; Split by '/', t means remove empty strings
    ;; Return the first part, which should be the hostname
    (car parts)))

(defun oai-restapi--get-token-auth-source (&optional service)
  "Retrieves the authentication token for the OpenAI SERVICE using auth-source."
  (require 'auth-source)
  (let ((service (or service oai-restapi-con-service)))
    (or (and (stringp service) (auth-source-pick-first-password :host (oai-restapi--openai-service-clear-dashes service)))
        (auth-source-pick-first-password :host service)
        (and (stringp service) (auth-source-pick-first-password :host (concat service "--0" ))))))


(defun oai-restapi--get-endpoint (messages &optional service)
  "Correct endpoint based on the SERVICE and type of requst.
If MESSAGES are provided, type of request is chat, otherwise completion."
  (oai--debug "oai-restapi--get-endpoint %s %s" messages service)
  (let* ((service (or (if service
                          (oai-restapi--openai-service-clear-dashes service))
                      ;; else
                      oai-restapi-con-service))
         (endpoint (car (oai-restapi--get-values oai-restapi-con-endpoints service))))
    (cond
     (endpoint endpoint)
     ((eq service 'azure-openai)
      (format "%s/openai/deployments/%s%s/completions?api-version=%s"
              oai-restapi-azure-openai-api-base oai-restapi-azure-openai-deployment
              (if messages "/chat" "") oai-restapi-azure-openai-api-version))
     (messages
      (car (oai-restapi--get-values oai-restapi-con-endpoints :openai)))
     (t
      (car (oai-restapi--get-values oai-restapi-con-endpoints :openai-completion))))))

(defun oai-restapi--get-headers (service)
  "Determine the correct headers based on the SERVICE."
  (let ((serv (if service
                  (oai-restapi--openai-service-clear-dashes service)
                ;; else
                oai-restapi-con-service))
        (token (oai-restapi--get-token service)))
    `(("Content-Type" . "application/json")
      ;; authentication
      ,@(cond
         ((eq serv 'azure-openai)
          `(("api-key" . ,token)))
         ((eq serv 'anthropic)
          `(("x-api-key" . ,token)
            ("anthropic-version" . ,oai-restapi-anthropic-api-version)))
         ((eq serv 'google)
          `(("Accept-Encoding" . "identity")
            ("Authorization" . ,(encode-coding-string (string-join `("Bearer" ,token) " ") 'utf-8))))
         (token
          `(("Authorization" . ,(encode-coding-string (string-join `("Bearer" ,token) " ") 'utf-8))))))))

;; (oai-restapi--get-headers "local")

(defun oai-restapi--get-lenght-recommendation (max-tokens)
  "Recomendation to limit yourself if MAX-TOKENS is lower 1000.
Useful for small max-tokens.
- words = tokens * 0.75
- tokens = words * 1.33333
- token = 4 characters
- word - 5 characters
- sentence - 15-25 words = 20 words = 26 tokens (tech/academ larger)
- paragraph - 6 sentences, 500-750 characters,
              150-300 words = 150 words = 200 tokens
- page - around 3-4 paragraphs, 500 words = 600 tokens."
  (when max-tokens
    (cond ((< max-tokens 75)
           (format "Answer very short with %d words or less." (* max-tokens 0.75)))
          ((and (>= max-tokens 75)
                (< max-tokens 500))
           (format "Answer very short with in %d sentences or %d lines or less." (/ max-tokens 29) (* (/ max-tokens 29) 3) ))
          ((and (>= max-tokens 500)
                (<= max-tokens 1000))
           (format "Answer short in %d paragraphs or %d pages or less." (/ max-tokens 200) (ceiling (/ max-tokens 600.0)))))))

;; -=-= Prepare content

(defun oai-restapi-prepare-content (element req-type sys-prompt sys-prompt-for-all-messages max-tokens)
  "Get content of ai block of element in current buffer.
Handle Tags, two types of REQ-TYPE and separation of PROMPT and MESSAGES.
Return:
- vector with messages for chat REQ-TYPE.
- string for completion REQ-TYPE.
Parameters ELEMENT REQ-TYPE SYS-PROMPT SYS-PROMPT-FOR-ALL-MESSAGES
MAX-TOKENS described in `oai-restapi-request-prepare'."
  (oai--debug "oai-restapi-prepare-content %s" sys-prompt-for-all-messages)
  ;; (let ((content (oai-block-get-content element))) ; string - is block content
  (if (eql req-type 'completion)
      ;; - *Completion*
      (oai-block-tags-replace (string-trim (oai-block-get-content element))) ; return string
    ;; - else - *Chat*
    (let* ((messages (oai-block--collect-chat-messages-at-point element
                                                                sys-prompt
                                                                sys-prompt-for-all-messages
                                                                (when oai-restapi-add-max-tokens-recommendation
                                                                  (oai-restapi--get-lenght-recommendation max-tokens))))
           (messages (oai-restapi--modify-vector-content messages 'user #'oai-block-tags-replace))
           (messages (oai-restapi--modify-vector-content messages 'user #'oai-block-tags--clear-properties))
           (messages (oai-block--pipeline oai-restapi-after-prepare-messages-hook messages)))
      messages))) ; return vector


;; -=-= Main

(defun oai-restapi-request-prepare (req-type element sys-prompt sys-prompt-for-all-messages model max-tokens top-p temperature frequency-penalty presence-penalty service stream &optional info)
  "Compose API request from data and start a server-sent event stream.
Call `oai-restapi-request' function as a next step.
Called from `oai-call-block' in main file.
ELEMENT org-element - is ai block, should be converted to market at
once.
REQ-TYPE symbol - is completion or chat mostly.  Set
  `oai-block--get-request-type'.
SYS-PROMPT string - first system instruction as a string.
SYS-PROMPT-FOR-ALL-MESSAGES from
  `oai-restapi-default-inject-sys-prompt-for-all-messages' variable.
MODEL string - is the model to use.
MAX-TOKENS integer - is the maximum number of tokens to generate.
TEMPERATURE integer - 0-2 lower - low 0.3 high-probability tokens
  producing predictable outputs.  1.5 diversity by flattening the
  probability distribution.
TOP-P integer - 0-1 lower - chooses tokens whose cumulative
  probability exceeds this threshold, adapting to context.
FREQUENCY-PENALTY integer - -2-2, lower less repeat words.
PRESENCE-PENALTY integer - -2-2, lower less repeat concepts.
SERVICE symbol or string - is the AI cloud service such as openai or
  azure-openai.
STREAM string - as bool, indicates whether to stream the response."
  (oai--debug "oai-restapi-request-prepare %s" sys-prompt-for-all-messages)
  (ignore info)
  (let* ((content (oai-restapi-prepare-content element req-type sys-prompt sys-prompt-for-all-messages max-tokens))
         (end-marker (oai-block--get-content-end-marker element))
         (callback (if (eql req-type 'completion) ; chat - ; set to oai-restapi--current-url-request-callback
                       ;; completion mode
                       (lambda (result) (oai-block--insert-single-response end-marker
                                                                             (oai-restapi--get-single-response-text result)
                                                                             nil))
                     ;; else - chat mode - RESULT is JSON in plist format
                     (if stream
                         (lambda (result) (oai-block--insert-stream-response end-marker
                                                                               (oai-restapi--normalize-response result) ; [DONE] is ignored. we use "finish_reason":"stop" instead.
                                                                                t))
                       ;; else - not stream
                       (lambda (result) (oai-block--insert-single-response end-marker
                                                                             (oai-restapi--get-single-response-text result)
                                                                             t 'final))))))
    ;; - Call and save buffer.
    (oai-timers--set
     (oai-restapi-request service model callback
                          :prompt (when (eql req-type 'completion) content) ; if completion
                          :messages (when (not (eql req-type 'completion)) content) ; chat
                          :max-tokens max-tokens
                          :temperature temperature
                          :top-p top-p
                          :frequency-penalty frequency-penalty
                          :presence-penalty presence-penalty
                          :stream stream)
     (oai-block-get-header-marker element))
    ;; - run timer that show /-\ looping, notification of status
    (oai-timers--progress-reporter-run
     #'oai-restapi--interrupt-url-request)))

;; -=-= Normalize, oai-restapi-request

;; Together.xyz 2025
;; '(id "nz7KyaB-3NKUce-9539d1912ce8b148" object "chat.completion" created 1750575101 model "meta-llama/Llama-3.3-70B-Instruct-Turbo-Free" prompt []
;;   choices [(finish_reason "length" seed 3309196889559996400 logprobs nil index 0
;;             message (role "assistant" content " The answer is simple: live a long time. But how do you do that? Well, itâs not as simple as it sounds." tool_calls []))] usage (prompt_tokens 5 completion_tokens 150 total_tokens 155 cached_tokens 0))

(defun oai-restapi--get-single-response-text (&optional response)
  "Return text from RESPONSE or nil and signal error if it have \"error\" field.
For Completion LLM mode. Used as callback for `oai-restapi-request'.
Same to `oai-restapi--normalize-response' that used for stream.
We use separate version, because streaming is complicated,
but we will keep them interchangable.
Result used for `oai-block--insert-single-response'.
Return text of message."
  (when response
    ;; (oai--debug "oai-restapi--get-single-response-text response:" response)
    (if-let ((err-obj (plist-get response 'error)))
        (let ((message (or (plist-get err-obj 'message)
                           err-obj)))
            (error message)) ; not used
      ;; else - no "error" field
      (if-let* ((choice (aref (plist-get response 'choices) 0))
                (text (or (plist-get choice 'text)
                          ;; Together.xyz, Github
                          (plist-get (plist-get choice 'message) 'content))))
          ;; - Decode text
          (decode-coding-string text 'utf-8)))))



;; Here is an example for how a full sequence of OpenAI responses looks like:
;; '((id "chatcmpl-9hM1UJgWe4cWKcJKvoMBzzebOOzli" object "chat.completion.chunk" created 1720119788 model "gpt-4o-2024-05-13" system_fingerprint "fp_d576307f90" choices [(index 0 delta (role "assistant" content "") logprobs nil finish_reason nil)])
;;   (id "chatcmpl-9hM1UJgWe4cWKcJKvoMBzzebOOzli" object "chat.completion.chunk" created 1720119788 model "gpt-4o-2024-05-13" system_fingerprint "fp_d576307f90" choices [(index 0 delta (content "Hello") logprobs nil finish_reason nil)])
;;   (id "chatcmpl-9hM1UJgWe4cWKcJKvoMBzzebOOzli" object "chat.completion.chunk" created 1720119788 model "gpt-4o-2024-05-13" system_fingerprint "fp_d576307f90" choices [(index 0 delta (content ",") logprobs nil finish_reason nil)])
;;   (id "chatcmpl-9hM1UJgWe4cWKcJKvoMBzzebOOzli" object "chat.completion.chunk" created 1720119788 model "gpt-4o-2024-05-13" system_fingerprint "fp_d576307f90" choices [(index 0 delta (content " Robert") logprobs nil finish_reason nil)])
;;   (id "chatcmpl-9hM1UJgWe4cWKcJKvoMBzzebOOzli" object "chat.completion.chunk" created 1720119788 model "gpt-4o-2024-05-13" system_fingerprint "fp_d576307f90" choices [(index 0 delta nil logprobs nil finish_reason "stop")])
;;   nil)
;;
;; and Anthropic:
;; '((type "message_start" message (id "msg_01HoMq4LgkUpHpkXqXoZ7R1W" type "message" role "assistant" model "claude-3-5-sonnet-20240620" content [] stop_reason nil stop_sequence nil usage (input_tokens 278 output_tokens 2)))
;;   (type "content_block_start" index 0 content_block (type "text" text ""))
;;   (type "ping")
;;   (type "content_block_delta" index 0 delta (type "text_delta" text "Hello Robert"))
;;   (type "content_block_delta" index 0 delta (type "text_delta" text "."))
;;   (type "content_block_stop" index 0)
;;   (type "message_delta" delta (stop_reason "end_turn" stop_sequence nil) usage (output_tokens 22))
;;   (type "message_stop"))
;;
;; and Google
;; '((created 1745008491
;;    model "gemini-2.5-pro-preview-03-25"
;;    object "chat.completion.chunk"
;;    choices [(delta (content "Hello Robert! How can I help you today?"
;;                     role "assistant")
;;              finish_reason "stop"
;;              index 0)]))
(defun oai-restapi--normalize-response (response)
  "This function normalizes JSON data in OpenAI-style but with some differences.
RESPONSE is one JSON message of the stream response as a chunk of full
response.
Return list or responses, with every response as `oai-block--response'."
  ;; (oai--debug "response:" response)
  (if-let ((error-message (plist-get response 'error)))
      (list (make-oai-block--response :type 'error :payload (or (plist-get response 'message) error-message)))

    (let ((response-type (plist-get response 'type)))

      ;; first try anthropic
      (cond
       ((string= response-type "ping") nil)
       ((string= response-type "message_start")
        (when-let ((role (plist-get (plist-get response 'message) 'role)))
          (list (make-oai-block--response :type 'role :payload role))))
       ((string= response-type "content_block_start")
        (when-let* ((text (plist-get (plist-get response 'content_block) 'text))
                    (text (when text (decode-coding-string (encode-coding-string text 'utf-8 't) 'utf-8))))
          (list (make-oai-block--response :type 'text :payload text))))
       ((string= response-type "content_block_delta")
        (when-let* ((text (plist-get (plist-get response 'delta) 'text))
                    (text (when text (decode-coding-string (encode-coding-string text 'utf-8 't) 'utf-8))))
          (list (make-oai-block--response :type 'text :payload text))))
       ((string= response-type "content_block_stop") nil)
       ((string= response-type "message_delta")
        (when-let* ((stop-reason (plist-get (plist-get response 'delta) 'stop_reason))
                    (stop-reason (when stop-reason (decode-coding-string (encode-coding-string stop-reason 'utf-8 't) 'utf-8))))
          (list (make-oai-block--response :type 'stop :payload stop-reason))))
       ((string= response-type "message_stop") nil)


       ;; try perplexity.ai
       ((and (plist-get response 'model) (string-prefix-p "llama-" (plist-get response 'model)))
        (let ((choices (plist-get response 'choices)))
          (when (and choices (> (length choices) 0))
            (let* ((choice (aref choices 0))
                   (message (plist-get choice 'message))
                   (delta (plist-get choice 'delta))
                   (role (or (plist-get delta 'role) (plist-get message 'role)))
                   (text (or (plist-get delta 'content) (plist-get message 'content)))
                   (finish-reason (plist-get choice 'finish_reason)))
              (append
               (when role
                 (list (make-oai-block--response :type 'role :payload role)))
               (when text
                 (setq text (decode-coding-string (encode-coding-string text 'utf-8 't) 'utf-8))
                 (list (make-oai-block--response :type 'text :payload text)))
               (when finish-reason
                 (list (make-oai-block--response :type 'stop :payload finish-reason))))))))

       ;; single message e.g. from non-streamed completion. Repeat `oai-restapi--get-single-response-text'
       ((let ((choices (plist-get response 'choices)))
          (and (= 1 (length choices))
               (plist-get (aref choices 0) 'message)))
        (let* ((choices (plist-get response 'choices))
               (choice (aref choices 0))
               (text (plist-get (plist-get choice 'message) 'content))
               (text (when text (decode-coding-string (encode-coding-string text 'utf-8 't) 'utf-8)))
               (role (plist-get (plist-get choice 'message) 'role))
               (finish-reason (or (plist-get choice 'finish_reason) 'stop)))
          (list (make-oai-block--response :type 'role :payload role)
                (make-oai-block--response :type 'text :payload text)
                (make-oai-block--response :type 'stop :payload finish-reason))))

       ;; try openai, deepseek, gemini streamed
       (t (let ((choices (plist-get response 'choices)))
            (cl-loop for choice across choices
                     append (let* ((delta (plist-get choice 'delta))
                                   (role (when-let ((role (plist-get delta 'role)))
                                           (if (and (string= "assistant" role)
                                                    (plist-get delta 'reasoning_content))
                                               "assistant_reason"
                                             role)))
                                   (text (plist-get (plist-get choice 'delta) 'content))
                                   (text (when text (decode-coding-string (encode-coding-string text 'utf-8 't) 'utf-8)))
                                   (reasoning-text (plist-get delta 'reasoning_content))
                                   (reasoning-text (when reasoning-text (decode-coding-string (encode-coding-string reasoning-text 'utf-8 't) 'utf-8)))
                                   (finish-reason (plist-get choice 'finish_reason))
                                   (result nil))
                              (when finish-reason
                                (push (make-oai-block--response :type 'stop :payload finish-reason) result))
                              (when (and reasoning-text (> (length reasoning-text) 0))
                                ;; (setq oai-restapi--currently-reasoning t)
                                (push (make-oai-block--response :type 'text :payload reasoning-text) result))
                              (when (and text (> (length text) 0))
                                (push (make-oai-block--response :type 'text :payload text) result))
                                ;; (when oai-restapi--currently-reasoning
                                ;;   (setq oai-restapi--currently-reasoning nil)
                                ;;   (push (make-oai-block--response :type 'role :payload "assistant") result)))
                              (when role
                                (push (make-oai-block--response :type 'role :payload role) result))
                              result))))))))


(cl-defun oai-restapi-request (service model callback &optional &key prompt messages max-tokens temperature top-p frequency-penalty presence-penalty stream)
  "Use API to LLM to request and get response.
Executed by `oai-restapi-request-prepare'
PROMPT is the query for completions MESSAGES is the query for chatgpt.
CALLBACK is the callback function.
MODEL is the
model to use.
MAX-TOKENS is the maximum number of tokens to generate.
TEMPERATURE is the temperature of the distribution.
TOP-P is the top-p value.
FREQUENCY-PENALTY is the frequency penalty.
PRESENCE-PENALTY is the presence penalty.
Variables used to save state:
not buffer local:
buffer local and nil by default:
- `oai-block--current-insert-position-marker' - in url callback to
  track where we insert.
- `oai-block--current-chat-role'
For `oai-restapi--url-request-on-change-function':
- `oai-restapi--current-request-is-streamed'
- `oai-restapi--current-url-request-callback'.

Parallel requests require to keep `url-request-buffer'
to be able  to kill it.  We  solve this by creating timer  in buffer with
name of url-request-buffer +1.
We count running ones in global integer only.

For not stream url return event and hook `after-change-functions'
 triggered only after url buffer already kill, that is why we don't use
 this hook.
Use argument SERVICE to find endpoint, MODEL as parameter to request."
  ;; - HTTP body preparation as a string
  (let ((url-request-extra-headers (oai-restapi--get-headers service))
        (url-request-method "POST")
        (endpoint (oai-restapi--get-endpoint messages service))
        (url-request-data
         (encode-coding-string (json-encode
                                (oai-restapi--payload :prompt prompt
					              :messages messages
					              :model model
					              :max-tokens max-tokens
					              :temperature temperature
					              :top-p top-p
					              :frequency-penalty frequency-penalty
					              :presence-penalty presence-penalty
					              :service service
					              :stream stream))
                               'utf-8)))
    ;; - regex check
    (if model
        (oai-restapi--check-model model endpoint)) ; not empty and if "api.openai.com" or "openai.azure.com"
    (oai--debug "oai-restapi-request service and (type-of service): %s %s" service (type-of service))
    (oai--debug "oai-restapi-request endpoint and (type-of endpoint): %s %s" endpoint (type-of endpoint))
    (oai--debug "oai-restapi-request headers: %s" url-request-extra-headers)
    (oai--debug "oai-restapi-request request-data:" (oai-debug--prettify-json-string url-request-data))


    (oai--debug "Main request before, that return a \"urllib buffer\".")
    (let ((url-request-buffer
           (url-retrieve ; <- - - - - - - - -  MAIN
            endpoint
            (lambda (events)
              ;; "Called within url-request-buffer after `after-change-functions'"
              (ignore events)
              ;; called one time at error or at the end of all receiving.

              (oai--debug "oai-restapi-request *url-retrieve callback*")

              (let (oai-restapi--url-buffer-last-position-marker)
                (oai-restapi--debug-urllib (current-buffer)))

              ;; (if (oai-restapi--maybe-show-openai-request-error) ; TODO: change to RESULT by global customizable option ; maybe use _events: (:error (error http 400)
                  ;; for stream
                  ;; (remove-hook 'after-change-functions #'oai-restapi--url-request-on-change-function t))
                ;; Called for not stream, call `oai-restapi--current-url-request-callback'
              (when (or nil ; oai-restapi--current-request-is-streamed
                        (not (oai-restapi--maybe-show-openai-request-error))) ; t if error
                (oai-restapi--url-request-on-change-function nil nil nil)) ; should be always called for stream


              ;; finally stop track buffer, error or not
              ;; (oai--debug "Main request lambda" _events)
              (oai-timers--interrupt-current-request (current-buffer) #'oai-restapi--stop-tracking-url-request)
              ;; (oai-timers--interrupt-current-request (current-buffer) #'oai-restapi--interrupt-url-request)
              ))))

      (oai--debug "Main request after." url-request-buffer)

      (with-current-buffer url-request-buffer
        ;; - it is `oai-block--insert-stream-response' or `oai-block--insert-single-response'
        (setq oai-restapi--current-url-request-callback callback)
        ;; - `oai-restapi--url-request-on-change-function', `oai-restapi--current-request-is-streamed'
        (setq oai-restapi--current-request-is-streamed stream)

        ;; - for stream add hook, otherwise remove - do word by word output (optional actually)
        (if stream
            (unless (member 'oai-restapi--url-request-on-change-function after-change-functions)
              (add-hook 'after-change-functions #'oai-restapi--url-request-on-change-function nil t))
          ;; else - not stream
          (remove-hook 'after-change-functions #'oai-restapi--url-request-on-change-function t)))
      url-request-buffer)))

;; -=-= oai-restapi-request-llm
(cl-defun oai-restapi-request-llm (service model callback &optional &key prompt messages max-tokens temperature top-p frequency-penalty presence-penalty)
  "Simplified version of `oai-restapi-request' without stream support.
Used for building agents or chain of requests.
Call CALLBACK called from callback of `url-retrieve' with nil or result of
`oai-restapi--normalize-response' of response.
Use argument SERVICE to find endpoint, MODEL as parameter to request.
Call CALLBACK at receive.  Call CALLBACK with nil if error.
One of argument PROMPT and MESSAGES used as main payload.
For MAX-TOKENS, TEMPERATURE, TOP-P, FREQUENCY-PENALTY, PRESENCE-PENALTY,
see `oai-restapi-request-prepare'."
  (oai--debug "oai-restapi-request-llm 1) %s %s %s" (current-buffer) service oai-restapi-con-token)
  (let ((url-request-extra-headers (oai-restapi--get-headers service))
        (url-request-method "POST")
        (endpoint (oai-restapi--get-endpoint messages service))
        (url-request-data
         (encode-coding-string (json-encode
                                (oai-restapi--payload :prompt prompt
					              :messages messages
					              :model model
					              :max-tokens max-tokens
					              :temperature temperature
					              :top-p top-p
					              :frequency-penalty frequency-penalty
					              :presence-penalty presence-penalty
					              :service service
					              :stream nil))
                               'utf-8)))
    (oai--debug "oai-restapi-request-llm 2) prompt: %s" prompt)
    (oai--debug "oai-restapi-request-llm 3) messages: %s" messages)
    (oai--debug "oai-restapi-request-llm 4) endpoint: %s %s" endpoint (type-of endpoint))
    (oai--debug "oai-restapi-request-llm 5) request-data:" (oai-debug--prettify-json-string url-request-data))

    ;; (setq url-request-buffer
    (url-retrieve ; <- - - - - - - - -  MAIN
     endpoint
     (lambda (events)
       "oai-restapi-request-llm main callback."
       (oai--debug "oai-restapi-request-llm 6) *url-retrieve callback*:" events)
       ;; debug
       (let (oai-restapi--url-buffer-last-position-marker)
         (oai-restapi--debug-urllib (current-buffer)))
       ;;
       (if (oai-restapi--maybe-show-openai-request-error) ; TODO: change to RESULT by global customizable option
           (funcall callback nil) ; signal error to callback
         ;; else - read from url-buffer
         (when (and (boundp 'url-http-end-of-headers) url-http-end-of-headers)
           ;; (save-excursion
           (goto-char url-http-end-of-headers)
           (oai--debug "oai-restapi-request-llm 7) " url-http-end-of-headers)

           (let ((data (oai-restapi--json-decode-not-streamed (buffer-substring-no-properties (point) (point-max)))))
             (when data
               (funcall callback (oai-restapi--get-single-response-text data))))))))))
            ;; - Done or Error
            ;; (oai--debug "on change 2)")
            ;; (funcall oai-restapi--current-url-request-callback nil))

            ;;       (let ((json-object-type 'plist)
            ;;             (json-key-type 'symbol)
            ;;             (json-array-type 'vector)
            ;;             (line (buffer-substring-no-properties (point) (point-max))))
            ;;         (oai--debug "oai-restapi-request-llm 8) " line)
            ;;         ;; (condition-case _err
            ;;             ;; (let* ((res1 (buffer-substring-no-properties (point) (point-max)))
            ;;             ;;       (res2 (json-read-from-string res1))
            ;;             ;;       (res3 (oai-restapi--normalize-response res2))
            ;;             ;;       (res4 (nth 1 res3))
            ;;             ;;       (res5 (oai-block--response-payload res4))
            ;;             ;;       (res (decode-coding-string res5 'utf-8))
            ;;             ;;       )
            ;;             ;;   (print (list "HERE6" res3 ))
            ;;             ;;   (print (list "HERE6h7" callback))
            ;;             ;;   ;; to prevent catching error to here
            ;;             ;;   (run-at-time 0 nil callback res)
            ;;             ;;   )
            ;;             ;; (message "no HERE7 error")
            ;;             ;; (oai--debug "oai-restapi-request-llm 6) normalize: %s"
            ;;             ;;             (oai-restapi--normalize-response
            ;;             ;;              (json-read-from-string
            ;;             ;;               (buffer-substring-no-properties (point) (point-max)))))
            ;;             (oai--debug "oai-restapi-request-llm 9) decoded: %s"
            ;;                         (oai-restapi--normalize-response (json-read-from-string line)))
            ;;                                               'utf-8))
            ;;             (funcall callback (decode-coding-string (oai-block--response-payload (oai-restapi--normalize-response
            ;;                                                                                     (json-read-from-string
            ;;                                                                                      line)))
            ;;                                                     'utf-8))
            ;;           ;; (error
            ;;           ;;  nil
            ;;           ;;  ;; (message "HERE7 error")
            ;;           ;;  ;; (funcall callback nil) ; signal error to callback
            ;;           ;;  )
            ;;           ))))))
           ;; (timeout (or timeout oai-timers-duration))
           ;; (waiter (run-with-timer 3 0 (lambda(b) (print b)) b))

      ;; url-request-buffer))

;; - Test!
;; (let ((service 'together)
;;       (model "meta-llama/Llama-3.3-70B-Instruct-Turbo-Free")
;;       (max-tokens 10)
;;       (temperature nil)
;;       (top-p nil)
;;       (frequency-penalty nil)
;;       (presence-penalty nil))
;;   (oai-timers--progress-reporter-run
;;    1
;;    (lambda (buf) (oai-timers--interrupt-current-request buf #'oai-restapi--interrupt-url-request))
;;    (oai-restapi-request-llm service model (lambda (result)
;;                                            (oai-timers--interrupt-current-request (current-buffer) #'oai-restapi--stop-tracking-url-request)
;;                                            (print (list "hay" result)))
;;                            :timeout 20
;;                            :messages  (vector (list :role 'system :content "You a helpful.")
;;                                               (list :role 'user :content "How to do staff?"))
;;                            :max-tokens max-tokens
;;                            :temperature temperature
;;                            :top-p top-p
;;                            :frequency-penalty frequency-penalty
;;                            :presence-penalty presence-penalty)))

;; (defvar oai-restapi-request-llm-retries  nil
;;   "Save url-buffer in temp buffer in which timer have been started.")
;; (make-variable-buffer-local 'oai-restapi-request-llm-retries)

(cl-defun oai-restapi-request-llm-retries (service model timeout callback &optional &key retries prompt messages header-marker max-tokens temperature top-p frequency-penalty presence-penalty)
  "`oai-restapi-request-llm' function with TIMEOUT and RETRIES.
Only one request per ai block is allowed at one time.
Timer function restart requst and restart timer with attempts-1.
In callback we add cancel timer function.
We save and cancel time only in callback.
TIMER is time to wait for one request.
Use argument SERVICE to find endpoint, MODEL as parameter to request.
How? we restart request if
1.  url-buffer is alive - hanged - in timer - in timer we check
url-buffer is alive.
2. url returned error, we check it in callback.
We store url-buf with marker of header in oai-timers.el"
  (oai--debug "oai-restapi-request-llm-retries0 timeout %s" timeout)
  (with-current-buffer (marker-buffer header-marker)
    (let* (
          ;; prepare request - apply tags to message
          (messages (oai-restapi--modify-vector-content messages 'user #'oai-block-tags-replace))
          (messages (oai-restapi--modify-vector-content messages 'user #'oai-block-tags--clear-properties))
          (messages (oai-block--pipeline oai-restapi-after-prepare-messages-hook messages)))
      (when (or (and retries (> retries 0))
                (not retries))
        (oai--debug "oai-restapi-request-llm-retries1 %s" (current-buffer))
        ;; - 1) run timer
        (let* ((left-retries (if retries (1- retries) 3))
               ;; (tmp-buf (current-buffer))
               ;; run timer in temp buffer - to limit request by timeout, and kill url-buffer
               ;; we start timer first because we pass it to callback to stop timer itself
               (timer (run-with-timer timeout
                                      0
                                      (lambda ()
                                        "Suppress errors, they don't visible."
                                        (oai--debug "timer of oai-restapi-request-llm-retries"
                                                    left-retries
                                                    (oai-timers--get-keys-for-variable header-marker)
                                                    (seq-find (lambda (x) (buffer-live-p  x)) (oai-timers--get-keys-for-variable header-marker)))
                                        ;; - get url-buffer to check if it hanging.
                                        (let ((urlbuf (seq-find (lambda (x) (buffer-live-p x))
                                                                (oai-timers--get-keys-for-variable header-marker))))
                                          ;; (with-current-buffer tmp-buf
                                          (oai--debug "timer of oai-restapi-request-llm-retries, buf3: %s %s" (current-buffer) urlbuf)
                                          ;; - Main action of timer: interrupt request
                                          (when urlbuf
                                            (oai-timers--interrupt-current-request urlbuf #'oai-restapi--interrupt-url-request)
                                            (oai--debug "in oai-restapi-request-llm-retries WE SHOULD RESTART HERE1")

                                            ;; - retry if request was hanging
                                            ;; - restart
                                            (if (> left-retries 0)
                                                ;; also save url-buffer
                                                (oai-restapi-request-llm-retries service model timeout callback
                                                                                 :retries left-retries
                                                                                 :messages messages
                                                                                 :max-tokens max-tokens
                                                                                 :header-marker header-marker
                                                                                 :temperature temperature
                                                                                 :top-p top-p
                                                                                 :frequency-penalty frequency-penalty
                                                                                 :presence-penalty presence-penalty)
                                              ;; else - failed
                                              (run-at-time 0 nil callback nil)
                                              (oai-block-insert-result-message "Failed" header-marker))
                                            (oai--debug "timer of oai-restapi-request-llm-retries 1111")
                                            (oai-timers--update-global-progress-reporter))))))

               (url-buffer
                (progn
                  (oai--debug "oai-restapi-request-llm-retries2 %s" (current-buffer))
                  ;; - 2) make request
                  (oai-restapi-request-llm service model
                                           (lambda (result-llm)
                                             (oai--debug "oai-restapi-request-llm callback1, result: %s" result-llm)
                                             (if timer
                                                 (cancel-timer timer))
                                             (oai--debug "oai-restapi-request-llm  callback2")
                                             ;; (with-current-buffer cb
                                             (oai--debug "oai-restapi-request-llm  callback3 %s" result-llm)
                                             (if result-llm
                                                 (progn
                                                   (oai--debug "oai-restapi-request-llm here")
                                                   (run-at-time 0 nil callback result-llm))
                                               ;; else - nil returned - error - retry
                                               (if (> left-retries 0)
                                                   (progn
                                                     ;; oai-restapi-request-llm
                                                    (oai--debug "oai-restapi-request-llm here2")
                                                    ;; retrie after 3 sec
                                                    (run-at-time 3 nil (lambda () (oai-restapi-request-llm-retries service model timeout callback
                                                                                                                   :retries left-retries
                                                                                                                   :messages messages
                                                                                                                   :max-tokens max-tokens
                                                                                                                   :header-marker header-marker
                                                                                                                   :temperature temperature
                                                                                                                   :top-p top-p
                                                                                                                   :frequency-penalty frequency-penalty
                                                                                                                   :presence-penalty presence-penalty))))
                                                 ;; else - failed
                                                 (oai--debug "oai-restapi-request-llm failed")
                                                 (run-at-time 0 nil callback nil)

                                                 (oai-block-insert-result-message "Failed" header-marker)

                                                 (oai-timers--update-global-progress-reporter)))

                                             ;; (funcall callback result-llm)
                                             (oai--debug "oai-restapi-request-llm  callback4")
                                             ;; )
                                             )
                                           :prompt prompt
                                           :messages  messages
                                           :max-tokens max-tokens
                                           :temperature temperature
                                           :top-p top-p
                                           :frequency-penalty frequency-penalty
                                           :presence-penalty presence-penalty))))
          ;; save url-buffer
          (oai--debug "oai-restapi-request-llm-retries3" oai-timers--element-marker-variable-dict)
          (oai-timers--set url-buffer header-marker)
          (oai--debug "oai-restapi-request-llm-retries4" oai-timers--element-marker-variable-dict))))))

;; -=-= error, payload, url-request-on-change-function

(defun oai-restapi--maybe-show-openai-request-error ()
  "If the API request returned an error, show it.
`REQUEST-BUFFER' is the buffer containing the request.
If http-code is nil - C\\-g was used to stop all.
Return t if error happen, otherwise nil.
If C\\-g was used return nil.
Should be executed in url-buffer only."
  (oai--debug "oai-restapi--maybe-show-openai-request-error1")
  (save-excursion ; ??
    (let ((http-code (url-http-symbol-value-in-buffer 'url-http-response-status (current-buffer))) ; should be integer, but may not be
          (http-data (if (and (boundp 'url-http-end-of-headers) url-http-end-of-headers)
                         ;; get data after HTTP headers from current url buffer
                         (progn
                           (string-trim (buffer-substring-no-properties url-http-end-of-headers
                                                                        (point-max))))
                       ;; else
                       ""))
          (http-header-first-line (buffer-substring-no-properties (point-min)
                                                                  (save-excursion
                                                                    (goto-char (point-min))
                                                                    (line-end-position))))
          ret)
      (unless (numberp http-code)
        (setq http-code nil))
      (oai--debug "oai-restapi--maybe-show-openai-request-error2 %s" http-code)
      (when (boundp 'url-http-end-of-headers)
        (oai--debug "oai-restapi--maybe-show-openai-request-error22 %s " url-http-end-of-headers))
      (setq ret
            (or
             (when (and http-code (/= http-code 200))
               (oai--debug "oai-restapi--maybe-show-openai-request-error3")
               (funcall oai-restapi-show-error-function (format "HTTP Error from the service: %s %s \n %s" http-code http-data http-header-first-line)
                        (oai-timers--get-variable (current-buffer))) ; header-marker
               t)
             (when (and (boundp 'url-http-end-of-headers) url-http-end-of-headers)
               (goto-char url-http-end-of-headers)
               (condition-case nil
                   (when-let* ((body (json-read))
                               (err (or (alist-get 'error body)
                                        (plist-get body 'error)))
                               (message (or (alist-get 'message err)
                                            (plist-get err 'message)))
                               (message (if (and message (not (string-blank-p message)))
                                            message
                                          (json-encode err))))
                     (funcall oai-restapi-show-error-function (concat (format "%s\n" http-header-first-line)
                                                                      "Error from the service API:\n\t" message)
                              (oai-timers--get-variable (current-buffer))) ; header-marker
                     )
                 (error nil)))))
      (oai--debug "oai-restapi--maybe-show-openai-request-error3 %s" ret)
      ret)))


(cl-defun oai-restapi--payload (&optional &key service model prompt messages max-tokens temperature top-p frequency-penalty presence-penalty stream)
  "Create the payload for the OpenAI API.
PROMPT is the query for completions MESSAGES is the query for chatgpt.
MODEL is the model to use.
MAX-TOKENS is the maximum number of tokens to generate.
TEMPERATURE is the temperature of the distribution.
TOP-P is the top-p value.
FREQUENCY-PENALTY is the frequency penalty.
PRESENCE-PENALTY is the presence penalty.
STREAM is a boolean indicating whether to stream the response.
Use argument SERVICE to find endpoint, MODEL as parameter to request."
  (let ((extra-system-prompt)
        (max-completion-tokens))

    (when (eq service 'anthropic)
      (when (string-equal (plist-get (aref messages 0) :role) "system")
        (setq extra-system-prompt (plist-get (aref messages 0) :content))
        (cl-shiftf messages (cl-subseq messages 1)))
      (setq max-tokens (or max-tokens 4096)))

    ;; o1 models currently does not support system prompt
    (when (and (or (eq service 'openai) (eq service 'azure-openai))
               (or (string-prefix-p "o1" model) (string-prefix-p "o3" model)))
      (setq messages (cl-remove-if (lambda (msg) (string-equal (plist-get msg :role) "system")) messages))
      ;; o1 does not support max-tokens
      (when max-tokens
        (setq max-tokens nil)
        (setq max-completion-tokens (or max-tokens 128000))))

    (oai--debug "oai-restapi--payload stream: %s" stream)

    (let* ((input (if messages `(messages . ,messages) `(prompt . ,prompt)))
           ;; TODO yet unsupported properties: n, stop, logit_bias, user
           (data (map-filter (lambda (x _) x)
                             `(,input
                               ,@(when model                 `((model . ,model)))
                               ;; ,@(when stream                `((stream . t)))
                               ;; ,@(when (not stream)          `((stream . nil)))
                               (stream . ,stream)
                               ,@(when max-tokens            `((max_tokens . ,max-tokens)))
                               ,@(when max-completion-tokens `((max-completion-tokens . ,max-completion-tokens)))
                               ,@(when temperature           `((temperature . ,temperature)))
                               ,@(when top-p                 `((top_p . ,top-p)))
                               ,@(when frequency-penalty     `((frequency_penalty . ,frequency-penalty)))
                               ,@(when presence-penalty      `((presence_penalty . ,presence-penalty)))))))

      (when extra-system-prompt
        (setq data (append data `((system . ,extra-system-prompt)))))
      data)))

     ;; (encode-coding-string (json-encode data) 'utf-8))))

(defun oai-restapi--clean-unicode-text (str)
  "Remove ASCII control chars except tab, newline, and carriage return.
Argument STR unicode multi-byte string."
  (apply #'string
         (seq-filter
          (lambda (ch)
            ;; (or
             (>= ch 32)                ; Unicode (including emoji, CJK, etc.) and printable ASCII
             ;; (memq ch '(?\t ?\n ?\r))
             ) ; Allow tab, linefeed, CR
          (string-to-list str))))

(defun oai-restapi--json-decode-not-streamed (string)
  "Decode JSON STRING to plist.
This is slow version compared to `json-read', because
`json-read-from-string' create temp buffer.
Return nil if error."

  (let ((json-object-type 'plist)
        (json-key-type 'symbol)
        (json-array-type 'vector))
    (condition-case _err
        (json-read-from-string
         (oai-restapi--clean-unicode-text
          (decode-coding-string (encode-coding-string
                                 (replace-regexp-in-string "\r.*?\r" "" string)
                                 'utf-8 't) 'utf-8))) ; just in case
      (error nil))))


(defun oai-restapi--url-request-on-change-function (_beg _end _len)
  "First function that read url-request buffer and extracts JSON stream responses.
Arguments _BEG _END _LEN are not used. They are:
the positions of the beginning and end of the range of changed text,
and the length in chars of the pre-change text replaced by that range.

Call `oai-restapi--current-url-request-callback' with data.
After processing call `oai-restapi--current-url-request-callback' with nil.
This  callback  here  is `oai-block--insert-stream-response'  for  chat  or
`oai-block--insert-single-response' for completion.
Called within `url-retrieve' buffer.
Return JSOIN in plist format."
  (when (and (boundp 'url-http-end-of-headers)
             url-http-end-of-headers
             (if oai-restapi--url-buffer-last-position-marker
                 (> (- (point-max) (marker-position oai-restapi--url-buffer-last-position-marker)) 6) ; [DONE]
               t))
    (save-excursion
      (save-match-data ; without it cause error integer-or-marker-p nil in 'url-http-chunked-encoding-after-change-function`
        ;; (save-restriction
        (if oai-restapi--url-buffer-last-position-marker
            (goto-char oai-restapi--url-buffer-last-position-marker)
          ;; else
          (goto-char url-http-end-of-headers)
          (setq oai-restapi--url-buffer-last-position-marker (point-marker)))
        (oai--debug "oai-restapi--url-request-on-change-function 1) %s" (- (point-max) (point)))
        ;; Avoid a bug where we skip responses because url has modified the http
        ;; buffer and we are not where we think we are.
        ;; TODO this might break
        ;; (unless (eolp)
        ;;   (beginning-of-line))
        (oai--debug "oai-restapi--url-request-on-change-function 2) streaming? %s" oai-restapi--current-request-is-streamed)
        ;; - Streamed
        ;; multiple JSON objects prefixed with "data: " separated by empty line
        ;; This is a fast version of JSON decoding. We falback to slow version if error.
        (when oai-restapi--current-request-is-streamed
          (let ((errored nil)
                (case-fold-search nil)
                psave
                line1) ; simple line
            (set-buffer-multibyte t) ; force UTF-8 for url-buffer
            (oai--debug "oai-restapi--url-request-on-change-function 3) %s %s" (point) (point-max))
            ;; loop per chunks separated by empty line
            (while (and (not errored) ; we decode chunks until unable to decode one, this mean that chunk should be received first.
                        (search-forward "data: " nil t)) ; set cursor after "data: {" on "{"

              (setq psave (point))

              (oai--debug "oai-restapi--url-request-on-change-function 4) found: %s %s" (point) oai-restapi--url-buffer-last-position-marker)
              (when (setq line1 (buffer-substring-no-properties (point) (line-end-position)))
                (if (string= line1 "[DONE]") ;; "[DONE]" string found
                    (progn
                      (oai--debug "oai-restapi--url-request-on-change-function 10) DONE %s %s" (point) (point-max))
                      (end-of-line)
                      (set-marker oai-restapi--url-buffer-last-position-marker (point))

                      (remove-hook 'after-change-functions #'oai-restapi--url-request-on-change-function t)

                      (funcall oai-restapi--current-url-request-callback nil) ; INSERT CALLBACK! - no do nothing
                      (oai--debug "oai-restapi--url-request-on-change-function 11) DONE")
                      ;; (oai-timers--interrupt-current-request (current-buffer) #'oai-restapi--stop-tracking-url-request)
                      )
                  ;; - else not DONE
                  (let ((json-object-type 'plist)
                        (json-key-type 'symbol)
                        (json-array-type 'vector)
                        ;; (tmp-buf (or oai-restapi--tmp-buf
                        ;;              (setq oai-restapi--tmp-buf (generate-new-buffer " *temp*" t))))
                        data
                        line) ; multi-line if splitted
                    ;;     ;;   (oai--debug "oai-restapi--url-request-on-change-function 5) \"%s\"" line1)
                    ;;     ;;   ;; (data (json-read-from-string line)) ; (setq data
                    ;;     ;;   ;; (with-current-buffer tmp-buf ; faster
                    ;;     ;;     ;; - Decoding attempt 1.
                    (condition-case _err
                        (progn
                          ;; (erase-buffer)
                          ;; (insert line1)
                          ;; (goto-char (point-min))
                          (setq data (json-read)))
                      (error
                       (setq errored t)
                       nil))
                    ;; - Decoding attempt 2.
                    (when errored
                      (oai--debug "oai-restapi--url-request-on-change-function 6) - Decoding attempt 2")
                      (goto-char psave)
                      (setq line
                            ;; if string splitted in url-buffer for some reason. we look for empty lines as a separateror.
                            (string-join
                             (nreverse
                              (let ((lines (list (buffer-substring-no-properties (point) (line-end-position))))
                                    line-cur)
                                (while (and (= (forward-line) 0) ; if not end of buffer
                                            (progn
                                              (setq line-cur (buffer-substring-no-properties (point) (line-end-position)))
                                              (unless (string-empty-p line-cur)
                                                (push line-cur lines)))))
                                lines)))) ; stop at empty next line.
                      ;; (when (string-empty-p line-cur) ; return -1 line
                      ;;   (forward-line -1))

                      (oai--debug "oai-restapi--url-request-on-change-function 7) - Decoding attempt 2: %s" line)
                      (setq data (oai-restapi--json-decode-not-streamed line))
                      (if data
                          (setq errored nil)))
                    (oai--debug "oai-restapi--url-request-on-change-function 8) data? %s" data)

                    (when data ;; errored is nil
                      ;; save only if data or DONE
                      (set-marker oai-restapi--url-buffer-last-position-marker (point))
                      (oai--debug "oai-restapi--url-request-on-change-function 9) - request-callback")
                      (funcall oai-restapi--current-url-request-callback data) ; INSERT CALLBACK!
                      )))))))

        ;; - Not-streamed
        ;; response is a single JSON object, no "data: " prefix. {"choices":[...
        (when (not oai-restapi--current-request-is-streamed)
          (oai--debug "oai-restapi--url-request-on-change-function NOT-STREAM: %s %s" (point) (point-max))
          (let ((data (oai-restapi--json-decode-not-streamed (buffer-substring-no-properties (point) (point-max)))))
            (when data
              (funcall oai-restapi--current-url-request-callback data))
            ;; - Done or Error
            (funcall oai-restapi--current-url-request-callback nil)))))))


;; -=-= Reporter & Requests interrupt functions
;; 1) `oai-timers--progress-reporter-run' - start global timer
;; 2) `oai-timers--interrupt-current-request' - interrupt, called to stop tracking on-changes or kill buffer
;; When  we kill  one buffer  and if  no others  we report  failure or
;; success  if there  are  other  we just  continue  and don't  change
;; reporter.
;; Functions:
;; Failure by time:
;; `oai-restapi-stop-all-url-requests' 'failed
;; `oai-timers--interrupt-current-request'
;; Success:
;; `oai-timers--interrupt-current-request'
;; Interactive:
;; `oai-restapi-interrupt-url-request'
;; Variables:
;; - `oai-timers--global-progress-reporter' - lambda that return a string,
;; - `oai-timers--global-progress-timer' - timer that output /-\ to echo area.
;; - `oai-timers--global-progress-timer-remaining-ticks'.
;; - `oai-timers--current-progress-timer' - count life of url buffer,
;; - `oai-timers--current-progress-timer-remaining-ticks'."

(defun oai-restapi--interrupt-url-request (url-buffer)
  "Remove on-update hook and kill URL-BUFFER.
Called from `oai-restapi-stop-url-request',
`oai-restapi-stop-all-url-requests'."
  ;; (oai--debug "oai-restapi--interrupt-url-request"
  ;;                (eq (current-buffer) url-buffer)
  ;;                (buffer-live-p url-buffer))
  (oai--debug "oai-restapi--stop-tracking-url-request %s" url-buffer)
  (if (eq (current-buffer) url-buffer)
      (progn
        (remove-hook 'after-change-functions #'oai-restapi--url-request-on-change-function t)
        (when (buffer-live-p url-buffer)
          (let (kill-buffer-query-functions)
            (kill-buffer url-buffer))))
    ;; else
    (when (and url-buffer (buffer-live-p url-buffer))
      (with-current-buffer url-buffer
        (remove-hook 'after-change-functions #'oai-restapi--url-request-on-change-function t)
        (let (kill-buffer-query-functions) ; set to nil
          (kill-buffer url-buffer))))))

(defun oai-restapi--stop-tracking-url-request (url-buffer)
  "Remove on-update hook and not kill URL-BUFFER.
Called from `oai-restapi-stop-url-request',
`oai-restapi-stop-all-url-requests'."
  (oai--debug "oai-restapi--stop-tracking-url-request %s %s"
                 (eq (current-buffer) url-buffer)
                 (buffer-live-p url-buffer))
  (if (eq (current-buffer) url-buffer)
      (remove-hook 'after-change-functions #'oai-restapi--url-request-on-change-function t)
    ;; else
    (if (and url-buffer (buffer-live-p url-buffer))
      (with-current-buffer url-buffer
        (remove-hook 'after-change-functions #'oai-restapi--url-request-on-change-function t)))))

(cl-defun oai-restapi-stop-url-request (&optional &key element url-buffer)
  "Interrupt the request for ELEMENT or URL-BUFFER.
If  no ELEMENT  or URL-BUFFER  provided we  use in  current ai  block at
current position at current buffer.
Return t if buffer was found.
Called from `oai-timers--progress-reporter-run'."
  (interactive)
  ;; (oai--debug "oai-restapi-stop-url-request, current buffer %s, url-buf %s, element %s"
  ;;                (current-buffer) url-buffer
  ;;                (oai-block-p))
  (if-let* ((element (or element (oai-block-p)))
            (url-buffers (if url-buffer
                             (list url-buffer)
                           ;; else
                           (oai-timers--get-keys-for-variable (oai-block-get-header-marker element)))))
      (progn
        (oai--debug "oai-restapi-stop-url-request, element %s, url-buffers %s"
                       element
                       url-buffers)
        (oai-timers--interrupt-current-request url-buffers #'oai-restapi--interrupt-url-request)
        t)
    ;; ;; else - called not at from some block, but from elsewhere
    ;; (oai--debug "oai-restapi-stop-url-request all %s" (oai-timers--get-keys-for-variable (oai-block-get-header-marker element)))
    (oai-restapi-stop-all-url-requests) ; kill all
    ;; else - nil
    ))

;;;###autoload
(cl-defun oai-restapi-stop-all-url-requests (&optional &key failed)
  "Called from `oai-restapi-stop-url-request' when not at some block.
Return t if buffer was found, nil otherwise.
Optional FAILED flag used to signal of failure to user, that timer is
over."
  (interactive)
  (oai-timers--interrupt-all-requests #'oai-restapi--interrupt-url-request failed))


;; -=-= chat-messages: collect/stringify

(defun oai-restapi--collect-chat-messages-at-point (&optional element default-system-prompt persistant-sys-prompts max-token-recommendation separator)
  "Get messsages for ai block at point or element in current buffer.
Replacement Links, remove text properties, split to messages.
ELEMENT is the element of the special block.
Description for DEFAULT-SYSTEM-PROMPT PERSISTANT-SYS-PROMPTS
MAX-TOKEN-RECOMMENDATION SEPARATOR at `oai-block--collect-chat-messages'."
  (oai-block--collect-chat-messages-from-string
   (oai-block-tags--clear-properties
    (oai-block-tags-replace
     (string-trim
      (oai-block-get-content element))))
   default-system-prompt persistant-sys-prompts max-token-recommendation separator))

;; (oai-restapi--normalize-response '(id "o3fA4D4-62bZhn-9617f44f6d399d91" object "chat.completion" created 1752904364 model "meta-llama/Llama-3.3-70B-Instruct-Turbo-Free" prompt [] choices [(finish_reason "stop" seed 819567834314233700 logprobs nil index 0 message (role "assistant" content "It works: `(2 3 1)` is returned." tool_calls []))] usage (prompt_tokens 131 completion_tokens 14 total_tokens 145 cached_tokens 0)))
;; (#s(oai-block--response role "assistant") #s(oai-block--response text "It works: `(2 3 1)` is returned.") #s(oai-block--response stop "stop"))
;; (oai-restapi--normalize-response '(id "o3f9cZv-4Yz4kd-9617f234fd6f9d91" object "chat.completion" created 1752904277 model "meta-llama/Llama-3.3-70B-Instruct-Turbo-Free" prompt [] choices [(finish_reason "stop" seed 589067420664432000 logprobs nil index 0 message (role "assistant" content "`(mapcar 'cdr '((a . 2) (x . 3) (2 . 1)))` returns `(2 3 1)`." tool_calls []))] usage (prompt_tokens 84 completion_tokens 34 total_tokens 118 cached_tokens 0)))
;; (#s(oai-block--response role "assistant") #s(oai-block--response text "`(mapcar 'cdr '((a . 2) (x . 3) (2 . 1)))` returns `(2 3 1)`.") #s(oai-block--response stop "stop"))

;; (cl-defun oai-restapi--stringify-chat-messages (messages &optional &key
;;                                                     default-system-prompt
;;                                                     (system-prefix "[SYS]: ")
;;                                                     (user-prefix "[ME]: ")
;;                                                     (assistant-prefix "[AI]: "))
;;   "Convert a chat message to a string.
;; MESSAGES is a vector of (:role :content) pairs.  :role can be
;; \='system, \='user or \='assistant.  If DEFAULT-SYSTEM-PROMPT is
;; non-nil, a [SYS] prompt is prepended if the first message is not
;; a system message.  SYSTEM-PREFIX, USER-PREFIX and
;; ASSISTANT-PREFIX are the prefixes for the respective roles
;; inside the assembled prompt string."
;;   (let ((messages (if (and default-system-prompt
;;                            (not (eql (plist-get (aref messages 0) :role) 'system)))
;;                       (cl-concatenate 'vector (vector (list :role 'system :content default-system-prompt)) messages)
;;                     messages)))
;;     (cl-loop for (_ role _ content) across messages
;;              collect (cond ((eql role 'system) (concat system-prefix content))
;;                            ((eql role 'user) (concat user-prefix content))
;;                            ((eql role 'assistant) (concat assistant-prefix content)))
;;              into result
;;              finally return (string-join result "\n\n"))))

;; -=-= chat-messages: modify content

(defun oai-restapi--find-last-user-index (vec)
  "Return the index of the last element in VEC whose :role is \='user, or nil."
  (let ((i (1- (length vec)))
        idx)
    (while (and (>= i 0) (not idx))
      (let ((elt (aref vec i)))
        (when (and (listp elt)
                   (eq (plist-get elt :role) 'user))
          (setq idx i)))
      (setq i (1- i)))
    idx))


(defun oai-restapi--modify-vector-content (vec role new-content)
  "Modify every string content of messages VEC that match role.
ROLE is symbol.  NEW-CONTENT may be string or function that accept one
string arguments and return new content."
  (unless (vectorp vec)
    (error "Not a vector"))
  (let ((i (1- (length vec)))
        (newvec (copy-sequence vec))
        el
        content)
    ;; loop over messages in vec
    (while (>= i 0)
      (setq el (aref newvec i)) ; from 0 to len-1
      (setq content (plist-get el :content))
      ;; replace content for role
      (when (eql role (plist-get el :role))
        (setq content (if (functionp new-content)
                          (funcall new-content content)
                        ;; else
                        new-content))
        (aset newvec i (plist-put (copy-sequence el) :content content)))
      (setq i (1- i))) ; ++1
    newvec))


(defun oai-restapi--modify-last-user-content (vec new-content)
  "Replacing last \='user :content with NEW-CONTENT in VEC.
NEW-CONTENT is either (string or function of old content).
Uses `oai-restapi--find-last-user-index`.
Return new vector based on VEC.
Used in `oai-restapi-request-prepare' to send history of conversation."
  (unless (vectorp vec)
    (error "Not a vector"))
  (let ((idx (oai-restapi--find-last-user-index vec))
        (newvec (copy-sequence vec)))
    (when idx
      (let* ((elt (aref newvec idx))
             (old-content (plist-get elt :content))
             (rep-content (if (functionp new-content)
                              ;; oai-restapi--modify-last-user-content
                              (funcall new-content old-content)
                            new-content))
             (new-elt (plist-put (copy-sequence elt) :content rep-content)))
        (aset newvec idx new-elt)))
    newvec))

;; -=-= Others

;; not used
;; (defun oai-restapi-switch-chat-model ()
;;   "Change `oai-restapi-con-model'."
;;   (interactive)
;;   (let ((model (completing-read "Model: "
;;                                 (append oai-restapi-openai-known-chat-models
;;                                         '("claude-3-opus-latest" "claude-3-5-sonnet-latest" "claude-3-7-sonnet-latest"
;;                                           "gemini-2.5-pro-preview-03-25" "gemini-2.5-flash-preview-04-17" "gemini-2.0-flash" "gemini-2.0-pro-exp"
;;                                           "deepseek-chat" "deepseek-reasoner"))
;;                                 nil t)))
;;     (setq oai-restapi-con-model model)))


(defun oai-restapi-get-buffers-for-element (&optional element)
  "Simplify getting url buffers associated with ai block ELEMENT.
Used in `oai-open-request-buffer'."
  (if-let ((element (or element (oai-block-p))))
      (oai-timers--get-keys-for-variable (oai-block-get-header-marker element))
    nil))

(provide 'oai-restapi)
;;; oai-restapi.el ends here
