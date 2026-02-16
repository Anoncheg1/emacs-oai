;;; oai.el --- AI-LLM chat blocks for org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025 github.com/Anoncheg1,codeberg.org/Anoncheg
;; Author: <github.com/Anoncheg1,codeberg.org/Anoncheg>
;; Keywords: org, comm, url, link
;; URL: https://github.com/Anoncheg1/emacs-oai
;; Version: 0.2
;; Created: 27 dec 2025
;; Package-Requires: ((emacs "29.1"))
;; Optional dependency: ((org-links "0.2"))
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; License

;; This file is NOT part of GNU Emacs.

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
;; Inspired by Robert Krahn's org-ai package <https://github.com/rksm/org-ai>
;;
;; OAI extend Org mode with "ai block" that allows you to interact
;; with the OpenAI-compatible REST APIs.
;;
;; It allows you to:
;; - Use #+begin_ai..#+end_ai blocks for org-mode
;; - Call multiple requests from multiple block and buffers in parallel.
;; - Use tags `@Backtrace` @Bt and Org links to insert target in query.
;; - Highlighting for major elements.
;; - Autofilling, hooks, powerful debugging
;; - Customization for engineering, there is :chain for sequence of
;;   calls out-of-the-box.
;;
;; For the Internet connection used built-in libs: url.el and url-http.el.
;;
;; See see https://github.com/Anoncheg1/emacs-oai for the full set
;; of features and setup instructions.
;;
;; Configuration:
;; (add-to-list 'load-path "path/to/oai") ; (optional)
;; (require 'oai)
;; (add-hook 'org-mode-hook #'oai-mode) ; oai.el
;; (setq oai-restapi-con-token "xxx") ; oai-restapi.el (optional)

;; Configuration, optional hooks:
;; (add-hook 'oai-block-after-chat-insertion-hook
;;   #'oai-optional-remove-distant-empty-lines-hook-function)
;; (add-hook 'oai-block-after-chat-insertion-hook
;;   #'oai-optional-remove-headers-hook-function)

;; One hooks add space “ “ before lines that in ai block that looks
;; like Org header, that fix confustion for Org logic.
;; Another hook remove empty lines if there is too much of them in response.

;; You will need an OpenAI API key-token.
;; It can be stored in the format:
;;  "machine api.openai.com login oai password <your-api-key>"
;; in your ~/.authinfo.gpg file (or other auth-source) and will be picked up
;; when the package is loaded.
;;
;; Keys binded by default:
;;
;; - In block #+begin_ai..#+end_ai blocks:
;;     - C-c C-c - to send the text to the OpenAI API and insert a response
;;     - C-c . - to inspect raw data (and C-u C-c .)
;;     - C-c C-.  - to see url.el raw HTTP data (working only during request)
;;     - M-h - mark element in ai block (C-u M-h - mark chat message)
;;     - C-c C-t - set :max-tokens
;; - in buffer with oai-mode enabled:
;;     - C-g - to stop all requsts.

;; Customization:
;; M-x customize-group RET oai
;; M-x customize-group RET oai-faces

;; Terms:
;; - chat roles or prefixes - [AI]: [ME:]
;; - parts or messages - major parts of chat with prefixes of roles


;; Other packages:
;; - Modern navigation in major modes https://github.com/Anoncheg1/firstly-search
;; - Search with Chinese	https://github.com/Anoncheg1/pinyin-isearch
;; - Ediff no 3-th window	https://github.com/Anoncheg1/ediffnw
;; - Dired history		https://github.com/Anoncheg1/dired-hist
;; - Selected window contrast	https://github.com/Anoncheg1/selected-window-contrast
;; - Copy link to clipboard	https://github.com/Anoncheg1/emacs-org-links
;; - Solution for "callback hell"	https://github.com/Anoncheg1/emacs-async1
;; - Restore buffer state	https://github.com/Anoncheg1/emacs-unmodified-buffer1
;; - outline.el usage		https://github.com/Anoncheg1/emacs-outline-it

;; Donate:
;; - BTC (Bitcoin) address: 1CcDWSQ2vgqv5LxZuWaHGW52B9fkT5io25
;; - USDT (Tether) address: TVoXfYMkVYLnQZV3mGZ6GvmumuBfGsZzsN
;; - TON (Telegram) address: UQC8rjJFCHQkfdp7KmCkTZCb5dGzLFYe2TzsiZpfsnyTFt9D

;;; TODO:
;; - make oai-variable.el and pass them to -api.el functions as parameters.
;; - provide ability to replace url-http with plz or oai-restapi with llm(plz)
;; - implement "#+PROPERTY: var foo=1" and "#+begin_ai :var
;;       foo=1" and to past to text in [foo]
;; - more tags? like: "Fix @problems then document the
;;         changes in @/CHANGELOG.md" @url, @file, @folder, @header? (Org)
;; - use oai-restapi-prepare-content for :chain
;; - Think about to pass callback for writing to chain implementations
;;    and main implementation, to make it more general.
;; - make org-block-tags optional or not
;; - key to enable full Org highlighting? think about it
;; - fontify latex [[file:/usr/share/emacs/30.2/lisp/org/org.el::16097::(defun org-inside-latex-macro-p ()]]
;; [[file:/usr/share/emacs/30.2/lisp/textmodes/tex-mode.el::1277::(setq-local font-lock-defaults]]
;; - small markdown mode on highlighting
;; - simple Elisp function to ask LLM
;; - add guide to use `oai-restapi-request' and with retries for simple ELisp LLM call and get result for TAB key and some place in buffer.
;; - add option for tag to expand only the last user prompt or in all.
;; - C-c C-k should jump to current begining of message, not next
;; - add buttons: 1) generate button based on LLM answer 2) handle clicking.
;; - default requst as one plist configuration
;; - support for https://github.com/LionyxML/markdown-ts-mode
;; - check big markdown-mode for insights for us.
;; - stop previous request if new one called with all equal parameters
;; - fill-paragraph should not break markdown quotes and bolds
;; - add new line before ai answer
;; - make font-lock better like in [[file:/usr/share/emacs/30.2/lisp/gnus/message.el
;; ::1701::(defun message-font-lock-make-cited-text-matcher (level maxlevel)]]
;; - make `oai-expand-block' executed with `org-babel-expand-src-block'.
;; - provide place or hook to add custom expansion of link to one line for user defined mode
;; - support vars as tags    https://orgmode.org/manual/Environment-of-a-Code-Block.html
;; - write test for `oai-block-tags-get-content' and `oai-block-tags--get-content-at-point-org' and noweb parameter usage
;; - add advanced forward section that check what type of region is
;; active and do appropriate forward with preserving region
;; - noweb evaluation with support of variables with some text. like <<call("as")>>
;; - rebind keys to C-x C-a
;; - function to replace "^[\s+]- **word1 [word2]:**" to "^^[\s+]- word1 [word2] :: " and highligh it.
;; - fix highlight to highlight when there is only "#+end_ai"
;; - create function that insert :max-token for given int
;;; Code:
;; Touch: Pain, water and warm.

;; -=-= includes
(require 'oai-debug)
(require 'oai-block)
(require 'oai-block-tags) ; `oai-block-tags-replace' for `oai-expand-block'
(require 'oai-restapi)
(require 'oai-prompt) ; for `oai-prompt-request-chain'

;; -=-= Customs and groups
(defgroup oai nil
  "OAI package customization."
  :group 'oai)

(defgroup oai-faces nil
  "Faces for OAI blocks."
  :tag "OAI Faces"
  :group 'oai)

(defcustom oai-fontification-flag t
  "Non-nil means enable fontification for markdown and Org elements in block."
  :type 'boolean
  :group 'oai)

(defcustom oai-req-type-functions (list :chat		#'oai-restapi-request-prepare ; at oai-restapi.el
                                        :completion	#'oai-restapi-request-prepare
                                        :chain		#'oai-prompt-request-chain) ; at oai-prompt.el
  "Custom variants to execute request.
If you specify :chain at #+begin_ai line, associated function will be
called.  See `oai-call-block' and `oai-restapi-request-prepare' for
parameters."
  :type '(plist :key-type symbol
                :value-type function
                :tag "Property list (symbol => funcion)")
  :group 'oai)

;; -=-= C-c C-c main interface

(defun oai-ctrl-c-ctrl-c ()
  "Remove result and parse ai block header parameters.
Returning t is Org requirement."
  (when (oai-block-p)
    (oai-block-remove-result)
    (oai-call-this-or-that #'oai-restapi-request-prepare
                           oai-req-type-functions
                           (oai-parse-org-header))
    t))

(defun oai-call-this-or-that (fn-default fn-list args)
  "If you specify :chain in ai block, we call related function.
FN-DEFAULT is `oai-restapi-request-prepare' FN-LIST is
`oai-req-type-functions' variable.
ARGS is `oai-parse-org-header' result."
  (let ((info (car (last args)))
        called)
    ;; loop over `oai-req-type-functions'
    (while (and fn-list (not called))
      (let ((key (pop fn-list))
            (fn (pop fn-list)))
        (when (and fn ; skip keys with missing value
                   (not (eq 'x (alist-get key info 'x)))) ; check key exist in info
          (setq called (apply fn
                              (cons (intern (substring (symbol-name key) 1)) ; key to symbol
                                    args))))))  ; (apply fn args)
    (unless called ; executed if key exist but evaluation return nil or key not exist
      (apply fn-default (cons 'chat args))))) ; call default function


(defun oai-parse-org-header ()
  "Parsing ai block header and parameters.
Return list of arguments args."
  (let* ((element (oai-block-p)) ; oai-block.el
         (info (oai-block-get-info element)) ; ((:max-tokens . 150) (:service . "together") (:model . "xxx")) ; oai-block.el
         ;; (req-type (oai-block--get-request-type info)) ; oai-block.el
         (sys-prompt-for-all-messages (or (not (eql 'x (alist-get :sys-everywhere info 'x)))
                                          (org-entry-get-with-inheritance "SYS-EVERYWHERE") ; org
                                          oai-restapi-default-inject-sys-prompt-for-all-messages)) ; oai-restapi.el
         (sys-prompt (or (org-entry-get-with-inheritance "SYS") ; org
                         (oai-block--get-sys :info info ; oai-block.el
                                             :default oai-restapi-default-chat-system-prompt)))
         (noweb-control (or (org-babel-noweb-p info :eval)
                            (org-entry-get (point) "oai-noweb" t)))) ; oai-restapi.el variable
    ;; - Process Org params and call agent
    (oai-block--let-params info
                           ;; format: (variable optional-default type)
                           ((service oai-restapi-con-service string) ; oai-restapi.el
                            (model (let ((m (oai-restapi--get-values oai-restapi-con-model service)))
                                       (if (not m)
                                         (user-error "Model not specified"))
                                       ;; else
                                       (car m))
                                   :type string) ; oai-restapi.el
                            (max-tokens oai-restapi-default-max-tokens :type number)
                            (top-p nil :type number)
                            (temperature nil :type number)
                            (frequency-penalty nil :type number)
                            (presence-penalty nil :type number)
                            (stream t :type bool))
                           ;; - body with some Org "Post-parsing":
                           (let (
                                 (service (or service
                                              oai-restapi-con-service)) ; default in oai-restapi.el
                                 (model (if (and (stringp model)
                                                 (string-equal model "nil"))
                                            nil
                                          ;; else
                                          model)))
                                 ;; return
                                 (list element noweb-control sys-prompt sys-prompt-for-all-messages ; message
                                          model max-tokens top-p temperature frequency-penalty presence-penalty service stream ; model params
                                          info)))))

;; -=-= interactive fn: key M-x: oai-expand-block
(defun oai-expand-block-deep ()
  "Output almost RAW information about request with headers and messages.
Return list of strings to print."
  (seq-let (element noweb-control sys-prompt sys-prompt-for-all-messages model max-tokens top-p temperature frequency-penalty presence-penalty service stream info) (oai-parse-org-header)
    (let* ((req-type-completion (not (eq 'x (alist-get :completion info 'x))))
           ;; (req-type
           (messages (unless req-type-completion
                       ;; - split content to messages
                       (oai-restapi-prepare-content element noweb-control 'chat sys-prompt sys-prompt-for-all-messages max-tokens)
                       ;; (oai-restapi--collect-chat-messages-at-point element
                       ;;                                              sys-prompt
                       ;;                                              sys-prompt-for-all-messages
                       ;;                                              (if oai-restapi-add-max-tokens-recommendation
                       ;;                                                  (oai-restapi--get-lenght-recommendation max-tokens)))
                       )))
      (list
       (oai-restapi--get-endpoint messages service)
       (oai-restapi--get-headers service)
       (oai-restapi--payload :prompt (when req-type-completion (oai-block-get-content element)) ; block content string
			     :messages messages
			     :model model
			     :max-tokens max-tokens
			     :temperature temperature
			     :top-p top-p
			     :frequency-penalty frequency-penalty
			     :presence-penalty presence-penalty
			     :service service
			     :stream stream)))))

(defun oai-expand-block (arg)
  "Show a temp buffer with what the ai block expands to.
If there is ai block at current position in current buffer.
This is what will be sent to the api.  ELEMENT is the ai block.
Like `org-babel-expand-src-block'.
Set `help-window-select' variable to to t to get focus.
When universal  ARG specifide  output more  raw information  splitted by
messages.
Return expanded content if at current point of current buffer supported
block was found, otherwise nil."
  ; org-babel-expand-src-block put overlay with `org-src--make-source-overlay'
  ; We add text properties in `oai-block-tags--replace-last-regex-smart'
  (interactive "P")
  (when-let* ((element (oai-block-p)) ; (oai-block-tags--block-at-point))) ; oai-block.el
              (res-str (if arg
                           (pp-to-string (oai-expand-block-deep))
                         ;; else
                         ;; (let* (
                         ;;        ;; get block

                         ;;        (expanded (string-trim
                         ;;                   (oai-block-get-content element))) ; oai-block.el
                         ;;        ;; (expanded (oai-block-tags-replace expanded))
                         ;;        ;; vector
                         ;;        (expanded (oai-block--collect-chat-messages-from-string expanded))
                         ;;        (expanded (oai-restapi--modify-vector-content expanded #'oai-block-tags-replace 'user))
                         ;;        ;; one string
                         ;;        (expanded (oai-block--stringify-chat-messages expanded)))
                         ;;   expanded))))
                         (oai-block-tags-get-content element
                                                     t ; tags-control
                                                     t  ; noweb-control
                                                     :eval ; noweb-context
                                                      t ; links-only-last
                                                      t ; not-clear-properties
                                                     ))))
    (if (called-interactively-p 'any)
        (let ((buf (get-buffer-create "*OAI Preview*")))
          (with-help-window buf (with-current-buffer buf
                                  (insert res-str)))
          (switch-to-buffer buf)
          t)
      ;; else
      res-str)))

;; -=-= interactive fn: key C-g: keyboard quit
(defun oai-keyboard-quit ()
  "Keyboard quit advice.
- If there is an active region at current position in current buffer, do
  nothing (normal \\<mapvar> & \\[keyboard-quit] will deactivate it).
- in debug-buffer - kill all requests."
  (interactive)
  ;; Checks:
  ;; - 1) no region mode?
  (when (not (region-active-p))
    ;; - 2) oai debug buffer?
    (if (string-equal (buffer-name (current-buffer)) oai-debug-buffer) ; in debug-buffer - kill all
        (oai-restapi-stop-all-url-requests)
      ;; - else: 3) oai-mode in current buffer or
      (when (and (bound-and-true-p oai-mode)
                     (not (minibufferp (window-buffer (selected-window))))) ; not in minubuffer
        ;; - stop current request
        (if (bound-and-true-p oai-debug-buffer)
            ;; - show all errors in debug mode
            (call-interactively #'oai-restapi-stop-url-request) ; oai-restapi.el
          ;; else - suppress error in normal mode
          (condition-case _
              (call-interactively #'oai-restapi-stop-url-request) ; oai-restapi.el
            (error nil)))))))

;; -=-= interactive fn: M-x oai-toggle-debug
(defun oai-toggle-debug ()
  "Enable/disable debug."
  (interactive)
  (if oai-debug-buffer
      (progn
        (setq oai-debug-buffer nil)
        (message "Disable oai debugging"))
    ;; else
    (setq oai-debug-buffer   "*debug-oai*")
    (message "Enable oai debugging")))

;; -=-= fn: Help function to rebind major mode with chaining
(defun oai--call-next-remap-protected (command &optional seen)
  "Call the next remapping of COMMAND, skipping any commands already in SEEN.
If no further remappings found, calls COMMAND interactively if possible."
  (let ((minor-mode-map-alist (cdr minor-mode-map-alist)))
    (let ((binding (key-binding (vector 'remap command))))
      (cond
       ;; No binding found, or recursion, fallback to original
       ((or (null binding) (memq binding seen))
        (when (commandp command)
          (call-interactively command)))
       ;; Valid binding, try further
       ((commandp binding)
        (oai--call-next-remap-protected command (cons binding seen)))))))

(defun oai--call-next-key-remap-protected (key &optional seen)
  "Call the next binding of KEY, skipping handlers already in SEEN.
If no further binding found, calls the major mode's or global binding.
KEY is a string representing the keystroke.
SEEN is a list of commands already called, used to prevent recursion."

  ;; Locally shadow minor-mode-map-alist to remove the highest-priority minor mode map.
  (let ((minor-mode-map-alist (cdr minor-mode-map-alist)))
    ;; Find the current binding for KEY after skipping the top minor mode.
    (let ((binding (key-binding (kbd key) nil nil)))
      (cond
       ;; If no binding found or we've already seen this binding, try major mode and then global map.
       ((or (null binding) (memq binding seen))
        ;; Attempt to find the binding in the major mode's keymap.
        (let* ((major-mode-map (current-local-map))
               (binding-major (and major-mode-map (lookup-key major-mode-map (kbd key)))))
          (if (commandp binding-major)
              ;; If found and it's a command, call interactively.
              (call-interactively binding-major)
            ;; Otherwise, try the global map for the key.
            (let ((global-binding (key-binding (kbd key) t t)))
              (if (commandp global-binding)
                  (call-interactively global-binding)
                ;; If no valid binding anywhere, notify the user.
                (message "No valid binding for %s" key))))))
       ;; If binding is a command, recursively try to find the next remapped binding,
       ;; and add this binding to SEEN for recursion protection.
       ((commandp binding)
        (oai--call-next-key-remap-protected key (cons binding seen)))
       ;; Handle the case where binding is not a command (function, lambda, etc.).
       (t
        (message "Binding for %s is not a command" key))))))

;; -=-= interactive fns: Org keys remapings
(defun oai-mark-at-point-org (&optional arg)
  "Mark element of ai block, large we every next execution.
If optional argument ARG is non-nil, mark current message of chat."
  (interactive "P")
  (if (oai-block-p)
      (if arg
          (progn (deactivate-mark)
                 (oai-block-mark-chat-message-at-point)
                 (message "Chat message"))
        ;; else
        (call-interactively #'oai-block-mark-at-point))
    ;; else
    (oai--call-next-remap-protected #'org-mark-element)))

(defun oai-expand-block-org ()
  "Show a temp buffer with what the ai block expands to."
  (interactive)
  (if (not (call-interactively #'oai-expand-block))
    ;; else
    (oai--call-next-key-remap-protected "C-c .")))

(defun oai-set-max-tokens-org ()
  "Jump to header of ai block and set max-tokens."
  (interactive)
  (if (oai-block-p)
      (oai-block-set-block-parameter ":max-tokens" oai-restapi-default-max-tokens)
    ;; else
    (oai--call-next-key-remap-protected "C-c C-t")))

;; -=-= interactive fn: Summarize *TODO:*
;; (defun oai-summarize ()
;;   "Jump to header of ai block and set max-tokens."
;;   (interactive)
;;   (let ((element (oai-block-p)))
;;       (when element
;;         ;; Determine the boundaries of the content
;;         (let ( ;; ai block range
;;               (beg (org-element-property :contents-begin element)) ; first line of content
;;               (end (org-element-property :contents-end element))) ; #+end_ai
;;           ;; Content exist?
;;           (when (or (not beg)
;;                     (not end))
;;             (error "Empty block"))
;;           ;; format
;;           (while


;; -=-= Minor mode: keymap
;;;###autoload
(defvar-keymap oai-mode-map
  :repeat nil
  :parent nil
  "<remap> <org-next-visible-heading>" #'oai-block-next-message ; C-c C-n todo make org
  "<remap> <org-previous-visible-heading>" #'oai-block-previous-message ; C-c C-p todo make org
  "<remap> <org-mark-element>" #'oai-mark-at-point-org ; M-h
  "C-c ." #'oai-expand-block-org
  "C-c C-." #'oai-open-request-buffer
  "C-c C-t" #'oai-set-max-tokens-org)

;; -=-= Minor mode: hook - Fontify Markdown blocks and Tags - function for hook

(defun oai-block--set-ai-keywords ()
  "Hook, that Insert our fontify functions in Org font lock keywords."
  ;; add fontify-ai-subblocks - markdown blocks and tables.
  ;; Put in order to `org-font-lock-keywords': (oai-block--font-lock-fontify-markdown-and-org) (oai-block-tags--font-lock-fontify-links) (oai-block--font-lock-fontify-markdown-blocks)
  (when oai-fontification-flag
    ;; 3) fontify markdown blocks (and clear small)
    (setq org-font-lock-extra-keywords (oai-block--insert-after
                                        org-font-lock-extra-keywords
                                        (seq-position org-font-lock-extra-keywords '(org-fontify-meta-lines-and-blocks))
                                        '(oai-block--font-lock-fontify-markdown-blocks)))
    ;; 2) fontify-links (and clear small)
    (setq org-font-lock-extra-keywords (oai-block--insert-after
                                        org-font-lock-extra-keywords
                                        (seq-position org-font-lock-extra-keywords '(org-fontify-meta-lines-and-blocks))
                                        '(oai-block-tags--font-lock-fontify-links)))
    ;; 1) fontify small elements
    (setq org-font-lock-extra-keywords (oai-block--insert-after
                                        org-font-lock-extra-keywords
                                        (seq-position org-font-lock-extra-keywords '(org-fontify-meta-lines-and-blocks))
                                        '(oai-block--font-lock-fontify-markdown-and-org)))))

;; -=-= Minor mode

;;;###autoload
(define-minor-mode oai-mode
  "Minor mode for `org-mode' integration with the OpenAI API."
  :init-value nil
  :lighter oai-mode-line-string ; " oai" string
  :keymap oai-mode-map
  :group 'oai
  (if oai-mode
      (progn
        (add-hook 'org-ctrl-c-ctrl-c-hook #'oai-ctrl-c-ctrl-c nil 'local)
        (advice-add 'keyboard-quit :before #'oai-keyboard-quit)
        (when oai-fontification-flag
          (add-hook 'org-font-lock-set-keywords-hook #'oai-block--set-ai-keywords nil 'local)
          (org-set-font-lock-defaults)
          (font-lock-refresh-defaults)))
    ;; else - off
    (remove-hook 'org-ctrl-c-ctrl-c-hook #'oai-ctrl-c-ctrl-c 'local)
    (advice-remove 'keyboard-quit #'oai-keyboard-quit)
    ;; font lock refrash
    (remove-hook 'org-font-lock-set-keywords-hook #'oai-block--set-ai-keywords)
    (org-set-font-lock-defaults)
    (font-lock-refresh-defaults)))


(defun oai-open-request-buffer ()
  "Opens the url request buffer for ai block at current position."
  (interactive)
  (if-let ((element (oai-block-p)))
      (if-let* ((url-buffer (car (oai-restapi-get-buffers-for-element element)))
                (display-buffer-base-action
                 (list '(
                         ;; display-buffer--maybe-same-window  ;FIXME: why isn't this redundant?
                         display-buffer-reuse-window ; pop up bottom window
                         display-buffer-in-previous-window ;; IF RIGHT WINDOW EXIST
                         display-buffer-in-side-window ;; right side window - MAINLY USED
                         display-buffer--maybe-pop-up-frame-or-window ;; create window
                         ;; ;; If all else fails, pop up a new frame.
                         display-buffer-pop-up-frame )
                       '(window-width . 0.6) ; 80 percent
                       '(side . right))))
          (progn
            (pop-to-buffer url-buffer)
            (with-current-buffer url-buffer
              (local-set-key (kbd "C-c ?") 'delete-window)))
        ;; else
        (message "No url buffer found"))
  ;; - else - no element - call original Org key
  (oai--call-next-key-remap-protected "C-c C-.")))

;; -=-= Minor mode - string line
(defvar oai-mode-line-string "")

(defun oai-update-mode-line (count)
  "Used in ora-timers.el to show COUNT of active requests."
  (oai--debug "oai-update-mode-line %s" count)
  (if (and count (> count 0))
      (setq oai-mode-line-string (format " oai[%d]" count))
    ;; else
    (setq oai-mode-line-string " oai"))
  (force-mode-line-update))

;; -=-= provide
(provide 'oai)
;;; oai.el ends here
