;;; oai.el --- AI-LLM blocks for org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025 github.com/Anoncheg1,codeberg.org/Anoncheg
;; Author: <github.com/Anoncheg1,codeberg.org/Anoncheg>
;; Keywords: org, comm, url, link
;; URL: https://github.com/Anoncheg1/emacs-oai
;; Version: 0.1
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
;; with the OpenAI-compatible REST APIs.  Fork of "org-ai".
;;
;; It allows you to:
;; - Use #+begin_ai..#+end_ai blocks for org-mode
;; - Chat with a language model from within an org mode buffer.
;; - Call multiple requests from multiple block and buffers.
;; - See how many requests in parallel now.
;; - Use tags `@Backtrace` @Bt and Org links to insert target in query.
;;
;; For the Internet connection used built-in libs: url.el and url-http.el.
;;
;; See see https://github.com/Anoncheg1/oai for the full set
;; of features and setup instructions.
;;
;; Configuration:
;; (add-to-list 'load-path "path/to/oai")
;; (require 'oai)
;; (add-hook 'org-mode-hook #'oai-mode) ; oai.el
;; (setq oai-restapi-con-token "xxx") ; oai-restapi.el
;; (setopt help-window-select t) ; for `oai-expand-block' function (optional)
;;
;; You will need an OpenAI API key.  It can be stored in the format
;;   machine api.openai.com login oai password <your-api-key>
;; in your ~/.authinfo.gpg file (or other auth-source) and will be picked up
;; when the package is loaded.
;;
;; Available commands (TODO to refine):
;;
;; - Inside org-mode / #+begin_ai..#+end_ai blocks:
;;     - C-c C-c to send the text to the OpenAI API and insert a response (org-ai.el)
;;     - Press C-c <backspace> (oai-kill-region-at-point) to remove the chat
;;       part under point.  (oai-block.el)
;;     - oai-mark-region-at-point will mark the region at point.  (oai-block.el)
;;     - oai-mark-last-region will mark the last chat part.  (oai-block.el)

;; Architecture:
;;   (raw info) Interface -> (structured Org info + raw Org body) Agent ->
;;   API to LLM + Callback (may be part of Agent or API)

;; Callback write result to ORG

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

;; Touch: Pain, water and warm.

;;; TODO:

;; - make org-ai-variable.el and pass them to -api.el functions as parameters.
;; - provide ability to replace url-http with plz or oai-restapi with llm(plz)
;; - implement "#+PROPERTY: var foo=1" and "#+begin_ai :var
;;       foo=1" and to past to text in [foo]
;; - implement contant-tags "Fix @problems then document the
;;         changes in @/CHANGELOG.md" @url, @file, @folder, @header? (Org)
;; - Force stop stream on C-g
;; - use oai-restapi-prepare-content for :chain
;; - Think about to pass callback for writing to chain implementations
;;    and main implementation, to make it more general.

;;; Code:

;; -=-= includes
(require 'oai-debug)
(require 'oai-block-tags) ; `oai-block-tags-replace' for `oai-expand-block'
(require 'oai-block)
(require 'oai-restapi)
(require 'oai-prompt) ; for `oai-prompt-request-chain'

;; -=-= C-c C-c main interface
(defcustom oai-fontification-flag t
  "Non-nil means enable fontification."
  :type 'boolean
  :group 'oai)

(defcustom oai-agent-call-functions (list :forexample #'oai-prompt-request-chain
                                          :chain #'oai-prompt-request-chain) ; at oai-prompt.el
  "Custom variants to execute request.
If you specify :chain at #+begin_ai line, associated function will be
called.  See `oai-call-block' and `oai-restapi-request-prepare' for
parameters."
  :type '(choice
          (plist :tag "Property list (symbol => funcion)"
                 :key-type symbol
                 :value-type function)
          (const :tag "Use auth-source." nil))
  :group 'oai)


(defun oai-ctrl-c-ctrl-c ()
  "Remove result and parse ai block header parameters.
Returning t is Org requirement."
  (when (oai-block-p)
    (oai-block-remove-result)
    (oai-call-this-or-that #'oai-restapi-request-prepare
                           oai-agent-call-functions
                           (oai-parse-org-header))
    t))

(defun oai-call-this-or-that (fn-default fn-list args)
  "If you specify :chain in ai block, we call related function.
FN-DEFAULT is `oai-restapi-request-prepare' FN-LIST is
`oai-agent-call-functions' variable."
  (let ((info (car (last args)))
        called)
    ;; loop over `oai-agent-call-functions'
    (while (and fn-list (not called))
      (let ((key (pop fn-list))
            (fn (pop fn-list)))
        (when (and fn ; skip keys with missing value
                   (not (eq 'x (alist-get key info 'x)))) ; check key exist in info
          (setq called (apply fn args))))) ; call agant function
    (unless called ; executed if key exist but evaluation return nil or key not exist
      (apply fn-default args)))) ; call default function


(defun oai-parse-org-header ()
  "Parsing ai block header and parameters.
Return list of arguments args."
  (let* ((element (oai-block-p)) ; oai-block.el
         (info (oai-block-get-info element)) ; ((:max-tokens . 150) (:service . "together") (:model . "xxx")) ; oai-block.el
         (req-type (oai-block--get-request-type info)) ; oai-block.el
         (sys-prompt-for-all-messages (or (not (eql 'x (alist-get :sys-everywhere info 'x)))
                                          (org-entry-get-with-inheritance "SYS-EVERYWHERE") ; org
                                          oai-restapi-default-inject-sys-prompt-for-all-messages)) ; oai-restapi.el
         (sys-prompt (or (org-entry-get-with-inheritance "SYS") ; org
                         (oai-block--get-sys :info info ; oai-block.el
                                             :default oai-restapi-default-chat-system-prompt)))) ; oai-restapi.el variable
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
                                 (list element req-type sys-prompt sys-prompt-for-all-messages ; message
                                          model max-tokens top-p temperature frequency-penalty presence-penalty service stream ; model params
                                          info)))))

;; -=-= interactive fn: key M-x: oai-expand-block
(defun oai-expand-block-deep ()
  "Output almost RAW information about request with headers and messages.
Return list of strings to print."
  (seq-let (element req-type sys-prompt sys-prompt-for-all-messages model max-tokens top-p temperature frequency-penalty presence-penalty service stream) (oai-parse-org-header)
    (let* ((messages (unless (eql req-type 'completion)
                       ;; - split content to messages
                       (oai-restapi--collect-chat-messages-at-point element
                                                                    sys-prompt
                                                                    sys-prompt-for-all-messages
                                                                    (if oai-restapi-add-max-tokens-recommendation
                                                                        (oai-restapi--get-lenght-recommendation max-tokens))))))
      (list
       (oai-restapi--get-endpoint messages service)
       (oai-restapi--get-headers service)
       (oai-restapi--payload :prompt (when (eql req-type 'completion) (oai-block-get-content element)) ; block content string
			     :messages messages
			     :model model
			     :max-tokens max-tokens
			     :temperature temperature
			     :top-p top-p
			     :frequency-penalty frequency-penalty
			     :presence-penalty presence-penalty
			     :service service
			     :stream stream)))))

;;;###autoload
(defun oai-expand-block (arg)
  "Show a temp buffer with what the ai block expands to.
If there is ai block at current position in current buffer.
This is what will be sent to the api.  ELEMENT is the ai block.
Like `org-babel-expand-src-block'.
Set `help-window-select' variable to get focus.
When universal  ARG specifide  output more  raw information  splitted by
messages."
  (interactive "P")
  (when-let* ((element (oai-block-p)) ; oai-block.el
              (res-str (if arg
                           (pp-to-string (oai-expand-block-deep))
                         ;; else
                         (let* (
                                ;; get block
                                (expanded (string-trim
                                           (oai-block-get-content element))) ; oai-block.el
                                (expanded (oai-block-tags-replace expanded))
                                ;; vector
                                (expanded (oai-block--collect-chat-messages-from-string expanded))
                                ;; one string
                                (expanded (oai-block--stringify-chat-messages expanded)))
                           expanded))))
    (if (called-interactively-p 'any)
        (let ((buf (get-buffer-create "*OAi Preview*")))
          (with-help-window buf (with-current-buffer buf
                                  (insert res-str)))
          (switch-to-buffer buf))
      ;; else
      res-str)))

;; -=-= interactive fn: key C-g: keyboard quit

;; (defvar org-ai-talk--reading-process)
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
              ;; ((and (boundp 'org-ai-talk--reading-process) ; org-ai-talk.el
              ;;       (fboundp 'org-ai-talk-stop) ; org-ai-talk.el
              ;;       org-ai-talk--reading-process ; org-ai-talk.el
              ;;       (process-live-p org-ai-talk--reading-process)) ; org-ai-talk
              ;;  (org-ai-talk-stop)) ; org-ai-talk
              ;; (org-ai-oobabooga--current-request ; org-ai-oobabooga
              ;;  (org-ai-oobabooga-stop)) ; org-ai-oobabooga
              ;; (org-ai--current-request-buffer-for-stream ; oai-restapi.el
              ;;  (org-ai-interrupt-current-request)) ; oai-restapi.el

              (call-interactively #'oai-restapi-stop-url-request) ; oai-restapi.el
            ;; (org-ai--current-request-buffer-for-image ; org-ai-openai-image.el
            ;;  (org-ai-image-interrupt-current-request)) ; org-ai-openai-image.el
            (error nil)))))))

;; (defun org-ai--install-keyboard-quit-advice () ; TODO: make Org only
;;   "Cancel current request when `keyboard-quit' is called."
;;   (advice-add 'keyboard-quit :before #'oai-keyboard-quit))

;; (defun org-ai--uninstall-keyboard-quit-advice ()
;;   "Remove the advice that cancels current request when `keyboard-quit' is called."
;;   (advice-remove 'keyboard-quit #'oai-keyboard-quit)) ; here

;; -=-= interactive fn: M-x oai-toggle-debug
;;;###autoload
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

;; -=-= Fontify Markdown blocks and Tags - function for hook

(defun oai-block--set-ai-keywords ()
  "Hook, that Insert our fontify functions in Org font lock keywords."
  ;; add fontify-ai-subblocks - markdown blocks and tables.
  (when oai-fontification-flag
    (setq org-font-lock-extra-keywords (oai-block--insert-after
                                        org-font-lock-extra-keywords
                                        (seq-position org-font-lock-extra-keywords '(org-fontify-meta-lines-and-blocks))
                                        '(oai-block--font-lock-fontify-ai-subblocks)))
    ;; add fontify-links
    (setq org-font-lock-extra-keywords (oai-block--insert-after
                                        org-font-lock-extra-keywords
                                        (seq-position org-font-lock-extra-keywords '(org-fontify-meta-lines-and-blocks))
                                        '(oai-block-tags--font-lock-fontify-links)))))

;; -=-= interactive fn: key M-h: Mark at point
(defun oai-mark-at-point (arg)
  "Mark entity at current poin in current buffer.
Mark Mardkown block or whole ai block.  If universal argument ARG is
non-nil, then mark one chat message."
  (interactive "P")
  (if arg
      (oai-mark-region-at-point)
    ;; else
    (oai-block-tags-mark-md-block-body)))

;; -=-= interactive fn: Set max tokens
(defun oai-set-max-tokens ()
  "Jump to header of ai block and set max-tokens."
  (interactive)
  (if-let ((el (oai-block-p)))

      (let ((beg (progn (push-mark)
                        (goto-char (org-element-property :contents-begin el))
                        (forward-line -1)
                        (point)))
            pos)
        (if (search-forward ":max-tokens" (line-end-position) t)
            (if (eq (line-end-position) (point))
                (insert " ")
                ;; else
              (forward-char))
          ;; else
          (goto-char beg)
          (forward-char 10)
          (insert " :max-tokens ")
          (setq pos (point))
          (insert (format "%s " oai-restapi-default-max-tokens))
          (goto-char pos)))
    ;; else
    (message "Not oai block here.")))

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


;; -=-= Minor mode
(defvar oai-mode-map (make-sparse-keymap)
  "Keymap for `oai-mode'.")

(let ((map oai-mode-map))
  ;; (define-key map (kbd "C-c M-a v") 'org-ai-image-variation) ; org-ai-openai-image.el
  ;; (define-key map (kbd "C-c M-a $") 'org-ai-open-account-usage-page) ; org-ai-openai-image.el
  (define-key map (kbd (string-join (list "C-c" " h"))) #'oai-mark-region-at-point) ; oai-block.el
  ;; (define-key map (kbd "C-c DEL") 'org-ai-kill-region-at-point) ; oai-block.el
  (define-key map (kbd (string-join (list "C-c" " <backspace>"))) #'oai-kill-region-at-point) ; oai-block.el
  ;; (define-key map (kbd (string-join (list "C-c" " r"))) 'org-ai-talk-capture-in-org) ; org-ai-talk.el
  (define-key map (kbd "M-h") #'oai-mark-at-point) ; oai-block.el
  (define-key map (kbd "C-c C-.") #'oai-open-request-buffer) ; oai-restapi.el
  (define-key map (kbd "C-c .") #'oai-expand-block)
  (define-key map (kbd (concat "C-c " "m")) #'oai-set-max-tokens))



(define-minor-mode oai-mode
  "Minor mode for `org-mode' integration with the OpenAI API."
  :init-value nil
  :lighter oai-mode-line-string ; " org-ai"
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

;;;###autoload
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
  (call-interactively (lookup-key org-mode-map (kbd "C-c ?")))))

;; -=-= Minor mode - string line
(defvar oai-mode-line-string "")

(defun oai-update-mode-line (count)
  "Used in ora-timers.el to show COUNT of active requests."
  (oai--debug "oai-update-mode-line %s" count)
  (if (and count (> count 0))
      (setq oai-mode-line-string (format " org-ai[%d]" count))
    ;; else
    (setq oai-mode-line-string " org-ai"))
  (force-mode-line-update)
  ;; (propertize (format " org-ai[%d]" count)
  ;;                   'face (if (> count 0) 'error 'default))
  )

;; -=-= provide
(provide 'oai)
;;; oai.el ends here
