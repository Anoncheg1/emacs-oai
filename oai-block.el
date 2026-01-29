;;; oai-block.el --- oai special block helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2025 github.com/Anoncheg1,codeberg.org/Anoncheg
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Author: <github.com/Anoncheg1,codeberg.org/Anoncheg>

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

;;; Commentary:

;; Defines functions for dealing with #+begin_ai..#+end_ai special blocks

;; None Org babel: We choose not to fake as babel source block and use
;; functionality because it require too much advices.

;; Note Org terms:
;; - element - "room" you are in (e.g., a paragraph) (TYPE PROPS) (org-element-at-point)

;; - context - "furniture" you are touching within that room (e.g., a
;;   bold word, a link). (TYPE PROPS) (org-element-context)
;; - org-dblock-start-re
;;
;; Content begin-end
;; (org-element-property :contents-begin  (oai-block-p)) return next line after #+begin
;; (org-element-property :contents-end  (oai-block-p)) return #+end line position
;; may be fixed with org-element-put-property, but it is not KISS.

;;; TODO:
;; - replace all cl-lib with built-in Elisp code
;; - simplify some functions
;; - replace Vibe-coding with human-readable code with comments where required, without lambdas.

;;; Code:
;; -=-= includes
(require 'org)
(require 'org-element)
(require 'org-macs)
(require 'cl-lib) ; for `cl-letf', cl-defun, cl-loop, cl-case
(require 'oai-debug)

;; -=-= customizable variables
(defcustom oai-block-fontify-markdown-flag t
  "Non-nil means enable fontinfication for ```lang blocks."
  :type 'boolean
  :group 'oai)

(defcustom oai-block-fontify-org-tables-flag nil
  "Non-nil means enable fontinfication for Org tables."
  :type 'boolean
  :group 'oai)

(defcustom oai-block-fontify-markdown-headers-and-formatting t
  "Non-nil means enable fontinfication for Org tables."
  :type 'boolean
  :group 'oai)

(defcustom oai-block-fontify-latex t
  "Non-nil means enable fontinfication for not quoted LaTex."
  :type 'boolean
  :group 'oai)

(defcustom oai-block-roles-prefixes '(("SYS" . system)
                                   ("+me" . user) ; for output
                                   ("ME" . user) ; to understand user input
                                   ("ai+" . assistant) ; for output
                                   ("AI" . assistant) ; to understand user input
                                   ("AI_REASON" . assistant_reason)) ; "AI_REASON" used in `oai-block--parse-part'
  "Map oai roles to chat prefixes to output to user.
When restapi -> prefix, first matched is used.
Used in `oai-block--parse-part' with ignoring case..
Closely bound with `oai-block--chat-prefixes-re' variable."
  :type '(repeat (cons (string :tag "Role Name")
                       (symbol :tag "Role Symbol")))
  :group 'oai)

;; (let ((role "Me"))
;;   (cdr (assoc-string role oai-block-roles-prefixes t))) ;; => 'user

(defcustom oai-block-roles-restapi
  '(("system" . system)
    ("user" . user)
    ("assistant" . assistant)
    ("assistant_reason" . assistant_reason))
  "Map RestAPI JSON reply roles to oai roles.
Used by `oai-block--insert-stream-response' in sensitive to case way."
  :type '(repeat (cons (string :tag "Role Name")
                       (symbol :tag "Role Symbol")))
  :group 'oai)

;; (cdr (assoc-string "assistant" oai-block-roles-restapi)) ; => assistant
;; (car (rassoc ' oai-block-roles-restapi)) ; => "+me"


(defcustom oai-block-parse-part-hook nil
  "Call hook function with raw string of current block after role prefix.
Implemented as a list of functions that called with two argument content
string after prefix and role prefix as a symbol from from
`oai-block-roles-prefixes'.  Executed from left to right and pass result
content string to each other.  Executed at step of reading ai block from
raw content of buffer before any processing but after splitting to
parts."
  :type 'hook
  :group 'oai)

(defcustom oai-block-jump-to-end-of-block t
  "If non-nil, jump to the end of the block after inserting response."
  :type 'boolean
  :group 'oai)

(defcustom oai-block-fill-function #'oai-block--fill-region
  "If non-nil this function will be called after insertion of text.
Current buffer is buffer with ai block with position of pointer right
after insertion of text.
Accept parameters: POS before insertion and and STREAM boolean flag.
Should check that position is not inside markdown block
and string is not quoted with \"> \".  Should be executed in
save-excursion to preserve relative point position.
TODO: for streaming: save and pass begining of paragraph or line."
  :type '(choice (const :tag "None" nil)
                 (function :tag "Function"))
  :group 'oai)

;; -=-= faces
(defface oai-block-quote
    '((((class color) (min-colors 88) (background dark)) :background "#282828" :foreground "shadow")
      (((class color) (min-colors 88) (background light)) :background "#eeeeee" :foreground "gray")
      (((class color) (min-colors 8)) (:background "cyan" :foreground "black"))
      (t :background "gray" :extend t))
  "Face for single markdown quoted text."
  :group 'oai-faces)

(defface oai-block-m-header1
  '((((background dark)) :foreground "chartreuse" :weight bold)
    (((background light)) :foreground "green" :weight light))
  "Face for single markdown header single # character."
  :group 'oai-faces)

(defface oai-block-m-header2
  '((((background dark)) :foreground "gold2" :weight light)
    (((background light)) :foreground "gold2" :weight bold))
  "Face for single markdown header two # characters."
  :group 'oai-faces)

(defface oai-block-m-header3
  '((((background dark)) :foreground "orange" :weight light)
    (((background light)) :foreground "orange" :weight bold))
  "Face for single markdown header three and more # characters."
  :group 'oai-faces)

(defface oai-block-m-header4
  '((((background dark)) :foreground "orange3" :weight light)
    (((background light)) :foreground "orange3" :weight bold))
  "Face for single markdown header three and more # characters."
  :group 'oai-faces)

(defface oai-chat-role	   ;Copied from `font-lock-variable-name-face'
  '((((class color) (min-colors 16) (background light)) (:foreground "gray" :slant italic))
    (((class color) (min-colors 16) (background dark)) (:foreground "DarkGoldenrod" :slant italic))
    (((class color) (min-colors 8)) (:foreground "yellow" :weight light :slant italic))
    (t :inverse-video t))
  "Face used for [AI]: [ME]:."
  :group 'oai-faces)

(defface oai-bold '((t :inherit default))
  "Face used for *,** and *** Org and markdown text formatting."
  :group 'oai-faces)

;; -=-= variables
(defvar oai-block-roles-restapi-unknown 'assistant
  "Used for restapi reply if role in JSON was not found.
In `oai-block--insert-stream-response'.")

(defvar oai-block-roles-prefixes-unknown 'assistant
  "Used in `oai-block--parse-part' for prefix not found.
In `oai-block-roles-prefixes'.")

;; ;; RestAPI -> Prefix
;; (let ((role 'user1))
;;   (or (car (rassoc role oai-block-roles-prefixes))
;;       (car (rassoc oai-block-roles-restapi-unknown oai-block-roles-prefixes)))) ; => "ai+"

;; ;; Prefix -> system
;; (let ((role "+me1"))
;;   (or (cdr (assoc-string role oai-block-roles-prefixes)) ; Get value by key
;;       oai-block-roles-prefixes-unknown)) ; => assistant

(defconst oai-block--ai-block-begin-re "^#\\+begin_ai.*$")
(defconst oai-block--ai-block-end-re "^#\\+end_ai.*$")

(defvar oai-block--markdown-begin-re "^\\s-*```\\([^\s\t\n[{]+\\)[\s\t]?$")
(defvar oai-block--markdown-end-re "^\\s-*```\\s-*$")
(defvar oai-block--markdown-beg-end-re "^[\s\t]*```\\(.*\\)$")
(defvar oai-block--chat-prefixes-re "^\\s-*\\[\\([^\]]+\\)\\(:\\]\\|\\]:\\)\\s-*"
  "Prefix should be at the begining of the line with spaces or without.
Or roles regex.")


(defface oai-block--me-ai-chat-prefixes-font-face
  '((t :weight bold))
  "Face font for chat roles (default bold).
You can customize this font with `set-face-attribute'."
  :group 'oai)

;; -=-= loading code: activate "ai" block in Org mode
(when (and (boundp 'org-protecting-blocks) (listp org-protecting-blocks))
  (add-to-list 'org-protecting-blocks "ai"))

(when (boundp 'org-structure-template-alist)
  (add-to-list 'org-structure-template-alist '("A" . "ai")))

;; -=-= fn: block-p, element-by-marker
;; `org-element-with-disabled-cache' is not available pre org-mode 9.6.6, i.e.
;; emacs 28 does not ship with it
(defmacro oai-block--org-element-with-disabled-cache (&rest body)
  "Run BODY without active org-element-cache."
  (declare (debug (form body)) (indent 0))
  `(cl-letf (((symbol-function #'org-element--cache-active-p) (lambda (&rest _) nil)))
     ,@body))

(defun oai-block-p ()
  "Are we inside a #+begin_ai...#+end_ai block?
Like `org-in-src-block-p'.  Return element."
  (oai-block--org-element-with-disabled-cache ;; with cache enabled we get weird Cached element is incorrect warnings
    (cl-loop with context = (org-element-context)
             while (and context
                        (not (equal 'special-block (org-element-type context)))
                        (not (string-equal "ai" (org-element-property :type context))))
             do (setq context (org-element-property :parent context))
             finally return context)))

;; -=-= info fn: get-info, get-request-type, get-sys
(defun oai-block-get-info (&optional element no-eval)
  "Parse the header of #+begin_ai...#+end_ai block.
ELEMENT is the element of the special block.
Like `org-babel-get-src-block-info' but instead of list return only
arguments.
To get value use: (alist-get :value (oai-block-get-info))
Use ELEMENT only in current moment.
When optional argument NO-EVAL is non-nil, do not evaluate Lisp
in parameters.
Return an alist of key-value pairs."
  (org-babel-parse-header-arguments
   (org-element-property
    :parameters
    (or element (oai-block-p))) no-eval))

(defun oai-block--get-request-type (info)
  "Look at the header of the #+begin_ai...#+end_ai block.
returns the type of request.  INFO is the alist of key-value
pairs from `oai-block-get-info'."
  (cond
   ((not (eql 'x (alist-get :chat info 'x))) 'chat)
   ((not (eql 'x (alist-get :completion info 'x))) 'completion)
   ((not (eql 'x (alist-get :complete info 'x))) 'completion)
   (t 'chat)))

(cl-defun oai-block--get-sys (&key info default)
  "Check if :sys exist in #+begin_ai parameters.
If exist return nil or string, if not exist  return `default'.
Argument INFO is the alist of key-value
pairs from `oai-block-get-info'.
DEFAULT is a string with default system prompt for LLM."
  (let ((sys-raw  (alist-get :sys info 'x)))
    ;; if 'x - not resent
    (if (eql 'x sys-raw)
        default
      ;; else - nil or string
      sys-raw)))

;; -=-= macro: let-params
(defmacro oai-block--let-params (info definitions &rest body)
  "A specialized `let*' macro for Oai parameters.
DEFINITIONS is a list of (VARIABLE &optional DEFAULT-FORM &key TYPE).
TYPE can be \='number, \='bool, \='string, or \='identity (no conversion).
Return one of:
- t symbol, if value for key not specified, if specied, return string.
- for number type, `string-to-number' used, that return 0 if number not
  recognized.
- for number if specified without value return t.
- Processed value of parameter (e.g., t/nil for bool).
Parameters are sourced from:
1. From Oai block header INFO alist.  (e.g., :model \"gpt-4\")
2. Org inherited property. (e.g., #+PROPERTY: model gpt-4)
3. DEFAULT-FORM."
  (setq info info) ; for melpazoid
  `(let* ,(cl-loop for def-item in definitions
                   collect
                   (let* ((sym (car def-item))
                          (default-form (cadr def-item))
                          (type (cadr (member :type def-item)))
                          (key (intern (concat ":" (symbol-name sym))))
                          (prop-name (symbol-name sym))
                          (postprocessor (cl-case type
                                       (number `(cond ((null val) nil)
                                                      ((and (stringp val) (string= val "nil")) nil)
                                                      ((stringp val) (string-to-number val))
                                                      ((numberp val) val)
                                                      (t val)))
                                       (bool `(cond ((null val) nil)
                                                    ((eq val t) t)
                                                    ((stringp val) (if (member (downcase val) '("t" "true" "yes" "on" "1")) t nil))
                                                    (t nil)))
                                       (string `(cond ((null val) nil)
                                                      ((and (not (stringp val)) (equal val t)) ; empty
                                                       nil)
                                                      ((stringp val)
                                                       (if (string-equal-ignore-case val "nil")
                                                           nil
                                                         ;; else
                                                         val))
                                                      (t (prin1-to-string val))))
                                       (identity `val)
                                       (t `val))))
                     `(,sym (let ((val (or (let* ((v1 (assoc ,key info))
                                                  (v2 (cdr v1)))
                                             (if (and v1 (not v2)) ; exist empty
                                                 t
                                               v2))
                                           (org-entry-get-with-inheritance ,prop-name)
                                           ,@(when default-form `(,default-form)))))
                              ,postprocessor))))
     ,@body))



;; info cases:
;; - string: '((:model . "openai/gpt-4.1"))
;; - int: '((:max-tokens . 3000))
;; - 'nil: '((:model))
;; - only key: '((:model))
;; - nil: '((:model . "nil"))
;; get value from info:
;; - (alist-get :model '((:model . "nil"))) => "nil"
;; - (alist-get :model '((:model))) => nil
;; - (alist-get :model '(())) => nil
;; - (assoc :model '(())) => nil => (cdr nil) => nil
;; - (assoc :model '((:model))) => (:model) => (cdr '(:model)) => nil
;; - (assoc :model '((:model . "nil"))) => (:model . "nil") => (:model . "nil") => "nil"
;; (concat \":\" (symbol-name sym))
;; Solution:
;; (let* ((v1 (assoc :model '((:model))))
;;           (v2 (cdr v1)))
;;     (if (and v1 (not v2))
;;         "nil"
;;       v2))

;; -=-= parts fn: get-content
;; (defun oai-block--contents-area (datum)
;;   "As `org-src--contents-area'."
;;   (let ((type (org-element-type datum)))
;;     (if (eq type 'special-block)
;;        (let
;;          (beg (org-element-property :contents-begin element))
;;          (end (org-element-property :contents-end element))
;;          (list beg end (buffer-substring-no-properties beg end))
;;         special-block
;;   (org-with-wide-buffer
;;    ((eq type 'latex-fragment)

(defun oai-block-contents-begin-end (&optional element)
  "Return typle :contents-begin and :contents-end for ai block only.
Optional argument ELEMENT should be ai block if specified."
  (when-let ((element (or element (oai-block-p))))
     (let* ((beg (org-element-property :contents-begin element))
            (end (org-element-property :contents-end element)))
       (if (and beg end)
           (list beg end)
         ;; else - empty block
         (org-with-wide-buffer
          (save-excursion
            (goto-char (org-element-property :begin element))
            (list (line-beginning-position 2) (line-beginning-position 2))))))))

(defun oai-block-get-content (&optional element)
  "Extracts the text content of the #+begin_ai...#+end_ai block.
ELEMENT is the element of the special block.

Will expand noweb templates if an `oai-noweb' property or
`noweb' header arg is \"yes\".
Use ELEMENT only in current moment, if buffer modified you will need new
ELEMENT."
  (when-let ((reg (oai-block-contents-begin-end element)))
    (seq-let (con-beg con-end) reg
      (org-with-wide-buffer
       (let* ((unexpanded-content (if (or (not con-beg) (not con-end))
                                      (error "Empty block")
                                    ;; else
                                    (string-trim (buffer-substring-no-properties con-beg con-end))))
              (info (oai-block-get-info element))
              (noweb-control (or (alist-get :noweb info nil)
                                 (org-entry-get (point) "oai-noweb" 1)
                                 "no"))
              (content (if (string-equal-ignore-case "yes" noweb-control)
                           (org-babel-expand-noweb-references (list "markdown" unexpanded-content))
                         unexpanded-content)))
         (string-trim content))))))

;; -=-= help function to call hooks as pipeline with one argument
(defun oai-block--pipeline (funcs init-val &rest args)
  "Process INIT-VAL through a pipeline of functions FUNCS.
Each function in FUNCS is called as (func val &rest ARGS), where VAL
is the result of previous function (or INIT-VAL for the first), and
ARGS are optional additional arguments supplied to this function.

Returns the result of the final function in FUNCS, or INIT-VAL if FUNCS is nil."
  (if funcs
      (let ((result init-val))
        (dolist (f funcs result)
          (setq result (apply f result args))))
    ;; else
    init-val))

;; (defun aa (x) (concat x "2"))
;; (oai-block--pipeline
;;  (list 'aa)
;;  "ss") ; => "ss2"
;;
;; (oai-block--pipeline
;;  '((lambda (x) (concat x "1"))
;;    (lambda (x) (concat x "2")))
;;  "ss") ; => "ss12"

;; (oai-block--pipeline
;;  '((lambda (x b) (concat x b "1"))
;;    (lambda (x b) (concat x b "2")))
;;  "ss" "vv") ; => "ssvv1vv2"

;; (oai-block--pipeline
;;  nil
;;  "ss") ; => "ss"

;; -=-= chat: insert message
(defun oai-block--insert-single-response (end-marker &optional text insert-me final)
  "Insert result to ai block.
Should be used in two steps: 1) for insertion of text 2) with TEXT equal
to nil, for finalizing by setting pointer to the end and insertion of me
role.
Here used for completion mode in `oai-restapi-request'.
- END-MARKER is where to put result, is a buffer and position at the end
  of block, from `oai-block--get-content-end-marker' function.
- TEXT  is  string  from  the  response of  OpenAI  API  extracted  with
  `oai-restapi--get-single-response-text'.
- END-MARKER
- if FINAL is non-nill we add `undo-boundary'.
Variable `oai-block-roles-prefixes' is used to format role to text."
  (oai--debug "oai-block--insert-single-response end-marker, text:" end-marker text)
  (let ((buffer (marker-buffer end-marker))
        (pos (marker-position end-marker))
        (text (when text (string-trim text))))
    (oai--debug "oai-block--insert-single-response buffer,pos:" buffer pos "")
    ;; - write in target buffer
    (if (and text (not (string-empty-p text)))

          (with-current-buffer buffer ; Where target ai block located.
            ;; set mark (point) to allow user "C-u C-SPC" command to easily select the generated text
            (push-mark end-marker t)
            (save-excursion
              ;; - go  to the end of previous line and open new one
              (goto-char pos)
              ;; - remove empty lines between end of block and user question.
              (goto-char (1- pos)) ; to use insert before end-marker to preserve it at the end of block
              (while (bolp)
                (delete-char -1))
              (print (point))
              (newline)
              (newline)
              ;; (print (point))
              (insert "[" (car (rassoc 'assistant oai-block-roles-prefixes)) "]: "
                      (if (string-match "\n" text) ; multiline answer we start with a new line.
                          "\n"
                        ;; else
                        "")
                      text)
              (newline)

              (condition-case hook-error
                  (run-hook-with-args 'oai-restapi-after-chat-insertion-hook 'end text pos nil)
                (error
                 (message "Error during \"after-chat-insertion-hook\": %s" hook-error)))
              (when final
                ;; - "auto-fill"
                (when oai-block-fill-function
                  (undo-boundary)
                  (funcall oai-block-fill-function pos nil))
                (org-element-cache-reset)
                (undo-boundary))
              ;; (setq pos (point))
              ;; (set-marker end-marker (point))
              ))

      ;; - else - DONE - text is nil
      ;; - special cases for DONE
      (with-current-buffer buffer
        (when insert-me
          (save-excursion
            ;; - go  to the end of previous line and open new one
            (goto-char pos)
            (insert "[" (car (rassoc 'user oai-block-roles-prefixes)) "]: \n")
            (forward-char -1)
            (setq pos (point)))
          (set-marker end-marker (point)))
        (when oai-block-jump-to-end-of-block
          (goto-char pos))
        ;; final
        (org-element-cache-reset)
        (undo-boundary)))))

;; Used in `oai-restapi--normalize-response' and in `oai-block--insert-stream-response'
(cl-deftype oai-block--response-type ()
  '(member role text stop error))

(cl-defstruct oai-block--response ; :type is not enforced now
  (type (user-error "No default value") :type oai-block--response-type)
  (payload (user-error "No default value") :type string))

;; (make-oai-block--response :type 'role :payload "user") ; #s(oai-block--response role "user")
;; (make-oai-block--response :type 'role) ; error
;; (make-oai-block--response :payload "role") ; error
;; (make-oai-block--response :type nil :payload "role") ; #s(oai-block--response nil "role")
;; (make-oai-block--response :type 'role :payload nil) ; #s(oai-block--response role nil)
;; (oai-block--response-type (make-oai-block--response :type 'role :payload "asd")) ; 'role
;; (oai-block--response-payload (make-oai-block--response :type 'role :payload "asd")) ; "asd"

(defvar-local oai-block--current-insert-position-marker nil
  "Where to insert the result.
Used for `oai-block--insert-stream-response'.")

(defvar-local oai-block--current-chat-role nil
  "During chat response streaming, this holds the role of the \"current speaker\".
Used for `oai-block--insert-stream-response'.")

(defun oai-block--insert-stream-response (end-marker &optional responses insert-me)
  "Insert result to ai block for chat mode.
When first chunk received we stop waiting timer for request.
END-MARKER'is where to put result,
RESPONSES is a list of oai-block--response, processed by
`oai-restapi--normalize-response', consist of type symbol and payload
string.
Used as callback for `oai-restapi-request', called in url buffer.

Called within url-buffer.
Use buffer-local variables:
`oai-block--current-insert-position-marker',
`oai-block--current-chat-role'.

If response is multiline `oai-block-fill-function' may not
work properly.(may be old)
Argument INSERT-ME insert [ME]: at stop type of message."
  ;; (oai--debug "oai-block--insert-stream-response1 %s" (oai-restapi--normalize-response response)) ; response
  (oai--debug "oai-block--insert-stream-response1" responses)
  (when responses
    (let ((buffer (marker-buffer end-marker))
          (pos (or oai-block--current-insert-position-marker
                   (marker-position end-marker)))
          (c-chat-role oai-block--current-chat-role)
          stop-flag)
      ;; (oai--debug "oai-block--insert-stream-response2 %s" normalized)
      ;; (oai--debug "oai-block--insert-stream-response" normalized)
      (unwind-protect ; we need to save variables to url buffer
          (with-current-buffer buffer ; target buffer with block
            (save-excursion
              ;; - LOOP Per message
              (dolist (response responses)
                (let ((type (oai-block--response-type response)) ; symbol
                      (payload (oai-block--response-payload response))) ; string
                  ;; (oai--debug "oai-block--insert-stream-response: %s %s %s" type end-marker oai-block--current-insert-position-marker)
                  ;; - Type of message: error
                  (when (eq type 'error)
                    (error (oai-block--response-payload response))) ; not used

                  (goto-char pos)
                  ;; - Remove lines above and provide space below, should be covered with tests.
                  (when (looking-at oai-block--ai-block-end-re) ; "#\\+end"
                    (goto-char (1- pos)) ; to use insert before end-marker to preserve it at the end of block
                    (while (bolp)
                      (delete-char -1))
                    (setq pos (point)))

                  ;; - Type of message
                  (pcase type
                    ('role (when (not (string= payload c-chat-role)) ; payload = role
                             (goto-char pos)

                             (setq c-chat-role payload)
                             (let* ((role-oai (or (cdr (assoc-string payload oai-block-roles-restapi))
                                                  oai-block-roles-restapi-unknown)) ; string to symbol
                                    (role-prefix (car (rassoc role-oai oai-block-roles-prefixes))))

                               (insert "\n[" role-prefix "]: " (when (eql role-oai 'assistant) "\n")) ; "\n[ME:] " or "\n[AI:] \n"

                               (condition-case hook-error
                                   (run-hook-with-args 'oai-restapi-after-chat-insertion-hook 'role payload pos t)
                                 (error
                                  (message "Error during \"after-chat-insertion-hook\" for role: %s" hook-error)))

                               (setq pos (point)))))
                    ('text (progn ; payload = text
                             (goto-char pos)
                             (insert payload)
                             ;; - "auto-fill" if not in code block
                             (when oai-block-fill-function
                               (funcall oai-block-fill-function pos t))

                             (condition-case hook-error
                                 (run-hook-with-args 'oai-restapi-after-chat-insertion-hook 'text payload pos t)
                               (error
                                (message "Error during \"after-chat-insertion-hook\" for text: %s" hook-error)))
                             (setq pos (point))
                             ;; (setq not-first t)
                             ))

                    ('stop (progn ; payload = stop_reason
                             (oai--debug "oai-block--insert-stream-response3 stop_reason: %s" payload)
                             (goto-char pos)
                             (let ((text (concat "\n\n[" (car (rassoc 'user oai-block-roles-prefixes)) "]: "))) ; "ME"
                               (if insert-me
                                   (insert text)
                                 ;; else
                                 (setq text ""))

                               (condition-case hook-error
                                   (run-hook-with-args 'oai-restapi-after-chat-insertion-hook 'end text pos t)
                                 (error
                                  (message "Error during \"after-chat-insertion-hook\": %s" hook-error)))
                               (setq pos (point)))

                             (org-element-cache-reset)
                             (setq stop-flag t)))))))
            ;; - without save-excursion - stop: go to the end.
            (when (and oai-block-jump-to-end-of-block
                       stop-flag)
              ;; for jumping
              (unless (region-active-p)
                (push-mark nil t))

              (goto-char pos)))
        ;; - after buffer - UNWINDFORMS - save variables to url-buffer
        (setq oai-block--current-insert-position-marker pos)
        (setq oai-block--current-chat-role c-chat-role)))))

;; -=-= chat: collect-chat-messages
(defun oai-block--get-chat-messages-positions (content-start content-end prefix-re)
  "Return a flat list of positions for chat messages in current buffer.
Positions CONTENT-START CONTENT-END used as boundaries.
Return positions  as start points  that match PREFIX-RE (normally  it is
`oai-block--chat-prefixes-re'), and additional positions of content start
and content end at the beginin and the end of flat list."
  (when (< content-end content-start)
    (error "Point is at wrong position"))
  (save-excursion
    (let (positions)
      (goto-char content-start)
      ;; Collect all chat header positions
      (while (re-search-forward prefix-re content-end t)
        (push (match-beginning 0) positions))
      (setq positions (nreverse positions))
      ;; Ensure content-start is included first
      (unless (and positions (= (car positions) content-start))
        (push content-start positions))
      ;; Ensure content-end is included at last
      (unless (and positions (= (car (last positions)) content-end))
        (setq positions (append positions (list content-end))))
      ;; return
      positions)))

;; (defun oai-block--chat-role-regions ()
;;   "Splits the special block by role prompt.
;; Return line begining positions of first line of content, roles, #+end_ai
;; line."
;;   (when-let* ((element (oai-block-p))
;;               (content-start (org-element-property :contents-begin element))
;;               (content-end (org-element-property :contents-end element)))
;;     (let ((result (save-match-data
;;                     (save-excursion
;;                       (goto-char content-start)
;;                       (cl-loop with result
;;                                while (search-forward-regexp oai-block--chat-prefixes-re content-end t)
;;                                do (push (match-beginning 0) result)
;;                                finally return result)))))
;;       (if result
;;           (cl-concatenate 'list (list content-start) (reverse result) (list content-end))
;;         (list content-start content-end)))))

(defun oai-block--chat-role-regions ()
  "Splits the special block by role prompt.
Return line begining positions of first line of content, roles, #+end_ai
line."
  (when-let* ((element (oai-block-p))
              (content-start (org-element-property :contents-begin element))
              (content-end (org-element-property :contents-end element)))
    (oai-block--get-chat-messages-positions content-start content-end oai-block--chat-prefixes-re)))


(defun oai-block--merge-by-role (messages &optional sep)
  "Merge consecutive MESSAGES plists with same :role.
Joining non-empty content by SEP (defaults to newline)."
  (setq sep (or sep "\n"))
  (let (result role content)
    (dolist (msg messages)
      (let ((r (plist-get msg :role))
            (c (plist-get msg :content)))
        (if
         ;; Same role: append content if it's non-empty
         (and role (eq r role))
          (when (and c (not (string-empty-p c)))
            (setq content (if (and content (not (string-empty-p content)))
                              (concat content sep c) c)))
          ;; else. New role: push previous role/content, start new batch
          (when (and role content (not (string-empty-p content)))
            (push (list :role role :content content) result))
          (setq role r content c))))
    ;; Push any remaining role/content
    (when (and role content (not (string-empty-p content)))
      (push (list :role role :content content) result))
    (nreverse result)))


;; Parse parts and build messages
(defun oai-block--parse-part (pos-beg pos-end)
  "Get part of chat as a plist with :role and :content in current buffer.
Positions POS-BEG POS-END used as limits.
Skip AI_REASON role string.
If prefix found two times error is thrown.
Uses `oai-block-roles-prefixes' variable and `oai-block--chat-prefixes-re'.
If content is empty string return nil otherwise plist."
  (save-excursion
    (goto-char pos-beg)
    ;; - find prefix
    (let ((first_chat_role 'user)
          content ; after prefix or from pos
          role-str
          role
          pre-end-pos) ; begining of content
      ;; get role and begining of content
      (save-match-data
        (when (re-search-forward oai-block--chat-prefixes-re pos-end t)
            (setq role-str (match-string 1))
            (setq pre-end-pos (match-end 0))
            (when (re-search-forward oai-block--chat-prefixes-re pos-end t)
              (error "Another role prefix found before POS-END"))))

      (unless (string= role-str "AI_REASON") ; works for nil
        ;; first - get role symbol
        (if role-str
          (setq role (or (cdr (assoc-string role-str oai-block-roles-prefixes t))
                         oai-block-roles-prefixes-unknown))
          ;; else
          (setq role first_chat_role))
        ;; get content
        (setq content (string-trim (buffer-substring-no-properties (or pre-end-pos pos-beg)
                                                                   pos-end)))
        (setq content (oai-block--pipeline oai-block-parse-part-hook content role)))
      ;; if content is empty return nil.
      (when (not (string-empty-p content))
        (list :role role :content content)))))

;; (defun oai-block--get-chat-parts (pos-beg pos-end)
;;   "Return a list of chat message plists (:role :content) in region POS-BEG to POS-END.

;; Finds message prefixes, assigns roles, skips empty messages and 'AI_REASON'.
;; Returns list of (:role ROLE :content CONTENT) plists in buffer order."
;;   (let ((prefix-regex oai-block--chat-prefixes-re)
;;         (parts ())
;;         (start pos-beg)
;;         (role nil))
;;     (save-excursion
;;       (goto-char pos-beg)
;;       ;; Loop through all messages delimited by prefixes in region
;;       (while (re-search-forward prefix-regex pos-end t)
;;         (let ((this-prefix-start (match-beginning 0))
;;               (this-prefix-end (match-end 0)))
;;           ;; The actual content is between start and this-prefix-start
;;           (let* ((content (string-trim (buffer-substring-no-properties start this-prefix-start)))
;;                  (this-role (oai-block--parse-role-string
;;                              (buffer-substring-no-properties this-prefix-start this-prefix-end))))
;;             (unless (or (string-empty-p content)
;;                         (string= this-role "AI_REASON"))
;;               (push (list :role (or role this-role) :content content) parts))
;;             ;; Prepare for next region: after prefix
;;             (setq role this-role)
;;             (setq start this-prefix-end))))
;;       ;; Gather last region after last prefix if any content
;;       (when (< start pos-end)
;;         (let ((content (string-trim (buffer-substring-no-properties start pos-end))))
;;           (unless (or (string-empty-p content)
;;                       (string= role "AI_REASON"))
;;             (push (list :role (or role "user") :content content) parts))))
;;       ;; Return in correct buffer order
;;       (nreverse parts))))


(defun oai-block--collect-chat-messages (content-start content-end &optional default-system-prompt persistant-sys-prompts max-token-recommendation separator)
  "Return a list of positions for chat messages within current oai block.
`DEFAULT-SYSTEM-PROMPT' used for [SYS] or the first [SYS]
prompt found in `CONTENT-STRING'.
If `PERSISTANT-SYS-PROMPTS' is non-nil, [SYS] prompts are
 intercalated.
SEPARATOR used for merging message with same role
Positions CONTENT-START and CONTENT-END used as limits for parsing ai
block, may be retrieved with :contents-begin and :contents-end
properties of ai block Org element.
MAX-TOKEN-RECOMMENDATION is string to add to first system message.
Return vector with plist with :role and :content."
  (let* ((separator (or separator "\n"))
         ;; 1) Positions: for prefixes [ME:], [AI:] in current buffer
         (positions (oai-block--get-chat-messages-positions content-start content-end oai-block--chat-prefixes-re))
         ;; 2) Parts: loop over positions to get strings of parts
         (parts (let ((lst positions)
                      (results '()))
                  (while (cdr lst)
                    (push (oai-block--parse-part (car lst) (cadr lst)) results) ; parse current block
                    (setq lst (cdr lst)))
                  (nreverse (remove nil results)))) ; reorder and filter nil
         ;; Merge messages with same role.
         (parts (oai-block--merge-by-role parts separator))
         ;; 3) sys-prompt: check if [SYS] is the first part in parts.
         (starts-with-sys-prompt-p (and parts (eql (plist-get (car parts) :role) 'system))))

    ;; 4) Parts: fix [SYS:]
    (when (and default-system-prompt (not starts-with-sys-prompt-p))
      ;; add
      (setq parts (cons (list :role 'system :content default-system-prompt) parts)))
    ;; Parts:
    (when max-token-recommendation
      (if (or starts-with-sys-prompt-p default-system-prompt)
          ;; modify content
          (setf (plist-get (car parts) :content)
                (concat
                 (plist-get (car parts) :content) " " max-token-recommendation))
        ;; else - add
        (setq parts (cons (list :role 'system :content max-token-recommendation) parts))))

    ;; Parts: add persistant-sys-prompts as a prefix to every 'user message
    (when persistant-sys-prompts
      (let ((lst parts)
            cur)
        (while lst
          (setq cur (car lst))
          (when (eql (plist-get cur :role) 'user)
            ;; modify content
            (setf (plist-get cur :content)
                  (concat
                   persistant-sys-prompts " "
                   (plist-get cur :content))))
          (setq lst (cdr lst)))))
    ;; 5) convert to vectors
    (apply #'vector parts)))


(defun oai-block--collect-chat-messages-at-point (&optional element default-system-prompt persistant-sys-prompts max-token-recommendation separator)
  "Collect messages for ai block at current positon.
Optional argument ELEMENT is AI block in current buffer.
Description for DEFAULT-SYSTEM-PROMPT PERSISTANT-SYS-PROMPTS
MAX-TOKEN-RECOMMENDATION SEPARATOR at `oai-block--collect-chat-messages'."
  (when-let* ((element (or element (oai-block-p)))
              (content-start (org-element-property :contents-begin element))
              (content-end   (org-element-property :contents-end element)))
    (oai-block--collect-chat-messages content-start content-end default-system-prompt persistant-sys-prompts max-token-recommendation separator)))

(defun oai-block--collect-chat-messages-from-string (content-string &optional default-system-prompt persistant-sys-prompts max-token-recommendation separator)
  "Collect messages from CONTENT-STRING.
Description for DEFAULT-SYSTEM-PROMPT PERSISTANT-SYS-PROMPTS
MAX-TOKEN-RECOMMENDATION SEPARATOR at `oai-block--collect-chat-messages'."
  (with-temp-buffer
    (erase-buffer)
    (insert content-string)
    (let ((content-start (point-min))
          (content-end   (point-max)))
      (oai-block--collect-chat-messages content-start content-end default-system-prompt persistant-sys-prompts max-token-recommendation separator))))

;; -=-= chat: stringify-chat-messages

;; [[file:~/sources/emacs-oai/oai-restapi.el::1986::(cl-defun oai-restapi--stringify-chat-messages (messages &optional &key]]
(defun oai-block--format-message (msg)
  "Return converted to a string plist MSG.
Used in `oai-expand-block'."
  (let* ((role (plist-get msg :role))
         (content (plist-get msg :content))
         (role-str (car (rassoc role oai-block-roles-prefixes))))
    (concat "[" role-str "]: " content)))

(defun oai-block--stringify-chat-messages (messages &optional default-system-prompt)
  "Convert a chat message to a string.
MESSAGES is a vector of plist with :role :content keys.  :role can be
\='system, \='user or \='assistant.
If DEFAULT-SYSTEM-PROMPT non-nil, a [SYS] prompt is prepended if the
first message is not a system message, otherwise DEFAULT-SYSTEM-PROMPT
argument is ignored.
Used in `oai-expand-block'.
Uses `oai-block-roles-prefixes' variable for mapping roles to prefixes."
  ;; 1) add default-system-prompt as first [SYS]: if not exist
  (let ((messages (if (and default-system-prompt
                           (not (eql (plist-get (aref messages 0) :role) 'system))) ; enforce that vector should consist of plists
                      ;; (cl-concatenate 'vector (vector (list :role 'system :content default-system-prompt)) messages)
                      (vconcat (vector (list :role 'system :content default-system-prompt)) messages)
                    messages)))
    ;; 2) convert every message to a string and join them.
    (string-join
     (mapcar #'oai-block--format-message messages)
     "\n\n")))

;; (oai-block--stringify-chat-messages '[(:role assistant :content "Be helpful; then answer.") (:role user :content "Be hnswer.")] "as")

;; -=-= Markdown block check pos

;; (defun oai-block--in-markdown (pos &optional lim-beg)
;;   "Check if POS in markdown block, quoted or is a table.
;; Optional argument LIM-BEG is ai block begining position.
;; Return t if pos in markdown block, table or quote.
;; Side-effect: set pointer position to POS.
;; Same as `oai-block-tags--is-special'."
;;   (goto-char pos)
;;   (prog1 (or ; ruturn
;;           ;; search backward
;;           (when (re-search-backward oai-block--markdown-begin-re lim-beg t)
;;             (goto-char pos)
;;             ;; backward for markdown block "end" after "begin"
;;             (not (re-search-backward oai-block--markdown-end-re (match-end 0) t)))
;;           (progn (goto-char pos)
;;                  (beginning-of-line)
;;                  (looking-at "^> ")))
;;     (goto-char pos)))

(defun oai-block--markdown-begin-end (pos limit-begin limit-end)
  "Return (begin end) of markdown shallow block.
If POS position is inside markdown block without specified language or
specified.  Used in `oai-block-mark-at-point'.
If there is two markdown begining with language we treat second as
inside of first.
Argument LIMIT-BEGIN LIMIT-END are positions ai block header and footer."
  (oai--debug "oai-block--markdown-begin-end %s %s %s" pos limit-begin limit-end)
  ;; fix limits
  (when (and limit-begin (< pos limit-begin))
    (setq limit-begin pos))
  (when (and limit-end (> pos limit-end))
    (setq limit-end pos))
  ;; (oai--debug "oai-block--markdown-begin-end0 %s %s %s" pos limit-begin limit-end)
  (let (beg
        beg-cont
        end found)
    (goto-char limit-begin)
    ;; 1) find begining with or without language
    (oai--debug "oai-block--markdown-begin-end11 %s %s %s %s" (point) limit-begin limit-end found)
    (while (and (< (point) limit-end)
                (re-search-forward oai-block--markdown-beg-end-re limit-end t)
                (not found))
      (oai--debug "oai-block--markdown-begin-endss yes")
      (unless (string-match-p "```" (match-string 1)) ; should not be flat quoted list
        (oai--debug "oai-block--markdown-begin-end yes")
        (setq beg-cont (point)) ; end of head line
        (setq beg (match-beginning 0)) ; end of head line
        (forward-line)
        ;; 2) find end
        (when (and (re-search-forward oai-block--markdown-end-re limit-end t)
                   (not found))
          ;; save
          (setq end (1+ (match-beginning 0)))
          (oai--debug "oai-block--markdown-begin-end1 %s %s %s" pos beg end)
          (when (and (>= pos beg) (<= pos end))
            (setq found (list (1+ beg-cont) end))))))
    found))


;; (defun oai-block--markdown-lang-begin-end (pos limit-beg limit-end)
;;   "Return (begin end) of markdown language block.
;; If POS in it.
;; Optional argument LIM-BEG is ai block begining position.
;; Return t if pos in markdown block, table or quote.
;; Side-effect: set pointer position to POS."
;;   (goto-char pos)
;;   ;; fix limits
;;   (when (and limit-begin (< point-pos limit-begin))
;;     (setq limit-begin point-pos))
;;   (when (and limit-end (> point-pos limit-end))
;;     (setq limit-end point-pos))
;;   (prog1 (let ((beg end))
;;            ;; not markdown blocks
;;            ;; backward for markdown block "begin"
;;            (when (re-search-backward oai-block--markdown-begin-re limit-beg t)
;;              ;; save
;;              (forward-line)
;;              (setq beg (line-end-position))
;;              ;; restore
;;              (goto-char pos)
;;              ;; backward for markdown block "end" after "begin"
;;              (unless (re-search-backward oai-block--markdown-end-re (match-end 0) t)
;;                (when (re-search-forward oai-block--markdown-end-re limit-end t)
;;                  (unless (re-search-backward oai-block--markdown-begin-re pos t)
;;                    (list beg (match-beginning 0)))))))
;;     (goto-char pos)))




(defun oai-block--at-special-p (pos &optional dont-check-tables)
  "Check if POS in markdown block, quoted or is a table.
Optional argument LIM-BEG is ai block begining position.
Return t if pos in markdown block, table or quote.
Side-effect: set pointer position to POS.
If Optional argument DONT-CHECK-TABLES is not-nil disable checking if
pos at Org table."
  (goto-char pos)
  (prog1
          ;; not quoted, in tables
          (progn (goto-char pos)
                 (beginning-of-line)
                 (or (looking-at "^\\s-*> ") ; from `oai-block-fill-region-as-paragraph'
                     (and (not dont-check-tables) (looking-at "^[ \t]*\\(|\\|\\+-[-+]\\).*")))) ; skip tables
    (goto-char pos)))

;; (defun oai-block-tags--in-markdown-quotes-at-line-p (pos)
;;   "Return t if POS is inside a markdown single backquote.
;; Region (`...`) on the current line.
;; Return t at first and last quote too."
;;   (goto-char pos)
;;   (prog1 (let ((bol (line-beginning-position))
;;                (eol (line-end-position))
;;                (pos (point))
;;                found)
;;            (goto-char bol)
;;            ;; Search for paired backquotes (`...`)
;;            (while (and (re-search-forward "`" eol t) (not found))
;;              (let ((start (match-beginning 0)))
;;                (when (re-search-forward "`" eol t)
;;                  (let ((end (match-end 0)))
;;                    (when (and (>= pos start)
;;                               (< pos end)) ; strictly between the two backquotes
;;                      (setq found t))))))
;;            found)
;;     (goto-char pos)))

(defun oai-block--in-markdown-quotes-p (pos &optional delimiter beg end)
  "Return t if POS is inside any markdown quotes at current line.
if BEG END not provided, look for DELIMITER at the current line only.
DELIMITER should be a string (\"`\" or \"```\"), defaults to \"`\"."
  (save-excursion
    (let ((delimiter (or delimiter "`"))
          (bol (or beg
                   (progn (goto-char pos) (line-beginning-position))))
          (eol (or end
                   (progn (goto-char pos) (line-end-position))))
          (found nil))
      (goto-char bol)
      (while (and (search-forward delimiter eol t) (not found))
        (let ((start (match-beginning 0)))
          (when (search-forward delimiter eol t)
            (let ((end (match-end 0)))
              (when (and (>= pos start)
                         (< pos end))
                (setq found t))))))
      found)))

(defun oai-block--in-markdown-single-quotes-p (pos)
  "Return t if POS is inside a markdown single backquote (`...`).
On current line or at quote itself."
  (oai-block--in-markdown-quotes-p pos "`"))

(defun oai-block--in-markdown-triple-quotes-p (pos)
  "Return t if POS is inside a markdown triple backquote (```...```).
On current line or at quote itself."
  (oai-block--in-markdown-quotes-p pos "```"))

(defun oai-block--in-markdown-any-quotes-p (pos)
  "Return t if POS is inside any markdown backquote block at line.
on the current line.
\(`...` or ```...```\) on current line or at quote itself."
  (or (oai-block--in-markdown-quotes-p pos "`")
      (oai-block--in-markdown-quotes-p pos "```")))


;; -=-= Interactive

(defun oai-block-mark-last-region ()
  "Mark the last prompt in an oai block."
  (interactive)
  (when-let* ((regions (reverse (oai-block--chat-role-regions)))
              (last-region-end (pop regions))
              (last-region-start (pop regions)))
        (goto-char last-region-end)
        (push-mark last-region-start t t)))

(defun oai-block-mark-region-at-point ()
  "Mark the prompt at point: [ME:], [AI:]."
  (interactive)
  (when-let* ((regions (oai-block--chat-role-regions))
              (start (cl-find-if (lambda (x) (>= (point) x)) (reverse regions)))
              (end (cl-find-if (lambda (x) (<= (point) x)) regions)))
    (when (= start end)
      (setq end (cl-find-if (lambda (x) (< start x)) regions)))
    (when (not end)
      (setq end start)
      (setq start (cl-find-if (lambda (x) (> end x)) (reverse regions))))
    (when (and start end)
      (goto-char start)
      (unless (region-active-p) (push-mark end t t))
      (cons start end))))

(defun oai-block-forward-section (&optional arg)
  "Move forward to end of section.
A negative argument ARG = -N means move backward."
  (interactive "^p")
  ;;   TODO:
  ;; With argument ARG, do it ARG times;
  ;; a negative argument ARG = -N means move backward N paragraphs.
  (when-let* ((regions (oai-block--chat-role-regions))
              (start (cl-find-if (lambda (x) (>= (point) x)) (reverse regions)))
              (end (cl-find-if (lambda (x) (< (point) x)) regions)))
    ;; (oai--debug "oai-forward-section1 %s %s" start end)
    (or arg (setq arg 1))
    (if (> arg 0)
        (progn (unless (region-active-p) (push-mark nil t)) ; save position
               (goto-char end))
      ;; else - backward
      (let ((prev (cl-find-if (lambda (x) (>= (1- start) x)) (reverse regions))))
        ;; (oai--debug "oai-forward-section2 %s %s %s" (>= (point) start) prev start)
        (when  prev ;; (>= (point) start))
          (if (and (> (point) start) ; if at the middle of first section
                   (not prev))
              (progn (unless (region-active-p) (push-mark nil t)) ; save position
                     (goto-char start))
            ;; else
            (unless (region-active-p) (push-mark nil t)) ; save position
            (goto-char (cl-find-if (lambda (x) (>= (1- start) x)) (reverse regions)))))))))

(defun oai-block-kill-region-at-point (&optional arg)
  "Kill the prompt at point.
The numeric ARG can be used for killing the last n."
  (interactive "P")
  (cl-loop repeat (or arg 1)
           do (when-let* ((region (oai-block-mark-region-at-point))
                          (start (car region))
                          (end (cdr region)))
                ;; (cl-destructuring-bind (start . end) region
                  (kill-region end start))))

;; (defun oai-block-kill-last-region (&optional arg)
;;   "Kill the prompt at point.
;; The numeric ARG can be used for killing the last n."
;;   (interactive "P")
;;   (when-let* ((regions (reverse (oai-block--chat-role-regions)))
;;               (last-region-end (pop regions))
;;               (last-region-start (pop regions)))
;;         (goto-char last-region-end)
;;         (push-mark last-region-start t t)))

;;   (cl-loop repeat (or arg 1)
;;            do (when-let ((region (oai-block-mark-region-at-point)))
;;                 (cl-destructuring-bind (start . end) region
;;                   (kill-region end start)))))

(defun oai-block-mark-at-point (arg)
  "Mark entity at current poin in current buffer.
Mark Mardkown block or whole ai block.  If universal argument ARG is
non-nil, then mark one chat message."
  (interactive "P")
  (oai--debug "oai-block-mark-at-point0 %s %s" (point) arg)
  (if arg
      (oai-block-mark-region-at-point)
    ;; else - find block
    (oai--debug "oai-block-mark-at-point01 %s" (point))
    (when-let ((pos
                ;; (progn (when (looking-at oai-block--markdown-beg-end-re)
                ;;              (forward-line)) ; fix for oai-block--markdown-begin-end that dont work if pointer at markdown begining
                           (point))
               (reg (oai-block-contents-begin-end))) ; get beg end

        (seq-let (beg end) reg
          (push-mark nil t)
          ;; - find markdown block
          (oai--debug "oai-block-mark-at-point1 %s %s %s" pos beg end)
          (if-let ((m-reg (oai-block--markdown-begin-end pos beg end)))
              (when m-reg
                (seq-let (m-beg m-end) m-reg
                  (push-mark m-beg t)
                  (goto-char (1- m-end))
                  (activate-mark))
                t)
            ;; else - no markdown block
            (goto-char pos)
            ;; (let ((el (elemet-at-point)))
            (call-interactively #'org-mark-element)
            (exchange-point-and-mark))))))


;; (defun oai-block-tags--markdown-mark-fenced-code-body (&optional limit-begin limit-end)
;;   "Mark content inside Markdown fenced code block (```), excluding header/footer.
;; LIMIT-BEGIN and LIMIT-END restrict the search region around point.
;; Returns t if was marked, nil otherwise.
;; Used in `oai-block-tags-mark-md-block-body'."
;;   ;; fill limit-begin and limit-end - if they was not profiled
;;   (when (not (and limit-begin limit-end))
;;     (when-let* ((region (oai-block-tags--get-org-block-region))) ; maybe use `org-src--contents-area' ?
;;       (setq limit-begin (car region))
;;       (setq limit-end (cadr region))))

;;   (when-let* ((r (oai-block-tags--markdown-fenced-code-body-get-range limit-begin limit-end))
;;               (beg (car r))
;;               (end (cadr r)))
;;     (push-mark beg t)
;;     (goto-char end)
;;     (forward-line -1)
;;     (end-of-line)
;;     (activate-mark)
;;     t))


(defun oai-block-set-max-tokens ()
  "Jump to header of ai block and set max-tokens."
  (interactive)
  (if-let ((element (oai-block-p)))
      (progn
        (push-mark)
        (goto-char (car (oai-block-contents-begin-end element)))
        (forward-line -1)
        (if (search-forward ":max-tokens" (line-end-position) t)
            ;; if - 1) ;modify :max-tokens
            (if (eq (line-end-position) (point))
                (insert " ")
              ;; else
              (forward-char))
          ;; else - 2) add :max-tokens
          ;; (goto-char beg)
          (forward-char 10)
          (insert " :max-tokens ")
          (let ((pos (point)))
            (insert (format "%s " oai-restapi-default-max-tokens))
            (goto-char pos))))
    ;; else
    (message "Not oai block here.")))

;; -=-= Markers

(defun oai-block-element-by-marker (marker)
  "Get ai block at MARKER position at marker buffer.
Used in prompt engineering only: oai-prompt.el."
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char marker)
      (oai-block-p))))

(defun oai-block--get-content-end-marker (&optional element)
  "Return a marker for the :contents-end property of ELEMENT.
Used in `oai-call-block'"
  (when-let* ((el (or element (oai-block-p)))
              (contents-end-pos (org-element-property :contents-end el)))
    (copy-marker contents-end-pos)))

(defun oai-block-get-header-marker (&optional element)
  "Return marker for ai block at current buffer at current positon.
Pointer between # an + characters.
Use ELEMENT only in current moment in element buffer."
  (when-let ((el (or element (oai-block-p))))
    ;; (with-current-buffer (org-element-property :buffer el)
    (save-excursion
      (goto-char (org-element-property :contents-begin el))
      (forward-line -1)
      (forward-char) ; 1+ to have something before marker for correct work.
      (copy-marker (point)))))

;; -=-= Result

(defun oai-block-insert-result-message (message header-marker)
  "Insert MESSAGE to #+RESULT of block in buffer of HEADER-MARKER."
  (with-current-buffer (marker-buffer header-marker)
    (save-excursion
      (goto-char header-marker)
      (oai-block-insert-result message))))

(defun oai-block-insert-result (result &optional result-params hash exec-time)
  "Modified `org-babel-insert-result' function.
Insert RESULT into the current buffer.
TODO: EXEC-TIME.
Optional argument RESULT-PARAMS not used.
Optional argument HASH not used."
  (ignore exec-time)
  (when (stringp result)
    (setq result (substring-no-properties result)))
  (save-excursion
    (let* ((visible-beg (point-min-marker))
           (visible-end (copy-marker (point-max) t))
           (existing-result (oai-block-where-is-result t nil hash))
           ;; When results exist outside of the current visible
           ;; region of the buffer, be sure to widen buffer to
           ;; update them.
           (outside-scope (and existing-result
                               (buffer-narrowed-p)
                               (or (> visible-beg existing-result)
                                   (<= visible-end existing-result))))
           beg end
           ;; indent
           )
      (when outside-scope (widen)) ;; ---- WIDDEN
      (goto-char existing-result) ;; must be true
      ;; (setq indent (current-indentation))
      (forward-line 1)
      (setq beg (point))
      (cond
       ((member "replace" result-params)
        (delete-region (point) (org-babel-result-end)))
       ((member "append" result-params)
        (goto-char (org-babel-result-end))
        (setq beg (point-marker))))
      (goto-char beg) (insert result "\n")
      (setq end (copy-marker (point) t))
      (org-babel-examplify-region beg end "")
      ;; finally
      (when outside-scope (narrow-to-region visible-beg visible-end)))) ;; ---- NARROW
  t)

(defun oai-block-where-is-result (&optional insert _info hash)
  "Modified `org-babel-where-is-src-block-result' function.
If Optional  argument INSERT is  non-nil just enshure that  result field
exist.
For _INFO HASH check `org-babel-where-is-src-block-result' function."
  (oai--debug "oai-block-where-is-result")
  (let ((context (oai-block-p)))
    (catch :found
      (org-with-wide-buffer
       (let* ((name (org-element-property :name context))
              (named-results (and name (org-babel-find-named-result name))))
         (goto-char (or named-results (org-element-property :end context)))
         (cond
          ;; Existing results named after the current source.
          (named-results
           (when (org-babel--clear-results-maybe hash)
             (org-babel--insert-results-keyword name hash))
           (throw :found (point)))
          ;; Named results expect but none to be found.
          (name)
          ;; No possible anonymous results at the very end of
          ;; buffer or outside CONTEXT parent.
          ((eq (point)
               (or (pcase (org-element-type (org-element-property :parent context))
                     ((or `section `org-data)
                      (org-element-property :end (org-element-property :parent context)))
                     (_ (org-element-property :contents-end
                         (org-element-property :parent context))))
                   (point-max))))
          ;; Check if next element is an anonymous result below
          ;; the current block.
          ((let* ((next (org-element-at-point))
                  (end (save-excursion
                         (goto-char
                          (org-element-property :end next))
                         (line-end-position)))
                  (empty-result-re (concat org-babel-result-regexp "$"))
                  (case-fold-search t))
             (re-search-forward empty-result-re end t))
           (forward-line 0)
           (when (org-babel--clear-results-maybe hash)
             (org-babel--insert-results-keyword nil hash))
           (throw :found (point)))))
     ;; ;; Ignore other elements.
     ;; (_ (throw :found nil))
       )
      ;; No result found.  Insert a RESULTS keyword below element, if
      ;; appropriate.  In this case, ensure there is an empty line
      ;; after the previous element.
      (when insert
        (save-excursion
          (goto-char (min (org-element-property :end context) (point-max)))
          (skip-chars-backward " \t\n")
          (forward-line)
          (unless (bolp) (insert "\n"))
          (insert "\n")
          (org-babel--insert-results-keyword
           (org-element-property :name context) hash)
          (point))))))

(defun oai-block-remove-result (&optional info keep-keyword)
  "Remove the result of the current source block.
INFO argument is currently ignored.
When KEEP-KEYWORD is non-nil, keep the #+RESULT keyword and just remove
the rest of the result."
  (interactive)
  (let ((location (oai-block-where-is-result nil info))
	(case-fold-search t))
    (when location
      (save-excursion
        (goto-char location)
	(when (looking-at org-babel-result-regexp)
	  (delete-region
	   (if keep-keyword (line-beginning-position 2)
	     (save-excursion
	       (skip-chars-backward " \r\t\n")
	       (line-beginning-position 2)))
	   (progn (forward-line) (org-babel-result-end))))))))



;; -=-= Fontify: help functions
(defun oai-block--fontify-markdown-subblocks (start end)
  "Fontify ```language ... ``` fenced mardown code blocks.
We search for begining of block, then for end of block, then fontify
 with `org-src-font-lock-fontify-block'.
Argument START and END are limits for searching."
  ;; (print (list "oai-block--fontify-markdown-subblocks" start end))
  (goto-char start)
  (let ((case-fold-search t))
    (while (and (< (point) end)
                (re-search-forward oai-block--markdown-beg-end-re end t))


      (let ((lang (match-string 1))
            (block-begin (match-end 0))
            (block-begin-begin (match-beginning 0)))
        ;; no ``` at the same line
        (when (and (or (not  lang)
                       (and lang (not (string-match-p "```" lang))))
                   (re-search-forward oai-block--markdown-end-re end t))
          (let ((block-end (match-beginning 0))
                (block-end-end (match-end 0)))
            ;; - fontify begin and end of markdown block
            (remove-text-properties block-begin-begin block-begin
                                    (list 'face '(org-block)))
            (remove-text-properties block-end block-end-end
                                    (list 'face '(org-block)))
            (add-text-properties
             block-begin-begin block-begin
             '(face org-block-begin-line))
            (add-text-properties
             block-end block-end-end
             '(face org-block-end-line))

            (remove-text-properties block-begin block-end '(face nil))
            ;; Add Org faces.
            (let ((src-face (nth 1 (assoc-string lang org-src-block-faces t))))
              (when (or (facep src-face) (listp src-face))
                (font-lock-append-text-property start end 'face src-face))
              (font-lock-append-text-property start end 'face 'org-block))

            ;; (put-text-property block-end block-end-end 'face 'org-block-end-line)
            ;; (unless (and lang (string-match-p "```" lang))
            (when (and lang
                       (not (string-match-p "```" lang))
                       (fboundp (org-src-get-lang-mode lang))) ; for org-src-font-lock-fontify-block
              ;; - fontify code inside markdown block
              (org-src-font-lock-fontify-block lang block-begin block-end)
              ;; - text property
              (put-text-property block-begin block-end
                                 'oai-markdown-block t)
              t)))))))

(defun oai-block--fontify-markdown-subblocks-shallow (lim-beg lim-end)
  "Fontify chat message prefixes like [ME:] with face.
Argument LIM-BEG ai block begining.
Argument LIM-END ai block ending."
  (let (sbeg send)
    (goto-char lim-beg)
    (prog1 (while (re-search-forward oai-block--markdown-end-re lim-end t)
             (setq sbeg (match-beginning 0))
             (setq send (match-end 0))
             (unless (oai-block--at-special-p send)
               (put-text-property sbeg send 'face '(org-meta-line))))
      (goto-char lim-end))))

(defun oai-block--fontify-org-tables (start end)
  "Set face for lines like Org tables.
For current buffer in position between START and END.
Executed in `font-lock-defaults' chain."
  (let (mbeg)
    (goto-char start) ; in case
    (while (re-search-forward "^[\s-]*|" end t)
      (setq mbeg (match-beginning 0)) ; (prop-match-beginning match))
      (unless (oai-block--at-special-p mbeg)
        (end-of-line)
        ;; (remove-text-properties mbeg (point)
        ;;                         (list 'face '(org-block)))
        (put-text-property mbeg (point)
                           'face 'org-table)
        t))))


(defun oai-block--fontify-markdown-headers (start end)
  "Fontify started with # character headers.
Argument START END are block begin and end, used as limits here."
    (goto-char start)
    (while (re-search-forward "^\\(#+\\)\\s-+\\(.*\\)$" end t)
      ;; (print (point))

      (let ((b1 (match-beginning 1))
            (e1 (match-end 1))
            ;; (b2 (match-beginning 2))
            (e2 (match-end 2))
            (hash-chars-length (1- (length (match-string 1))))
            (colors '(oai-block-m-header1 oai-block-m-header2 oai-block-m-header3 oai-block-m-header4)))
        ;; Group 1: the '#' chars
        (put-text-property b1 e1
                           'face
                           (if (<= hash-chars-length 3)
                               (nth hash-chars-length colors)
                             ;; else
                             'oai-block-m-header4))
        ;;   (setq i (1+ i)))
        ;; (when (looking-at "\\s-[1-9][).]\\s-")
           (goto-char e1)
           (when (re-search-forward "\\s-\\([1-9a-lA-L][).]\\)\\s-" (line-end-position) t)
             ;; (print (list (match-beginning 1)  e1))
             (when (eql (match-beginning 1)  (1+ e1))
               (put-text-property (match-beginning 1) (match-end 1)
                                  'face
                                  (if (<= hash-chars-length 3)
                                      (nth hash-chars-length colors)
                                    ;; else
                                    'oai-block-m-header4))))
        ;;   (put-text-property (match-beginning 1) (match-end 1)
        ;;                      'face (nth (1- i) colors))
        ;;   ;; (print (list "cyes" (match-string 1)))
        ;;   )
        ;; )
        ;; (put-text-property b1 e1
        ;;                    'face 'outline-2)
        ;; ;; Group 2: the header text
        ;; (put-text-property b2 e2
        ;;                    'face 'outline-1)
        ;; )
        (goto-char e2)))
    (goto-char end))

(defun oai-block--fontify-markdown-single-quotes-and-formatting (start end)
  "Fontify markdown features between START and END.
- Bold markers (*, ** and ***).
- Headers: '#' and header text.
Org vs Markdown -
- Markdown - formatting only applies to contiguous spans of text
 with the markers on the same line.
- Org - may be split with new line.
We dont support Org-like split.  LLMs commonly think that Org dont
support splitting."
  (let (e1 b2 e2)
    (goto-char start)
    ;; 1. *Bold*
    ;; (while (re-search-forward "\\(^\\|[^*]\\)\\(\\*\\*\\*\\|\\*\\*\\)" end t) ; lines not started with *
    (while (re-search-forward "\\*\\{1,3\\}\\w" end t) ; lines not started with *
      (goto-char (match-beginning 0))
      (if (re-search-forward "\\*\\{1,3\\}\\(\\w[^*]+\\)\\*\\{1,3\\}" (line-end-position) t)
      ;; (if (re-search-forward "\\*\\{2,3\\}\\(\\w[^*]+\\)\\*\\{2,3\\}" (line-end-position) t)
          (progn
            (setq e1 (match-end 0))
            (setq b2 (match-beginning 1)) ; **asd**
            (setq e2 (match-end 1))
            (unless (oai-block--at-special-p b2)


              ;; Only fontify the marker, not surrounding text
              (put-text-property b2 e2
                                 ;; 'face '(bold)))
                                 'face 'oai-bold))
            (goto-char e1))
        ;; else
        (forward-line)))
    ;; 2. `quote` RosyBrown1
    (goto-char start)
    (while (re-search-forward "`[^`]" end t) ; lines not started with *
      (goto-char (match-beginning 0))
      (if (re-search-forward "`\\([^`]+\\)`" (line-end-position) t)
          ;; (if (re-search-forward "`\\(\\([^`]+\\|\\)\\)`" (line-end-position) t)
          (progn
            (setq e1 (match-end 0))
            (setq b2 (match-beginning 1))
            (setq e2 (match-end 1))
            (unless (oai-block--at-special-p b2)


              ;; Only fontify the marker, not surrounding text
              (put-text-property b2 e2
                                 'face 'oai-block-quote)) ; org-clock-overlay org-agenda-restriction-lock
            ;; org-document-info
                                 ;; org-formula
                                        ; org-headline-done
                                 ;; 'face '(default)))
            (goto-char e1))
        ;; else
        (forward-line)))
    (goto-char end))) ;; Return t if performed work.


(defun oai-block--fontify-me-ai-chat-prefixes (lim-beg lim-end)
  "Fontify chat message prefixes like [ME:] with face.
Argument LIM-BEG ai block begining.
Argument LIM-END ai block ending."
  (let (sbeg send)
    (goto-char lim-beg)
    (prog1 (while (re-search-forward oai-block--chat-prefixes-re lim-end t)
             (setq sbeg (match-beginning 0))
             (setq send (match-end 0))
             (unless (oai-block--at-special-p send)
               ;; (goto-char send)
               ;; (remove-text-properties sbeg (point) (list 'face '(org-block)))
               (put-text-property sbeg send 'face 'oai-chat-role))) ; 'oai-block--me-ai-chat-prefixes-font-face
               ;; (put-text-property sbeg send 'face '(default)))) ; 'oai-block--me-ai-chat-prefixes-font-face
      (goto-char lim-end))))

(defun oai-block--fontify-latex-blocks (lim-beg lim-end)
  "Fontify LaTeX math blocks.
We search for \\[...\\] multiline \\(...\\) from LIM-BEG to LIM-END."
  (let (sbeg send)
    (goto-char lim-beg)
    ;; Multiline \\[ ... \\]
    (while (re-search-forward "^[\s-]*\\\\\\[\\(.\\|\n\\)*?\\\\\\]" lim-end t)
      ;; Mybe we should use two separate regexs?: "^[ \t]*\\\\\\[[ \t]*$" and "^[ \t]*\\\\\\][ \t]*$"
      (setq sbeg (match-beginning 0))
      (setq send (match-end 0))

      (unless (or (oai-block--at-special-p send t) ; multiline block with language
                  (oai-block--in-markdown-any-quotes-p send)) ; line
        (org-src-font-lock-fontify-block "latex" sbeg send))
      (goto-char send))
    ;; Inline \\( ... \\) - at one line
    (goto-char lim-beg)
    (while (re-search-forward "[^\\(]\\\\\(.*\\\\\)[^\\)]" lim-end t)
      (setq sbeg (match-beginning 0))
      (setq send (match-end 0))
      (unless (or (oai-block--at-special-p send t)
                  (oai-block--in-markdown-any-quotes-p send)) ; line
        (org-src-font-lock-fontify-block "latex" sbeg send))
      (goto-char send))
    (goto-char lim-end)))

;; -=-= Fontify: main
(defun oai-block--font-lock-fontify-markdown-and-org (limit)
  "Fontify markdown elements in ai blocks, up to LIMIT.
This is special fontify function, that return t when match found.
We insert advice right after `org-fontify-meta-lines-and-blocks-1' witch
called as a part of Org Font Lock mode configuration of keywords (in
`org-set-font-lock-defaults' and corresponding font-lock highlighting
rules in `font-lock-defaults' variable.
TODO: fontify if there is only end of ai block on page."
  ;; (print (list "oai-block--font-lock-fontify-markdown-and-org" (point) limit))
  (let ((case-fold-search t)
        beg end)
    (while (and (< (point) limit)
                (re-search-forward oai-block--ai-block-begin-re limit t))
      (setq beg (match-end 0))
      (if (re-search-forward oai-block--ai-block-end-re limit t)
          (setq end (match-beginning 0))
        ;; else - end of block not found, apply block to the limit
        (setq end limit))
      ;; - apply fontification
      ;; As a general rule, we apply the element (container) faces
      ;; first and then prepend the object faces on top.
      (save-match-data

        ;; [AI]: [ME]:
        (oai-block--fontify-me-ai-chat-prefixes beg end)
        ;; table
        (when oai-block-fontify-org-tables-flag
          (oai-block--fontify-org-tables beg end))
        ;; headers and *bold*
        (when oai-block-fontify-markdown-headers-and-formatting
          (oai-block--fontify-markdown-headers beg end)
          (oai-block--fontify-markdown-single-quotes-and-formatting beg end))
        ;; LaTeX startin with [ or (
        (when oai-block-fontify-latex
          (oai-block--fontify-latex-blocks beg end))
        ;; ```block
        (when oai-block-fontify-markdown-flag
          ;; (oai-block--fontify-markdown-subblocks-shallow beg end)
          (oai-block--fontify-markdown-subblocks beg end)))
      (goto-char end))
    ;; required by font lock mode:
    (goto-char limit))) ; return t

(defun oai-block--font-lock-fontify-markdown-blocks (limit)
  "Fontify markdown subblocks in ai blocks, up to LIMIT.
Used as separate function with `oai-block--font-lock-fontify-markdown-and-org'
for applying after others to replace smaller elements.
TODO: fontify if there is only end of ai block on page."
  ;; (print (list "oai-block--font-lock-fontify-markdown-and-org" (point) limit))
  (let ((case-fold-search t)
        beg end)
    (while (and (< (point) limit)
                (re-search-forward oai-block--ai-block-begin-re limit t))
      (setq beg (match-end 0))
      (if (re-search-forward oai-block--ai-block-end-re limit t)
          (setq end (match-beginning 0))
        ;; else - end of block not found, apply block to the limit
        (setq end limit))
      ;; - apply fontification
      ;; As a general rule, we apply the element (container) faces
      ;; first and then prepend the object faces on top.
      (save-match-data
        ;; ```block
        (when oai-block-fontify-markdown-flag
          ;; (oai-block--fontify-markdown-subblocks-shallow beg end)
          (oai-block--fontify-markdown-subblocks beg end)))
      (goto-char end))
    ;; required by font lock mode:
    (goto-char limit))) ; return t

(defun oai-block--insert-after (list pos element)
  "Insert ELEMENT at after position POS in LIST.
Used to inject font-locks to `org-font-lock-extra-keywords' variable."
  (nconc (take (1+ pos) list) (list element) (nthcdr (1+ pos) list)))


;; -=-= Fill-region, paragraph

(defmacro oai-block--apply-to-region-lines (func start end &rest args)
  "Apply FUNC to each line in region from START to END with ARGS.
START and END is a pointer.  FUNC is called with
\(line-start line-end . ARGS) for each line.
Executed inside `save-excursion'.
FUNC should place  point to to the  next line after execution  if end at
the end of the line."
  `(let ((end-marker (copy-marker ,end)))
     (save-excursion
       (goto-char ,start)
       (while (< (point) (marker-position end-marker))
         (let ((line-start (line-beginning-position)) ; may be replace to just (point)
               (line-end (line-end-position)))
           (if (< line-start line-end) ; not empty line
               (apply ,func line-start line-end (list ,@args))
             (forward-line)))))))

(defun oai-block-fill-region-as-paragraph (from to &optional justify nosqueeze squeeze-after)
  "Ignore lines that begin with \"< \".
For `fill-region-as-paragraph' that applied per lines.
Argument FROM TO JUSTIFY NOSQUEEZE SQUEEZE-AFTER is arguments of
fill-region-as-paragraph."
  (oai--debug "oai-block-fill-region-as-paragraph %s %s" from to)
  (goto-char (min from to))
  (if (not (or (looking-at "^> ")
               (looking-at "^[ \t]*\\(|\\|\\+-[-+]\\).*")))
      (funcall #'fill-region-as-paragraph from to justify nosqueeze squeeze-after)
    ;; else
    (goto-char to)
    (unless (bolp)
      (forward-line))))

(defun oai-block-fill-paragraph (&optional justify region)
  "Fill every line as paragraph in the current AI block.
Ignore code blocks that start with '```sometext' and end with '```'.
Optional argument JUSTIFY is parameter of `fill-paragraph'.
Optional argument REGION todo.
TODO: use `forward-paragraph' instead of `forward-line'.
Was causing freezing."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list (when current-prefix-arg 'full) t)))
  (ignore region)
  ;; inspired by `org-fill-element'
  (oai--debug "oai-block-fill-paragraph %s %s" (point) (current-buffer))

  (with-syntax-table org-mode-transpose-word-syntax-table
    (let ((element (oai-block-p))) ; TODO: replace with last replay only
      (when element
        ;; Determine the boundaries of the content
        (let ( ;; ai block range
              (beg (org-element-property :contents-begin element)) ; first line of content
              (end (org-element-property :contents-end element)) ; begining of #+end_ai
              ;; markdown block range
              block-start block-end)
          ;; Content exist?
          (when (or (not beg)
                  (not end))
              (error "Empty block"))
          ;; Ignore code blocks that start with "```sometext" and end with "```"
          (save-excursion
            (while (< beg end)
              (goto-char beg)
              ;; - search forward for markdkown begining from ai block begining
              (if (re-search-forward "^[ \t\f]*```\\w" end t) ; ex. "    ```elisp"
                  (progn
                    ;; (setq block-start (copy-marker (line-beginning-position))) ; save markdown block begining
                    (setq block-start (line-beginning-position)) ; save markdown block begining
                    ;; - search forward from current for markdown ending
                    (if (re-search-forward "^[ \t\f]*```[ \t\f]*$" end t) ; ex. "    ```      "
                        (progn
                          (setq block-end (copy-marker (line-beginning-position)))
                          ;; from begining of ai block to begin to markdown block
                          ;; FILL
                          (oai-block--apply-to-region-lines #'oai-block-fill-region-as-paragraph beg block-start justify)
                          ;; go to the end of markdown block
                          (goto-char (marker-position block-end))
                          (set-marker block-end nil)
                          (forward-line 1)
                          (setq beg (point)))
                      ;; else - not found end of block
                      ;; FILL
                      (oai-block--apply-to-region-lines #'oai-block-fill-region-as-paragraph beg block-start justify)
                      ;; (set-marker block-start nil)
                      (setq beg end)))
                ;; else - no markdown block begining - apply to whole block
                ;; FILL
                (oai-block--apply-to-region-lines #'oai-block-fill-region-as-paragraph beg end justify)
                (setq beg end)))
            t))))))


(defun oai-block--fill-region (&optional pos stream)
  "Fill ai block for not streaming and for streaming.
If STREAM is non-nil this function  called after insertion of a chink of
text, otherwise after insertion of full response.
Ignore markdown blocks, quoted text and Org tables.
Optional argument POS position before insertion not used, TODO: set to
to begin of paragraph."

  (interactive)
  (ignore pos)
  ;; (setq _pos _pos) ; for melpazoid
  (oai--debug "oai-block--fill-region %s %s" stream (point))
  (save-excursion
    (if stream
        ;; if at current line ``` or we are at begining of markdown block in ai block.
        (let ((case-fold-search t) ; if nil
              (p (point)))
          (unless
              ;; not markdown blocks
              (or (with-syntax-table org-mode-transpose-word-syntax-table
                    ;; backward for ai block
                    (when (re-search-backward oai-block--ai-block-begin-re nil t)
                      (goto-char p)
                      ;; backward for markdown block "begin". Same logic as in finction `oai-block-tags--is-special'
                      (when (re-search-backward oai-block--markdown-begin-re (match-end 0) t)
                        (goto-char p)
                        ;; backward for markdown block "end" after "begin"
                        (not (re-search-backward oai-block--markdown-end-re nil t)))))
                  ;; not quotes and not tables
                  (progn (goto-char p)
                         (beginning-of-line)
                         (or (looking-at "^> ") ; from `oai-block-fill-region-as-paragraph'
                             (looking-at "^[ \t]*\\(|\\|\\+-[-+]\\).*")))) ; skip tables
            (goto-char p)
            (fill-region-as-paragraph (line-beginning-position) (line-end-position))))
      ;; else not stream, single response. We add hack to skip markdown blocks.
      (oai-block-fill-paragraph) ; fill per line. wrapped in save-excursion inside.
      )))

;; (defun my/org-fill-element-advice (orig-fun &optional justify)
;;   "Advice around `org-fill-element`.
;; If at headline, skip filling. Otherwise call original function."
;;   (let ((element (save-excursion (end-of-line) (org-element-at-point))))
;;     (unless (oai-block-tags--markdown-fenced-code-body-get-range)
;;       (funcall orig-fun justify))))

;; (advice-add 'org-fill-element :around #'my/org-fill-element-advice)


;; (defun oai-restapi--forward-paragraph (arg)
;;   "Normal with `forward-paragraph' Skipping markdown blocks.
;; Works for positive ARG now only, negative not supported now."
;;   (print (list "oai-restapi--forward-paragraph" arg))
;;   (funcall #'forward-paragraph arg)
;;   (or arg (setq arg 1))
;;   (when-let* ((r (oai-block-tags--markdown-fenced-code-body-get-range))
;;                     (beg (car r)) ; after header
;;                     (end (cadr r))) ; at end line
;;     (when (< arg 0) (not (bobp))
;;           (when (> end (point))
;;             (goto-char beg)
;;             (forward-line -1)))
;;     (when (> arg 0) (not (eobp))
;;         ;; inside or at the first line? if at first line, do nothin, if in the middle of mardkown, then go to the end
;;         (unless (save-excursion (forward-line -1) (eq beg (point)))
;;           (goto-char end)
;;           (forward-line)))))


(provide 'oai-block)
;;; oai-block.el ends here
