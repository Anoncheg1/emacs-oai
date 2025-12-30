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

;; Changelog
;; - DONE: complete is short for completion
;; - DONE: oai-block-get-info fail if there is nothing in block.
;; - DONE: rename all CONTEXT to ELEMENT because context cause confusing (Org terms).
;; - DONE: rename all except interface functions to "oai-block-" prefix.


;;; Code:
;; -=-= includes
(require 'org)
(require 'org-element)
(require 'org-macs)
(require 'cl-lib) ; for `cl-letf', cl-defun, cl-loop, cl-case
(require 'oai-debug)

;; -=-= variables
(defcustom oai-block-fontify-markdown-flag t
  "Non-nil means enable fontinfication for ```lang blocks."
  :type 'boolean
  :group 'oai)

(defcustom oai-block-fontify-org-tables-flag nil
  "Non-nil means enable fontinfication for Org tables."
  :type 'boolean
  :group 'oai)

(defconst oai-block--ai-block-begin-re "^#\\+begin_ai.*$")
(defconst oai-block--ai-block-end-re "^#\\+end_ai.*$")

(defvar oai-block--markdown-begin-re "^[\s-]*```\\([^ \t\n[{]+\\)[\s-]?\n")
(defvar oai-block--markdown-end-re "^[\s-]*```[\s-]?$")
(defvar oai-block--chat-prefixes-re "^[ \t]*\\[\\([A-Z_]+\\)\\(:\\]\\|\\]:\\)\\s-*"
  "Prefix should be at the begining of the line with spaces or without.
Or roles regex.")

(defcustom oai-block-parse-part-hook nil
  "Call hook function with raw string of current block after role prefix.
Implemented as a list of functions that called with two argument content
string after prefix and role prefix as a symbol from from
`oai-block-roles'.  Executed from left to right and pass result content
string to each other.  Executed at step of reading ai block from raw
content of buffer before any processing but after splitting to parts."
  :type 'hook
  :group 'oai)

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

;; (cl-defun oai-block--check-info-key (&key info key)
;;   (not (eql 'x (alist-get key info 'x))))

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

(defun oai-block-get-content (&optional element)
  "Extracts the text content of the #+begin_ai...#+end_ai block.
ELEMENT is the element of the special block.

Will expand noweb templates if an `oai-noweb' property or
`noweb' header arg is \"yes\".
Use ELEMENT only in current moment, if buffer modified you will need new
ELEMENT."
  (let* ((element (or element (oai-block-p)))
         (content-start (org-element-property :contents-begin element))
         (content-end (org-element-property :contents-end element))
         (unexpanded-content (if (or (not content-start) (not content-end))
                                 (error "Empty block")
                               ;; else
                               (string-trim (buffer-substring-no-properties content-start content-end))))
         (info (oai-block-get-info element))
         (noweb-control (or (alist-get :noweb info nil)
                            (org-entry-get (point) "oai-noweb" 1)
                            "no"))
         (content (if (string-equal-ignore-case "yes" noweb-control)
                      (org-babel-expand-noweb-references (list "markdown" unexpanded-content))
                    unexpanded-content)))
    (string-trim content)))
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

;; -=-= fn: collect-chat-messages
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


;; (progn (re-search-forward oai-block--chat-prefixes-re)
;;        (print (match-string 1)))
(defvar oai-block-roles '(("SYS"		system)
                          ("ME"		user)
                          ("AI"		assistant)
                          ("unknown_role"	assistant)
                          ("missing_role"	user)))
;; Get value by key: (cadr (assoc-string "unknown_role" oai-block-roles))

(defun oai-block--roles-get-key (role-value)
  "Return string prefix for ROLE-VALUE symbol.
Uses `oai-block-roles' variable."
  (or (car (seq-find (lambda (pair) (eq (cadr pair) role-value)) oai-block-roles))
      (error "Role symbol not found in oai-block-roles")))

;; (oai-block--roles-get-key 'user) ; => "ME"

;; Parse parts and build messages
(defun oai-block--parse-part (pos-beg pos-end)
  "Get part of chat as a plist with :role and :content in current buffer.
Positions POS-BEG POS-END used as limits.
Skip AI_REASON role string.
If prefix found two times error is thrown.
Uses `oai-block-roles' variable and `oai-block--chat-prefixes-re'.
If content is empty string return nil otherwise plist."
  (save-excursion
    (goto-char pos-beg)
    ;; - find prefix
    (let (content ; after prefix or from pos
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
        ;; get role symbol
        (when role-str
          (setq role (cadr (assoc-string role-str oai-block-roles))))
        (setq role (if role-str
                       (or role (cadr (assoc-string "unknown_role" oai-block-roles)))
                     ;; else - no role-str
                     (cadr (assoc-string "missing_role" oai-block-roles)) ; user for the first message
                     ))
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

;; -=-= stringify-chat-messages

;; [[file:~/sources/emacs-oai/oai-restapi.el::1986::(cl-defun oai-restapi--stringify-chat-messages (messages &optional &key]]
(defun oai-block--stringify-chat-messages (messages &optional default-system-prompt)
  "Convert a chat message to a string.
MESSAGES is a vector of (:role :content) pairs.  :role can be
\='system, \='user or \='assistant.
If DEFAULT-SYSTEM-PROMPT non-nil, a [SYS] prompt is prepended if the
first message is not a system message, otherwise DEFAULT-SYSTEM-PROMPT
argument is ignored.
Uses `oai-block-roles' variable for mapping roles to prefixes."
  (let ((messages (if (and default-system-prompt
                           (not (eql (plist-get (aref messages 0) :role) 'system)))
                      (cl-concatenate 'vector (vector (list :role 'system :content default-system-prompt)) messages)
                    messages)))
    (cl-loop for (_ role _ content) across messages
             collect (let ((role-str (oai-block--roles-get-key role)))
                       (concat "[" role-str "]: " content))
             into result
             finally return (string-join result "\n\n"))))


;; -=-= Interactive

(defun oai-mark-last-region ()
  "Mark the last prompt in an oai block."
  (interactive)
  (when-let* ((regions (reverse (oai-block--chat-role-regions)))
              (last-region-end (pop regions))
              (last-region-start (pop regions)))
        (goto-char last-region-end)
        (push-mark last-region-start t t)))

(defun oai-mark-region-at-point ()
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

(defun oai-forward-section (&optional arg)
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

(defun oai-kill-region-at-point (&optional arg)
  "Kill the prompt at point.
The numeric ARG can be used for killing the last n."
  (interactive "P")
  (cl-loop repeat (or arg 1)
           do (when-let ((region (oai-mark-region-at-point)))
                (cl-destructuring-bind (start . end) region
                  (kill-region end start)))))

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
      (when outside-scope (narrow-to-region visible-beg visible-end)) ;; ---- NARROW
      ))
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


;; -=-= Markdown block, fontify mostly
(defun oai-block--fontify-markdown-subblocks (start end)
  "Fontify ```language ... ``` fenced mardown code blocks.
We search for begining of block, then for end of block, then fontify
 with `org-src-font-lock-fontify-block'.
Argument START and END are limits for searching."
  (goto-char start)
  (let ((case-fold-search t))
    (while (and (< (point) end)
                (re-search-forward oai-block--markdown-begin-re end t))
      (let* ((lang (match-string 1))
             (block-begin (match-end 0))
             (block-begin-begin (match-beginning 0)))
        ;; (print (list "re-search-forward4" (point) end))
        (when (re-search-forward oai-block--markdown-end-re end t)
          (let ((block-end (match-beginning 0))
                (block-end-end (match-end 0)))
            (when (fboundp (org-src-get-lang-mode lang)) ; for org-src-font-lock-fontify-block
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
              ;; - fontify code inside markdown block
              (org-src-font-lock-fontify-block lang block-begin block-end)
              t)))))))

(defun oai-block--in-markdown (pos &optional lim-beg)
  "Check if POS in markdown block, quoted or is a table.
Optional argument LIM-BEG is ai block begining position.
Return t if pos in markdown block, table or quote.
Side-effect: set pointer position to POS.
Same as `oai-block-tags--is-special'."
  (goto-char pos)
  (prog1 (or
          ;; not markdown blocks
          ;; backward for markdown block "begin"
          (when (re-search-backward oai-block--markdown-begin-re lim-beg t)
            (goto-char pos)
            ;; backward for markdown block "end" after "begin"
            (not (re-search-backward oai-block--markdown-end-re (match-end 0) t)))
          (progn (goto-char pos)
                 (beginning-of-line)
                 (looking-at "^> ")))
    (goto-char pos)))

(defun oai-block--fontify-org-tables (start end)
  "Set face for lines like Org tables.
For current buffer in position between START and END.
Executed in `font-lock-defaults' chain."
  (let (mbeg)
    (goto-char start) ; in case
    (while (re-search-forward "^[\s-]*|" end t)
      (setq mbeg (match-beginning 0)) ; (prop-match-beginning match))
      (unless (oai-block--in-markdown mbeg start)
        (end-of-line)
        ;; (remove-text-properties mbeg (point)
        ;;                         (list 'face '(org-block)))
        (put-text-property mbeg (point)
                           'face 'org-table)
        t))))

(defun oai-block--fontify-me-ai-chat-prefixes (lim-beg lim-end)
  "Fontify chat message prefixes like [ME:] with face.
Argument LIM-BEG ai block begining.
Argument LIM-END ai block ending."
  (let (sbeg send)
    (goto-char lim-beg)
    (prog1 (while (re-search-forward oai-block--chat-prefixes-re lim-end t)
             (setq sbeg (match-beginning 0))
             (setq send (match-end 0))
             (unless (oai-block--in-markdown send lim-beg)
               ;; (goto-char send)
               ;; (remove-text-properties sbeg (point) (list 'face '(org-block)))
               (put-text-property sbeg send 'face '(bold)))) ; 'oai-block--me-ai-chat-prefixes-font-face
      (goto-char lim-end))))

(defun oai-block--font-lock-fontify-ai-subblocks (limit)
  "Fontify markdown subblocks in ai blocks, up to LIMIT.
This is special fontify function, that return t when match found.
We insert advice right after `org-fontify-meta-lines-and-blocks-1' witch
called as a part of Org Font Lock mode configuration of keywords (in
`org-set-font-lock-defaults' and corresponding font-lock highlighting
rules in `font-lock-defaults' variable.
TODO: fontify if there is only end of ai block on page"

      (let ((case-fold-search t)
            beg end)
        (while (and (< (point) limit)
                    (re-search-forward oai-block--ai-block-begin-re limit t))
          (setq beg (match-end 0))
          (if (re-search-forward oai-block--ai-block-end-re nil t)
              (setq end (match-beginning 0))
            ;; else - end of block not found, apply block to the limit
            (setq end limit))
          ;; - apply fontification
          (save-match-data
            (oai-block--fontify-me-ai-chat-prefixes beg end))
          (when oai-block-fontify-markdown-flag
            (save-match-data
              (oai-block--fontify-markdown-subblocks beg end)))
          (when oai-block-fontify-org-tables-flag
            (save-match-data
              (oai-block--fontify-org-tables beg end)))
          (goto-char end))
        ;; required by font lock mode:
        (goto-char limit))) ; return t

(defun oai-block--insert-after (list pos element)
  "Insert ELEMENT at after position POS in LIST."
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


(provide 'oai-block)
;;; oai-block.el ends here
