;;; oai-block.el --- oai special block helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Robert Krahn and contributers
;; Copyright (C) 2025 github.com/Anoncheg1
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Author: github.com/Anoncheg1,codeberg.org/Anoncheg
;; Keywords: org, ai, llm, url, http
;; URL: https://github.com/Anoncheg1/oai
;; Version: 0.1,  Fork from orig. version: 0.5.6 (commit cc4a4eb778e4689573ebd2d472b8164f4477e8b8)
;; Created: 20 Aug 2025
;; Package-Requires: ((emacs "27.1"))

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

;;; Changelog
;; - DONE: complete is short for completion
;; - DONE: oai-block-get-info fail if there is nothing in block.
;; - DONE: rename all CONTEXT to ELEMENT because context cause confusing (Org terms).
;; - DONE: rename all except interface functions to "oai-block-" prefix.

;;; Commentary:

;; Defines functions for dealing with #+begin_ai..#+end_ai special blocks

;; None Org babel: We choose not to fake as babel source block and use
;; functionality because it require too much advices.

;; Note Org terms:
;; - element - "room" you are in (e.g., a paragraph) (TYPE PROPS) (org-element-at-point)
;; - context - "furniture" you are touching within that room (e.g., a bold word, a link). (TYPE PROPS) (org-element-context)
;; - org-dblock-start-re
;;
;; Content begin-end
;; (org-element-property :contents-begin  (oai-block-p)) return next line after #+begin
;; (org-element-property :contents-end  (oai-block-p)) return #+end line position
;; may be fixed with org-element-put-property, but it is not KISS.

;;; Code:
;;; -=-= all
(require 'org)
(require 'org-element)
(require 'org-macs)

(when (and (boundp 'org-protecting-blocks) (listp org-protecting-blocks))
  (add-to-list 'org-protecting-blocks "ai"))

(when (boundp 'org-structure-template-alist)
  (add-to-list 'org-structure-template-alist '("A" . "ai")))


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

(defun oai-block-element-by-marker (marker)
  "Get ai block at MARKER position at marker buffer.
Used in prompt engineering only: oai-prompt.el."
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char marker)
      (oai-block-p))))

(defun oai-block-get-info (&optional element no-eval)
  "Parse the header of #+begin_ai...#+end_ai block.
`ELEMENT' is the element of the special block.
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

(defun oai-block--string-equal-ignore-case (string1 string2)
  "Helper for backwards compat.
STRING1 and STRING2 are strings.  Return t if they are equal
ignoring case."
  (eq 't (compare-strings string1 0 nil string2 0 nil t)))

(defun oai-block-get-content (&optional element)
  "Extracts the text content of the #+begin_ai...#+end_ai block.
`ELEMENT' is the element of the special block.

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
         (content (if (oai-block--string-equal-ignore-case "yes" noweb-control)
                      (org-babel-expand-noweb-references (list "markdown" unexpanded-content))
                    unexpanded-content)))
    content))

(defun oai-block--get-request-type (info)
  "Look at the header of the #+begin_ai...#+end_ai block.
returns the type of request.  `INFO' is the alist of key-value
pairs from `oai-block-get-info'."
  (cond
   ((not (eql 'x (alist-get :chat info 'x))) 'chat)
   ((not (eql 'x (alist-get :completion info 'x))) 'completion)
   ((not (eql 'x (alist-get :complete info 'x))) 'completion)
   ((not (eql 'x (alist-get :image info 'x))) 'image)
   ((not (eql 'x (alist-get :sd-image info 'x))) 'sd-image)
   ((not (eql 'x (alist-get :local info 'x))) 'local-chat)
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

;; (defmacro oai-block--let-params (info definitions &rest body)
;;   "A specialized `let*' macro for Oai parameters.
;; DEFINITIONS is a list of (VARIABLE &optional DEFAULT-FORM &key TYPE).
;; TYPE can be 'number, 'bool, 'string, or 'identity (no conversion).
;; Return one of:
;; - nil symbol, if key/property not specified or explicit nil (after type processing).
;; - Processed value of parameter (e.g., t/nil for bool).
;; Parameters are sourced from:
;; 1. From Oai block header `info' alist. (e.g., :model \"gpt-4\")
;; 2. Org inherited property. (e.g., #+PROPERTY: model gpt-4)
;; 3. DEFAULT-FORM."
;;   `(let* ,(cl-loop for def-item in definitions
;;                    collect
;;                    (let* ((sym (car def-item))
;;                           (default-form (cadr def-item))
;;                           (type (cadr (member :type def-item)))
;;                           (key (intern (concat ":" (symbol-name sym))))
;;                           (prop-name (symbol-name sym)))
;;                      `(,sym (let ((val (or (let* ((v1 (assoc ,key info))
;;                                                   (v2 (cdr v1)))
;;                                              (if (and v1 (not v2))
;;                                                  "nil"
;;                                                v2))
;;                                            (org-entry-get-with-inheritance ,prop-name)
;;                                            ,@(when default-form `(,default-form)))))

;;                               (cond ((eql type 'number)
;;                                      (cond ((null val) nil)
;;                                            ((and (stringp val) (string= val "nil")) nil)
;;                                            ((stringp val) (string-to-number val))
;;                                            ((numberp val) val) (t val)))
;;                                     ((eql type 'bool)
;;                                      (cond ((null val) nil) ((eq val t) t) ((stringp val) (if (member (downcase val) '("t" "true" "yes" "on" "1")) t nil))
;;                                            (t nil)))
;;                                     ((eql type 'string)
;;                                      (cond ((null val) nil) ((stringp val) val)
;;                                            (t (prin1-to-string val))))
;;                                     ((eql type 'identity) val) (t val))
;;                               ))))  ; Default no-op
;;      ,@body))

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
1. From Oai block header `INFO' alist.  (e.g., :model \"gpt-4\")
2. Org inherited property. (e.g., #+PROPERTY: model gpt-4)
3. DEFAULT-FORM."
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


(defvar oai-block--roles-regex "\\[SYS\\]:\\|\\[ME\\]:\\|\\[ME:\\]\\|\\[AI\\]:\\|\\[AI_REASON\\]:")

(defun oai-block--chat-role-regions ()
  "Splits the special block by role prompt.
Return line begining positions of first line of content, roles, #+end_ai
line."
  (if-let* ((element (oai-block-p))
            (content-start (org-element-property :contents-begin element))
            (content-end (org-element-property :contents-end element)))
      (let ((result (save-match-data
                      (save-excursion
                        (goto-char content-start)
                        (cl-loop with result
                                 while (search-forward-regexp oai-block--roles-regex content-end t) ; todo, make as global variable
                                 do (push (match-beginning 0) result)
                                 finally return result)))))
        (if result
            (cl-concatenate 'list (list content-start) (reverse result) (list content-end))
          (list content-start content-end)))))



;;; -=-= Interactive

(defcustom oai-block-fontify-markdown t
  "Fontinfy ```lang blocks."
  :type 'boolean
  :group 'oai)

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
      (push-mark end t t)
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
        (progn (push-mark nil t) ; save position
               (goto-char end))
      ;; else - backward
      (let ((prev (cl-find-if (lambda (x) (>= (1- start) x)) (reverse regions))))
        ;; (oai--debug "oai-forward-section2 %s %s %s" (>= (point) start) prev start)
        (when  prev ;; (>= (point) start))
          (if (and (> (point) start) ; if at the middle of first section
                   (not prev))
              (progn (push-mark nil t) ; save position
                     (goto-char start))
            ;; else
            (push-mark nil t) ; save position
            (goto-char (cl-find-if (lambda (x) (>= (1- start) x)) (reverse regions)))))))))

(defun oai-kill-region-at-point (&optional arg)
  "Kill the prompt at point.
The numeric `ARG' can be used for killing the last n."
  (interactive "P")
  (cl-loop repeat (or arg 1)
           do (when-let ((region (oai-mark-region-at-point)))
                (cl-destructuring-bind (start . end) region
                  (kill-region end start)))))

;;; -=-= Markers

(defun oai-block--get-content-end-marker (&optional element)
  "Return a marker for the :contents-end property of ELEMENT.
Used in `oai-call-block'"
  (let ((el (or element (oai-block-p))))
    (let ((contents-end-pos (org-element-property :contents-end el)))
      (when contents-end-pos
        (copy-marker contents-end-pos)))))

(defun oai-block-get-header-marker (&optional element)
  "Return marker for ai block at current buffer at current positon.
Use ELEMENT only in current moment in element buffer."
  (let ((el (or element (oai-block-p))))
    ;; (with-current-buffer (org-element-property :buffer el)
    (if el
        (save-excursion
          (goto-char (1+ (org-element-property :contents-begin el))) ; 1+ to have something before marker for correct work.
          (forward-line -1)
          (copy-marker (point))))))

;;; -=-= Result

(defun oai-block-insert-result-message (message header-marker)
  "Insert MESSAGE to #+RESULT of block in buffer of HEADER-MARKER."
  (with-current-buffer (marker-buffer header-marker)
    (save-excursion
      (goto-char header-marker)
      (oai-block-insert-result message))))

(defun oai-block-insert-result (result &optional result-params hash exec-time)
  "Modified `org-babel-insert-result' function.
Insert RESULT into the current buffer.
TODO: EXEC-TIME."
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
        (goto-char (org-babel-result-end)) (setq beg (point-marker))))
      (goto-char beg) (insert (concat result "\n"))
      (setq end (copy-marker (point) t))
      (org-babel-examplify-region beg end "")
      ;; finally
      (when outside-scope (narrow-to-region visible-beg visible-end)) ;; ---- NARROW
      ))
  t)

(defun oai-block-where-is-result (&optional insert _info hash)
  "Modified `org-babel-where-is-src-block-result' function."
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
                     (_ (org-element-contents-end
                         (org-element-property :parent context))))
                   (point-max))))
          ;; Check if next element is an anonymous result below
          ;; the current block.
          ((let* ((next (org-element-at-point))
                  (end (save-excursion
                         (goto-char
                          (org-element-post-affiliated next))
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


;;; -=-= Markdown block, fontify mostly
(defvar oai-block--markdown-begin-re "^[\s-]*```\\([^ \t\n[{]+\\)[\s-]?\n")
(defvar oai-block--markdown-end-re "^[\s-]*```[\s-]?$")

(defun oai-block--fontify-markdown-subblocks (start end)
  "Fontify ```language ... ``` fenced mardown code blocks.
 We search for begining of block, then for end of block, then fontify
 with `org-src-font-lock-fontify-block'."
  (goto-char start)
  (let ((case-fold-search t))
    (while (and (< (point) end)
                (re-search-forward oai-block--markdown-begin-re end t))
      (let* ((lang (match-string 1))
             (block-begin (match-end 0)))
        ;; (print (list "re-search-forward4" (point) end))
        (when (re-search-forward oai-block--markdown-end-re end t)
          (let ((block-end (match-beginning 0)))
            (when (fboundp (org-src-get-lang-mode lang)) ; for org-src-font-lock-fontify-block
              (org-src-font-lock-fontify-block lang block-begin block-end)
              t
              )))))))

(defun oai-block--font-lock-fontify-ai-subblocks (limit)
  "Fontify markdown subblocks in ai blocks, up to LIMIT.
This is special fontify function, that return t when match found.
We insert advice right after `org-fontify-meta-lines-and-blocks-1' witch
called as a part of Org Font Lock mode configuration of keywords (in
`org-set-font-lock-defaults' and corresponding font-lock highlighting
rules in `font-lock-defaults' variable."
  (if oai-block-fontify-markdown
      (let ((case-fold-search t)
            (ret))
        (while (and (re-search-forward "^#\\+begin_ai[^\n]*\n" limit t)
                    (< (point) limit))
          (let ((beg (match-end 0)))
            (when (re-search-forward "^#\\+end_ai.*$" nil t)
              (let ((end (match-beginning 0)))
                (save-match-data
                  (setq ret (oai-block--fontify-markdown-subblocks beg end)))
                ))))
        ;; required by font lock mode:
        (goto-char limit)
        ret
        )))

(defun oai-block--insert-after (list pos element)
  "Insert ELEMENT at after position POS in LIST."
  (nconc (take (1+ pos) list) (list element) (nthcdr (1+ pos) list)))


;;; provide
(provide 'oai-block)
;;; oai-block.el ends here
