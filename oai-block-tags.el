;;; oai-block-tags.el --- Handling links inside ai block  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 github.com/Anoncheg1
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

;; Link is Org links, tags is AI links in form of @something.
;
;; Main functions:
;; - `oai-block-tags-replace' is main function for replace links.
;; - `oai-block-tags-get-content-ai-messages' is used to prepare ai block for request
;; - `oai-block-tags-get-content' is used to get content for by links, tags or noweb

;; How this works?
;; - for highlighting this add hook to Org with font-lock logic
;; - for replacing tags we operate at string variable and grab things from buffers
;; - noweb and links are extended after splitting block to messages

;; Steps:
;; 1) find tags/links
;; 2) with ol.el we find target of link and compose markdown block as a string
;; 3) use `oai-block-tags--replace-last-regex-smart' to replace substring.
;;
;; We support @links:
;; - @Backtrace
;; - #PATH - directory/file
;; - @name - same to Org [[target]]
;;
;; We support Org ol.el package links:
;; - [[PATH]]
;;
;; We support "org-links" package new links:
;; - [[PATH::NUM::LINE]]
;; - [[PATH::NUM-NUM::LINE]] - range
;; - [[PATH::NUM-NUM]] - range
;; - [[PATH::NUM]] creating
;;
;; To check links use "C-c ." key, or M-x oai-expand-block.
;; - `oai-block-tags--get-content-at-point' - get string
;;  representation of some position for LLM to add to message

;; *Position and line number*
;; - `line-number-at-pos'
;; - `oai-block-tags--line-num-to-positon'

;; -=-= includes
(require 'org)
(require 'ol)
(require 'oai-debug)
(require 'oai-block)
;; TODO!!!!!!!!!!!
;; for: oai-restapi--modify-vector-last-user-content,
;; oai-restapi--modify-vector-content, oai-restapi-add-max-tokens-recommendation
;; oai-restapi--get-length-recommendation
;; (require 'oai-restapi) ; disabled because of reverse dependency with oai-restapi
(require 'org-links nil 'noerror)

;;; Code:
;; -=-= variables

(defcustom oai-block-tags-backtrace-max-lines 12
  "Max lines to get from Backtrace buffer from begining.
All lines are rarely required, first 4-8 are most imortant."
  :type 'integer
  :group 'oai)

(defcustom oai-block-tags-use-simple-directory-content-flag nil
  "Non-nil means use `directory-files' with simple list of item.
Otherwise ls command used.  Also `directory-files-and-attributes' may be
used."
  :type 'boolean
  :group 'oai)

(defcustom oai-block-tags-error-on-missing-link-flag t
  "Non-nil means signal error for not found link.
Used to set `org-link-search-must-match-exact-headline' before
`org-link-search' function call."
  :type 'boolean
  :group 'oai)

(defcustom oai-block-tags-check-double-targets-found-flag t
  "Non-nil means signal error if link in ai block point to targets in same file."
  :type 'boolean
  :group 'oai)

(defcustom oai-block-tags-noweb-split-messages t
  "Non-nil means after expansion of noweb message is splitted if changed.
Used in `oai-block-tags-get-content'."
  :type 'boolean
  :group 'oai)

(defcustom oai-block-tags-tagslinks-split-messages t
  "Non-nil means after expansion of links message is splitted if changed.
Used in `oai-block-tags-get-content'."
  :type 'boolean
  :group 'oai)

(defvar oai-block-tags--regexes-backtrace "@\\(Backtrace\\|B\\([\s-]\\|$\\)\\)")

(defvar oai-block-tags--regexes-path "\\(^\\|[\s-]\\)@\\(\\(\\.\\.?/\\|\\.\\.?\\\\\\|\\.\\.?\\|[A-Za-z]:\\\\\\|~[a-zA-Z0-9_.-]*/*\\|/\\|\\\\\\)[a-zA-Z0-9_./\\\\-]*\\)"
  "Unix Posix and Windows, currently we support Linux only.
See: .
[[file:./doc.org::*Regex: file path][Regex: file path]]
and
[[file:./tests/oai-tests-block-tags.el::94:
:;; -=-= Test: oai-block-tags--regexes-path]].")

;; (defvar oai-block-tags--markdown-prefixes '(:backtrace "backtrace"
;;                                             :path-directory "shell")
;;   "Right after ``` markdown block begining.")


(defvar oai-block-tags-org-blocks-types '(comment-block center-block dynamic-block example-block
                                                        export-block quote-block special-block
                                                        src-block verse-block inline-src-block
                                                        latex-fragment) ; check: center-block dynamic-block
                                                          ; add: ? footnote-definition? inline-src-block?
  "Org block types that we wrap to markdown and may get by the first line.")


(defvar oai-block-tags--org-link-any-re (cl-letf (((symbol-function 'org-link-types)
                                                   (lambda () (list "file"))))
                                          ;; set to nil
                                          (let (org-link-types-re ; ret
                                                ;; org-link-any-re
                                                org-link-types-re
                                                org-link-angle-re org-link-plain-re) ;; org-link-bracket-re
                                            (org-link-make-regexps) ; constructor of org-link-types-re, org-link-angle-re, org-link-plain-re, org-link-bracket-re
                                            ;; org-link-types-re
                                            ))
  "`org-link-any-re' but with one type \"file\" in `org-link-types'.")

;; -=-= @Backtrace

(defun oai-block-tags--take-n-lines (string n)
  "Return a string with the first N lines from STRING.
If N exceeds the number of lines, return all lines.  If N <= 0, return
an empty string."
  (let* ((lines (string-split string "\n"))
         (lines-to-keep (cl-subseq lines 0 (min (max 0 n) (length lines)))))
    (mapconcat #'identity lines-to-keep "\n")))


(defun oai-block-tags--get-backtrace-buffer-string ()
  "Return the contents of the *Backtrace* buffer as a string, or nil.
Nil if buffer does not exist."
  (let ((buf (get-buffer "*Backtrace*")))
    (when buf
      (with-current-buffer buf
        (string-trim (substring-no-properties (buffer-string)))))))

; -=-= Links: Files & Directories

(defvar oai-block-tags-get-directory-switches "-AltGg")

(defun oai-block-tags--get-directory-content (path-string)
  "Return string with list of files at PATH-STRING."
  (if oai-block-tags-use-simple-directory-content-flag
      (concat (apply #'mapconcat #'identity (directory-files path-string)  '("\n")))
    ;; else
    (let* ((dired-listing-switches oai-block-tags-get-directory-switches)
           (buf (dired-noselect path-string))
           (kill-buffer-query-functions nil))
      (unwind-protect
          (with-current-buffer buf
            (buffer-substring-no-properties (point-min) (point-max)))
        (kill-buffer buf)))))


(defun oai-block-tags--filepath-to-language (path-or-mode-string)
  "Get short name of language that for path major mode string.
PATH-OR-MODE-STRING may be, and we check in this order:
- a symbol, like value of '`major-mode' variable.
- path of file.
- string with name of mode like \"emacs-lisp-mode\" - \"elisp\" will be
 returned
- directory
- .ai file-path
Uses Org babel source block `org-src-lang-modes' names, return left for
 right.
First we check `auto-mode-alist' and then just try to interpret as major
 mode line.
Return  name of language."
  (oai--debug "oai-block-tags--filepath-to-language %s" path-or-mode-string)
  ;; symbol - get "emacs-lisp-mode" or "nil"
  (let* ((symb (symbolp path-or-mode-string))
         (mode-symbol-string (if symb
                                 (symbol-name path-or-mode-string)
                               ;; else - string - path or mode
                               ;; (print (list "bvb" path-or-mode-string (symbol-name (assoc-default path-or-mode-string auto-mode-alist 'string-match))))
                               ;; (or (when (string-match "\\.ai\\'" path-or-mode-string)
                               ;;       "ai")
                               (symbol-name (assoc-default path-or-mode-string auto-mode-alist 'string-match))))
         (mode-symbol-string (if (and (not symb) (string-equal mode-symbol-string "nil"))
                                 path-or-mode-string ; string with mode name
                               ;; else
                               mode-symbol-string))
         ;; for "emacs-lisp-mode" - remove mode
         ;; (string-remove-suffix "-mode" "emacs-lisp-mode")
         (mode-string (when (string-suffix-p "-mode" mode-symbol-string)
                          (string-remove-suffix "-mode" mode-symbol-string))))

         ;; (apply #'mapconcat #'identity (butlast (string-split mode-symbol-string "-")) '("-"))))
    (print mode-symbol-string)
    (cond
     ((and mode-string
           (car (rassq (intern mode-string) org-src-lang-modes))))
     ((and (not (string-empty-p mode-string))
           mode-string))
     ;; special cases:
     ((and (stringp path-or-mode-string)
           (not (string-empty-p path-or-mode-string))
           (cond
            ;; directory
            ((file-directory-p path-or-mode-string)
             "shell")
            ;; .ai file
            ((string-match "\\.ai\\'" path-or-mode-string)
             "ai"))))
     (t
      "auto"))))

(defun oai-block-tags--replace-first-match (regexp replacement string &optional add)
  "Replace the first occurrence of REGEXP with REPLACEMENT in STRING.
If optional argument ADD is non-nil we replacement added instead of
 replacing.
Return new string."
  (let ((index (string-match regexp string)))
    (if index
        (if add
            (concat (substring string 0 (match-end 0))
                    replacement
                    (substring string (match-end 0)))
          ;; else
          (concat (substring string 0 index)
                  replacement
                  (substring string (match-end 0))))
      ;; else
      string)))


(cl-defun oai-block-tags--compose-m-block (content &optional &key lang header inner)
  "Return markdown block for LLM with CONTENT.
Markdown block marked as auto language If optional argument LANG with
 string is not provided.
Optional arguments
- LANG is language of content, may be \"ai\".
- HEADER is a line above markdown to describe it for LLM, should not have
 new line characters at edges.
- INNER if non-nil AI language content should be wrapped in
 markdown block, HEADER is ignored
- HEADER added after first chat prefix or just at the begining if
 CONTENT dont starts with chat prefix.
To detect LANG use `oai-block-tags--filepath-to-language'.
- INNER, if non-nil, ai block wrapped in markdown."
  (oai--debug "oai-block-tags--compose-m-block N1 inner=%s lang=%s header=%s" inner lang header)
  (oai--debug "oai-block-tags--compose-m-block N2" content)
  (if (or (not content)
          (string-empty-p content))
      nil
    ;; else
    (if (and lang (string-equal-ignore-case "ai" lang) (not inner))
        content
      ;; else - any bock
      (let ((content (when content
                       (concat "\n```" (or lang "auto") "\n"
                               (string-replace "```" "\\`\\`\\`"
                                               (replace-regexp-in-string oai-block--chat-prefixes-re
                                                                         "_\\&"
                                                                         content))
                               "\n```"))))
        (oai--debug "oai-block-tags--compose-m-block N3" content)
      (concat (when header (concat "\n" header)) content))))) ; no error if content is nil

(defun oai-block-tags--compose-block-for-path (path-string content)
  "Return mardown block with description.
PATH-STRING may be path to directory or to a file.
For provided PATH-STRING and CONTENT string, return string that will be
 good understood by AI."
  (oai-block-tags--compose-m-block
   ;; content:
   content
   :lang (oai-block-tags--filepath-to-language path-string)
   :header (concat "Here " (file-name-nondirectory (directory-file-name path-string))
                   (when (file-directory-p path-string)
                       " directory contents:"))))


(defun oai-block-tags--compose-block-for-path-full (path-string)
  "Return file or directory in prepared mardown block.
PATH-STRING may be path to file or a directory.
Return string or nil or raise user-error."
  (oai--debug "oai-block-tags--compose-block-for-path-full %s" path-string)
  (oai-block-tags--compose-block-for-path path-string
                                          (if (file-directory-p path-string)
                                              (oai-block-tags--get-directory-content path-string)
                                            ;; else
                                            ;; raise user-error if something
                                            (org-file-contents path-string)))) ; oai-block-tags--read-file-to-string-safe

;; -=-= help functions:  block-at-point, contents-area, get-content

(defun oai-block-tags--block-at-point (&optional element)
  "Get Org block if point at one of `oai-block-tags-org-blocks-types'.
Point should be at header of block.
Otherwise return nil.
Optional argument ELEMENT is any Org element."
  (org-element-with-disabled-cache
    (let ((context (or element (org-element-context))))
      (while (and context
                  (not (member (org-element-type context) oai-block-tags-org-blocks-types)))
        (setq context (org-element-property :parent context)))
      context)))

(defun oai-block-tags--contents-area (&optional element)
  "Return cons with start and end position of content.
Works for ai blocks ELEMENT and supported Org block
 `oai-block-tags-org-blocks-types'.
Start and first line after header, end at of line of the first not empty
 line before footer."
  (when-let* ((element (or element
                           (oai-block-tags--block-at-point))))
    (if (string-equal "ai" (org-element-property :type element))
        (oai-block--contents-region element)
      ;; else - not ai org element
      (when-let* ((res (org-src--contents-area element)))
        (cons (car res) (cadr res))))))

;; -=-= functions: get-content, get-content-ai-messages
(defun oai-block-tags-get-content-ai-messages (&optional element noweb-control links-only-last not-clear-properties ai-block-markers disable-tags req-type sys-prompt sys-prompt-for-all-messages max-tokens-string)
  "Get content of ai block with expansion of links and cleaning.
Execution in not `org-mode' is supported.
Same to `oai-restapi-prepare-content'
Do: expand tags and links, expand noweb, clear properties and trim.
Expand links and tags only for :eval context, for :tangle, dont expand.
If ELEMENT not specified, :begin of current element is used, in not Org
 mode `point-min' is used.
Optional arguments ELEMENT LINKS-ONLY-LAST
 NOT-CLEAR-PROPERTIES REQ-TYPE SYS-PROMPT SYS-PROMPT-FOR-ALL-MESSAGES
 MAX-TOKENS documented at `oai-block-tags-get-content'.
If DISABLE-TAGS boolen flag is non-nil, links will not be expanded, it
 is like inverse NOWEB-CONTROL.
Optional argument AI-BLOCK-MARKERS is a list of header markers created
 with `oai-block-get-header-marker', used to check that target of link
 or noweb reference don't point to current block to prevent recursion,
 also used in `oai-block-tags-replace'.
MAX-TOKENS-STRING is string.
Return vector with messages for ai block, or string if REQ-TYPE is
 compeltion or nil if loop."
  (oai--debug "oai-block-tags-get-content-ai-messages N1 %s %s %s" noweb-control links-only-last not-clear-properties ai-block-markers)
  (let ((current-block-marker (oai-block-get-header-marker element)))
    (unless (member current-block-marker ai-block-markers)
      ;; add to block to list of markers to prevent loop
      (push current-block-marker ai-block-markers)
      (oai--debug "oai-block-tags-get-content-ai-messages N2 %s %s %s" noweb-control disable-tags ai-block-markers)
      (if (eql req-type 'completion)
          ;; - *Completion*
          (let* ((str (oai-block-get-content element noweb-control)) ; legacy: may be executed in `org-mode' only
                 (str (if noweb-control
                          (oai-block-tags-replace str ai-block-markers)
                        ;; else
                        str))
                 (str (oai-block-tags--clear-properties str)))
            str) ; return string

        ;; else - req-type = chat
        (let* (;; 1) get messages as vector from content
               (messages (oai-block-collect-chat-messages-at-point element
                                                                   sys-prompt
                                                                   sys-prompt-for-all-messages
                                                                   max-tokens-string
                                                                   t)) ; not-merge - user may use links and organize message by self.
               (_ (oai--debug "oai-block-tags-get-content-ai-messages N2_1" messages))
               ;; 2) noweb expansion
               (messages (if noweb-control
                             (if links-only-last
                                 (oai-restapi--modify-vector-last-user-content messages
                                                                               #'oai-block--apply-noweb
                                                                               oai-block-tags-noweb-split-messages)
                               ;; else
                               (oai-restapi--modify-vector-content messages
                                                                   #'oai-block--apply-noweb
                                                                   'user
                                                                   oai-block-tags-noweb-split-messages))
                           ;; else
                           messages))
               ;; 3) tags and links expansion
               (messages (if (not disable-tags)
                             (if links-only-last
                                 (oai-restapi--modify-vector-last-user-content messages
                                                                               #'oai-block-tags-replace
                                                                               oai-block-tags-tagslinks-split-messages
                                                                               ai-block-markers)
                               ;; else
                               (oai-restapi--modify-vector-content messages
                                                                   #'oai-block-tags-replace
                                                                   'user
                                                                   oai-block-tags-tagslinks-split-messages
                                                                   ai-block-markers))
                           ;; else
                           messages))
               (_ (oai--debug "oai-block-tags-get-content-ai-messages N2_2" messages))
               ;; 4) clear properties (for sending to LLM)
               (messages (if not-clear-properties
                             messages
                           ;; else
                           (oai-restapi--modify-vector-content messages #'oai-block-tags--clear-properties))))
          ;; (oai--debug "oai-block-tags-get-content-ai-messages2" messages)
          messages))))) ; return


;; Loop:
;;  oai-block-tags-get-content -> oai-block-tags-replace (both frequently used)
;;  oai-block-tags-get-content-ai - do noweb and start links expansion
;;  oai-block-tags--get-replacement-for-org-link -> oai-block-tags--get-replacement-for-org-link
;;  oai-block-tags--get-org-links-content
;;  oai-block-tags--get-content-at-point
;;  oai-block-tags--get-content-at-point-org
;;  oai-block-tags--get-content-org-block-at-point -> oai-block-tags-get-content - check target of link and source block

;; To prevent loop we accept `ai-block-markers' argument for both
;; functions and pass it through the chain to compare target of link
;; with `ai-block-markers' before calling
;; `oai-block-tags-get-content' `oai-block-tags-replace'.
(defun oai-block-tags-get-content (&optional element noweb-control links-only-last not-clear-properties ai-block-markers disable-tags req-type sys-prompt sys-prompt-for-all-messages max-tokens)
  "Get content of supported blocks in current position in current buffer.
With properly expansion of tags, links and noweb references.
For evaluation, tangling, or exporting.
For ai block we replace links and tags in last user message only.
it should point to element
Optional arguments:
AI-BLOCK-MARKERS is a list of header markers created with
 `oai-block-get-header-marker', used to check that target of link or
 noweb reference don't point to current block to prevent recursion, also
 used in `oai-block-tags-replace'.
If current block already in AI-BLOCK-MARKERS, tags and noweb links are
 not expanded.
ELEMENT is Org block.  If provided, point may be not at element.
If NOWEB-CONTROL boolean, if non-nil, expand noweb links and links in
 block.
If NOT-CLEAR-PROPERTIES is not-nil, don't clear region highlighting for
 replaced links.
If LINKS-ONLY-LAST is not-nil, links expansion will be made for last
 user message only, otherwise for all user message.
Called from `oai-expand-block', goint to use it everywhere.
REQ-TYPE SYS-PROMPT SYS-PROMPT-FOR-ALL-MESSAGES MAX-TOKENS
 arguments documented in `oai-restapi-request-prepare'.
Return string with expanded content."
  (oai--debug "oai-block-tags-get-content N1 %s" ai-block-markers)
  (when-let* ((element (or element (oai-block-p) (oai-block-tags--block-at-point))))
    (let ((max-tokens-string
           (when (and max-tokens oai-restapi-add-max-tokens-recommendation)
             (oai-restapi--get-length-recommendation max-tokens))))
      (oai--debug "oai-block-tags-get-content N2 %s" element)
      (cond
       ((string-equal "ai" (org-element-property :type element))
        (string-trim
         (oai-block--stringify-chat-messages
          (oai-block-tags-get-content-ai-messages element noweb-control links-only-last not-clear-properties ai-block-markers disable-tags req-type sys-prompt sys-prompt-for-all-messages max-tokens-string))))

       ((eq (org-element-type element) 'src-block)
        (goto-char (org-element-property :begin element))
        (oai-block-tags--clear-properties
         (oai-block-tags-replace (org-babel--expand-body (org-babel-get-src-block-info))))) ; org-babel-execute-src-block

       ((member (org-element-type element) oai-block-tags-org-blocks-types)
        (oai--debug "oai-block-tags-get-content blocks-types")
        (oai-block-tags--clear-properties
         (oai-block-tags-replace (caddr (org-src--contents-area element)))))))))

;; -=-= help functions: markdown-block-p, markdown in string

(defun oai-block-tags--markdown-block-p ()
  "Return range if current position in current buffer in markdown block.
Caution: move pointer.
Execution in not `org-mode' is supported.
Wrap `oai-block--markdown-block-p' to work with Org blocks, not only ai
 block.
Return cons with begining of lines for markdown block header and footer
 or nil."
  ;; check that we are in Org block
  (save-excursion
    (let* ((region (if (derived-mode-p 'org-mode)
                       (oai-block-tags--contents-area)
                     ;; else
                     (cons (point-min) (point-max))))
           (beg (car region))
           (end (cdr region))
           (ret (oai-block--markdown-block-p beg end)))
      (oai--debug "oai-block-tags--markdown-block-p %s %s" beg end)
      ret)))


(defun oai-block-tags--markdown-block-regions (str)
  "Same as `oai-block--markdown-block-regions', but for STR string.
Return list of integers or nil."
  (save-match-data
    (let ((search-pos 0)
          (block-boundaries '()))
      ;; Find all the '```' positions
      (while (string-match "```" str search-pos)
        (push (match-beginning 0) block-boundaries)
        (setq search-pos (match-end 0)))
      ;; Sort and pair boundaries
      (sort block-boundaries #'<))))

(defun oai-block-tags--markdown-block-string-p (str pos)
  "Check if POS is inside markdown block and return its positions.
Substring '```content' without last '```'.
Don't count new lines and don't language markdown block begining from
 end.
Used in `oai-block-tags--replace-last-regex-smart'.
Return list range if POS (an index) is inside a '```' code block in STR,
 otherwise return nil."
  (save-match-data
    (let ((block-boundaries (oai-block-tags--markdown-block-regions str)))
      (catch 'inside
        (let ((bounds block-boundaries))
          (while bounds
            (let ((start (pop bounds))
                  (end (and bounds (pop bounds))))
              (when (and end (>= pos start) (< pos end))
                (throw 'inside (list start end))))))
        nil))))

;; -=-= help functions: get content for blocks

(defun oai-block-tags--get-content-org-block-at-point (&optional element ai-block-markers inner)
  "Return markdown block for supported Org blocks at current position.
Works only supported blocks in `oai-block-tags-org-blocks-types' and ai
 block.
ai block handled specially in oai-block-tags--compose-m-block'.
Move pointer to the end of block.

Optional argiments:
- ELEMENT is ai block.
- AI-BLOCK-MARKERS used to prevent loop by coparing with ELEMENT at
 current position.
- INNER, if non-nil, ai block wrapped in markdown.
Return full content of block or nil."
  ;; 1) enshure that we are inside some Org block
  (oai--debug "oai-block-tags--get-content-org-block-at-point %s" inner ai-block-markers)
  (when-let ((element (or element (oai-block-tags--block-at-point))))
    ;; (oai--debug "oai-block-tags--get-content-org-block-at-point3 %s" tags-control)
    (oai-block-tags--compose-m-block
     ;; content
     (oai-block-tags-get-content element nil nil nil ai-block-markers) ; may cause recursion
     :lang (if (eq (org-element-type element) 'src-block)
               (org-element-property :language element)
             ;; else
             (when (oai-block-p element)
               "ai"))
     :header (when-let ((name (org-element-property :name element))) ; nil or string
               (concat "Block name: " name))
     :inner inner)))

;; [[file:~/sources/emacs-oai/oai-block-tags.el::735::((and (member type oai-block-tags-org-blocks-types)]]


(defun oai-block-tags--get-m-block-at-point ()
  "Get language markdown block or inline markdown block at current line.
Pointer should be at markdown header or inside qutoes on line.
Execution in not `org-mode' is supported.
Called for current point in current buffer.
Move pointer.
Return non-nil string of markdown block with header if exist at current
 position."
  (oai--debug "oai-block-tags--get-m-block-at-point N1" (- (point) (line-beginning-position)))
  (if-let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
            (range (oai-block-tags--markdown-block-string-p
                    line
                    (- (point) (line-beginning-position)))))
      ;; flat mardown block in one line
      (progn
        (oai--debug "oai-block-tags--get-m-block-at-point N2" range (- (point) (line-beginning-position)))
        (oai--debug "oai-block-tags--get-m-block-at-point N3" line)
        (oai--debug "oai-block-tags--get-m-block-at-point N4" (line-beginning-position))
        (concat (substring line (car range) (cadr range)) "```"))
    ;; else - looking at header of block?
    (beginning-of-line)
    (when (looking-at oai-block--markdown-beg-end-re)
      (when-let* ((range (oai-block-tags--markdown-block-p))
                  (beg (car range))
                  (end (save-excursion (goto-char (cdr range))
                                       (line-end-position))))
        (string-trim-left (buffer-substring-no-properties beg end))))))

(defun oai-block-tags--get-content-chat-message-at-point (&optional ai-block-markers)
  "Get chat message at point, if pointer at message prefix.
Execution in not `org-mode' is supported.
Move pointer.
Optional argument AI-BLOCK-MARKERS explained in
 `oai-block-tags-get-content'.
Return string or nil."
  (beginning-of-line)
  (when (and
         ;; at message?
         (looking-at oai-block--chat-prefixes-re)
         ;; not in markdown?
         (not (oai-block-tags--markdown-block-p)))
    (when-let* ((regions (oai-block--chat-role-regions))
                (reg (car (oai-block--find-region-with-position regions (point))))
                (beg (car reg))
                (end (cdr reg))
                (result (string-trim-right (buffer-substring-no-properties beg end))))
      ;; noweb and links
      (let* ((current-block-marker (oai-block-get-header-marker))
             (tags-control (not (member current-block-marker ai-block-markers))))
        (push current-block-marker ai-block-markers)
        (if tags-control
            (oai-block-tags-replace result ai-block-markers)
          ;; else
          result)
        ;; ;; else - whole ai block
        ;; (oai-block-tags--get-content-org-block-at-point element source-block-marker))
        ))))

;; -=-= help function: line number for position

(defun oai-block-tags--line-num-to-positon (line-num &optional end-flag buffer)
  "Return the buffer position at the beginning of LINE-NUM in BUFFER or nil.
LINE-NUM is 1-based.  If BUFFER is nil, use the current buffer.
If END-FLAG is non-nil, then return end of line position.
Returns nil if LINE-NUM is out of range."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (when (zerop (forward-line (1- line-num)))
        (if end-flag
          (line-end-position)
          ;; else
          (point))))))
;; - test:
;; (print (list (line-beginning-position) (oai-block-tags--line-num-to-positon (line-number-at-pos (point))))) ; should be (2 2) - equal to each other

;; -=-= help functions: find targets of Links and get content

(defun oai-block-tags--path-is-current-buffer-p (path)
  "Return non-nil if PATH references the file currently visited by this buffer.
Handles symlinks, remote files (TRAMP), and buffers without files."
  ;; (oai--debug "oai-block-tags--path-is-current-buffer-p" buffer-file-name path)
  (when buffer-file-name
    (ignore-errors
      (let ((buffer-file (file-truename buffer-file-name))
            (input-file  (file-truename (expand-file-name path))))
        (string= buffer-file input-file)))))


(defun oai-block-tags--get-content-at-point-not-org (&optional ai-block-markers)
  "Return prepared block at current POS position.
Works with outline, programming, text buffers.
1) Use `beginning-of-defun' for programming mode
2) Use `outline-regexp' if outline or outline-minor mode active
3) AI file - if at message prefix.
4) Use `paragraph-separate' variable.
POS should be at begining of the line.
Optional argument AI-BLOCK-MARKERS explained in
 `oai-block-tags-get-content'.
Return string or nil."
  (oai--debug "oai-block-tags--get-content-at-point-not-org")
  (beginning-of-line) ; just in case
  (cond
   ;; 1) defun
   ((and (derived-mode-p 'prog-mode)
         (eq (save-excursion
               (end-of-line)
               (beginning-of-defun)
               (point))
             (line-beginning-position)))
    (oai-block-tags--compose-m-block
     ;; content:
     (buffer-substring-no-properties (point)
                                     (save-excursion (end-of-defun)
                                                     (forward-line -1)
                                                     (line-end-position)))
     :lang (oai-block-tags--filepath-to-language major-mode)
     :header "Function:"))
   ;; 2) outline
   ((and (or (derived-mode-p 'outline-mode) ; major
             (symbol-value (intern-soft outline-minor-mode))) ; minor
         ;; at header
         (eq 0 (string-match outline-regexp
                             (buffer-substring-no-properties (line-beginning-position)
                                                             (line-end-position)))))
    (let ((beg-pos (line-beginning-position))
          (end-pos (or (save-excursion (outline-next-heading)
                                       (forward-line -1)
                                       (line-end-position))
                       (point-max)))) ; return position or nil
      (oai-block-tags--compose-m-block
       ;; content:
       (buffer-substring-no-properties beg-pos end-pos)
       :lang (oai-block-tags--filepath-to-language major-mode)
       :header "Outliner:")))

   ;; 3) ai file
   ((and (string-equal "ai" (oai-block-tags--filepath-to-language (buffer-file-name)))

         (or (save-excursion (oai-block-tags--get-m-block-at-point))
             (oai-block-tags--get-content-chat-message-at-point ai-block-markers))))

   ;; 4) paragraph
   ;; ((and paragraph-separate
   ;;       (= (point) (save-excursion (start-of-paragraph-text) (point))))
   ;;  (save-excursion
   ;;    (forward-line)
   ;;    (backward-paragraph)
   ;;    (oai-block-tags--compose-m-block
   ;;          ;; content:
   ;;     (buffer-substring-no-properties (save-excursion (forward-line)
   ;;                                                     (line-beginning-position))
   ;;                                     (progn
   ;;                                       (forward-paragraph)
   ;;                                       (line-end-position)))
   ;;          :lang (oai-block-tags--filepath-to-language major-mode))))
   (t
    (user-error "No outline, function, ai message or markdown block was found to get a block"))))

(defun oai-block-tags--get-content-at-point-org (&optional ai-block-markers)
  "Prepare block for LLM of Org element at current position.
Optional AI-BLOCK-MARKERS argument used to prevent loop.
Cursor position may be not at the begining of the line for
 `oai-block-tags--get-m-block-at-point'.
Return string or nil."
  (let* ((element (or (oai-block-tags--block-at-point) (org-element-context)))
         (type (org-element-type element))) ; Org block or ai block or some element (not in block)
    (oai--debug "oai-block-tags--get-content-at-point-org type %s" type (- (point) (line-beginning-position)))
    ;; - (1) case - headline
    (cond
     ((eq type 'headline)
      (beginning-of-line)
      (let (replacement-list ; result
            el ; current element in loop
            type ; type of current element in loop
            )
        (push "\n```text" replacement-list)
        ;; Loop over headlines, to process every blocks and org elements to markdown for LLM
        (while (< (point) (org-element-property :end element))
          ;; supported sub-elements: headline, blocks
          ;; we add new line at begining of every "push"
          (setq el (org-element-context)) ; may be ai block
          (setq type (org-element-type el))

          (push (cond
                 ;; 1. Sub: Headline
                 ((eq type 'headline)
                  ;; make string: #*level + title
                  (prog1 (concat "\n" (make-string (org-element-property :level el) ?#) " " (org-element-property :raw-value el))
                    ;; MOVE!
                    (while (progn (forward-line) (end-of-line) (bolp)))
                    (beginning-of-line)))
                 ;; 1. Sub: Block
                 ((member type  oai-block-tags-org-blocks-types)
                  (concat "\n"
                  (prog1 (oai-block-tags--get-content-org-block-at-point el ai-block-markers t) ; noweb issue
                     ;; MOVE!
                    (org-forward-element))))

                 (t ; others
                  (prog1
                      (concat "\n" (buffer-substring-no-properties (line-beginning-position) (org-element-property :end el)))
                    ;; MOVE!
                    (org-forward-element))))
                replacement-list)) ; push to
        (push "\n```\n" replacement-list)
        (apply #'concat (reverse replacement-list))))
     ;; - (2) case - at first line of Markdown block one line or multiline - in org block
     ((and (member type oai-block-tags-org-blocks-types)
           (unwind-protect
               (save-excursion (oai-block-tags--get-m-block-at-point))
             (oai--debug "oai-block-tags--get-content-at-point-org (2) case"))))
     ;; - (3) case - at chat message prefix
     ((and (oai-block-p element)
           (unwind-protect
               (oai-block-tags--get-content-chat-message-at-point ai-block-markers)
             (oai--debug "oai-block-tags--get-content-at-point-org (3) case"))))
     ;; - (4) case - Org Block - at begining
     ((and (member type oai-block-tags-org-blocks-types)
           (let ((case-fold-search t))
             (save-excursion
               (beginning-of-line)
               (or
                (looking-at "[ \t]*#\\+BEGIN_\\(\\S-+\\)")
                (looking-at org-babel-src-name-regexp)))))
      (oai--debug "oai-block-tags--get-content-at-point-org (4) case %s" (point) type ai-block-markers)
      (oai-block-tags--get-content-org-block-at-point element ai-block-markers)) ; noweb issue
     ;; - (5) case - #+name: without block
     ((and (eq type 'keyword)
           (looking-at org-babel-src-name-regexp))
      (user-error "Reference to #+name: \"%s\" without actual block" (org-element-property :value element )))
     ;; - (6) case - Org element with :end
     ;; ((when-let ((end (org-element-property :end element))) ; safe
     ;;    (oai--debug "oai-block-tags--get-content-at-point-org (6) case %s" (point) type end ai-block-markers)
     ;;    (oai-block-tags--compose-m-block (buffer-substring-no-properties (line-beginning-position) end))))

     (t
      (user-error "Cant get content at point for link in Org buffer")))))

(defun oai-block-tags--get-content-at-point (&optional ai-block-markers)
  "Get prepared block at current position.
Support any mode buffers.  Here code for Org mode.
If at current position there is a Org block or markdown block
Return markdown block for LLM for current element at current position.
May return nil.
For Org buffer only.
Optional AI-BLOCK-MARKERS argument used to prevent loop.
Supported: blocks and headers.
- Org header - loop over elements and convert to markdown
- at markdown block header or inside markdown block
- at src header or inside src block
Move pointer to the end of block.
Return string or nil"
  (oai--debug "oai-block-tags--get-content-at-point %s" ai-block-markers)
  (if (not (derived-mode-p 'org-mode))
      (oai-block-tags--get-content-at-point-not-org ai-block-markers)
    ;; else - Org mode
    (oai-block-tags--get-content-at-point-org ai-block-markers)))


;; (featurep 'org-links)
(declare-function org-links--local-get-target-position-for-link "org-links")

;; (when (require 'org-links nil 'noerror)
(defun oai-block-tags--get-org-links-content (link &optional ai-block-markers)
  "In current buffer search for LINK and get content at position.
Works for any mode.
Support for `org-links' package with additional links types.
Headlines not wrapped in markdown blocks.
LINK is string in format is what inside [[...]] or Plain link.
Target may be not in Org buffer.
Optional AI-BLOCK-MARKERS argument used to prevent loop.
Return string or nil."
  ;; (require 'org-links)
  (oai--debug "oai-block-tags--get-org-links-content N1 %s " link)
  (if-let ((nums (org-links--local-get-target-position-for-link link))) ; may be nil
      (let ((num1 (car nums))
            (num2 (cadr nums))) ; may be nil
        (oai--debug "oai-block-tags--get-org-links-content N2 %s %s" num1 num2)
        ;; 1) Case1: num1 and num2 - get range
        (if num2
            (if-let ((pos1 (oai-block-tags--line-num-to-positon num1))
                     (pos2 (or (oai-block-tags--line-num-to-positon num2 'end-of-line) (point-max))))
                (progn
                  (oai--debug "oai-block-tags--get-org-links-content N3 %s %s" pos1 pos2)
                  (oai-block-tags--compose-m-block (buffer-substring-no-properties pos1 pos2)
                                                   :lang (oai-block-tags--filepath-to-language (or (and (derived-mode-p 'fundamental-mode)
                                                                                                        buffer-file-name)
                                                                                                   major-mode))))
              ;; pos1 is nil
              (user-error "In link %s of NUM-NUM format was not possible to find first NUM in buffer %s" link (current-buffer)))
          ;; else - 1) Case1: only num1, num2 is nil - get object at num1 or just line.
          (if-let ((pos1 (oai-block-tags--line-num-to-positon num1)))
              (save-excursion
                (oai--debug "oai-block-tags--get-org-links-content N4 %s" pos1)
                (goto-char pos1)
                (oai-block-tags--get-content-at-point ai-block-markers))
            (user-error "In link %s of NUM format was not possible to find first position in buffer %s" link (current-buffer)))))
    ;; else - not org-links type link.
    nil))

;; (oai-block-tags--get-org-links-content "9-10")

(defun oai-block-tags--org-search-local (link type path)
  "Search for LINK Org object with TYPE at PATH in current buffer.
Where PATH is :path of LINK Org object.
Wrap `org-link-search' function, like in `org-link-open' function.
Now we use it for TYPE radio and fuzzy.
Move pointer to found link and return type of matched result, which is
either `dedicated' or `fuzzy'.  If not found give raise error."
  (oai--debug "oai-block-tags--org-search-local %s %s %s" link type path)
  (if (equal type "radio")
      (org-link--search-radio-target path)
    ;; else - fuzzy, custom-di, coderef
    (let ((org-link-search-must-match-exact-headline oai-block-tags-error-on-missing-link-flag)) ;; should found?
      ;; (print (list "oai-block-tags--org-search-local" org-link-search-must-match-exact-headline))
      ;; Not working: :-(
      ;; (save-excursion
      ;;   (with-restriction (point-min) (point-max)
      (org-link-search
       (pcase type
	 ("custom-id" (concat "#" path))
	 ("coderef" (format "(%s)" path))
	 (_ path))
       ;; Prevent fuzzy links from matching themselves.
       (and (equal type "fuzzy")
	    (+ 2 (org-element-property :begin link)))))))


(defun oai-block-tags--get-replacement-for-org-file-link-in-other-file (path option)
  "Find link target and return prepared block for LLM.
1) open file PATH in new buffer
2) call `oai-block-tags--get-replacement-for-org-link'.  with OPTION
Return string."
  (oai--debug "oai-block-tags--get-replacement-for-org-file-link-in-other-file %s %s" path option)
  ;; Code from org-open-file -> find-file-other-window was used:
  (setq path (abbreviate-file-name path))
  (let ((value (or (get-file-buffer path)
                   ;; open with safe variables and no eval
                   (let ((enable-local-variables :safe)
                         (enable-local-eval nil))
                   (find-file-noselect path t nil nil))))) ; buf name open with mode, return buffer
    (with-current-buffer value
      (oai-block-tags--get-replacement-for-org-link (org-link-make-string option)))))

;; (get-file-buffer "~/docsmy_short/modified/mastodon")
;; (find-file-noselect "~/docsmy_short/modified/mastodon" t nil nil)
;; (oai-block-tags--get-replacement-for-org-file-link-in-other-file "~/docsmy_short/modified/mastodon" "5466::*[2024-03-05 Tue]")

(defun oai-block-tags--string-is-integer (str)
  "Return num if STR is an integer, nil otherwise."
  (when (stringp str)
    (let ((val (string-to-number str)))
      (when (and (string= (number-to-string val) str) ; check direct conversion
                 (not (string-match-p "\\." str)))
        val))))     ; disallow decimals


(defun oai-block-tags--get-replacement-for-org-link (link-string &optional ai-block-markers)
  "Return string that explain LINK-STRING for LLM or nil.
Supported targets:
- Org block in current buffer \"file:\"
- file: - targets in other files
- file & directory `oai-block-tags--compose-block-for-path-full'
- local link. Use current buffer to find target of link.
Use current buffer, current position to output error to result of block
if two targets found.
Uses `oai-block--block-header-marker' variable to check that target of
link or noweb reference don't point to current block to prevent
recursion, created with `oai-block-get-header-marker'.
Optional AI-BLOCK-MARKERS argument used to prevent loop.
Return replacement string or nil."
  ;; Some code was taken from:
  ;; `org-link-open' for type and opening,  `org-link-search' for search in current buffer.
  ;; from `org-link-open-from-string'
  ;; - - 1) convert string to Org element
  (oai--debug "oai-block-tags--get-replacement-for-org-link %s %s %s" link-string (point) (current-buffer))
  (let ((link-el (with-temp-buffer
                   (let ((org-inhibit-startup nil))
                     (insert link-string)
                     (org-mode)
                     (goto-char (point-min))
                     (org-element-link-parser)))))
    (if (not link-el)
      (user-error "No valid link in %s" link-string))
    ;; from `org-link-open'
    ;; - - 2) extract path and type
    (let ((type (org-element-property :type link-el))
          (path (org-element-property :path link-el)))
      ;; - - 3) process link depending on type
      (oai--debug "oai-block-tags--get-replacement-for-org-link 3) %s %s" type link-string)
      (pcase type
        ;; - - 3.1) "file:" prefix
        ("file" ; org-link-search
         (let* ((option (org-element-property :search-option link-el))) ;; nil if no ::, may be "" if after :: there is empty last part
           ;; (print (list "option" option))
           (oai--debug "oai-block-tags--get-replacement-for-org-link 3.1) %s %s" (oai-block-tags--path-is-current-buffer-p path) path)
           ;; cases: 1) no option
           ;;        2) in this buf + option is fuzzy or NUM-NUM (org-links handl it well.)
           ;;        3) in this buf + option is number
           ;;        4) not in this buf + option

           (if (and option
                    (not (string-empty-p option)))
               ;; PATH and OPTION
               (if (oai-block-tags--path-is-current-buffer-p path)
                   (if-let ((num (oai-block-tags--string-is-integer option)))
                       ;; case 2) PATH::NUM
                       (progn (org-goto-line num) (oai-block-tags--get-content-at-point ai-block-markers))
                     ;; else case 3) *recursion call*! without path
                     (oai-block-tags--get-replacement-for-org-link  (org-link-make-string option) ai-block-markers)) ; recursive call
                 ;; - else case 4) <other-file> - *recursive call*!
                 (oai-block-tags--get-replacement-for-org-file-link-in-other-file path option))
             ;; else case 1) - no ::, only path
             (oai-block-tags--compose-block-for-path-full path))))

        ;; LOCAL LINKS!
        ;; ((or "coderef" "custom-id" "fuzzy" "radio")
        ((or "radio" "fuzzy")
         (save-excursion
           (org-with-wide-buffer

            (or ; return result value, not boolean
             ;; 1) search with `org-links' and get content with `oai-block-tags--get-content-at-point'
             (when (and (featurep 'org-links) ;; (require 'org-links nil 'noerror)
                        (string-equal type "fuzzy"))
               (oai-block-tags--get-org-links-content (org-element-property :raw-link link-el)
                                                      ai-block-markers)) ; NUM-NUM
             ;; 0) search with `org-link-search' and get content with `oai-block-tags--get-content-at-point'
             (let ((ln-before (line-number-at-pos)))
               (let (
                     ;; - 1) find target of link-el & link-string
                     (found (oai-block-tags--org-search-local link-el type path))  ; <- Search!
                     target-pos)
                 ;; - 2) move pointer to search result
                 (setq target-pos (point))
                 ;; (print (list "oai-block-tags--get-replacement-for-org-link found" found (point)))
                 ;; 2.1) several targets with same name exist? = error
                 (when (and oai-block-tags-check-double-targets-found-flag
                            (eq found 'dedicated)
                            (not (eq (line-number-at-pos) ln-before))) ; found?
                   (let ((ln-found (line-number-at-pos))
                         oai-block-tags-error-on-missing-link-flag)
                     (with-restriction (line-end-position) (point-max)
                       (condition-case nil
                           (setq found (oai-block-tags--org-search-local link-el type path))
                         (error nil)))
                     (when (and (not (eq (line-number-at-pos) ln-found)) ; found?
                                (eq found 'dedicated))
                       ;; (print (list (line-number-at-pos) ln-found (progn (forward-line ln-found)
                       ;;                                                   (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
                       (user-error "Two targets found for link %s\n- %s: %s\n- %s: %s" link-string
                                   (line-number-at-pos) (buffer-substring-no-properties (line-beginning-position) (line-end-position))
                                   ln-found (progn (forward-line (- ln-found (line-number-at-pos)))
                                                   (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))))

                 ;; - 4) Move to position of target position
                 (goto-char target-pos)
                 ;; - 5) `oai-block-tags--get-content-at-point'
                 (oai-block-tags--get-content-at-point ai-block-markers)))))))))))

;; -=-= help functions: markdown blocks

(defun oai-block-tags--string-count-char-in-direction (string position char direction)
  "Count CHAR in DIRECTION from POSITION on the same line in STRING."
  (if (char-equal (aref string position) ?\n)
      0
    ;; else
    (let ((step (if (eq direction 'right) 1 -1))
          (count 0)
          (len (length string))
          (pos (+ position (if (eq direction 'right) 1 -1))))
      (while (and (>= pos 0)
                  (< pos len)
                  (not (char-equal (aref string pos) ?\n)))
        (when (char-equal (aref string pos) char)
          (setq count (1+ count)))
        (setq pos (+ pos step)))
      count)))

(defun oai-block-tags--string-is-quoted-p (string position)
  "Check if POSITION is quoted at current line in STRING."
  (and (eql 1 (% (oai-block-tags--string-count-char-in-direction string position ?` 'left) 2))
       (eql 1 (% (oai-block-tags--string-count-char-in-direction string position ?` 'right) 2))))

;; -=-= Replace links in text
;; Supported:
;; - @Backtrace
;; - @/path/file.txt
;; - @./name - file
;; - @name - <<target>> or #+NAME: name - in current file

(defun oai-block-tags--replace-last-regex-smart (str-orig regexp &optional replacement)
  "Replace the last match of REGEXP in STR-ORIG with REPLACEMENT.
reserve any extra captured groups.
Check that found regexp not in markdown block.
If REPLACEMENT not provided return found str-orig for regexp or nil if not
found."
  (oai--debug "oai-block-tags--replace-last-regex-smart N1 %s" str-orig)
  (oai--debug "oai-block-tags--replace-last-regex-smart N2" replacement)
  (let ((pos 0)
        (last-pos nil)
        (last-end nil))
    ;;
    (while (and pos
                (string-match regexp str-orig pos))

      (setq pos (match-beginning 0))
      ;; (print (list "sdasd" pos))
      (unless (or (oai-block-tags--markdown-block-string-p str-orig pos) ; not in ``` multiline markdown block
                  (oai-block-tags--string-is-quoted-p str-orig pos))
        (setq last-pos pos)
        (setq last-end (match-end 0))) ; end

      ;; (print (list "sdasd2" last-end))
      (setq pos (match-end 0))) ; move forward

    (if replacement
        (if last-pos
            ;; 1) replace
            (progn (add-text-properties 0 (length replacement) '(face region) replacement) ; side effect function
                   (concat (substring str-orig 0 last-pos)
                           replacement
                           ;; last-group
                           (substring str-orig last-end)))
          ;; else - return just str-orig
          str-orig)
      ;; else no replacement
      (if last-pos
          (replace-regexp-in-string "^[` ]*" ""
                                    (replace-regexp-in-string "[` ]*\$" ""
                                                              (match-string 0 str-orig))) ;; (substring str-orig last-pos last-end)
        nil))))


(defun oai-block-tags-replace (string &optional ai-block-markers)
  "Replace links in STRING with their targets.
Check every type of links if it exist in text, find replacement for the
fist link and replace link substring with
`oai-block-tags--replace-last-regex-smart' once.
Used for function `oai-restapi--modify-vector-content'.
Uses `oai-block--block-header-marker' variable to check that target of
link or noweb reference don't point to current block to prevent
recursion, created with `oai-block-get-header-marker'.
Called from:
- `oai-expand-block' interactive function
- `oai-restapi-request-prepare'
- `oai-restapi-request-llm-retries'.
Optional AI-BLOCK-MARKERS argument used to prevent loop.
Return modified string with text properties or the same string."
  (oai--debug "oai-block-tags-replace N0 %s" string)
  ;; - "@Backtrace" substring exist - replace the last one only
  ;; Result will be *Wrapped in markdown*
  (let ((i 9))
    (while (and (string-match oai-block-tags--regexes-backtrace string)
                (not (zerop i)))
      ;; check if not in ``` multiline markdown block
      (unless (or (oai-block-tags--markdown-block-string-p string (match-beginning 0))
                  (oai-block-tags--string-is-quoted-p string (match-beginning 0)))
        (setq i (1- i))
        (oai--debug "oai-block-tags-replace N1 backtrace %s %s" (match-string 0 string))
        (if-let* ((bt (or (oai-block-tags--get-backtrace-buffer-string)
                          (user-error "No backtrace buffer for @Backtrace tag"))) ; *Backtrace* buffer exist
                  (bt (oai-block-tags--take-n-lines bt oai-block-tags-backtrace-max-lines))
                  (bt (oai-block-tags--compose-m-block bt
                                                       :lang "backtrace")) ; prepare string
                  (new-string (oai-block-tags--replace-last-regex-smart string oai-block-tags--regexes-backtrace bt))) ; insert backtrace
            (setq string new-string)))))


  ;; - Path @/path/file.txt - replace the last one only
  (let ((matches '())
        (start 0)
        mbeg)
    ;; Collect all matches and their positions
    (while (string-match oai-block-tags--regexes-path string start)
      (setq mbeg (1- (match-beginning 2))) ; add @
      ;; check if not in ``` multiline markdown block
      (unless (or (oai-block-tags--markdown-block-string-p string mbeg)
                  (oai-block-tags--string-is-quoted-p string mbeg))
        (push (cons (match-string 2 string) mbeg) matches))
      (setq start (match-end 2)))

    ;; Sort in reverse order so replacing substrings doesn't affect positions
    (setq matches (sort matches (lambda (a b) (> (cdr a) (cdr b)))))

    (dolist (match matches)
      (let* ((tag-str (car match)) ; no @
             (pos (cdr match))
             (end (1+ (+ pos (length tag-str)))) ; and + @
             (replacement (oai-block-tags--compose-block-for-path-full tag-str))
             (replacement (concat replacement "\n")))
        ;; Replace the substring in string
        (setq string (concat
                      (substring string 0 pos)
                      replacement
                      (substring string end))))))

  ;; - Org links [[link]]
  ;; We search  for link regex,  when found we check  if there
  ;; are double of  found substring after founded  one, if one
  ;; more exist we skip the first  one that found. if no other
  ;; exist we replace it.
  ;; *Dont Wrap in markdown*
  (let ((matches '())
        (start 0))
    ;; Collect all matches up front
    (while (string-match oai-block-tags--org-link-any-re string start)
      ;; check if not in ``` multiline markdown block
      (unless (or (oai-block-tags--markdown-block-string-p string (match-beginning 0))
                  (oai-block-tags--string-is-quoted-p string (match-beginning 0)))
        (push (cons (match-string 0 string) (match-beginning 0)) matches))
      (setq start (match-end 0)))
    ;; Sort in reverse: safest for substring replacement
    (setq matches (sort matches (lambda (a b) (> (cdr a) (cdr b)))))
    (dolist (match matches)
      (let* ((link (car match))
             (pos (cdr match))
             (end (+ pos (length link)))
             (replacement (oai-block-tags--get-replacement-for-org-link link ai-block-markers))
             (replacement (concat replacement "\n")))
        ;; Replace in the string
        (setq string (concat
                      (substring string 0 pos)
                      replacement
                      (substring string end))))))

  (oai--debug "oai-block-tags-replace N4" string)
  string) ; return

;; Usage:
;; (oai-block-tags-replace  "[[./]]")
;; (oai-block-tags-replace  "11[[sas]]222[[bbbaa]]3333[[sas]]4444")
;; (oai-block-tags-replace  "11[[file:/mock/org.org::1::* headline]]4444")


(defun oai-block-tags--clear-properties (string)
  "Remove text properties from STRING.
Used as argument fo function `oai-restapi--modify-vector-content'.
Used for `oai-expand-block' that show fontificated of markdown blocks,
made by `oai-block-tags--replace-last-regex-smart'.
Return modified STRING."
  (set-text-properties 0 (length string) nil string)
  (string-trim string))


;; -=-= Fontify @Backtrace & @path & [[links]]

(defun oai-block-tags--font-lock-fontify-links (limit)
  "Fontify Org links in #+begin_ai ... #+end_ai blocks, up to LIMIT.
This is special fontify function, that return t when match found.
1) search for ai block begin and then end, 2) call fontify on range that
goto to the begining firstly function `org-activate-links' used to
highlight any link.
TODO: maybe we should use something like
`oai-block-tags--markdown-block-string-p'"
  (let ((case-fold-search t)
        ret)
    ;; - loop per ai block
    (while (and (re-search-forward oai-block--ai-block-begin-re limit t)
                (< (point) limit))
      (let ((beg (match-end 0))
            end lbeg lend)
        (if (re-search-forward oai-block--ai-block-end-re limit t)
            (setq end (match-beginning 0))
          ;; else
          (setq end limit))
          (save-match-data
            ;; fontify Org links [[..]]
            ;; - [[link][]]
            (progn
              (goto-char beg)
              (while (re-search-forward oai-block-tags--org-link-any-re end t)
                (setq lbeg (match-beginning 0))
                (setq lend (match-end 0))
                (unless (or (oai-block--at-special-p lbeg)
                            (oai-block--markdown-quotes-p lbeg))
                  (remove-text-properties lbeg lend '(face nil))
                  (setq ret (org-activate-links lend)))
                (goto-char lend)))
            ;; - @Backtrace
            (progn
              (goto-char beg)
              (while (re-search-forward oai-block-tags--regexes-backtrace end t)
                (setq lbeg (match-beginning 0))
                (setq lend (match-end 0))
                (unless (or (oai-block--at-special-p lbeg)
                            (oai-block--markdown-quotes-p lbeg))
                  (add-face-text-property lbeg lend 'org-link)
                  (setq ret t))
                (goto-char lend)))
            ;; - @/tmp/
            (progn
              (goto-char beg)
              (while (re-search-forward oai-block-tags--regexes-path end t)
                (setq lbeg (match-beginning 0))
                (setq lend (match-end 0))
                (unless (or (oai-block--at-special-p lbeg)
                            (oai-block--markdown-quotes-p lbeg))
                  (add-face-text-property lbeg lend 'org-link)
                  (setq ret t))
                (goto-char lend))))))
    ;; required by font lock mode:
    (goto-char limit)
    ret))

(provide 'oai-block-tags)
;;; oai-block-tags.el ends here
