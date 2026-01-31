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
;; `oai-block-tags-replace' is main function for replace links.
;; Link is Org links, tags is AI links in form of @something.
;;
;; How this works? (TODO)
;; - for highlighting this add hook to Org with font-lock logic
;; - for replacing tags we operate at string variable
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
;; To check links use "C-c ?" key, or M-x oai-expand-block.
;; `oai-block-tags--get-content-at-point' - extract target from current position

;; *Position and line number*
;; - `line-number-at-pos'
;; - `oai-block-tags--line-num-to-positon'

;;; TODO:

;; -=-= includes
(require 'org)
(require 'ol)
(require 'oai-debug)
(require 'oai-block)
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

(defvar oai-block-tags--regexes-backtrace "@\\(Backtrace\\|B\\([\s-]\\|$\\)\\)")
;; (defvar oai-block-tags--regexes-path "@\\(\\.\\.?/\\|\\.\\.?\\\\\\|\\.\\.?\\|/\\|\\\\\\|[A-Za-z]:\\\\\\)[a-zA-Z0-9_./\\\\-]*"
(defvar oai-block-tags--regexes-path "\\(^\\|[\s-]\\)@\\(\\.\\.?\\|\\.\\.?/\\|\\.\\.?\\\\\\|/\\|\\\\\\|[A-Za-z]:\\\\\\|~[a-zA-Z0-9_.-]*/*\\)[a-zA-Z0-9_./\\\\-]*"
  "Unix Posix and Windows, currently we support Linux only.
See: .
[[file:./doc.org::*Regex: file path][Regex: file path]]
and
[[file:./tests/oai-tests-block-tags.el::94:
:;; -=-= Test: oai-block-tags--regexes-path]].")

;; (let ((s "`@/asd vv`"))
;;   (when (string-match oai-block-tags--regexes-path s)
;;     (substring s (match-beginning 0) (match-end 0))))

(defvar oai-block-tags--markdown-prefixes '(:backtrace "elisp-backtrace"
                                            :path-directory "ls-output")
  "Right after ``` markdown block begining.")
(defvar oai-block-tags--markdown-postfix "\n```\n")



(defvar oai-block-tags-org-blocks-types '(comment-block center-block dynamic-block example-block
                                                        export-block quote-block special-block
                                                        src-block verse-block inline-src-block
                                                        latex-fragment) ; check: center-block dynamic-block
                                                          ; add: ? footnote-definition? inline-src-block?
  "Org block types that we wrap to markdown and may get by the first line.")


;; (defvar oai-block--markdown-begin-re "^[\s-]*```\\([^ \t\n[{]+\\)[\s-]?\n")
;; (defvar oai-block--markdown-begin-re "^\\s-*```\\([\w\s_\-]*\\)\\s-*$")
;; (defvar oai-block--markdown-end-re "^\\s-*```\\s-*$")
;; (defvar oai-block--ai-block-begin-re "^#\\+begin_ai.*$")
;; (defvar oai-block--ai-block-end-re "^#\\+end_ai.*$")

(defvar oai-block-tags--org-link-any-re (cl-letf (((symbol-function 'org-link-types)
                                              (lambda () (list "file"))))
                                     (let (
                                           org-link-types-re ; ret
                                           ;; org-link-any-re
                                           org-link-types-re
                                           org-link-angle-re org-link-plain-re ;; org-link-bracket-re
                                           ;; (org-link-make-regexps)
                                           )
                                       (org-link-make-regexps) ; constructor of org-link-types-re, org-link-angle-re, org-link-plain-re, org-link-bracket-re
                                       ;; org-link-types-re
                                       ))
  "`org-link-any-re' but with one type \"file\" in `org-link-types'." )

;; -=-= @Backtrace

(defun oai-block-tags--take-n-lines (string n)
  "Return a string with the first N lines from STRING.
If N exceeds the number of lines, return all lines.  If N <= 0, return
an empty string."
  (let* ((lines (string-split string "\n"))
         (lines-to-keep (cl-subseq lines 0 (min (max 0 n) (length lines)))))
    (mapconcat #'identity lines-to-keep "\n")))

;; (oai-block-tags--take-n-lines "a\nb\nc\nd" 2)
;; (oai-block-tags--take-n-lines "a\nb\nc\nd" 4)
;; (oai-block-tags--take-n-lines "a\nb\nc" 10)
;; (oai-block-tags--take-n-lines "a\nb\nc" 0) ;; ""
;; (oai-block-tags--take-n-lines "a\nb\nc" -3) ;; ""
;; (oai-block-tags--take-n-lines "" 4) ;; ""
;; (oai-block-tags--take-n-lines "x\ny\nz\n" 2)
;; (oai-block-tags--take-n-lines nil 2) ;; error


(defun oai-block-tags--get-backtrace-buffer-string ()
  "Return the contents of the *Backtrace* buffer as a string, or nil.
Nil if buffer does not exist."
  (let ((buf (get-buffer "*Backtrace*")))
    (when buf
      (with-current-buffer buf
        (string-trim (substring-no-properties (buffer-string)))))))

;; -=-= Links: Files & Directories

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

;; replaced with `org-file-contents'
;; (defun oai-block-tags--read-file-to-string-safe (path-string &optional coding)
;;   "Read file to string at PATH-STRING, that should be a readable file."
;;   ;; check
;;   (when (not (and (file-exists-p path-string)
;;                   (file-regular-p path-string)
;;                   (file-readable-p path-string)))
;;            (user-error "File does not exist or not readable: %s" path-string))
;;   ;; read
;;   (condition-case err
;;       (with-temp-buffer
;;         (when coding
;;           (set-buffer-file-coding-system coding))
;;         (insert-file-contents path-string)
;;         (buffer-string))
;;     (error (message "Error reading file %s: %s" path-string err)
;;            nil)))

(defun oai-block-tags--filepath-to-language (path-or-mode-string)
  "Get short name of language that for path major mode string.
PATH-OR-MODE-STRING may be, and we check in this order:
- a symbol, like value of '`major-mode' variable.
- path of file.
- string with name of mode like `emacs-lisp-mode'.
Return Org babel source block language name.
First we check `auto-mode-alist' and then just try to interpret as major
mode line."
  ;; symbol - get "emacs-lisp-mode" or "nil"
  (let* ((symb (symbolp path-or-mode-string))
         (mode-symbol-string (if symb
                                 (symbol-name path-or-mode-string)
                               ;; else - string - path or mode
                               (symbol-name (assoc-default path-or-mode-string auto-mode-alist 'string-match))))
         (mode-symbol-string (if (and (not symb) (string-equal mode-symbol-string "nil"))
                                 path-or-mode-string ; string with mode name
                               ;; else
                               mode-symbol-string))
         (mode-string (apply #'mapconcat #'identity (butlast (string-split mode-symbol-string "-")) '("-"))))
    (or (car (rassq (intern mode-string) org-src-lang-modes))
        (and (not (string-empty-p mode-string))
             mode-string)
        "auto")))

(cl-defun oai-block-tags--compose-m-block (content &optional &key lang header)
  "Return markdown block for LLM with CONTENT.
Markdown block marked as auto language If optional argument LANG is
not provided.
HEADER is a line above mardown to describe it for LLM, should not have
new line characters at edges.
To detect LANG use `oai-block-tags--filepath-to-language'."
  (concat (when header (concat "\n" header))
          (when content (concat "\n```" (or lang "auto") "\n"
                                (string-replace "```" "\\`\\`\\`" content)
                                "\n```"))))

;; (oai-block-tags--compose-m-block "aaa" :lang "bbb" :header "ccc")


(defun oai-block-tags--compose-block-for-path (path-string content)
  "Return mardown block with description.
PATH-STRING may be path to directory or to a file.
For provided PATH-STRING and CONTENT string, return string that will be
good understood by AI."
  (oai-block-tags--compose-m-block
   ;; content:
   content
   :lang (if (file-directory-p path-string)
             (plist-get oai-block-tags--markdown-prefixes :path-directory)
           ;; else file
           (oai-block-tags--filepath-to-language path-string))
   :header (concat "Here " (file-name-nondirectory (directory-file-name path-string)) (if (file-directory-p path-string) " folder:" ":"))))


(defun oai-block-tags--compose-block-for-path-full (path-string)
  "Return file or directory in prepared mardown block.
PATH-STRING may be path to file or a directory."
  (oai--debug "oai-block-tags--compose-block-for-path-full %s" path-string)
  (oai-block-tags--compose-block-for-path path-string
                                          (if (file-directory-p path-string)
                                              (oai-block-tags--get-directory-content path-string)
                                            ;; else
                                            (org-file-contents path-string)))) ; oai-block-tags--read-file-to-string-safe

;; (oai-block-tags--compose-block-for-path-full "/home/g/sources/nongnu/Makefile")
;; -=-= help functions: blocks
;; (defun oai-block-tags--get-org-block-element ()
;;   "Return Org block element at current position in current buffer.
;; Same logic as in `oai-block-tags--get-org-block-region'."
;;   (cl-letf (((symbol-function #'org-element--cache-active-p) (lambda (&rest _) nil)))
;;     (with-syntax-table org-mode-transpose-word-syntax-table
;;       (when-let* ((element
;;                    (cl-loop with context = (org-element-context)
;;                             while (and context
;;                                        (not (member (org-element-type context) oai-block-tags-org-blocks-types)))
;;                             do (setq context (org-element-property :parent context))
;;                             finally return context)))
;;         element))))

;; (defun oai-block-tags--get-org-block-element ()
;;   "Return Org block element at current position in current buffer.
;; If position at begining of of ai block or in any place of supported
;; blocks in `oai-block-tags-org-blocks-types'.
;; Return nil otherwise."
;;   (or (and (org-looking-at-p oai-block--ai-block-begin-re)
;;            (oai-block-p))
;;       (and (when (member (org-element-type (org-element-at-point)) oai-block-tags-org-blocks-types)
;;              (org-element-at-point)))))

(defun oai-block-tags--get-org-block-region (&optional element)
  "Return (beg end) pair for any Org block ELEMENT or nil.
beg end is content begin and #+end.
`org-src--contents-area'
Works for ai block also."
  (when-let* ((element (or (and element
                                (when (member (org-element-type element)
                                              oai-block-tags-org-blocks-types)
                                  element))
                           (oai-block-p)
                           (and (when (member (org-element-type (org-element-at-point)) oai-block-tags-org-blocks-types)
                                (org-element-at-point))))))
    (if (string-equal "ai" (org-element-property :type element))
        (oai-block-contents-begin-end element)
      ;; else
      (nbutlast (org-src--contents-area element) 1)))) ; not support

      ;; (let ((beg (or (org-element-property :contents-begin element)
      ;;                (org-element-property :begin element)))
      ;;       (end (or (org-element-property :contents-end element)
      ;;                (org-element-property :end element))))
      ;;   ;; Bug end is wrong if "#\\+end_" at next line.
      ;;   (when (and beg end)
      ;;     ;; - skip headers if begin at header and fix end bug.
      ;;     (save-excursion
      ;;       (goto-char beg)
      ;;       (when (or (looking-at "#\\+begin_")
      ;;                 (search-forward "#+begin_" end t))
      ;;         (forward-line) ; at begin of line after
      ;;         (setq beg (point)))
      ;;       (when
      ;;           (search-forward "#+end_" end t)
      ;;         (setq end (line-beginning-position)))
      ;;       ;; ;; (goto-char end)
      ;;       ;; (when (or (looking-at "#\\+end_")
      ;;       ;;           (search-backward "#+end_" beg t))
      ;;       ;; (forward-line -1)
      ;;       ;; (setq end (line-beginning-position))
      ;;       ))
      ;;   (list beg end))))

(defun oai-block-tags-get-content (&optional element)
  "For supported blocks With properly expansion of tags and noweb references.
For evaluation, tangling, or exporting."
  (if (eq (org-element-type element) 'src-block)
                           (org-babel--expand-body (org-babel-get-src-block-info))
                         ;; else
                         (if (string-equal "ai" (org-element-property :type element))
                             (oai-block-get-content element)
                           ;; else
                           (string-trim (buffer-substring-no-properties beg end))))

  (when-let* ((element (or element (oai-block-p))))
    (oai-block-get-content)))

(defun oai-block-tags--markdown-fenced-code-body-get-range (&optional limit-begin limit-end)
  "Return (begin end) if point is inside a Markdown fenced language block.
Blocks without language not supported
Markdown block with language specified.
Search for begining and end of block limited by LIMIT-BEGIN and
LIMIT-END optional parameters.
Begin in return at first line after header, end is last line.
Return nil if begin or end of markdown block was not found or block is empty."
  (save-excursion
    (beginning-of-line)
    (let ((point-pos (point))
          begin end)
      ;; fix limits
      (when (and limit-begin (< point-pos limit-begin))
          (setq limit-begin point-pos))
      (when (and limit-end (> point-pos limit-end))
          (setq limit-end point-pos))

      ;; Find nearest header backward
      (when (or (when (looking-at oai-block--markdown-begin-re nil)
                  (forward-line)
                  (setq point-pos (point)))
                (re-search-backward oai-block--markdown-begin-re (or limit-begin nil) t))
        (setq begin (match-end 0))
        ;; Check there is no block footer between begin and point
        (let ((inter-fence-pos nil))
          (save-excursion
            (when (re-search-forward oai-block--markdown-end-re point-pos t)
              (setq inter-fence-pos t)))
          (unless inter-fence-pos
            ;; From header, find next closing fence
            (goto-char begin)
            (when (re-search-forward oai-block--markdown-end-re (or limit-end nil) t)
              (setq end (match-beginning 0))
              (when (and (>= point-pos begin) (< point-pos end))
                (list (1+ begin) (1- end))))))))))


(defun oai-block-tags--markdown-block-range ()
  "Return range if current position in current buffer in markdown block.
Works for language markdown block only inside some org block."
  ;; check that we are in Org block
  (when-let* ((region (oai-block-tags--get-org-block-region))
              (beg (car region))
              (end (cadr region)))
  ;;             (beg (or (org-element-property :contents-begin element)
  ;;                      (org-element-property :begin element))
  ;;             (end (or (org-element-property :contents-end element)
  ;;                      (org-element-property :begin element)))
      (oai-block-tags--markdown-fenced-code-body-get-range beg end)))

;; -=-= help functions: get content for blocks

(defun oai-block-tags--get-org-content-m-block (&optional element)
  "Return markdown block for blocks in Org mode at current position.
May ELEMENT instead.
Used for request, noweb activated with :eval context.
Works only supported blocks in `oai-block-tags-org-blocks-types'.
Move pointer to the end of block.
Steps: find max, min region of special-block/src-block/buffer
`org-babel-read-element' from ob-core.el"
  ;; (org-element-property :name (oai-block-p))
  ;; 1) enshure that we are inside some Org block
  (oai--debug "oai-block-tags--get-org-content-m-block")
  (when-let* ((element (or element (oai-block-p) (org-element-at-point)))

              ;; (region (oai-block-tags--get-org-block-region element))
              ;; (beg (car region))
              ;; (end (cadr region))
              (content (if (eq (org-element-type element) 'src-block)
                           (org-babel--expand-body (org-babel-get-src-block-info))
                         ;; else
                         (if (string-equal "ai" (org-element-property :type element))
                             (oai-block-get-content element)
                           ;; else
                           (string-trim (buffer-substring-no-properties beg end))))))
    (oai-block-tags--compose-m-block
     ;; content
     content
     :lang (if (eq (org-element-type element) 'src-block)
               (org-element-property :language element))
     :header (when-let ((name (org-element-property :name element))) ; nil or string
               (concat "Block name: " name)))))


(defun oai-block-tags--position-in-markdown-block-str-p (str pos) ; TODO: rewrite as `oai-block-tags--markdown-fenced-code-body-get-range' to return range or implement new.
  "Return list range if POS (an index) is inside a '```' code block in STR.
Otherwise return nil.
Substring '```content' without last '```'.
Used in `oai-block-tags--replace-last-regex-smart'."
  (save-match-data
    (let ((search-pos 0)
          (block-boundaries '()))
      ;; Find all the '```' positions
      (while (string-match "```" str search-pos)
        (push (match-beginning 0) block-boundaries)
        (setq search-pos (match-end 0)))
      ;; Sort and pair boundaries
      (setq block-boundaries (sort block-boundaries #'<))
      (catch 'inside
        (let ((bounds block-boundaries))
          (while bounds
            (let ((start (pop bounds))
                  (end (and bounds (pop bounds))))
              (when (and end (>= pos start) (< pos end))
                (throw 'inside (list start end))))))
        nil))))


(defun oai-block-tags--get-m-block ()
  "Get language markdown block or markdown block at current line.
Called for current point in current buffer.

Return markdown language block with language.
Return non-nil string of markdown block if exist at current position."
  (oai--debug "oai-block-tags--get-m-block1")
  (if-let ((range (oai-block-tags--markdown-block-range)))
      (save-excursion
        (buffer-substring-no-properties
         (progn (goto-char (car range))
                ;; (forward-line -1) ; get markdown header too
                (line-beginning-position 0))
         (progn (goto-char (cadr range))
                (forward-line) ; get markdown footer too
                (line-end-position))))
    ;; else - flat mardown block in one line
    (when-let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
                (range (oai-block-tags--position-in-markdown-block-str-p line
                                                                         (- (point) (line-beginning-position)))))
      (oai--debug "oai-block-tags--get-m-block2" range)
      (concat (substring line (car range) (cadr range)) "```"))))


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
;; (print (list (line-beginning-position) (oai-block-tags--line-num-to-positon (line-number-at-pos (point)))))

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

;; TODO!!!!!!!
;; (print major-mode)
;; (oai-block-tags--filepath-to-language)
(defun oai-block-tags--get-content-at-point-not-org ()
  "Return prepared block at current position.
Works in any mode buffers.
1) Use `outline-regexp' if outline or outline-minor mode active
2) Use `beginning-of-defun' for programming mode
3) Use `paragraph-separate' variable."
  (oai--debug "oai-block-tags--get-content-at-point-not-org")
  (cond
   ;; 2) defun
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
   ;; 1) outline
   ((and (or (derived-mode-p 'outline-mode) ; major
             (symbol-value (intern-soft outline-minor-mode))) ; minor
         ;; at header
         (eq 0 (string-match outline-regexp
                             (buffer-substring-no-properties (line-beginning-position)
                                                             (line-end-position))))
         (when-let* ((beg-pos (line-beginning-position))
                    (end-pos (save-excursion (outline-next-heading)
                                             (forward-line -1)
                                             (line-end-position)))) ; return position or nil
           (oai-block-tags--compose-m-block
            ;; content:
            (buffer-substring-no-properties beg-pos end-pos)
                                            :lang (oai-block-tags--filepath-to-language major-mode)
                                            :header "Outliner:"))))
   ;; 3) paragraph
   (paragraph-separate
    (save-excursion
      (forward-line)
      (backward-paragraph)
      (oai-block-tags--compose-m-block
            ;; content:
       (buffer-substring-no-properties (save-excursion (forward-line)
                                                       (line-beginning-position))
                                       (progn
                                         (forward-paragraph)
                                         (line-end-position)))
            :lang (oai-block-tags--filepath-to-language major-mode))))
   (t
    (user-error "No outline, function or paragraph was found to get a block"))))

(defun oai-block-tags--get-content-at-point-org ()
  "For position at begining of the line we prepare block for LLM."
  (let* ((element (org-element-context))
         (type (org-element-type element)))
    (oai--debug "oai-block-tags--get-content-at-point type %s" type)
    ;; - (1) case - headline
    (cond
     ((eq type 'headline)
      (let ((replacement-list)
            ;; (list "**" (org-element-property :title element) "**" "\n")) ; prepare header title
            el ; current element in loop
            type ; type of current element in loop
            )
        (push "\n```text" replacement-list)
        ;; Loop over headlines, to process every blocks and org elements to markdown for LLM
        (while (< (point) (org-element-property :end element))
          ;; supported sub-elements: headline, blocks
          (setq el (org-element-context))
          (setq type (org-element-type el))

          (push (cond
                 ;; 1. Sub: Headline
                 ((eq type 'headline)
                  ;; make string: #*level + title
                  (prog1 (concat "\n" (make-string (org-element-property :level el) ?#) " " (org-element-property :raw-value el))
                    (forward-line))) ; MOVE!
                 ;; 1. Sub: Block
                 ((member type  oai-block-tags-org-blocks-types)
                  (prog1 (oai-block-tags--get-org-content-m-block el)
                    ;; (condition-case nil
                    (org-forward-element) ; MOVE!
                    ;; (org-next-item)
                    ;; (error nil))
                    ))
                 (t ; others
                  ;; (oai--debug "AAA1 %s" (buffer-substring-no-properties (line-beginning-position) (point-max)))
                  (prog1
                      (concat "\n" (buffer-substring-no-properties (line-beginning-position) (org-element-property :end el)))
                    ;; (condition-case nil
                    (org-forward-element)
                    ;; (org-next-item)
                    ;; (error nil))
                    )))
                replacement-list)
          ;; (oai--debug "AAA" replacement-list)
          ) ; push to
        (push "\n```\n" replacement-list)
        ;; (print (list "!!!!!!!!!" (reverse replacement-list)))
        (apply #'concat (reverse replacement-list))))
     ;; - (2) case - Markdown block - in ai block
     ((oai-block-tags--get-m-block))
     ;; - case - Org Block
     ((member type  oai-block-tags-org-blocks-types)
      (oai-block-tags--get-org-content-m-block element))
     ;; - case - Org element with :end
     ((if-let ((end (org-element-property :end element)))
          (oai-block-tags--compose-m-block (buffer-substring-no-properties (line-beginning-position) end))))
     ;; (oai-block-tags--markdown-block-range
     ;; - (3) case -  Org block (or ai block)
     (t
      (user-error "Cant get content at point for link")))))

(defun oai-block-tags--get-content-at-point ()
  "Return prepared block at current position.
Support any mode buffers.  Here code for Org mode.
If at current position there is a Org block or markdown block
Return markdown block for LLM for current element at current position.
May return nil.
For Org buffer only.
Supported: blocks and headers.
- Org header - loop over elements and convert to markdown
- at markdown block header or inside markdown block
- at src header or inside src block
Move pointer to the end of block."
  (oai--debug "oai-block-tags--get-content-at-point")
  (if (not (derived-mode-p 'org-mode))
      (oai-block-tags--get-content-at-point-not-org)
    ;; else - Org mode
    (oai-block-tags--get-content-at-point-org)))



;; (featurep 'org-links)
(declare-function org-links--local-get-target-position-for-link "org-links")

;; (when (require 'org-links nil 'noerror)
(defun oai-block-tags--get-org-links-content (link)
  "In current buffer search for LINK and get content at position.
Works for any mode.
Support for `org-links' package with additional links types.
Headlines not wrapped in markdown blocks.
LINK is string in format is what inside [[...]] or Plain link.
Target may be not in Org buffer."
  ;; (require 'org-links)
  (oai--debug "oai-block-tags--get-org-links-content1 %s " link)
  (if-let ((nums (org-links--local-get-target-position-for-link link))) ; may be nil
      (let ((num1 (car nums))
            (num2 (cadr nums))) ; may be nil
        (oai--debug "oai-block-tags--get-org-links-content2 %s %s" num1 num2)
        ;; 1) Case1: num1 and num2 - get range
        (if num2
            (if-let ((pos1 (oai-block-tags--line-num-to-positon num1))
                     (pos2 (or (oai-block-tags--line-num-to-positon num2 'end-of-line) (point-max))))
                (progn
                  (oai--debug "oai-block-tags--get-org-links-content3 %s %s" pos1 pos2)
                  (oai-block-tags--compose-m-block (buffer-substring-no-properties pos1 pos2)
                                                   :lang (oai-block-tags--filepath-to-language (or (and (derived-mode-p 'fundamental-mode)
                                                                                                        buffer-file-name)
                                                                                                   major-mode))))
              ;; pos1 is nil
              (user-error "In link %s of NUM-NUM format was not possible to find first NUM in buffer %s" link (current-buffer)))
          ;; else - 1) Case1: only num1, num2 is nil - get object at num1 or just line.
          (if-let ((pos1 (oai-block-tags--line-num-to-positon num1)))
              (save-excursion
                (oai--debug "oai-block-tags--get-org-links-content4 %s" pos1)
                (goto-char pos1)
                (oai-block-tags--get-content-at-point))
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
Change buffer to file
Called for file type.
1) open file PATH in new buffer
2) call `oai-block-tags--get-replacement-for-org-link'.  with OPTION"
  (oai--debug "oai-block-tags--get-replacement-for-org-file-link-in-other-file %s %s" path option)
  ;; Code from org-open-file -> find-file-other-window was used:
  (setq path (abbreviate-file-name path))
  (let ((value (or (get-file-buffer path)
                (find-file-noselect path t t nil)))) ; buf name
    (when (listp value)
      (setq value (car value)))
    (with-current-buffer value
      (oai-block-tags--get-replacement-for-org-link (concat "[[" option "]]")))))

(defun oai-block-tags--string-is-integer (str)
  "Return num if STR is an integer, nil otherwise."
  (when (stringp str)
    (let ((val (string-to-number str)))
      (when (and (string= (number-to-string val) str) ; check direct conversion
                 (not (string-match-p "\\." str)))
        val))))     ; disallow decimals


(defun oai-block-tags--get-replacement-for-org-link (link-string)
  "Return string that explain LINK-STRING for LLM or nil.
Supported targets:
- Org block in current buffer \"file:\"
- file: - targets in other files
- file & directory `oai-block-tags--compose-block-for-path-full'
- local link. Use current buffer to find target of link.
Use current buffer, current position to output error to result of block
if two targets found.
Return replacement string."
  ;; Some code was taken from:
  ;; `org-link-open' for type and opening,  `org-link-search' for search in current buffer.
  ;; from `org-link-open-from-string'
  ;; - - 1) convert string to Org element
  (oai--debug "oai-block-tags--get-replacement-for-org-link %s" link-string)
  (let ((link-el (with-temp-buffer
                   (let ((org-inhibit-startup nil))
                     (insert link-string)
                     (org-mode)
                     (goto-char (point-min))
                     (org-element-link-parser)))))
    (if (not link-el)
      (user-error "No valid link in %S" link-string))
    ;; from `org-link-open'
    ;; - - 2) extract path and type
    (let ((type (org-element-property :type link-el))
          (path (org-element-property :path link-el)))
      ;; (print (list "type?" type path link-el))
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
               (if (oai-block-tags--path-is-current-buffer-p path)
                   (if-let ((num (oai-block-tags--string-is-integer option)))
                       ;; case 2) PATH::NUM
                       (progn (org-goto-line num) (oai-block-tags--get-content-at-point))
                     ;; else case 3) recursion call without path
                     (oai-block-tags--get-replacement-for-org-link (concat "[[" option "]]"))) ; recursive call
                 ;; - else case 4) <other-file>
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
                (oai-block-tags--get-org-links-content (org-element-property :raw-link link-el))) ; NUM-NUM
            ;; 0) search with `org-link-search' and get content with `oai-block-tags--get-content-at-point'
            (let ((ln-before (line-number-at-pos))
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
              (oai-block-tags--get-content-at-point))))))))))
;; - test:
;; (if (not (let ((oai-block-tags-use-simple-directory-content-flag t))
;;            (and
;;             (string-match "oai-block-tags.el" (oai-block-tags--get-replacement-for-org-link "file:./"))
;;             (string-match "oai-block-tags.el" (oai-block-tags--get-replacement-for-org-link "[[./]]"))
;;             (string-match "oai-block-tags.el" (oai-block-tags--get-replacement-for-org-link "[[file:./]]"))
;;             (string-match "oai-block-tags.el" (oai-block-tags--get-replacement-for-org-link "[[file:.]]"))
;;             )))
;;     (error "oai-block-tags-use-simple-directory-content-flag dir ./"))

;; [[file:oai-block-tags.el::`(ref)']]]
;; (oai-block-tags--get-replacement-for-org-link  "[[xx]]")

;; -=-= help functions: markdown blocks

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

;; (defun oai-block-tags--string-count-char-in-direction (string position char direction)
;;   "Count CHAR in DIRECTION from POSITION on the same line in STRING."
;;   ;; Special case: Cursor is at or after newline, nothing to count.
;;   (if (or (and (eq direction 'left)
;;                (> position 0)
;;                (char-equal (aref string (1- position)) ?\n))
;;           (and (eq direction 'right)
;;                (char-equal (aref string position) ?\n)))
;;       0
;;     ;; Normal case
;;     (let* ((count 0)
;;            (step (if (eq direction 'right) 1 -1))
;;            (pos (+ position step))
;;            (len (length string)))
;;       (while (and (>= pos 0) (< pos len))
;;         (let ((c (aref string pos)))
;;           (cond
;;            ((char-equal c char) (setq count (1+ count)))
;;            ((char-equal c ?\n) (setq pos len)))  ; break
;;           (setq pos (+ pos step))))
;;       count)))

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
  (oai--debug "oai-block-tags--replace-last-regex-smart" str-orig)
  (let ((pos 0)
        (last-pos nil)
        (last-end nil))
    ;;
    (while (and pos
                (string-match regexp str-orig pos))

      (setq pos (match-beginning 0))
      ;; (print (list "sdasd" pos))
      (unless (or (oai-block-tags--position-in-markdown-block-str-p str-orig pos) ; not in ``` multiline markdown block
                  (oai-block-tags--string-is-quoted-p str-orig pos))
        (setq last-pos pos)
        (setq last-end (match-end 0))) ; end

      ;; (print (list "sdasd2" last-end))
      (setq pos (match-end 0))) ; move forward

    (if replacement
        (if last-pos
            ;; (replace-match replacement 'fixedcase 'literal str-orig)
            ;; (if (eq (aref str-orig (1- last-end)) ?\s) ;; if space after match
            ;; (replace-match replacement 'fixedcase 'literal str-orig 1)
            ;; 1) replace
            (progn (add-text-properties 0 (length replacement) '(face region) replacement)
                   (concat (substring str-orig 0 last-pos)
                           replacement
                           ;; last-group
                           (substring str-orig last-end)))
          ;; else - return just str-orig
          str-orig)
      ;; else no replacement
      ;; (when last-pos (oai--debug "oai-block-tags--replace-last-regex-smart2 c%sc" (match-string 0 str-orig)))
      (if last-pos
          (replace-regexp-in-string "^[` ]*" ""
                                    (replace-regexp-in-string "[` ]*\$" ""
                                                              (match-string 0 str-orig))) ;; (substring str-orig last-pos last-end)
        nil))))


(defun oai-block-tags-replace (string)
  "Replace links in STRING with their targets.
Check every type of links if it exist in text, find replacement for the
fist link and replace link substring with
`oai-block-tags--replace-last-regex-smart' once.
Return modified string with text properties or the same string.
Used for function `oai-restapi--modify-vector-content'.
Called from:
- `oai-expand-block' interactive function
- `oai-restapi-request-prepare'
- `oai-restapi-request-llm-retries'."
  (oai--debug "oai-block-tags-replace: \"%s\"" string)
  ;; - "@Backtrace" substring exist - replace the last one only
  ;; Result will be *Wrapped in markdown*
  (let ((i 99))
    (while (and (string-match oai-block-tags--regexes-backtrace string)
                (not (zerop i)))
      (setq i (1- i))
      (oai--debug "oai-block-tags-replace: backtrace")
      (if-let* ((bt (or (oai-block-tags--get-backtrace-buffer-string)
                        (user-error "No backtrace buffer for @Backtrace tag"))) ; *Backtrace* buffer exist
                (bt (oai-block-tags--take-n-lines bt oai-block-tags-backtrace-max-lines))
                (bt (concat "\n```" (plist-get oai-block-tags--markdown-prefixes :backtrace) "\n"
                            bt
                            oai-block-tags--markdown-postfix)) ; prepare string
                (new-string (oai-block-tags--replace-last-regex-smart string oai-block-tags--regexes-backtrace bt))) ; insert backtrace
          (setq string new-string))))

  ;; - Path @/path/file.txt - replace the last one only
  ;; Result will be *Wrapped in markdown*
  (let ((i 99))
    (while (and (string-match oai-block-tags--regexes-path string)
                (not (zerop i)))
      (setq i (1- i))
      (oai--debug "oai-block-tags-replace: regexes-path")
      (if-let* ((path-string (oai-block-tags--replace-last-regex-smart string oai-block-tags--regexes-path)) ; find the last
                ;; remove first @ character from link

                (path-string (if (> (length path-string) 0)
                                 (substring path-string 1)
                               ""))
                (replacement (concat (oai-block-tags--compose-block-for-path-full path-string) "\n"))
                (new-string (oai-block-tags--replace-last-regex-smart string
                                                                      oai-block-tags--regexes-path
                                                                      replacement)))
          (setq string new-string))))

  ;; - Org links [[link]]
  ;; We search  for link regex,  when found we check  if there
  ;; are double of  found substring after founded  one, if one
  ;; more exist we skip the first  one that found. if no other
  ;; exist we replace it.
  ;; *Dont Wrap in markdown*
  (let ((i 9))
    (while (and (string-match oai-block-tags--org-link-any-re string) ; exist in text?
                (not (zerop i)))
      (setq i (1- i))
      (oai--debug "oai-block-tags-replace: link-any-r")
      (if-let* ((link (oai-block-tags--replace-last-regex-smart string oai-block-tags--org-link-any-re)) ; find the last

                (replacement (concat (oai-block-tags--get-replacement-for-org-link link) "\n")) ; add empty line after it.
                (new-string (oai-block-tags--replace-last-regex-smart string
                                                                      oai-block-tags--org-link-any-re
                                                                      replacement)))
          (setq string new-string))))
  ;; old
  ;; (when (not (string-match oai-block-tags--org-link-any-re string)) ; exist in text?
  ;;   (let ((new-string string) ; ai block content
  ;;         (path-string (oai-block-tags--replace-last-regex-smart string oai-block-tags--org-link-any-re))
  ;;         (replaced nil)
  ;;         (pos-end 0)
  ;;         (pos-beg 0)
  ;;         (link)
  ;;         (replacement))
  ;;     (print (list "oai-block-tags-replace 0" path-string))
  ;;     ;; 1) find link
  ;;     (while (or (= pos-end 0) ; skip first
  ;;                (string-match oai-block-tags--org-link-any-re new-string pos-end)) ; loop over links in text
  ;;       (print "found link")
  ;;       (setq pos-beg (match-beginning 0))
  ;;       (setq pos-end (match-end 0))
  ;;       (setq link (match-string 0 new-string))
  ;;       (print (list "oai-block-tags-replace 1" (oai-block-tags--position-in-markdown-block-str-p new-string pos-beg)))
  ;;       (when (not (oai-block-tags--position-in-markdown-block-str-p new-string pos-beg)) ; skip links inside markdown
  ;;         (print (list "oai-block-tags-replace 2"))


  ;;         ;; check that there is no clone of this below, hence we replace the last one.
  ;;         (print (list  (regexp-quote link) new-string pos-end (string-match (regexp-quote link) new-string pos-end)))
  ;;         (when (not (and (string-match (regexp-quote link) new-string pos-end)
  ;;                         (not (oai-block-tags--position-in-markdown-block-str-p new-string pos-beg))))
  ;;           (print (list "oai-block-tags-replace 3" link))
  ;;           ;; 2) get target for link
  ;;           (setq replacement (oai-block-tags--get-replacement-for-org-link link))
  ;;           (print (list "oai-block-tags-replace 4 replacement" replacement))
  ;;           (when replacement
  ;;             (setq new-string (concat (substring new-string 0 pos-beg)
  ;;                                      replacement
  ;;                                      ;; last-group
  ;;                                      (substring new-string pos-end)))
  ;;             (setq pos-end (+ pos-beg (length replacement)))
  ;;             ;; (print (list pos-end new-string))
  ;;             (setq replaced t)
  ;;             ;; (oai-block-tags--replace-last-regex-smart)
  ;;             ))))
  ;;     (when replaced
  ;;       (setq string new-string))))


  ;; (if-let* ((path-string (oai-block-tags--replace-last-regex-smart string oai-block-tags--org-link-any-re)))
  ;;     (progn
  ;;       (print (list "link" path-string))
  ;;       string))
  ;; - default
  string)

(defun oai-block-tags--clear-properties (string)
  "Remove text properties from STRING.
Used as argument fo function `oai-restapi--modify-vector-content'.
Used for `oai-expand-block' that show fontificated of markdown blocks,
made by `oai-block-tags--replace-last-regex-smart'.
Return modified STRING."
  (set-text-properties 0 (length string) nil string)
  string)

;; (oai-block-tags-replace  "[[./]]")
;; (oai-block-tags-replace  "11[[sas]]222[[bbbaa]]3333[[sas]]4444")
;; (oai-block-tags-replace  "11[[file:/mock/org.org::1::* headline]]4444")
  ;; [[file:/mock/org.org::1::* headline]]

;; (let* ((link (with-temp-buffer
;;                (let ((org-inhibit-startup nil))
;;                  (insert "[[file:~/docsmy_short/modified/emacsh::*graphiz - graphs][graphiz - graphs]]")
;;                  (org-mode)
;;                  (goto-char (point-min))
;;                  (org-element-link-parser))))
;;            (type (org-element-property :type link))
;;            (path (org-element-property :path link))
;;            (follow (org-link-get-parameter type :follow))
;;            (option (org-element-property :search-option link))) ;; after ::
;;       (print (list type path option follow)))


;; -=-= Fontify @Backtrace & @path & [[links]]

;; (defun oai-block-tags--is-special (pos &optional lim-beg)
;;   "Check if POS in markdown block, quoted or is a table.
;; Optional argument LIM-BEG is ai block begining position.
;; Return t if pos in markdown block, table or quote.
;; Side-effect: set pointer position to POS."
;;   (goto-char pos)
;;   (prog1 (or
;;           ;; not markdown blocks
;;           ;; backward for markdown block "begin"
;;           (when (re-search-backward oai-block--markdown-begin-re lim-beg t)
;;             (goto-char pos)
;;             ;; backward for markdown block "end" after "begin"
;;             (not (re-search-backward oai-block--markdown-end-re (match-end 0) t)))
;;           ;; not quotes
;;           (progn (goto-char pos)
;;                  (beginning-of-line)
;;                  (or (looking-at "^> ") ; from `oai-block-fill-region-as-paragraph'
;;                      (looking-at "^[ \t]*\\(|\\|\\+-[-+]\\).*")))) ; skip tables
;;     (goto-char pos)))

(defun oai-block-tags--font-lock-fontify-links (limit)
  "Fontify Org links in #+begin_ai ... #+end_ai blocks, up to LIMIT.
This is special fontify function, that return t when match found.
1) search for ai block begin and then end, 2) call fontify on range that
goto to the begining firstly function `org-activate-links' used to
highlight any link.
TODO: maybe we should use something like
`oai-block-tags--position-in-markdown-block-str-p'"
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
            ;; (message beg)
            ;; - [[link][]]
            (progn
              (goto-char beg)
              (while (re-search-forward oai-block-tags--org-link-any-re end t)
                (setq lbeg (match-beginning 0))
                (setq lend (match-end 0))
                ;; (print (list lbeg beg end (oai-block--in-special lbeg beg)))
                (unless (or (oai-block--at-special-p lbeg)
                            (oai-block--in-markdown-any-quotes-p lbeg))
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
                            (oai-block--in-markdown-any-quotes-p lbeg))
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
                            (oai-block--in-markdown-any-quotes-p lbeg))
                  (add-face-text-property lbeg lend 'org-link)
                  (setq ret t))
                (goto-char lend))))))
    ;; required by font lock mode:
    (goto-char limit)
    ret))

;; -=-= key to select block "C-c h" (similar to "M-h")

;; (defun oai-block-tags-mark-md-block-body ()
;;   "Mark content of Markdown code block, or fallback to `org-mark-element'.
;; Mark or select block content around cursor.
;; Forst we get Org block boudaries."
;;   (interactive)
;;   (or (when-let* ((region (oai-block-tags--get-org-block-region))
;;                   (beg (car region))
;;                   (end (cadr region)))
;;         ;; if pointer in Mardown block, we mark Markdown block only
;;         (or (oai-block-tags--markdown-mark-fenced-code-body beg end)
;;             ;; else - no markdown - mark ai block only
;;             (unless (= beg end)
;;               (push-mark beg t)
;;               (goto-char (1- end))
;;               (activate-mark)))
;;         t)
;;       (org-mark-element)))

(provide 'oai-block-tags)
;;; oai-block-tags.el ends here
