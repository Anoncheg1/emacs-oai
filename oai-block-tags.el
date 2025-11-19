;;; oai-block-tags.el --- Handling links inside ai block  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 github.com/Anoncheg1

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
;; How this works?
;; We appply `oai-block-tags-replace' to text of last user request, this way:
;; in oai.el: (oai-restapi--modify-last-user-content expanded #'oai-block-tags-replace)
;;
;;
;; We support @links:
;; - @Backtrace
;; - #PATH - directory/file
;; - @name - same to Org [[target]]
;;
;; We support Org ol.el package links:
;; - [[PATH]]
;;
;; We support "org-links" package new links: (TODO)
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
;;
;;; -=-= includes
(require 'org)
(require 'ol)
(require 'oai-debug)
(require 'org-links nil 'noerror)

;;; Code:
;;; -=-= variables
;; (defvar oai-block-tags--regexes '(
;;                                ;; :backtrace "@Backtrace`?\\([^a-zA-Z]\\|$\\)"
;;                                ;; :backtrace "\\(`?@Backtrace`?\\)\\([^a-zA-Z\"']\\|$\\)"
;;                                :backtrace "\\(`?@\\(Backtrace\\|Bt\\)`?\\)\\([^a-zA-Z\"']\\|$\\)"
;;                                :path "`?@\\(\\.\\.?/\\|\\.\\.?\\\\\\|\\.\\.?\\|/\\|\\\\\\|[A-Za-z]:\\\\\\)[a-zA-Z0-9_./\\\\-]*`?"
;;                                           ))
(defvar oai-block-tags--regexes-backtrace "\\(`?@\\(Backtrace\\|B\\)`?\\)\\([^a-zA-Z\"']\\|$\\)")
(defvar oai-block-tags--regexes-path "`?@\\(\\.\\.?/\\|\\.\\.?\\\\\\|\\.\\.?\\|/\\|\\\\\\|[A-Za-z]:\\\\\\)[a-zA-Z0-9_./\\\\-]*`?")

(defvar oai-block-tags--markdown-prefixes '(:backtrace "```elisp-backtrace"
                                            :path-directory "```ls-output"
                                            :path-file  "```"))
(defvar oai-block-tags--markdown-postfix "\n```\n")

(defvar oai-block-tags--backtrace-max-lines 12
  "Max lines to get from Backtrace buffer from begining.
All lines are rarely required, first 4-8 are most imortant.")

(defvar oai-block-tags-use-simple-directory-content nil
  "If non-nil use `directory-files' with simple list of item.
Otherwise ls command used.  Also `directory-files-and-attributes' may be
used.")

(defvar oai-block-tags-error-on-missing-link t
  "If non-nil signal error for not found link.
Used to set `org-link-search-must-match-exact-headline' before
`org-link-search' function call.")

(defvar oai-block-tags--check-double-targets-found t
  "Signal error if link in ai block point to targets in same file.")

(cl-assert
  (equal (mapcar (lambda (s)
                   (when (string-match oai-block-tags--regexes-path s)
                     (substring s (match-beginning 0) (match-end 0))))
                 '("@/file-s_s"
                   "@/file.t_xt"
                   "@./file.txt"
                   "@/some/path/file.txt"
                   "@C:\\some\\file.txt"
                   "@L:\\folder\\file.txt"
                   "@\\network\\share"
                   "@.\\windowsfile"
                   "@/file/"
                   "@/file.txt/"
                   "@./file.txt/"
                   "@/some/path/file.txt/"
                   "@C:\\some\\file.txt\\"
                   "@L:\\folder\\file.txt\\"
                   "@\\network\\share\\"
                   "@.\\windowsfile\\"
                   "@Backtrace"
                   "@not/a/path"
                   "@Backtrace"
                   "@not/a/path"
                   "@not/a/path/"
                   "@../right"
                   "@../right/"
                   "@.."
                   "@."
                   "@/"))
         '("@/file-s_s" "@/file.t_xt" "@./file.txt" "@/some/path/file.txt" "@C:\\some\\file.txt" "@L:\\folder\\file.txt" "@\\network\\share" "@.\\windowsfile" "@/file/" "@/file.txt/" "@./file.txt/" "@/some/path/file.txt/" "@C:\\some\\file.txt\\" "@L:\\folder\\file.txt\\" "@\\network\\share\\" "@.\\windowsfile\\" nil nil nil nil nil "@../right" "@../right/" "@.." "@." "@/")))

(defvar oai-block-tags-org-blocks-types '(comment-block center-block dynamic-block example-block
                                                        export-block quote-block special-block
                                                        src-block verse-block)
  "Org block types that we wrap to markdown and may get by the first line.")


(defvar oai-block--markdown-begin-re "^[\s-]*```\\([^ \t\n[{]+\\)[\s-]?\n")
(defvar oai-block--markdown-end-re "^[\s-]*```[\s-]?$")
(defvar oai-block--ai-block-begin-re "^#\\+begin_ai[^\n]*\n")
(defvar oai-block--ai-block-end-re "^#\\+end_ai.*$")

(defvar oai-block--org-link-any-re (cl-letf (((symbol-function 'org-link-types)
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

;;; -=-= @Backtrace

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

;;; -=-= Links: Files & Directories

(defvar oai-block-tags-get-directory-switches "-AltGg")

(defun oai-block-tags--get-directory-content (path-string)
  "Return string with list of files at PATH-STRING."
  (if oai-block-tags-use-simple-directory-content
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

(cl-assert
 (string-equal (oai-block-tags--filepath-to-language 'emacs-lisp-mode) "elisp"))
(cl-assert
 (string-equal (oai-block-tags--filepath-to-language "emacs-lisp-mode") "elisp"))
(cl-assert
 (string-equal (oai-block-tags--filepath-to-language "/tmp/a.el") "elisp"))
(cl-assert
 (string-equal (oai-block-tags--filepath-to-language "/tmp/a.py") "python"))
(cl-assert
 (string-equal (oai-block-tags--filepath-to-language "asaas") "auto")) ;unknwon
(cl-assert
 (string-equal (oai-block-tags--filepath-to-language "/tmp/a.elfff") "auto")) ;unknwon
(cl-assert
 (string-equal (oai-block-tags--filepath-to-language "/tmp/txt") "auto")) ;unknwon
(cl-assert
 (string-equal (oai-block-tags--filepath-to-language "/tmp/a.org") "org"))
(cl-assert
 (string-equal (oai-block-tags--filepath-to-language "a.txt") "text"))

(defun oai-block-tags--compose-block-for-path (path-string content)
  "Return mardown block with description.
PATH-STRING may be path to directory or to a file.
For provided PATH-STRING and CONTENT string, return string that will be
good understood by AI."
  (concat
   "Here " (file-name-nondirectory (directory-file-name path-string)) (if (file-directory-p path-string) " folder" "") ":\n"
   ;; prefix
   (if (file-directory-p path-string)
       (plist-get oai-block-tags--markdown-prefixes :path-directory)
     ;; else - not derectory
     (concat
      (plist-get oai-block-tags--markdown-prefixes :path-file) ; "```"
      (oai-block-tags--filepath-to-language path-string))) ; "elisp"
   "\n"
   content
   "\n```"))

(cl-assert
 (string-equal
  (file-name-nondirectory (directory-file-name "/tmp/asd")) "asd"))
(cl-assert
 (string-equal
  (file-name-nondirectory (directory-file-name "/tmp/")) "tmp"))
(cl-assert
 (string-equal
  (file-name-nondirectory (directory-file-name "/tmp")) "tmp"))

(cl-assert
 (string-equal (oai-block-tags--compose-block-for-path "a.el" "ss")
"Here a.el:
```elisp
ss
```"))

(defun oai-block-tags--compose-block-for-path-full (path-string)
  "Return file or directory in prepared mardown block.
PATH-STRING may be path to file or a directory."
  (oai-block-tags--compose-block-for-path path-string
                                          (if (file-directory-p path-string)
                                              (oai-block-tags--get-directory-content path-string)
                                            ;; else
                                            (org-file-contents path-string)))) ; oai-block-tags--read-file-to-string-safe

;;; -=-= help functions: blocks
(defun oai-block-tags--get-org-block-element ()
  "Return Org block element at current position in current buffer.
Same logic as in `oai-block-tags--get-org-block-region'."
  (cl-letf (((symbol-function #'org-element--cache-active-p) (lambda (&rest _) nil)))
    (with-syntax-table org-mode-transpose-word-syntax-table
      (when-let* ((element
                   (cl-loop with context = (org-element-context)
                            while (and context
                                       (not (member (org-element-type context) oai-block-tags-org-blocks-types)))
                            do (setq context (org-element-property :parent context))
                            finally return context)))
        element))))

(defun oai-block-tags--get-org-block-region (&optional element)
  "Return (beg end) pair for any Org block ELEMENT or nil.
Works for ai block also."
    (when-let* ((element
                 (or element
                     (oai-block-tags--get-org-block-element))))
      (let ((beg (or (org-element-property :contents-begin element)
                     (org-element-property :begin element)))
            (end (or (org-element-property :contents-end element)
                     (org-element-property :end element))))
        ;; Bug end is wrong if "#\\+end_" at next line.
        (when (and beg end)
          ;; - skip headers if begin at header and fix end bug.
          (save-excursion
            (goto-char beg)
            (when (or (looking-at "#\\+begin_")
                      (search-forward "#+begin_" end t))
              (forward-line) ; at begin of line after
              (setq beg (point)))
            (when
                (search-forward "#+end_" end t)
              (setq end (line-beginning-position)))
            ;; ;; (goto-char end)
            ;; (when (or (looking-at "#\\+end_")
            ;;           (search-backward "#+end_" beg t))
            ;; (forward-line -1)
            ;; (setq end (line-beginning-position))
            ))
        (list beg end))))

(defun oai-block-tags--markdown-fenced-code-body-get-range (&optional limit-begin limit-end)
  "Return (begin end) if point is inside a Markdown fenced block.
Search for begining and end of block limited by LIMIT-BEGIN and
LIMIT-END optional parameters.
Begin in return at first line after header, end is last line.
Return nil if begin or end of markdown block was not found."
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
                (list begin end)))))))))

(cl-assert (equal '(39 42)
                  (with-temp-buffer
                    (org-mode)
                    (insert "#+NAME: asd\n#+begin_src text\n```elisp")
                    (let ((p (point)))
                      (insert "\naa\n```\n#+end_src\n")
                      (goto-char p)
                      (oai-block-tags--markdown-fenced-code-body-get-range)))))

(defun oai-block-tags--markdown-block-range ()
  "Return range if current position in current buffer in markdown block.
Works for markdown block only inside some org block"
  ;; check that we are in Org block
  (when-let* ((region (oai-block-tags--get-org-block-region)) ; not working properly!!!
              (beg (car region))
              (end (cadr region)))
  ;; (when-let* ((element ;; (oai-block-p))
  ;;              (cl-loop with context = (org-element-context)
  ;;                       while (and context
  ;;                                  (not (member (org-element-type context) oai-block-tags-org-blocks-types))
  ;;                                  ;; (not (equal 'special-block (org-element-type context)))
  ;;                                  ;; (not (string-equal "ai" (org-element-property :type context)))
  ;;                                  )
  ;;                       do (setq context (org-element-property :parent context))
  ;;                       finally return context))
  ;;             (beg (or (org-element-property :contents-begin element)
  ;;                      (org-element-property :begin element))
  ;;             (end (or (org-element-property :contents-end element)
  ;;                      (org-element-property :begin element)))
      ;; (let ((search-pos 0)
      ;;       (block-boundaries '()))
      ;; (print (list beg end))
      (oai-block-tags--markdown-fenced-code-body-get-range beg end)))
      ;; ;; Find all the '```' positions
      ;; (while (string-match "```" str search-pos)
      ;;   (push (match-beginning 0) block-boundaries)
      ;;   (setq search-pos (match-end 0)))
      ;; ;; Sort and pair boundaries
      ;; (setq block-boundaries (sort block-boundaries #'<))
      ;; (catch 'inside
      ;;   (let ((bounds block-boundaries))
      ;;     (while bounds
      ;;       (let ((start (pop bounds))
      ;;             (end (and bounds (pop bounds))))
      ;;         (when (and end (>= pos start) (< pos end))
      ;;           (throw 'inside t)))))
      ;;   nil))))

;; (re-search-backward oai-block--markdown-begin-re (or limit-begin nil) t)

;;; -=-= help functions: get content for blocks

;; (defun oai-block-tags--get-org-content-m-block (&optional element)
;;   "Return markdown block for LLM for current ELEMENT at current position.
;; Move pointer to the end of block.
;; Steps: find max, min region of special-block/src-block/buffer
;; `org-babel-read-element' from ob-core.el"
;;   ;; (org-element-property :name (oai-block-p))
;;   ;; 1) enshure that we are inside some Org block
;;   (when-let* ((element (or element (oai-block-tags--get-org-block-element)))
;;               (region (oai-block-tags--get-org-block-region element))
;;               (beg (car region))
;;               (end (cadr region)))

;;     ;; Compose result block
;;     ;; (goto-char end) ; for return
;;     ;; (print "oai-block-tags--get-org-content-m-block")
;;     (concat
;;      ;; - 0 - Name
;;      (when-let ((name (org-element-property :name element))) ; nil or string
;;        (concat "\nBlock name: " name))
;;      ;; - 1 - Header ```
;;      (if (eq (org-element-type element) 'src-block)
;;          (concat "\n```"  (org-element-property :language element) "\n")
;;        ;; else
;;        "\n```text\n")
;;      ;; - 2 - Body
;;      (string-trim (buffer-substring-no-properties beg end))
;;      ;; - 3 - Footer ```
;;      "\n```")))

(cl-defun oai-block-tags--compose-m-block (content &optional &key lang header)
  "Return markdown block for LLM with CONTENT.
Markdown block marked as auto language If optional argument LANG is
not provided.
HEADER is a line above mardown to describe it for LLM, should not have
new line characters at edges."
  (concat (when header (concat "\n" header))
          (when content (concat "\n```" (or lang "auto") "\n"
                                (string-replace "```" "\\`\\`\\`" content)
                                "\n```"))))

;; (oai-block-tags--compose-m-block "aaa" :lang "bbb" :header "ccc")

(defun oai-block-tags--get-org-content-m-block (&optional element)
  "Return markdown block for blocks in Org mode at current position.
May ELEMENT instead.
Support `oai-block-tags-org-blocks-types'.
Move pointer to the end of block.
Steps: find max, min region of special-block/src-block/buffer
`org-babel-read-element' from ob-core.el"
  ;; (org-element-property :name (oai-block-p))
  ;; 1) enshure that we are inside some Org block
  (when-let* ((element (or element (oai-block-tags--get-org-block-element)))
              (region (oai-block-tags--get-org-block-region element))
              (beg (car region))
              (end (cadr region)))
    (oai-block-tags--compose-m-block
     ;; content
     (string-trim (buffer-substring-no-properties beg end))
     :lang (if (eq (org-element-type element) 'src-block)
               (org-element-property :language element))
     :header (when-let ((name (org-element-property :name element))) ; nil or string
               (concat "Block name: " name)))))

(cl-assert
 (string-equal
  "\nBlock name: asd\n```elisp\naa\n```"
  (with-temp-buffer
    (org-mode)
    (insert "#+NAME: asd\n#+begin_src elisp\naa\n#+end_src\n")
    (goto-char 11)
    (oai-block-tags--get-org-content-m-block))))



(defun oai-block-tags--position-in-markdown-block-str-p (str pos) ; TODO: rewrite as `oai-block-tags--markdown-fenced-code-body-get-range' to return range or implement new.
  "Return list range if POS (an index) is inside a '```' code block in STR.
Otherwise return nil.
Substring '```content' without last '```'."
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
              (throw 'inside block-boundaries)))))
      nil)))

;; (progn (string-match "```" "aaa```bbb```ccc")
(if-let* ((line "aaa```bbb```ccc")
          (range (oai-block-tags--position-in-markdown-block-str-p line 5))
          (mar (substring line (car range) (cadr range))))
    (unless (string-equal mar "```bbb")
      (error "Error: oai-block-tags--position-in-markdown-block-str-p1"))
  ;; else
  (error "Error: oai-block-tags--position-in-markdown-block-str-p2"))

(if (not (oai-block-tags--position-in-markdown-block-str-p "aaa```bbb```ccc" 5))   ;; => t   (inside first block)
    (error "Error: oai-block-tags--position-in-markdown-block-str-p3"))
(if (oai-block-tags--position-in-markdown-block-str-p "aaa```bbb```ccc" 10)  ;; => nil (outside any block)
    (error "Error: oai-block-tags--position-in-markdown-block-str-p4"))


(defun oai-block-tags--get-m-block ()
  "Called for current point.
Return non-nil string of markdown block if exist at current position."
  (oai--debug "oai-block-tags--get-m-block1")
  (if-let ((range (oai-block-tags--markdown-block-range)))
      (save-excursion
        (buffer-substring-no-properties
         (progn (goto-char (car range))
                (forward-line -1)
                (line-beginning-position))
         (progn (goto-char (cadr range))
                ;; (forward-line)
                (line-end-position))))
    ;; else check if mardown block in one line
    (when-let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
                (range (oai-block-tags--position-in-markdown-block-str-p line
                                                                         (- (point) (line-beginning-position)))))
      (oai--debug "oai-block-tags--get-m-block2" range)
      (substring line (car range) (cadr range)))))


;;; -=-= help function: line number for position

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

;;; -=-= help functions: find targets of Links and get content

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
    (user-error "No outline, function or paragraph was found to get a block.?"))))


;; Test: outline
(cl-assert
 (string-equal
  "\nOutliner:\n```elisp\n;; -- header1\ntext1\n```"
  (with-temp-buffer
    (emacs-lisp-mode)
    (outline-minor-mode)
    (setq-local outline-regexp ";; \\-\\- ")
    (insert "text0\n")
    (let ((p (point)))
      (insert ";; -- header1\ntext1\n")
      (insert ";; -- header2\ntext2\n")
      (goto-char (1+ p))
      (oai-block-tags--get-content-at-point-not-org)))))

;; Test: defun
(cl-assert
 (string-equal
  "\nFunction:\n```elisp\n(defun f1 ()\nt\n)\n```"
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun f1 ()\nt\n)\n\n(defun f2 ()\nt\n)")
    (goto-char 1)
    (oai-block-tags--get-content-at-point-not-org))))

;; Test: paragraph
(cl-assert
 (string-equal
  "\n```text\n;; -- header2\ntext2\n```"
  (with-temp-buffer
    (text-mode)
    (setq-local paragraph-start "\f\\|[ \t]*$")
    (setq-local paragraph-separate "[ \t\f]*$")
    (let ((p))
      (progn
        (insert "text0\n")
        (insert "\n")
        (insert ";; -- header1\ntext1\n")
        (setq p (point))
        (insert "\n")
        (insert ";; -- header2\ntext2"))
      (goto-char p))
      (oai-block-tags--get-content-at-point-not-org))))


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
    (let* ((element (org-element-context)) ; should not be changed
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
          (push "```text" replacement-list)
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
                    (prog1
                        (concat "\n" (buffer-substring-no-properties (line-beginning-position) (org-element-property :end el)))
                      ;; (condition-case nil
                      (org-forward-element)
                      ;; (org-next-item)
                      ;; (error nil))
                      )))
                  replacement-list)) ; push to
          (push "\n```\n" replacement-list)
          ;; (print (list "!!!!!!!!!" (reverse replacement-list)))
          (apply #'concat (reverse replacement-list))))
       ;; - (2) case - Markdown block
       ((oai-block-tags--get-m-block))
       ;; (oai-block-tags--markdown-block-range
       ;; - (3) case -  Org block (or ai block)
       (t
        (oai-block-tags--get-org-content-m-block))))))

(cl-assert
 (string-equal
  "\nBlock name: asd\n```elisp\naa\n```"
  (with-temp-buffer
    (org-mode)
    (insert "ssd\n#+NAME: asd\n#+begin_src elisp\naa\n#+end_src\n")
    (goto-char 15)
    (oai-block-tags--get-content-at-point))))

(cl-assert
 (string-equal
  "```elisp\naa\n```"
  (with-temp-buffer
    (org-mode)
    (insert "#+NAME: asd\n#+begin_src text\n```elisp")
    (let ((p (point)))
      (insert "\naa\n```\n#+end_src\n")
      (goto-char p)
      ;; (oai-block-tags--get-org-block-region)))
      ;; (oai-block-tags--markdown-block-range)))
      ;; (oai-block-tags--get-m-block)))
      (oai-block-tags--get-content-at-point)))))

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
                   :lang (oai-block-tags--filepath-to-language major-mode)))
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
    (let ((org-link-search-must-match-exact-headline oai-block-tags-error-on-missing-link)) ;; should found?
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
Called for file type.
1) open file PATH in new buffer
2) call `oai-block-tags--get-replacement-for-org-link'.  with OPTION"
  (oai--debug "oai-block-tags--get-replacement-for-org-file-link-in-other-file %s %s" path option)
  ;; Code from org-open-file -> find-file-other-window was used:
  (let ((value (find-file-noselect path nil nil nil))) ; buf name
    (when (listp value)
      (setq value (car value)))
    (with-current-buffer value
      (oai-block-tags--get-replacement-for-org-link (concat "[[" option "]]")))))


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
           ;;        2) in this buf + option
           ;;        3) not in this buf + option

           (if (and option
                    (not (string-empty-p option)))
               (if (oai-block-tags--path-is-current-buffer-p path)
                   ;; case 2) recursion call without path
                   (oai-block-tags--get-replacement-for-org-link (concat "[[" option "]]")) ; recursive call
                 ;; - else case 3) <other-file>
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
              (when (and oai-block-tags--check-double-targets-found
                         (eq found 'dedicated)
                         (not (eq (line-number-at-pos) ln-before))) ; found?
                (let ((ln-found (line-number-at-pos))
                      oai-block-tags-error-on-missing-link)
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
;; (if (not (let ((oai-block-tags-use-simple-directory-content t))
;;            (and
;;             (string-match "oai-block-tags.el" (oai-block-tags--get-replacement-for-org-link "file:./"))
;;             (string-match "oai-block-tags.el" (oai-block-tags--get-replacement-for-org-link "[[./]]"))
;;             (string-match "oai-block-tags.el" (oai-block-tags--get-replacement-for-org-link "[[file:./]]"))
;;             (string-match "oai-block-tags.el" (oai-block-tags--get-replacement-for-org-link "[[file:.]]"))
;;             )))
;;     (error "oai-block-tags-use-simple-directory-content dir ./"))

;; [[file:oai-block-tags.el::`(ref)']]]
;; (oai-block-tags--get-replacement-for-org-link  "[[xx]]")

;;; -=-= help functions: markdown blocks

(defun oai-block-tags--markdown-mark-fenced-code-body (&optional limit-begin limit-end)
  "Mark content inside Markdown fenced code block (```), excluding header/footer.
LIMIT-BEGIN and LIMIT-END restrict the search region around point.
Returns t if was marked, nil otherwise.
Used in `oai-block-tags-mark-md-block-body'."
  ;; fill limit-begin and limit-end - if they was not profiled
  (when (not (and limit-begin limit-end))
    (when-let* ((region (oai-block-tags--get-org-block-region)))
      (setq limit-begin (car region))
      (setq limit-end (cadr region))))

  (when-let* ((r (oai-block-tags--markdown-fenced-code-body-get-range limit-begin limit-end))
              (beg (car r))
              (end (cadr r)))
    (set-mark beg)
    (goto-char end)
    (forward-line -1)
    (end-of-line)
    (activate-mark)
    t))

;; (defun oai-block-mark-src-block-body ()
;;   "Mark Org blocks content around cursor.
;; Excluding header and footer."
;;   (interactive)
;;   (let ((elem (org-element-at-point)))
;;     (goto-char (org-element-property :begin elem))
;;     (forward-line 1)
;;     (set-mark (point))
;;     (let ((case-fold-search t))
;;           (re-search-forward "#\\+end_" nil t))
;;     (beginning-of-line)
;;     ;; (goto-char (org-element-property :end elem))
;;     ;; (forward-line -2)
;;     ;; (end-of-line)
;;     (activate-mark)
;;     t))

;; (defun oai-block-tags--in-markdown-fences-p ()
;;   (let* ((element (oai-block-p))
;;          (limit-begin (org-element-property :contents-begin element))
;;          (limit-end (org-element-property :contents-end element)))
;;     (oai-block-tags--markdown-mark-fenced-code-body-get-range limit-begin limit-end)))

;;; -=-= Replace links in text
;; Supported:
;; - @Backtrace
;; - @/path/file.txt
;; - @./name - file
;; - @name - <<target>> or #+NAME: name - in current file

(defun oai-block-tags--replace-last-regex-smart (string regexp &optional replacement)
  "Replace the last match of REGEXP in STRING with REPLACEMENT.
reserve any extra captured groups.
Check that found regexp not in markdown block.
If REPLACEMENT not provided return found string for regexp or nil if not
found."
  (let ((pos 0)
        (last-pos nil)
        (last-end nil)
        ;; (last-group "")
        )
    (while (and pos
                (string-match regexp string pos)
                (not (oai-block-tags--position-in-markdown-block-str-p string (setq pos (match-beginning 0))))) ; not in markdonw block
      (setq last-pos pos) ; beg
      (setq last-end (match-end 0)) ; end
      (setq pos last-end) ; move forward
      )

    (if replacement
        (if last-pos
            ;; (replace-match replacement 'fixedcase 'literal string)
            ;; (if (eq (aref string (1- last-end)) ?\s) ;; if space after match
                ;; (replace-match replacement 'fixedcase 'literal string 1)
              ;; 1) replace
              (concat (substring string 0 last-pos)
                      replacement
                      ;; last-group
                      (substring string last-end))
          ;; else - return just string
          string)
      ;; else no replacement
      (if last-pos
          (replace-regexp-in-string "^[` ]*" ""
                                     (replace-regexp-in-string "[` ]*\$" ""
                                                               (match-string 0 string))) ;; (substring string last-pos last-end)
      nil))))

(cl-assert
 (string-equal (oai-block-tags--replace-last-regex-smart "asdasd@Backtraceasdasdasd" "\\(@Backtrace\\)" "111")
        "asdasd111asdasdasd"))

(cl-assert
 (string-equal (oai-block-tags--replace-last-regex-smart "asda\n```\nvas@Backtraceasdasd\n```\nasd" "\\(@Backtrace\\)" "111")
               "asda\n```\nvas@Backtraceasdasd\n```\nasd"))

(cl-assert
 (string-equal (oai-block-tags--replace-last-regex-smart "asdasd@Backtraceasdasdasd" "@Backtrace")
        "@Backtrace"))

;; search without replace
(cl-assert (string-equal (oai-block-tags--replace-last-regex-smart
                   "foo `@Backtrace` bar `@Backtrace `@BacktraceX"
                   oai-block-tags--regexes-backtrace)
                  "@Backtrace"))

(cl-assert (string-equal (oai-block-tags--replace-last-regex-smart
                   "foo `@Backtrace` bar `@Backtrace `@Backtrace`X"
                   oai-block-tags--regexes-backtrace)
                  "@Backtrace"))

(cl-assert (string-equal (oai-block-tags--replace-last-regex-smart
            "foo `@Backtrace` bar `@Backtrace `@B`X"
            oai-block-tags--regexes-backtrace)
                  "@B"))

(cl-assert
 (string-equal (oai-block-tags--replace-last-regex-smart
                "foo `@Backtrace` bar `@Backtrace `@Backtrace`X"
                oai-block-tags--regexes-backtrace
                "REPLACED")
               "foo `@Backtrace` bar `@Backtrace REPLACEDX"))
;; with space
(cl-assert
 (equal (oai-block-tags--replace-last-regex-smart
         "foo `@Backtrace` bar `@Backtrace `@BacktraceX"
         oai-block-tags--regexes-backtrace
         "REPLACED")
        "foo `@Backtrace` bar REPLACED`@BacktraceX"))

(cl-assert
 (equal (oai-block-tags--replace-last-regex-smart
         "foo `@Backtrace` bar `@B `@BacktraceX"
         oai-block-tags--regexes-backtrace
         "REPLACED")
        "foo `@Backtrace` bar REPLACED`@BacktraceX"))

(cl-assert
 (equal (oai-block-tags--replace-last-regex-smart
         "foo `@/asd.txt` X"
         oai-block-tags--regexes-path
         "REPLACED")
        "foo REPLACED X"))

(cl-assert
 (equal (oai-block-tags--replace-last-regex-smart "foo `@.` bar " oai-block-tags--regexes-path "REPLACED")
        "foo REPLACED bar "))

(cl-assert
 (string-equal
  (oai-block-tags--replace-last-regex-smart "asd `@/tmp/t.txt` assd" oai-block-tags--regexes-path "path")
  "asd path assd"))
 ;; (oai-block-tags--replace-last-regex-smart "asd `[[/tmp][sd]]` assd" (plist-get oai-block-tags--regexes :path) "path")

(defun oai-block-tags-replace (string)
  "Replace links in STRING with their targets.
Check every type of links if it exist in text, find replacement for the
fist link and replace link substring with
`oai-block-tags--replace-last-regex-smart' once.
Return modified string or the same string."
  (oai--debug "oai-block-tags-replace: \"%s\"" string)
  ;; - "@Backtrace" substring exist - replace the last one only
  ;; *Wrap in markdown*
  (when (string-match oai-block-tags--regexes-backtrace string)
    (if-let* ((bt (oai-block-tags--get-backtrace-buffer-string)) ; *Backtrace* buffer exist
              (bt (oai-block-tags--take-n-lines bt oai-block-tags--backtrace-max-lines))
              (bt (concat "\n" (plist-get oai-block-tags--markdown-prefixes :backtrace) "\n"
                          bt
                          oai-block-tags--markdown-postfix)) ; prepare string
              (new-string (oai-block-tags--replace-last-regex-smart string oai-block-tags--regexes-backtrace bt))) ; insert backtrace
        (setq string new-string)))

  ;; - Path @/path/file.txt - replace the last one only
  ;; *Wrap in markdown*
  (when (string-match oai-block-tags--regexes-path string)
    (if-let* ((path-string (oai-block-tags--replace-last-regex-smart string oai-block-tags--regexes-path)) ; find the last
              ;; remove first @ character from link
              (path-string (if (> (length path-string) 0)
                               (substring path-string 1)
                             ""))
              (replacement (concat "\n" (oai-block-tags--compose-block-for-path-full path-string) "\n"))
              (new-string (oai-block-tags--replace-last-regex-smart string
                                                                    oai-block-tags--regexes-path
                                                                    replacement)))
        (setq string new-string)))

  ;; - Org links [[link]]
  ;; We search  for link regex,  when found we check  if there
  ;; are double of  found substring after founded  one, if one
  ;; more exist we skip the first  one that found. if no other
  ;; exist we replace it.
  ;; *Dont Wrap in markdown*
  (when (string-match oai-block--org-link-any-re string) ; exist in text?
    (if-let* ((link (oai-block-tags--replace-last-regex-smart string oai-block--org-link-any-re)) ; find the last

              (replacement (concat "\n" (oai-block-tags--get-replacement-for-org-link link) "\n" )) ; add empty line after it.
              (new-string (oai-block-tags--replace-last-regex-smart string
                                                                    oai-block--org-link-any-re
                                                                    replacement)))
        (setq string new-string)))
  ;; old
  ;; (when (not (string-match oai-block--org-link-any-re string)) ; exist in text?
  ;;   (let ((new-string string) ; ai block content
  ;;         (path-string (oai-block-tags--replace-last-regex-smart string oai-block--org-link-any-re))
  ;;         (replaced nil)
  ;;         (pos-end 0)
  ;;         (pos-beg 0)
  ;;         (link)
  ;;         (replacement))
  ;;     (print (list "oai-block-tags-replace 0" path-string))
  ;;     ;; 1) find link
  ;;     (while (or (= pos-end 0) ; skip first
  ;;                (string-match oai-block--org-link-any-re new-string pos-end)) ; loop over links in text
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


  ;; (if-let* ((path-string (oai-block-tags--replace-last-regex-smart string oai-block--org-link-any-re)))
  ;;     (progn
  ;;       (print (list "link" path-string))
  ;;       string))
  ;; - default
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

;; - just output test, too hard to compare with something.
(let* ((temp-dir (make-temp-file "my-tmp-dir-" t))     ;; Create temp directory
       (file1 (expand-file-name "file1.txt" temp-dir)) ;; Known file name
       (file2 (expand-file-name "file2.el" temp-dir))
       (file3 (expand-file-name "file3.py" temp-dir)))
      (with-temp-file file1
        (insert "Contents for file1"))
      (with-temp-file file2
        (insert "(defun aa() )"))
      (with-temp-file file3
        (insert "import os"))
      ;; (oai-block-tags-replace (format "ssvv `@%s` bbb" file1)))
      ;; (string-join (string-split (oai-block-tags-replace (format "ssvv `@%s` bbb" file1)) "\n" ) "\\n"))
      (if (not (string-equal "ssvv \nHere file1.txt:\n```text\nContents for file1\n```\n bbb"
                    (oai-block-tags-replace (format "ssvv `@%s` bbb" file1))))
          (error "error in loading of oai-block-tags.el 1"))
      ;; ;; (print (oai-block-tags-replace (format "ssvv `@%s` bbb" file2))))
      ;; (string-join (string-split (oai-block-tags-replace (format "ssvv `@%s` bbb" file2)) "\n" ) "\\n"))
      ;; (oai-block-tags-replace (format "ssvv `@%s` bbb" file2)))
      (if (not (string-equal "ssvv \nHere file2.el:\n```elisp\n(defun aa() )\n```\n bbb"
                             (oai-block-tags-replace (format "ssvv `@%s` bbb" file2))))
          (error "error in loading of oai-block-tags.el 2"))
      ;; (string-join (string-split (oai-block-tags-replace (format "ssvv [[%s]] bbb" file3)) "\n" ) "\\n"))
      ;; (oai-block-tags-replace (format "ssvv [[%s]] bbb" file3)))
;; "ssvv \\nssssss\\nHere file3.py:\\n```python\\nimport os\\n```\\n\\n bbb"
;;                           "ssvv \n\nHere file3.py:\\n```python\\nimport os\\n```\\n\\n bbb"
      (if (not (string-equal "ssvv \nHere file3.py:\n```python\nimport os\n```\n bbb"
                             (oai-block-tags-replace (format "ssvv [[%s]] bbb" file3))))
          (error "error in loading of oai-block-tags.el 3")))


;;; -=-= Fontify @Backtrace & @path & [[links]]

(defun oai-block-tags--font-lock-fontify-links (limit)
  "Fontify Org links in #+begin_ai ... #+end_ai blocks, up to LIMIT.
This is special fontify function, that return t when match found.
1) search for ai block begin and then end, 2) call fontify on range that
goto to the begining firstly function `org-activate-links' used to
highlight any link.
TODO: maybe we should use something like
`oai-block-tags--position-in-markdown-block-str-p'"
  (if oai-block-fontify-markdown
      (let ((case-fold-search t)
            (ret))
        (while (and (re-search-forward oai-block--ai-block-begin-re limit t)
                    (< (point) limit))
          (let ((beg (match-end 0)))
            (when (re-search-forward oai-block--ai-block-end-re nil t)
              (let ((end (match-beginning 0)))
                (save-match-data
                  ;; fontify Org links [[..]]
                  ;; (message beg)
                  ;; - [[link][]]
                  (progn
                    (goto-char beg)
                    (while (re-search-forward oai-block--org-link-any-re end t)
                      (goto-char (match-beginning 0))
                      (setq ret (org-activate-links end))))
                  ;; - @Backtrace
                  (progn
                    (goto-char beg)
                    (while (re-search-forward oai-block-tags--regexes-backtrace limit t)
                      (add-face-text-property (match-beginning 0) (match-end 0) 'org-link)
                      (setq ret t)))
                  ;; - @/tmp/
                  (progn
                    (goto-char beg)
                    (while (re-search-forward oai-block-tags--regexes-path limit t)
                      (add-face-text-property (match-beginning 0) (match-end 0) 'org-link)
                      (setq ret t)))
                  ;; fontify markdown sub-blocks
                  ;; (oai-block--fontify-markdown-subblocks beg end)
                  )))))
        ;; required by font lock mode:
        (goto-char limit)
        ret)))

;;; -=-= key to select block "C-c h" (similar to "M-h")

(defun oai-block-tags-mark-md-block-body ()
  "Mark content of Markdown code block, or fallback to `org-mark-element'.
Mark or select block content around cursor.
`oai-block-tags--get-org-block-region'  do  same thing,  but  we
make this function to no relay on oai-block."
  (interactive)
  (or (when-let* ((region (oai-block-tags--get-org-block-region))
                  (beg (car region))
                  (end (cadr region)))
        ;; if pointer in Mardown block, we mark Markdown block only
        (or (oai-block-tags--markdown-mark-fenced-code-body beg end)
            ;; else - no markdown - mark ai block only
            (progn
              (set-mark beg)
              (goto-char end)
              (activate-mark)))
        t)
      (org-mark-element)))

(provide 'oai-block-tags)
;;; oai-block-tags.el ends here
