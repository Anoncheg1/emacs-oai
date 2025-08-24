;;; oai-block-tags.el --- Logging for oai in separate buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 github.com/Anoncheg1

;; This file is NOT part of GNU Emacs.

;; oai-block-tags.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; oai-block-tags.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with oai.el.
;; If not, see <https://www.gnu.org/licenses/>.

;;; Code
;;; -=-= variables
(require 'org)

(defvar oai-block-tags--regexes '(
                               ;; :backtrace "@Backtrace`?\\([^a-zA-Z]\\|$\\)"
                               :backtrace "\\(`?@Backtrace`?\\)\\([^a-zA-Z]\\|$\\)"
                               :path "@\\(\\.\\.?/\\|\\.\\.?\\\\\\|\\.\\.?\\|/\\|\\\\\\|[A-Za-z]:\\\\\\)[a-zA-Z0-9_./\\\\-]*"
                                          ))

(defvar oai-block-tags--markdown-prefixes '(:backtrace "```elisp-backtrace\n"
                                         :path-directory "```ls-output\n"
                                         :path-file  "```\n"))

(defvar oai-block-tags--backtrace-max-lines 12)

(cl-assert
 (let ((regex (plist-get oai-block-tags--regexes :path)))
  (equal (mapcar (lambda (s)
                   (when (string-match regex s)
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
         '("@/file-s_s" "@/file.t_xt" "@./file.txt" "@/some/path/file.txt" "@C:\\some\\file.txt" "@L:\\folder\\file.txt" "@\\network\\share" "@.\\windowsfile" "@/file/" "@/file.txt/" "@./file.txt/" "@/some/path/file.txt/" "@C:\\some\\file.txt\\" "@L:\\folder\\file.txt\\" "@\\network\\share\\" "@.\\windowsfile\\" nil nil nil nil nil "@../right" "@../right/" "@.." "@." "@/"))))

;;; -=-= Backtrace

(defun oai-block-tags--take-n-lines (string n)
  "Return a string with the first N lines from STRING.
If N exceeds the number of lines, return all lines. If N <= 0, return an empty string."
  (let* ((lines (split-string string "\n"))
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
  "Return the contents of the *Backtrace* buffer as a string, or nil if it does not exist."
  (let ((buf (get-buffer "*Backtrace*")))
    (when buf
      (with-current-buffer buf
        (string-trim (substring-no-properties (buffer-string)))))))

;;; -=-= Links: Files & Directories

(defun oai-block-tags--get-directory-content (path-string)
  (with-current-buffer (dired-noselect path-string)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun oai-block-tags--read-file-to-string-safe (path-string &optional coding)
  (if (and (file-exists-p path-string)
           (file-regular-p path-string)
           (file-readable-p path-string))
      (condition-case err
          (with-temp-buffer
            (when coding
              (set-buffer-file-coding-system coding))
            (insert-file-contents path-string)
            (buffer-string))
        (error (message "Error reading file %s: %s" path-string err)
               nil))
    ;; else
    (error "File does not exist or not readable: %s" path-string)
    nil))

(defun oai-block-tags--compose-block-for-path (path-string content)
  "Return file/directory content in mardown block without last ```."
  (concat
   "\nHere " (file-name-nondirectory (directory-file-name path-string)) ":\n"
   ;; prefix
   (if (file-directory-p path-string)
       (plist-get oai-block-tags--markdown-prefixes :path-directory)
     ;; else
     (plist-get oai-block-tags--markdown-prefixes :path-file))
   content))

;;; -=-= Replace

(defun oai-block-tags--replace-last-regex-smart (string regexp &optional replacement)
  "Replace the last match of REGEXP in STRING with REPLACEMENT,
preserving any extra captured groups.
If REPLACEMENT not provided return found string for regexp."
  (let ((pos 0)
        (last-pos nil)
        (last-end nil)
        (last-group ""))
    (while (and pos
                 (string-match regexp string pos))
      (setq pos (match-beginning 0))
      (setq last-pos pos)
      ;; (print (list "aa" (match-string 0 string) (match-string 1 string)))
      (setq last-end (match-end 0))
      (setq last-group (match-string 2 string))
      (setq pos (1+ pos)))
    (if last-pos
        (if replacement
            (concat (substring string 0 last-pos)
                    replacement
                    last-group
                    (substring string last-end))
          ;; else
          (substring string last-pos last-end)) ; what was found
      string)))

(cl-assert
 (equal (oai-block-tags--replace-last-regex-smart "asdasd@Backtraceasdasdasd" "@Backtrace" "111")
        "asdasd111asdasdasd"))

(cl-assert
 (equal (oai-block-tags--replace-last-regex-smart "asdasd@Backtraceasdasdasd" "@Backtrace")
        "@Backtrace"))


(cl-assert
 (equal (let ((regex (plist-get oai-block-tags--regexes :backtrace)))
          (oai-block-tags--replace-last-regex-smart
           "foo `@Backtrace` bar `@Backtrace `@Backtrace`X"
           regex
           "REPLACED"))
        "foo `@Backtrace` bar `@Backtrace `REPLACED`X"))
(cl-assert
 (equal (let ((regex (plist-get oai-block-tags--regexes :backtrace)))
          (oai-block-tags--replace-last-regex-smart
           "foo `@Backtrace` bar `@Backtrace `@BacktraceX"
           regex
           "REPLACED"))
        "foo `@Backtrace` bar `REPLACED `@BacktraceX"))

(cl-assert
 (equal (let ((regex (plist-get oai-block-tags--regexes :backtrace)))
          (oai-block-tags--replace-last-regex-smart "foo `@.` bar " (plist-get oai-block-tags--regexes :path) "REPLACED"))
        "foo `REPLACED` bar "))


(defun oai-block-tags-replace (string)
  "Replace links in STRING with their targets.
And return modified string or the same string.
TODO: fix when @Backtrace or @path surounded by (`) character."
  (let ((backtrace-re (plist-get oai-block-tags--regexes :backtrace))
        (path-re (plist-get oai-block-tags--regexes :path))
        (markdown-postfix "\n```\n"))
    ;;return
    (cond ((string-match backtrace-re string) ; "@Backtrace" substring exist
           (if-let* ((bt (oai-block-tags--get-backtrace-buffer-string)) ; *Backtrace* buffer exist
                     (bt (oai-block-tags--take-n-lines bt oai-block-tags--backtrace-max-lines))
                     (bt (concat "\n" (plist-get oai-block-tags--markdown-prefixes :backtrace)
                                 bt
                                 markdown-postfix)) ; prepare string
                     (new-string (oai-block-tags--replace-last-regex-smart string backtrace-re bt))) ; insert backtrace
               new-string
             ;; else
             string
             ;; (if (and (equal (length string) (length new-string))
             ;;          (string-equal string new-string))
             ;;     (error "@Backtrace not found")
             ;;   ;; else
             ;;   new-string)
             ))
        ((string-match path-re string)
         (if-let* ((path-string (oai-block-tags--replace-last-regex-smart string path-re))
                   (content (if (file-directory-p path-string)
                                (oai-block-tags--get-directory-content path-string)
                              ;; else
                              (oai-block-tags--read-file-to-string-safe
                               ;; remove first @ character from link
                               (if (> (length path-string) 0)
                                   (substring path-string 1)
                                 ""))))
                   (content (oai-block-tags--compose-block-for-path path-string content))

                   (new-string (oai-block-tags--replace-last-regex-smart string path-re
                                                         (concat content markdown-postfix))))
               new-string
             ;; else
             string))
        (t string))))

;;; -=-= Fontify Backtrace & links

(defun oai-block-tags--font-lock-fontify-links (limit)
  "Fontify Org links in #+begin_ai ... #+end_ai blocks, up to LIMIT.
This is special fontify function, that return t when match found.
1) search for ai block begin and then end, 2) call fontify on range that goto to the begining firstly
`org-activate-links'."
  (if oai-block-fontify-markdown
      (let ((case-fold-search t)
            (ret))
        (while (and (re-search-forward "^#\\+begin_ai[^\n]*\n" limit t)
                    (< (point) limit))
          (let ((beg (match-end 0)))
            (when (re-search-forward "^#\\+end_ai.*$" nil t)
              (let ((end (match-beginning 0)))
                (save-match-data
                  ;; fontify Org links [[..]]
                  ;; (message beg)
                  (progn
                    (goto-char beg)
                    (while (re-search-forward org-link-any-re end t)
                      (goto-char (match-beginning 0))
                      (setq ret (org-activate-links end))
                      ))
                  (progn
                    (goto-char beg)
                    (while (re-search-forward (plist-get oai-block-tags--regexes :backtrace) limit t)
                      (add-face-text-property (match-beginning 0) (match-end 0) 'org-link)
                      (setq ret t)))
                  ;; fontify markdown sub-blocks
                  ;; (oai-block--fontify-markdown-subblocks beg end)
                  )
                ))))
        ;; required by font lock mode:
        (goto-char limit)
        ret)))


(provide 'oai-block-tags)
;;; oai-block-tags.el ends here
