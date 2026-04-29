;;; oai-tests-block-tags.el ---  -*- lexical-binding: t -*-
;; Copyright (c) 2025 github.com/Anoncheg1,codeberg.org/Anoncheg
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Author: <github.com/Anoncheg1,codeberg.org/Anoncheg>

;; (eval-buffer)
;; (ert t)
;; emacs -Q --batch -l ert.el -l oai-debug.el -l ../emacs-org-links/org-links.el -l oai-block-tags.el -l oai-tests-block-tags.el -f ert-run-tests-batch-and-exit
;;
;;; License

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

;; -=-= includes

(require 'oai-block-tags)
;; (print (list "vvvvvvvvvvvvvvvvvvvvvv1" (bound-and-true-p debug)))
(require 'ert)
(defvar ert-enabled nil)
; org-links - is optional dependency

;; (eval-buffer) or (load-file "path/to/async-tests.el")
;; Running Tests: Load the test file and run:
;; (eval-buffer)
;; (ert t)
;; to execute all tests. Individual tests can be run with (ert 'test-name).
;;; Code:
;; (setopt oai-debug-buffer "*debug-oai*")

;; -=-= help functions
(defun oai-tests-block-tags-insert-block (&optional name not-clear)
  (unless not-clear
    ;; (mark-whole-buffer) ; output Mark set
    ;; (call-interactively #'kill-region)
    (kill-region (point-min) (point-max))
    )
  (when name
    (insert "#+name: " name "\n"))
  (insert "#+begin_ai\n")
  (let ((p1 (point)))
    (insert "\n#+end_ai")
    (goto-char p1)))

;; -=-= Tests --------------------------------------------------------
(ert-deftest oai-tests-block-tags--read-file-to-string-safe--read-ok ()
  "Should read a regular readable file and return its contents."
  (let ((tmpfile (make-temp-file "oai-test")))
    (unwind-protect
        (progn
          (write-region "Hello, test!" nil tmpfile)
          (should (equal (org-file-contents tmpfile)
                         "Hello, test!")))
      (delete-file tmpfile))))

(ert-deftest oai-tests-block-tags--read-file-to-string-safe--file-missing ()
  "Should signal user-error if the file does not exist."
  (should-error (org-file-contents "/no/such/file")
                :type 'user-error))

;; (ert-deftest oai-tests-block-tags--read-file-to-string-safe--nonregular ()
;;   "Should signal user-error if path is not a regular file."
;;   (let ((tmpdir (make-temp-file "oai-test-dir" t)))
;;     (unwind-protect
;;         ;; (org-file-contents tmpdir)))
;;         (should-error (org-file-contents tmpdir)
;;                       :type 'user-error)
;;       (delete-directory tmpdir))))

(ert-deftest oai-tests-block-tags--read-file-to-string-safe--unreadable ()
  "Should signal user-error if the file is not readable."
  (let ((tmpfile (make-temp-file "oai-test-unreadable")))
    (unwind-protect
        (progn
          (write-region "not readable" nil tmpfile)
          (set-file-modes tmpfile 0)
          (should-error (org-file-contents tmpfile)
                        :type 'user-error))
      ;; Restore permissions so we can delete it
      (set-file-modes tmpfile #o600)
      (delete-file tmpfile))))

(ert-deftest oai-tests-block-tags--read-file-to-string-safe--with-coding ()
  "Should honor the coding argument; reading ASCII content as UTF-8 should work."
  (let ((tmpfile (make-temp-file "oai-test-coding")))
    (unwind-protect
        (progn
          (write-region "abc" nil tmpfile)
          (should (equal
                   (org-file-contents tmpfile 'utf-8)
                   "abc")))
      (delete-file tmpfile))))

;; -=-= Test: oai-block-tags--regexes-path
;; (setq oai-block-tags--regexes-path "@\\(\\.\\.?/\\|\\.\\.?\\\\\\|\\.\\.?\\|/\\|\\\\\\|[A-Za-z]:\\\\\\|~[a-zA-Z0-9_.-]*/*\\)[a-zA-Z0-9_./\\\\-]*")
(defun any (l) (seq-some #'identity l))

(ert-deftest oai-tests-block-tags--regexes-path ()
  (should
   (equal (mapcar (lambda (s)
                    (when (string-match oai-block-tags--regexes-path s)
                      (substring s (match-beginning 0) (match-end 0))))
                  '(
                    "@/file-s_s"
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
                    "@../right"
                    "@../right/"

                    "@/"
                    "@.."
                    "@."
                    ;; new
                    "@~/a"
                    "@~/"
                    ))
          '("@/file-s_s" "@/file.t_xt" "@./file.txt"
            "@/some/path/file.txt" "@C:\\some\\file.txt" "@L:\\folder\\file.txt"
            "@\\network\\share" "@.\\windowsfile" "@/file/" "@/file.txt/" "@./file.txt/"
            "@/some/path/file.txt/" "@C:\\some\\file.txt\\" "@L:\\folder\\file.txt\\"
            "@\\network\\share\\" "@.\\windowsfile\\" "@../right" "@../right/"
            "@/" "@.." "@." "@~/a" "@~/")
          ))

  (should (not (any (mapcar (lambda (s)
                              (when (string-match oai-block-tags--regexes-path s)
                                (substring s (match-beginning 0) (match-end 0))))
                            '(
                              "@Backtrace"
                              "@not/a/path"
                              "@Backtrace"
                              "@not/a/path"
                              "@not/a/path/"))))))
;; -=-= Test: oai-block-tags--get-replacement-for-org-link - dir
(ert-deftest oai-tests-block-tags--get-replacement-for-org-link-dir ()
  ""
  (let ((oai-block-tags-use-simple-directory-content-flag t)
                res)
    (setq res (oai-block-tags--get-replacement-for-org-link "file:./"))
    (setq res (string-match "Here . directory contents" res))
    (should (eq 1 res))
    (setq res (oai-block-tags--get-replacement-for-org-link "[[./]]"))
    (setq res (string-match "Here . directory contents" res))
    (should (eq 1 res))
    (setq res (oai-block-tags--get-replacement-for-org-link "[[file:./]]"))
    (setq res (string-match "Here . directory contents" res))
    (should (eq 1 res))
    (setq res (oai-block-tags--get-replacement-for-org-link "[[file:.]]"))
    (setq res (string-match "Here . directory contents" res))
    (should (eq 1 res))))

(when (require 'org-links nil 'noerror)
  (ert-deftest oai-tests-block-tags--get-replacement-for-org-link-header1 ()
    ""
    (let ((res))
      (setq res (with-temp-buffer
                  (setq-local  buffer-file-name "/mock/org.org")
                  (org-mode)
                  (insert "* n [2026-04-14 Tue]\n\nas\n\n* vv")
                  (set-buffer-modified-p nil)
                  (with-temp-buffer
                    (org-mode)
                    (oai-block-tags--get-replacement-for-org-link
                     "[[file:/mock/org.org::1::*n \\[2026-04-14 Tue\\]][n [2026-04-14 Tue]​]]")
                    )))
      (should (string-equal res "
```text
# n [2026-04-14 Tue]
as


```
"))))

  (ert-deftest oai-tests-block-tags--get-replacement-for-org-link-header2 ()

    (let ((res))
      (setq res (with-temp-buffer
                  (setq-local  buffer-file-name "/mock/org.org")
                  (org-mode)
                  (insert "* n [2026-04-14 Tue]\n\nas\n\n* vv")
                  (set-buffer-modified-p nil)
                    (oai-block-tags--get-replacement-for-org-link
                     "[[file:/mock/org.org::1::*n \\[2026-04-14 Tue\\]][n [2026-04-14 Tue]​]]")
                    ))
      (should (string-equal res "
```text
# n [2026-04-14 Tue]
as


```
")))
    ))

;; -=-= Test: oai-block-tags-replace
(ert-deftest oai-tests-block-tags--replace-org-links-norm-header ()
  (let ((kill-buffer-query-functions)
        res1 res2
        target)
    (with-temp-buffer
      (org-mode)
      (setq-local buffer-file-name "/mock/org.org")
      (insert "* headline\nasdas\n** sub-headline\n asd")
      (setq res1 (oai-block-tags-replace  "11[[file:/mock/org.org::* headline]]4444"))
      (setq target
            "11
```text
# headline
asdas

## sub-headline
 asd
```

4444")
      (should (string-equal res1 target))
      (setq res2 (oai-block-tags-replace  "11[[* headline]]4444"))
      (should (string-equal res2 target))
      (set-buffer-modified-p nil))))


(when (require 'org-links nil 'noerror)
  (ert-deftest oai-tests-block-tags--replace-org-links-nn-header ()
    (let ((kill-buffer-query-functions)
          ;; (org-link-file-path-type 'absolute)
          ;; (org-link-search-must-match-exact-headline nil)
          target
          res1 res2 res3
          org-execute-file-search-functions
          )
      (with-temp-buffer
        (org-mode)
        (add-hook 'org-execute-file-search-functions (intern "org-links-additional-formats"))

        (setq buffer-file-name "/mock/org.org")
        (insert "* headline\nasdas\n** sub-headline\n asd")
          (setq target "11
```text
# headline
asdas

## sub-headline
 asd
```

4444")
          (setq res1 (oai-block-tags-replace "11[[file:/mock/org.org::1::* headline]]4444"))
          (should (string-equal target res1))

          (setq res2  (oai-block-tags-replace  "11[[1::* headline]]4444"))
          (should (string-equal target res2))
          ;; - check for two same links
          (setq target "11
```text
# headline
asdas

## sub-headline
 asd
```

4444
```text
# headline
asdas

## sub-headline
 asd
```

5555")
          (setq res3  (oai-block-tags-replace  "11[[1::* headline]]4444[[1::* headline]]5555"))
          (should (string-equal target res3))
          )
        ;; (advice-remove 'org-open-file (intern "org-links-org-open-file-advice"))

        ;; (insert "[[file:/mock/org.org::1::* headline]]")

        (set-buffer-modified-p nil)))

  (ert-deftest oai-tests-block-tags--replace-org-links-num-num ()
    (let ((kill-buffer-query-functions)
          org-execute-file-search-functions
          res1
          target)
      (with-temp-buffer
        (org-mode)
        (add-hook 'org-execute-file-search-functions (intern "org-links-additional-formats"))
        (setq buffer-file-name "/mock/org.org")
        (insert "* headline\nasdas\n** sub-headline\n asd")

        (setq target "11
```org
* headline
asdas
```
4444")
        (setq res1 (oai-block-tags-replace  "11[[file:/mock/org.org::1-2::* headline]]4444"))
        (should (string-equal res1
                              target))
        ;; (advice-remove 'org-open-file (intern "org-links-org-open-file-advice"))
        (set-buffer-modified-p nil)))))

;; (ert-deftest oai-block-tags--replace-org-links-num-num ()
;;   (let ((kill-buffer-query-functions))
;;     (with-temp-buffer
;;       (org-mode)
;;       (setq buffer-file-name "/mock/org.org")
;;       (insert "* headline\nasdas\n** sub-headline\n asd")
;;       (let (target)
;;         (setq target "11
;; ```auto
;; * headline
;; asdas
;; ```
;; 4444")

;;       (should (string-equal (oai-block-tags-replace  "11[[file:/mock/org.org::1-2::* headline]]4444")
;;                             target))
;;       )
;;         (set-buffer-modified-p nil))))





;; -=-= Test: get-replacement-for-org-file-link-in-other-file
(when (require 'org-links nil 'noerror)
  (ert-deftest oai-tests-block-tags--get-replacement-for-org-file-link-in-other-file ()
    (let ((kill-buffer-query-functions)
          target
          res1 res2
          org-execute-file-search-functions)
      (with-temp-buffer
        (org-mode)
        (add-hook 'org-execute-file-search-functions (intern "org-links-additional-formats"))
        (insert "* headline\nasdas\n** sub-headline\n asd\nss2")
        (setq buffer-file-name "/mock/org.org")
        (read-only-mode)
        (set-buffer-modified-p nil)
    ;; (print (buffer-substring-no-properties (point-min) (point-max)))))
        (setq res1 (oai-block-tags--get-replacement-for-org-file-link-in-other-file
                      "/mock/org.org" "2-3"))

        (setq target
              "\n```org\nasdas\n** sub-headline\n```")
        (should (string-equal target res1))
        (setq target
              "
```text
## sub-headline
 asd
ss2
```
")
        ;; (print (buffer-substring-no-properties (point-min) (point-max)))))
        (setq res2 (oai-block-tags--get-replacement-for-org-file-link-in-other-file
                      "/mock/org.org" "*sub-headline"))
        (should (string-equal target res2))

        ;; (advice-remove 'org-open-file (intern "org-links-org-open-file-advice"))

        ))))

;; -=-= Test: oai-block-tags--take-n-lines
(ert-deftest oai-tests-block-tags--take-n-lines ()
  (should (string-equal (oai-block-tags--take-n-lines "a\nb\nc\nd" 2) "a\nb"))
  (should (string-equal (oai-block-tags--take-n-lines "a\nb\nc\nd" 4) "a\nb\nc\nd"))
  (should (string-equal (oai-block-tags--take-n-lines "a\nb\nc" 10) "a\nb\nc"))
  (should (string-equal (oai-block-tags--take-n-lines "a\nb\nc" 0) ""))
  (should (string-equal (oai-block-tags--take-n-lines "a\nb\nc" -3) ""))
  (should (string-equal (oai-block-tags--take-n-lines "" 4)  ""))
  (should (string-equal (oai-block-tags--take-n-lines "x\ny\nz\n" 2) "x\ny"))
  (should-error (oai-block-tags--take-n-lines nil 2) :type 'error))
;; -=-= tags tests
(ert-deftest oai-tests-block-tags--replace-test ()
    (let* ((temp-file (make-temp-file "mytest"))
       (res (progn
              (with-temp-file temp-file
                (insert "Hello, world test!"))
              (prog1
                  (oai-block-tags-replace (format "aas @%s bb." temp-file))
                (delete-file temp-file))))
       (res (string-split res "\n")))
      ;; (pp res))
      (should (string-equal "aas " (nth 0 res)))
      (should (string-equal "```auto" (nth 2 res)))
      (should (string-equal "Hello, world test!" (nth 3 res)))
      (should (string-equal "```" (nth 4 res)))
      (should (string-equal " bb." (nth 5 res)))))


;; -=-= Test: oai-block-tags-replace - for directory
(defmacro with-temp-files (filenames &rest body)
  "Create a temporary directory, populate it with FILENAMES (as empty files),
run BODY with access to TEMP-DIR and TEMP-FILES, then clean up."
  (declare (indent 1))
  `(let* ((temp-dir (file-name-concat (temporary-file-directory)
                                      (make-temp-name "test1")))
          (temp-files (mapcar (lambda (name) (expand-file-name name temp-dir)) ,filenames)))
     (make-directory temp-dir)
     (dolist (f temp-files)
       (write-region "" nil f nil 'quiet))
     (unwind-protect
         (progn
           ;; Provide temp-dir and temp-files inside BODY
           ,@body)
       ;; Cleanup
       (delete-directory temp-dir t nil))))

;; (let ((string "ssvv @/asd/asvv.txt bbb"))
;;   (string-match oai-block-tags--regexes-path string 0)
;;     (cons (match-string 1 string) (match-beginning 0)))

(ert-deftest oai-tests-block-tags--oai-block-tags-replace ()
  (with-temp-files '("file1.txt" "file2.txt")
    (let ((res (string-split (oai-block-tags-replace (format "ssvv @%s bbb" temp-dir)) "\n"))
          (regex-pattern "ssvv \nHere test[^ ]+ folder:\n```ls-output\n  /tmp/test[^ ]+:\n  -rw-rw-r-- 1 [^ ]+ 0 [A-Za-z]+ [0-9]+ [0-9:]+ file1.txt\n  -rw-rw-r-- 1 [^ ]+ 0 [A-Za-z]+ [0-9]+ [0-9:]+ file2.txt\n\n```\n bbb")
          ;; (dired-listing-switches "-AlthG")
          )
      ;; (pp res)))
      ;; LINES of regex-pattern:
      (should (string-match-p "^ssvv" (nth 0 res)))
      (should (string-match-p "^Here test[^ ]+ directory contents:" (nth 1 res)))
      (should (string-match-p "^```shell" (nth 2 res)))
      (should (string-match-p "^  /\\w*/\\w*[^ ]+:" (nth 3 res)))
      ;; "  -rw-rw-r-- 1 g 0 Nov  5 21:13 file1.txt"
      (should (string-match-p "file[12].txt" (nth 4 res)))
      (should (string-match-p "file[12].txt" (nth 5 res)))
      (should (string-match-p "^```$" (nth 7 res)))
      (should (string-match-p "^ bbb$" (nth 8 res))))))


;; -=-= Test: oai-block-tags--contents-area
(ert-deftest oai-tests-block-tags--get-org-block-region ()
  (let (kill-buffer-query-functions
        org-execute-file-search-functions
        res)
    (with-temp-buffer
      (org-mode)
      (add-hook 'org-execute-file-search-functions (intern "org-links-additional-formats"))
      (insert "#+begin_ai :max-tokens 100 :stream nil :sys \"Be helpful\"  :service github :model \"openai\"\n#+end_ai")
      (goto-char (point-min))
      ;; (print (oai-block-tags--contents-area))
      (setq res (oai-block-tags--contents-area))
      (should (= (car res) 91 ))
      (should (= (cdr res) 91 ))

      (goto-char (point-max))
      (insert "\n")
      ;; (let ((res (oai-block-tags--contents-area)))
      ;;   (should (= (car res) 91 ))
      ;;   (should (= (cadr res) 91 )))
      (insert "#+begin_ai :max-tokens 100 :stream nil :sys \"Be helpful\"  :service github :model \"openai\"\n\n\n#+end_ai")
      ;; (goto-char (point-min))
      ;; (print (oai-block-tags--contents-area))
      (setq res (oai-block-tags--contents-area))
      (should (= (car res) 190 ))
      (should (= (cdr res) 190 ))
      (insert "\n")
      (insert "#+begin_ai :max-tokens 100 :stream nil :sys \"Be helpful\"  :service github :model \"openai\"\nasda\nasd\n#+end_ai")
      (setq res (oai-block-tags--contents-area))
      (should (= (car res) 291 ))
      (should (= (cdr res) 300 )))))

;; -=-= Test: oai-block-tags--filepath-to-language
(ert-deftest oai-tests-block-tags--filepath-to-language ()
  (should
   (string-equal (oai-block-tags--filepath-to-language 'emacs-lisp-mode) "elisp"))
  (should
   (string-equal (oai-block-tags--filepath-to-language "emacs-lisp-mode") "elisp"))
  (should
   (string-equal (oai-block-tags--filepath-to-language "/tmp/a.el") "elisp"))
  (should
   (string-equal (oai-block-tags--filepath-to-language "/tmp/a.py") "python"))
  (should
   (string-equal (oai-block-tags--filepath-to-language "asaas") "auto")) ;unknwon
  (should
   (string-equal (oai-block-tags--filepath-to-language "/tmp/a.elfff") "auto")) ;unknwon
  (should
   (string-equal (oai-block-tags--filepath-to-language "/tmp/txt") "auto")) ;unknwon
  (should
   (string-equal (oai-block-tags--filepath-to-language "/tmp/a.org") "org"))
  (should
   (string-equal (oai-block-tags--filepath-to-language "/") "shell")) ; directory
  (should
   (string-equal (oai-block-tags--filepath-to-language "a.txt") "text"))
  (should
   (string-equal (oai-block-tags--filepath-to-language "as/a.ai") "ai"))
  (should
   (string-equal (oai-block-tags--filepath-to-language "/tmp/emacs-file2026-04-22.ai") "ai"))
  (should
   (string-equal (oai-block-tags--filepath-to-language "as/a.aisds") "auto"))
  (should
   (string-equal (oai-block-tags--filepath-to-language "aisds") "auto"))
  (should
   (string-equal (oai-block-tags--filepath-to-language nil) "auto")))

;; -=-= Test: oai-block-tags--replace-first-match
(ert-deftest oai-tests-block-tags--replace-first-match ()
  (should (string-equal
           (oai-block-tags--replace-first-match oai-block--chat-prefixes-re "bar1 " "[ai:] foo baz foo")
           "bar1 foo baz foo"))
  (should (string-equal
           (oai-block-tags--replace-first-match oai-block--chat-prefixes-re "bar1 " "[ai:] foo baz foo" t)
           "[ai:] bar1 foo baz foo"))
  (should (string-equal
           (oai-block-tags--replace-first-match oai-block--chat-prefixes-re "wtf " " foo\n[ME]: foo" t)
          " foo\n[ME]: wtf foo"))
  (should (string-equal
           (oai-block-tags--replace-first-match "f[^ ]* " "bar " "foo foo baz foo")
           "bar foo baz foo")))

;; -=-= Test: oai-block-tags-replace
(ert-deftest oai-tests-block-tags--replace ()
  (let* ((temp-dir (make-temp-file "my-tmp-dir-" t))     ;; Create temp directory
         (file1 (expand-file-name "file1.txt" temp-dir)) ;; Known file name
         (file2 (expand-file-name "file2.el" temp-dir))
         (file3 (expand-file-name "file3.py" temp-dir))
         res)
    (with-temp-file file1
      (insert "Contents for file1"))
    (with-temp-file file2
      (insert "(defun aa() )"))
    (with-temp-file file3
      (insert "import os"))
    ;; (oai-block-tags-replace (format "ssvv `@%s` bbb" file1)))
    ;; (string-join (string-split (oai-block-tags-replace (format "ssvv `@%s` bbb" file1)) "\n" ) "\\n"))
    ;; (oai-block-tags-replace (format "ssvv `@%s` bbb" file1)))
    (setq res (oai-block-tags-replace (format "ssvv @%s bbb" file1)))
    (should (string-equal res "ssvv \nHere file1.txt\n```text\nContents for file1\n```\n bbb"))
    ;; ;; (print (oai-block-tags-replace (format "ssvv `@%s` bbb" file2))))
    ;; (string-join (string-split (oai-block-tags-replace (format "ssvv `@%s` bbb" file2)) "\n" ) "\\n"))
    ;; (oai-block-tags-replace (format "ssvv `@%s` bbb" file2)))
    (setq res (oai-block-tags-replace (format "ssvv @%s bbb" file2)))
    (should (string-equal res "ssvv \nHere file2.el\n```elisp\n(defun aa() )\n```\n bbb"))
    ;; (string-join (string-split (oai-block-tags-replace (format "ssvv [[%s]] bbb" file3)) "\n" ) "\\n"))
    ;; (oai-block-tags-replace (format "ssvv [[%s]] bbb" file3)))
    ;; "ssvv \\nssssss\\nHere file3.py:\\n```python\\nimport os\\n```\\n\\n bbb"
    ;;                           "ssvv \n\nHere file3.py:\\n```python\\nimport os\\n```\\n\\n bbb"
    ;; (oai-block-tags-replace (format "ssvv [[%s]] bbb" file3)))
    (setq res (oai-block-tags-replace (format "ssvv [[%s]] bbb" file3)))
    (should (string-equal res "ssvv \nHere file3.py\n```python\nimport os\n```\n bbb"))))


;; -=-= Test: replace-last-regex-smart
(ert-deftest oai-tests-block-tags--replace-last-regex-smart ()
  (should
   (string-equal (oai-block-tags--replace-last-regex-smart "asdasd@Backtraceasdasdasd" "\\(@Backtrace\\)" "111")
                 "asdasd111asdasdasd"))

  (should
   (string-equal
    (oai-block-tags--replace-last-regex-smart "Same code: [[file:~/tmp/emacs::27-30]]```" oai-block-tags--org-link-any-re)
    "[[file:~/tmp/emacs::27-30]]"))

  (should (not (oai-block-tags--replace-last-regex-smart "Same code: ```[[file:~/tmp/emacs::27-30]]```" oai-block-tags--org-link-any-re)))

  (should
   (string-equal (oai-block-tags--replace-last-regex-smart "asda\n```\nvas@Backtraceasdasd\n```\nasd" "\\(@Backtrace\\)" "111")
                 "asda\n```\nvas@Backtraceasdasd\n```\nasd"))

  (should
   (string-equal (oai-block-tags--replace-last-regex-smart "asdasd@Backtraceasdasdasd" "@Backtrace")
                 "@Backtrace"))

  ;; search without replace
  (should (string-equal (oai-block-tags--replace-last-regex-smart
                         "foo @Backtrace` bar `@Backtrace `@BacktraceX"
                         oai-block-tags--regexes-backtrace)
                        "@Backtrace"))

  (should (string-equal (oai-block-tags--replace-last-regex-smart
                         "foo `@Backtrace` bar `@Backtrace `@Backtrace`X"
                         oai-block-tags--regexes-backtrace)
                        "@Backtrace"))

  (should (string-equal (oai-block-tags--replace-last-regex-smart
                         "foo `@Backtrace` bar `@Backtrace @B X"
                         oai-block-tags--regexes-backtrace)
                        "@B"))

  (should
   (string-equal (oai-block-tags--replace-last-regex-smart
                  "foo `@Backtrace` bar `@Backtrace  @B X"
                  oai-block-tags--regexes-backtrace
                  "REPLACED")
                 "foo `@Backtrace` bar `@Backtrace  REPLACEDX"))
  ;; with space
  (should
   (equal (oai-block-tags--replace-last-regex-smart
           "foo `@Backtrace` bar @Backtrace `@Backtrace "
           oai-block-tags--regexes-backtrace
           "REPLACED")
          "foo `@Backtrace` bar @Backtrace `REPLACED "))

  (should
   (equal (oai-block-tags--replace-last-regex-smart
           "foo `@Backtrace` bar @B `@BacktraceX"
           oai-block-tags--regexes-backtrace
           "REPLACED")
          "foo `@Backtrace` bar @B `REPLACEDX"))

  (should
   (equal (oai-block-tags--replace-last-regex-smart
           "foo @/asd.txt` X"
           oai-block-tags--regexes-path
           "REPLACED")
          "fooREPLACED` X"))

  (should
   (equal (oai-block-tags--replace-last-regex-smart "foo @. bar " oai-block-tags--regexes-path "REPLACED")
          "fooREPLACED bar "))

  (should
   (string-equal
    (oai-block-tags--replace-last-regex-smart "asd @/tmp/t.txt assd" oai-block-tags--regexes-path "path")
    "asdpath assd")))

;; -=-= Test: oai-block-tags--get-content-at-point-not-org
(ert-deftest oai-tests-block-tags--get-content-at-point-not-org1 ()
  ;; Test: outline
  (should
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
        (oai-block-tags--get-content-at-point-not-org))))))

(ert-deftest oai-tests-block-tags--get-content-at-point-not-org2 ()
  ;; Test: defun
  (should
   (string-equal
    "\nFunction:\n```elisp\n(defun f1 ()\nt\n)\n```"
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "(defun f1 ()\nt\n)\n\n(defun f2 ()\nt\n)")
      (goto-char 1)
      (oai-block-tags--get-content-at-point-not-org)))))

;; (ert-deftest oai-tests-block-tags--get-content-at-point-not-org3 ()
;;   ;; Test: paragraph
;;   (should
;;    (string-equal
;;     "\n```text\n;; -- header2\ntext2\n```"
;;     (with-temp-buffer
;;       (text-mode)
;;       (setq-local paragraph-start "\f\\|[ \t]*$")
;;       (setq-local paragraph-separate "[ \t\f]*$")
;;       (let ((p))
;;         (progn
;;           (insert "text0\n")
;;           (insert "\n")
;;           (insert ";; -- header1\ntext1\n")
;;           (setq p (point))
;;           (insert "\n")
;;           (insert ";; -- header2\ntext2"))
;;         (goto-char p))
;;       (oai-block-tags--get-content-at-point-not-org)))))

;; -=-= Test: oai-block-tags--get-content-at-point
(ert-deftest oai-tests-block-tags--get-content-at-point1 ()
  (should
   (string-equal
    "\nBlock name: asd\n```elisp\naa\n```"
    (with-temp-buffer
      (org-mode)
      (insert "ssd\n#+NAME: asd\n#+begin_src elisp\naa\n#+end_src\n")
      (goto-char 15)
      (oai-block-tags--get-content-at-point)))))

(ert-deftest oai-tests-block-tags--get-content-at-point2 ()
  ;; (should
  ;;  (string-equal
  ;;   "```elisp\naa\n```"
    (with-temp-buffer
      (org-mode)
      (insert "#+NAME: asd\n#+begin_src text\n```elisp")
      (let ((p (point))
            res)
        (insert "\naa\n```\n#+end_src\n")
        (goto-char p)
        ;; (oai-block-tags--contents-area)))
        ;; (oai-block-tags--markdown-block-range)))
        ;; (oai-block-tags--get-m-block)))
        (setq res (oai-block-tags--get-content-at-point))
        (should (string-equal res "```elisp\naa\n```")))))

(ert-deftest oai-tests-block-tags--get-content-at-point3 ()
  (should
   (string-equal
    "```aa```"
    (with-temp-buffer
      (org-mode)
      (insert "#+NAME: asd\n#+begin_src text\n```elisp```as ```")
      (let ((p (point)))
        (insert "aa```\n#+end_src\n")
        (goto-char p)
        ;; (oai-block-tags--contents-area)))
        ;; (oai-block-tags--markdown-block-range)))
        ;; (oai-block-tags--get-m-block)))
        (oai-block-tags--get-content-at-point))))))


;; -=-= Test: oai-block-tags--get-content-org-block-at-point
(ert-deftest oai-tests-block-tags--get-content-org-block-at-point ()
  (should
   (string-equal
    "\nBlock name: asd\n```elisp\naa\n```"
    (with-temp-buffer
      (org-mode)
      (insert "#+NAME: asd\n#+begin_src elisp\naa\n#+end_src\n")
      (goto-char 11)
      (oai-block-tags--get-content-org-block-at-point)))))

;; (ert-deftest oai-tests-block-tags--get-org-content-m-block2 ()
;;   (should
;;    (string-equal
;;     "\nBlock name: asd\n```elisp\naa\n```"
;;     (with-temp-buffer
;;       (org-mode)
;;       (insert "#+NAME: asd\n#+begin_ai\naa\n#+end_ai\n")
;;       (goto-char 11)
;;       (oai-block-tags--get-content-org-block-at-point)))))

;; -=-= Test: oai-block-tags--compose-block-for-path
(ert-deftest oai-tests-block-tags--compose-block-for-path ()
  (should
   (string-equal
    (file-name-nondirectory (directory-file-name "/aa/asd")) "asd"))
  (should
   (string-equal
    (file-name-nondirectory (directory-file-name "/aa/")) "aa"))
  (should
   (string-equal
    (file-name-nondirectory (directory-file-name "/aa")) "aa"))

  (should
   (string-equal (oai-block-tags--compose-block-for-path "a.el" "ss")
                 "
Here a.el
```elisp
ss
```")))

;; -=-= Test: oai-block-tags--compose-m-block
(ert-deftest oai-tests-block-tags--compose-m-block ()
  (should
   (string-equal (oai-block-tags--compose-m-block "aaa" :lang "bbb" :header "ccc") "\nccc\n```bbb\naaa\n```"))
  ;; Header are ignored for AI block
  (should
   (string-equal (oai-block-tags--compose-m-block "asda\n[me:] asdas" :lang "ai" :header "some header") "asda\n[me:] asdas"))
  (should
   (string-equal (oai-block-tags--compose-m-block "[me:] asda\n[ai:] asdas" :lang "ai" :header "some header") "[me:] asda\n[ai:] asdas"))
  (should
   (string-equal (oai-block-tags--compose-m-block "Text\nasdas" :lang "ai" :header "Header:")
                 "Text\nasdas"))
  (should
   (string-equal (oai-block-tags--compose-m-block "Text\nasdas" :lang "ai" :header "Header:" :inner t)
                 "\nHeader:\n```ai\nText\nasdas\n```"))
  (should (equal (oai-block-tags--compose-m-block nil :lang "ai" :inner t) nil))
  (should (equal (oai-block-tags--compose-m-block nil :lang "ai" :inner nil) nil))
  (should (equal (oai-block-tags--compose-m-block nil :lang nil :inner t) nil)))

;; -=-= Test: oai-block-tags--markdown-block-string-p
(ert-deftest oai-tests-block-tags--position-in-markdown-block-str-p ()
  (let* ((line "aaa```bbb```ccc")
         (range (oai-block-tags--markdown-block-string-p line 5)))
    (should (string-equal (substring line (car range) (cadr range)) "```bbb")))

  (should (oai-block-tags--markdown-block-string-p "aaa```bbb```ccc" 5))
  (should-not (oai-block-tags--markdown-block-string-p "aaa```bbb```ccc" 10))
  (should (equal (oai-block-tags--markdown-block-string-p "a```f```d ```elie aa ``` asd" 14) '(10 21))))




;; -=-= Test: oai-block-tags--check-if-char-at-in-direction !!
;; (ert-deftest oai-tests-block-tags--check-if-char-at-in-direction ()
;;   (should (oai-block-tags--check-if-char-at-in-direction "ab c    " 3 ?c 'right))  ;; t
;;   (should-not (oai-block-tags--check-if-char-at-in-direction "ab d    " 3 ?c 'right))  ;; nil
;;   (should-not (oai-block-tags--check-if-char-at-in-direction "ab c\ndef" 5 ?b 'left))  ;; nil
;;   (should-not (oai-block-tags--check-if-char-at-in-direction "ab d\nef" 5 ?b 'left))   ;; nil
;;   (should (oai-block-tags--check-if-char-at-in-direction "ab d\nef" 5 ?e 'left))   ;; t
;;   ;; at \n
;;   (should-not (oai-block-tags--check-if-char-at-in-direction "ab d\nef" 4 ?d 'left))   ;; nil
;;   (should-not (oai-block-tags--check-if-char-at-in-direction "ab d\nef" 4 ?e 'right))  ;; nil
;;   (should (oai-block-tags--check-if-char-at-in-direction "ab   c " 4 ?c 'right))   ;; t
;;   (should (oai-block-tags--check-if-char-at-in-direction "ab   c " 4 ?b 'left))    ;; t

;;   (should (oai-block-tags--check-if-char-at-in-direction "ab d     c" 5 ?c 'right))  ;; t
;;   (should-not (oai-block-tags--check-if-char-at-in-direction "ab d    \n c" 5 ?c 'right))  ;; nil
;;   (should (oai-block-tags--check-if-char-at-in-direction "like: ` file:/home/////////" 6 ?` 'left))
;;   )

;; -=-= Test: oai-block-tags--string-is-quoted-p
(ert-deftest oai-tests-block-tags--string-count-char-in-direction ()
  (should (eql 0 (oai-block-tags--string-count-char-in-direction "`as`\n`as`" 0  ?` 'left))) ; 0
  (should (eql 1 (oai-block-tags--string-count-char-in-direction "`as`\n`as`" 3  ?` 'left))) ; 1
  (should (eql 0 (oai-block-tags--string-count-char-in-direction "`as`\n`as`" 4  ?` 'left))) ; 0
  (should (eql 1 (oai-block-tags--string-count-char-in-direction "`as`\n`as`" 0  ?` 'right))) ; 1
  (should (eql 0 (oai-block-tags--string-count-char-in-direction "`as`\n`as`" 5  ?` 'left))) ; 0
  (should (eql 1 (oai-block-tags--string-count-char-in-direction "`as`\n`as`" 5  ?` 'right))) ; 1
  (should (eql 1 (oai-block-tags--string-count-char-in-direction "`as`\n`as`" 7  ?` 'right))) ; 1
  (should (eql 1 (oai-block-tags--string-count-char-in-direction "`as`\n`as`" 0  ?` 'right))) ; 1

  (should (eql 0 (oai-block-tags--string-count-char-in-direction "ab c    " 3 ?c 'right)))  ;; 0
  (should (eql 0 (oai-block-tags--string-count-char-in-direction "ab d    " 3 ?c 'right)))  ;; 0
  (should (eql 0 (oai-block-tags--string-count-char-in-direction "ab c\ndef" 5 ?b 'left)))  ;; 0
  (should (eql 0 (oai-block-tags--string-count-char-in-direction "ab d\nef" 5 ?b 'left)))   ;; 0
  (should (eql 0 (oai-block-tags--string-count-char-in-direction "ab d\nef" 5 ?e 'left)))   ;; 0
  ;; at \n
  (should (eql 0 (oai-block-tags--string-count-char-in-direction "ab d\nef" 4 ?d 'left)))   ;; 0
  (should (eql 0 (oai-block-tags--string-count-char-in-direction "ab d\nef" 4 ?e 'right)))  ;; 0
  (should (eql 1 (oai-block-tags--string-count-char-in-direction "ab   c " 4 ?c 'right)))   ;; 1
  (should (eql 1 (oai-block-tags--string-count-char-in-direction "ab   c " 4 ?b 'left)))    ;; 1

  (should (eql 1 (oai-block-tags--string-count-char-in-direction "ab d     c" 5 ?c 'right)))  ;; t
  (should (eql 0 (oai-block-tags--string-count-char-in-direction "ab d    \n c" 5 ?c 'right)))  ;; 0
  (should (oai-block-tags--string-count-char-in-direction "like: ` file:/home/////////" 6 ?` 'left)) ;; 0
  )

;; -=-= Test: oai-block-tags--string-is-quoted-p
(ert-deftest oai-tests-block-tags--string-is-quoted-p ()
  (should (oai-block-tags--string-is-quoted-p "`as`\n`as`" 1)) ; t
  (should-not (oai-block-tags--string-is-quoted-p "`as`\n`as`" 0))) ; nil

;; -=-= Test: noweb: oai-block-tags--get-content-org-block-at-point, oai-block-get-content.

(ert-deftest oai-tests-block-tags--noweb1 ()

  ;; oai-block-get-content
  (should
   (string-equal
    "Never a foot too far, even."
    (with-temp-buffer
      (org-mode)
      (insert "#+NAME: initialization
#+BEGIN_SRC emacs-lisp
  (setq sentence \"Never a foot too far, even.\")
#+END_SRC

#+begin_ai :noweb yes
<<initialization()>>
#+end_ai")
      (let (org-confirm-babel-evaluate)
        (oai-block-get-content))))))

(ert-deftest oai-tests-block-tags--noweb2 ()
  ;; oai-block-get-content
  (should
   (string-equal
    "<<initialization()>>"
    (with-temp-buffer
      (org-mode)
      (insert "#+NAME: initialization
#+BEGIN_SRC emacs-lisp
  (setq sentence \"Never a foot too far, even.\")
#+END_SRC

#+begin_ai :noweb
<<initialization()>>
#+end_ai")
      (oai-block-get-content))))

  (should
   (string-equal
    "(setq sentence \"Never a foot too far, even.\")"
    (with-temp-buffer
      (org-mode)
      (insert "#+NAME: initialization
#+BEGIN_SRC emacs-lisp
  (setq sentence \"Never a foot too far, even.\")
#+END_SRC

#+begin_ai :noweb yes
<<initialization>>
#+end_ai")
      (oai-block-get-content)))))

;; (ert-deftest oai-tests-block-tags--noweb ()
;;   ;; oai-block-get-content
;;   (should
;;    (string-equal
;;     "Never a foot too far, even."
;;     (with-temp-buffer
;;       (org-mode)
;;       (insert "
;; #+NAME: initialization1
;; #+begin_ai :noweb yes
;; text
;; #+end_ai

;; #+NAME: initialization
;; #+begin_ai :noweb yes
;; ss
;; <<initialization1()>>
;; #+end_ai

;; #+begin_ai :noweb yes
;; <<initialization>>
;; #+end_ai")
;;       (oai-block-get-content)))))

;; -=-= Test: oai-block-tags--get-content-chat-message-at-point
(ert-deftest oai-tests-block-tags--get-content-chat-message-at-point1 ()
  (with-temp-buffer
    ;; (setq ert-enabled nil)
    (org-mode)
    (transient-mark-mode)
    (oai-tests-block-tags-insert-block) ; output "Mark set"
    (let (p1 p2 res)
      (insert "[ai:] vvv1\n")
      (setq p1 (point))
      (insert "[ME:] bla\nbbb\n")
      (setq p2 (point))
      ;; (insert "```elisp\n")
      ;; (insert "as\n")
      ;; (setq p2 (point))
      (insert "[ai:] vvv\n\n")
      (should-not (setq res (oai-block-tags--get-content-chat-message-at-point)))
      (goto-char p1)
      (setq res (oai-block-tags--get-content-chat-message-at-point))
      (should (string-equal res "[ME:] bla\nbbb"))
      (goto-char p2)
      (setq res (oai-block-tags--get-content-chat-message-at-point))
      (should (string-equal res "[ai:] vvv" )))))

(ert-deftest oai-tests-block-tags--get-content-chat-message-at-point2 ()
  (with-temp-buffer
    ;; (setq ert-enabled nil)
    ;; (org-mode)
    (fundamental-mode)
    ;; (transient-mark-mode)
    (let (p1 p2 res)
      (insert "[ai:] vvv1\n")
      (setq p1 (point))
      (insert "[ME:] bla\nbbb\n")
      (setq p2 (point))
      ;; (insert "```elisp\n")
      ;; (insert "as\n")
      ;; (setq p2 (point))
      (insert "[ai:] vvv\n\n")
      (should-not (setq res (oai-block-tags--get-content-chat-message-at-point)))
      (goto-char p1)
      (setq res (oai-block-tags--get-content-chat-message-at-point))
      (should (string-equal res "[ME:] bla\nbbb"))
      (goto-char p2)
      (setq res (oai-block-tags--get-content-chat-message-at-point))
      (should (string-equal res "[ai:] vvv" )))))

;; -=-= Test: oai-block-tags-get-content-ai-messages
(ert-deftest oai-tests-block-tags--get-content-ai-messages1 ()
  (with-temp-buffer
    (org-mode)
    (let* ((element (progn (insert "#+begin_ai :stream t :sys \"A helpful LLM.\" :stream2 :max-tokens 50 :max-tokens2 :model \"gpt-3.5-turbo\" :model1 :model2 t :model3 :temperature 0.7\n#+end_ai\n")
                           (goto-char 1)
                           (oai-block-p)))
           ;; (info (progn (goto-char (org-element-property :begin element)) (oai-block-get-info)))
           )
      (should-error (oai-block-tags-get-content-ai-messages element t nil nil nil nil nil 'chat "sys1" "3") :type 'error))))
      ;; (should-error (oai-block-tags-get-content-ai-messages nil element 'chat "sys1" "sys-all2" 3) :type 'error))))

(ert-deftest oai-tests-block-tags--get-content-ai-messages2 ()
  (with-temp-buffer
    (org-mode)
    (let* ((element (progn (insert "#+begin_ai :stream t :sys \"A helpful LLM.\" :stream2 :max-tokens 50 :max-tokens2 :model \"gpt-3.5-turbo\" :model1 :model2 t :model3 :temperature 0.7\nss\n#+end_ai\n")
                           (goto-char 1)
                           (oai-block-p)))
           (res (oai-block-tags-get-content-ai-messages element t nil nil nil nil 'chat "sys1" "3")))
      (should (eq (length res) 2))
      (should (string-match "sys1" (plist-get (aref res 0) :content)))
      (should (eql 'system (plist-get (aref res 0) :role)))
      (should (eql 'user (plist-get (aref res 1) :role)))
      ;; (should (string-match "sys-all2" (plist-get (aref res 1) :content)))
      (should (string-match "ss" (plist-get (aref res 1) :content))))))

(ert-deftest oai-tests-block-tags--get-content-ai-messages3 ()
  (with-temp-buffer
    (org-mode)
    (let* ((element (progn (insert "#+begin_ai :stream t :sys \"A helpful LLM.\" :stream2 :max-tokens 50 :max-tokens2 :model \"gpt-3.5-turbo\" :model1 :model2 t :model3 :temperature 0.7\nss\n[AI:]vv\n[ME:]tt\n#+end_ai\n")
                           (goto-char 1)
                           (oai-block-p)))
           (res (oai-block-tags-get-content-ai-messages element t nil nil nil nil 'chat "sys1" "3")))
      (should (eq (length res) 4))
      (should (eql 'system (plist-get (aref res 0) :role)))
      (should (eql 'user (plist-get (aref res 1) :role)))
      (should (eql 'assistant (plist-get (aref res 2) :role)))
      (should (eql 'user (plist-get (aref res 3) :role)))
      (should (string-match "tt" (plist-get (aref res 3) :content))))))

;; -=-= Test: oai-block-tags-get-content-ai-messages with tags
;; oai-block-tags-replace and
(ert-deftest oai-tests-block-tags--get-content-ai-messages-loop ()
  (with-temp-buffer
    (progn
      (org-mode)
      ;; block 1. Named
      (oai-tests-block-tags-insert-block "aal")
      (insert "test\n\n[ai]:\nMy output have limit of 150-tokens.\n\n[ME]:")
      (let ((block (oai-block-p)))
        (goto-char (point-max))
        (forward-line)
        ;; block 2. with link
        (oai-tests-block-tags-insert-block nil t)
        (insert "blas\n")
        (insert "[[aal]]")
        (oai-block-tags-replace (oai-block-get-content))))))

(when (require 'org-links nil 'noerror)
  (ert-deftest oai-tests-block-tags--block-get-content--tags-replace ()
    (with-temp-buffer
      (progn
        (org-mode)
        ;; 1) insert aal block
        (oai-tests-block-tags-insert-block "aal")
        (insert "test\n\n[ai]:\nMy output have limit of 150-tokens.\n\n[ME]:")
        (let ((block (oai-block-p))
              res)
          (goto-char (point-max))
          (newline)
          ;; 2) insert block with link to block 1)
          (oai-tests-block-tags-insert-block nil t)
          (insert "blas\n")
          (insert "[[aal]]")
          ;; 3) get-content and replace-tags
          (setq res
                (oai-block-tags--clear-properties
                 (oai-block-tags-replace (oai-block-get-content))))
          (should (string-equal res
                                "blas
[ME]: test

[ai]: My output have limit of 150-tokens."))))))


  (ert-deftest oai-tests-block-tags--get-content-ai-messages-direct2 ()
    (with-temp-buffer
      (progn
        (org-mode)
        (oai-tests-block-tags-insert-block "aal")
        (insert "test\n\n[ai]:\nMy output have limit of 150-tokens.\n\n[ME]:")
        (let ((block (oai-block-p))
              res)
          (goto-char (point-max))
          (newline)
          (oai-tests-block-tags-insert-block nil t)
          (insert "blas\n")
          (insert "[[aal]]")
          (setq res
                (oai-block-tags-get-content))
          (should (string-equal res
                                "[ME]: blas
test

[ai]: My output have limit of 150-tokens.")))))))

;; (name (org-element-property :name (oai-block-p))))

;; (oai-block-tags-replace (oai-block-get-content))


;; -=-= provide
(provide 'oai-tests-block-tags)

;;; oai-tests-block-tags.el ends here
