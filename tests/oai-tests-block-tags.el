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
(ert-deftest oai-tests-block-tags--regexes-path ()
  (should
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
          '("@/file-s_s" "@/file.t_xt" "@./file.txt" "@/some/path/file.txt" "@C:\\some\\file.txt" "@L:\\folder\\file.txt" "@\\network\\share" "@.\\windowsfile" "@/file/" "@/file.txt/" "@./file.txt/" "@/some/path/file.txt/" "@C:\\some\\file.txt\\" "@L:\\folder\\file.txt\\" "@\\network\\share\\" "@.\\windowsfile\\" nil nil nil nil nil "@../right" "@../right/" "@.." "@." "@/"))))
;; -=-= Tests: oai-block-tags--markdown-fenced-code-body-get-range
(ert-deftest oai-tests-block-tags--markdown-mark-fenced-code-body-get-range1 ()
  "Test fenced code detection."
  (let ((payload "text before
```elisp
code block
line2
```
text after"))
    (with-temp-buffer
      (insert payload)
      ;; Move point to inside the code block
      (goto-char (point-min))
      (re-search-forward "code block")
      (beginning-of-line)
      (forward-line -1)
      (let* ((limit-begin (point-min))
             (limit-end (point-max))
             (range (oai-block-tags--markdown-fenced-code-body-get-range
                     limit-begin limit-end)))
        (should (equal range (list 22 39))))))
  )

(ert-deftest oai-tests-block-tags--markdown-mark-fenced-code-body-get-range2 ()
  "Test fenced code detection."
(let ((payload "text before
```elisp
```
code block
line2
```
text after"))
    (with-temp-buffer
      (insert payload)
      ;; Move point to inside the code block
      (goto-char (point-min))
      (re-search-forward "code block")
      (let* ((limit-begin (point-min))
             (limit-end (point-max))
             (range (oai-block-tags--markdown-fenced-code-body-get-range
                     limit-begin limit-end)))
        (should
        (equal range nil)))))
)

;; -=-= Test: oai-block-tags--get-replacement-for-org-link - dir
(ert-deftest oai-tests-block-tags--get-replacement-for-org-link-dir ()
  ""
  (let ((oai-block-tags-use-simple-directory-content-flag t)
                res)
    (setq res (oai-block-tags--get-replacement-for-org-link "file:./"))
    (setq res (string-match "Here . folder" res))
    (should (eq 1 res))
    (setq res (oai-block-tags--get-replacement-for-org-link "[[./]]"))
    (setq res (string-match "Here . folder" res))
    (should (eq 1 res))
    (setq res (oai-block-tags--get-replacement-for-org-link "[[file:./]]"))
    (setq res (string-match "Here . folder" res))
    (should (eq 1 res))
    (setq res (oai-block-tags--get-replacement-for-org-link "[[file:.]]"))
    (setq res (string-match "Here . folder" res))
    (should (eq 1 res))))

;; -=-= Test: oai-block-tags-replace
(ert-deftest oai-tests-block-tags--replace-org-links-norm-header ()
  (let ((kill-buffer-query-functions)
        res1 res2
        target)
    (with-temp-buffer
      (org-mode)
      (setq buffer-file-name "/mock/org.org")
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
        (setq res2 (oai-block-tags--get-replacement-for-org-file-link-in-other-file
                      "/mock/org.org" "*sub-headline"))
        (should (string-equal target res2))

        ;; (advice-remove 'org-open-file (intern "org-links-org-open-file-advice"))
        (set-buffer-modified-p nil)
        ))))

;; -=-= tags tests
(ert-deftest oai-tests-block-tags--replace-test ()
    (let* ((temp-file (make-temp-file "mytest"))
           (res
            ;; (unwind-protect
                (progn
                  (with-temp-file temp-file
                    (insert "Hello, world test!"))

                  (prog1 (oai-block-tags-replace (format "aas `@%s`bb." temp-file))
                    ;; (should (string= (oai-block-tags-replace temp-file) "Expected result")))
                    (delete-file temp-file))))
           (res (string-split res "\n"))
           )
      ;; res)
      (should (string-equal "aas " (nth 0 res)))
      (should (string-equal "```auto" (nth 2 res)))
      (should (string-equal "Hello, world test!" (nth 3 res)))
      (should (string-equal "```" (nth 4 res)))
      (should (string-equal "bb." (nth 5 res)))))


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

(ert-deftest oai-tests-block-tags--oai-block-tags-replace ()
  (with-temp-files '("file1.txt" "file2.txt")
                   (let ((res (string-split (oai-block-tags-replace (format "ssvv `@%s` bbb" temp-dir)) "\n"))
                         (regex-pattern "ssvv \nHere test[^ ]+ folder:\n```ls-output\n  /tmp/test[^ ]+:\n  -rw-rw-r-- 1 [^ ]+ 0 [A-Za-z]+ [0-9]+ [0-9:]+ file1.txt\n  -rw-rw-r-- 1 [^ ]+ 0 [A-Za-z]+ [0-9]+ [0-9:]+ file2.txt\n\n```\n bbb")
                         ;; (dired-listing-switches "-AlthG")
                         )
                     ;; (print res)))
                     ;; LINES of regex-pattern:
                     (should (string-match-p "ssvv " (nth 0 res)))
                     (should (string-match-p "Here test[^ ]+ folder:" (nth 1 res)))
                     (should (string-match-p "```ls-output" (nth 2 res)))
                     (should (string-match-p "  /tmp/test[^ ]+:" (nth 3 res)))
                     ;; "  -rw-rw-r-- 1 g 0 Nov  5 21:13 file1.txt"
                     (should (string-match-p "file[12].txt" (nth 4 res)))
                     (should (string-match-p "file[12].txt" (nth 5 res)))
                     (should (string-match-p "^```$" (nth 7 res)))
                     (should (string-match-p "^ bbb$" (nth 8 res))))))


;; -=-= Test: oai-block-tags--get-org-block-region
(ert-deftest oai-tests-block-tags--get-org-block-region ()
  (let (kill-buffer-query-functions
        org-execute-file-search-functions)
    (with-temp-buffer
      (org-mode)
      (add-hook 'org-execute-file-search-functions (intern "org-links-additional-formats"))
      (insert "#+begin_ai :max-tokens 100 :stream nil :sys \"Be helpful\"  :service github :model \"openai\"\n#+end_ai")
      (goto-char (point-min))
      ;; (print (oai-block-tags--get-org-block-region))
      (let ((res (oai-block-tags--get-org-block-region)))
        (should (= (car res) 91 ))
        (should (= (cadr res) 91 )))

      (goto-char (point-max))
      (insert "\n")
      ;; (let ((res (oai-block-tags--get-org-block-region)))
      ;;   (should (= (car res) 91 ))
      ;;   (should (= (cadr res) 91 )))
      (insert "#+begin_ai :max-tokens 100 :stream nil :sys \"Be helpful\"  :service github :model \"openai\"\n\n\n#+end_ai")
      ;; (goto-char (point-min))
      ;; (print (oai-block-tags--get-org-block-region))
      (let ((res (oai-block-tags--get-org-block-region)))
        (should (= (car res) 190 ))
        (should (= (cadr res) 192 )))
      )))

;; -=-= Test: oai-block-tags mark-block
(ert-deftest oai-tests-block-tags--mark-block ()
  (let (kill-buffer-query-functions
        org-execute-file-search-functions)
    (with-temp-buffer
      (org-mode)
      (add-hook 'org-execute-file-search-functions (intern "org-links-additional-formats"))
      (insert "#+begin_ai :max-tokens 100 :stream nil :sys \"Be helpful\"  :service github :model \"openai\"\n#+end_ai")
      (goto-char (point-min))
      (should (equal (oai-block-tags--markdown-mark-fenced-code-body) nil))
      (goto-char (point-min))
      (progn
        (insert "#+begin_ai :max-tokens 100 :stream nil :sys \"Be helpful\"  :service github :model \"openai\"")
        (insert "\n")
        (insert "```elisp")
        (insert "\n\n\n")
        (insert "```")
        (insert "\n")
        (insert "#+end_ai")
        (insert "\n"))
      (goto-line 1)
      (should (equal (oai-block-tags--markdown-mark-fenced-code-body) nil))
      (goto-line 2)
      (should (equal (oai-block-tags--markdown-mark-fenced-code-body) t))
      (should (= 100 (region-beginning)))
      (should (= 101 (region-end)))
      (goto-line 3)
      (should (equal (oai-block-tags--markdown-mark-fenced-code-body) t))
      (goto-line 4)
      (should (equal (oai-block-tags--markdown-mark-fenced-code-body) t))
      (goto-line 5)
      (should (equal (oai-block-tags--markdown-mark-fenced-code-body) nil))
      (goto-line 1)
      (should (equal (oai-block-tags-mark-md-block-body) t))
      (should (= 91 (region-beginning)))
      (should (= 106 (region-end)))
      (goto-line 3)
      (should (equal (oai-block-tags-mark-md-block-body) t))
      (should (= 100 (region-beginning)))
      (should (= 101 (region-end))))))

;; -=-= Test: oai-block-tags--filepath-to-language
(ert-deftest oai-tests-oai-block-tags--filepath-to-language ()
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
   (string-equal (oai-block-tags--filepath-to-language "a.txt") "text")))

;; -=-= Test: oai-block-tags-replace
(ert-deftest oai-tests-block-tags--replace ()
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
    ;; (oai-block-tags-replace (format "ssvv `@%s` bbb" file1)))
    (should (string-equal "ssvv \nHere file1.txt:\n```text\nContents for file1\n```\n bbb"
                          (oai-block-tags-replace (format "ssvv `@%s` bbb" file1))))
    ;; ;; (print (oai-block-tags-replace (format "ssvv `@%s` bbb" file2))))
    ;; (string-join (string-split (oai-block-tags-replace (format "ssvv `@%s` bbb" file2)) "\n" ) "\\n"))
    ;; (oai-block-tags-replace (format "ssvv `@%s` bbb" file2)))
    (should (string-equal "ssvv \nHere file2.el:\n```elisp\n(defun aa() )\n```\n bbb"
                          (oai-block-tags-replace (format "ssvv `@%s` bbb" file2))))
    ;; (string-join (string-split (oai-block-tags-replace (format "ssvv [[%s]] bbb" file3)) "\n" ) "\\n"))
    ;; (oai-block-tags-replace (format "ssvv [[%s]] bbb" file3)))
    ;; "ssvv \\nssssss\\nHere file3.py:\\n```python\\nimport os\\n```\\n\\n bbb"
    ;;                           "ssvv \n\nHere file3.py:\\n```python\\nimport os\\n```\\n\\n bbb"
    ;; (oai-block-tags-replace (format "ssvv [[%s]] bbb" file3)))
    (should (string-equal "ssvv \nHere file3.py:\n```python\nimport os\n```\n bbb"
                          (oai-block-tags-replace (format "ssvv [[%s]] bbb" file3))))))


;; -=-= Test: replace-last-regex-smart
(ert-deftest oai-tests-block-tags--replace-last-regex-smart ()
  (should
   (string-equal (oai-block-tags--replace-last-regex-smart "asdasd@Backtraceasdasdasd" "\\(@Backtrace\\)" "111")
                 "asdasd111asdasdasd"))

  (should
   (string-equal
    (oai-block-tags--replace-last-regex-smart "Same code: [[file:~/tmp/emacs::27-30]]```" oai-block--org-link-any-re)
    "[[file:~/tmp/emacs::27-30]]"))

  (should (not (oai-block-tags--replace-last-regex-smart "Same code: ```[[file:~/tmp/emacs::27-30]]```" oai-block--org-link-any-re)))

  (should
   (string-equal (oai-block-tags--replace-last-regex-smart "asda\n```\nvas@Backtraceasdasd\n```\nasd" "\\(@Backtrace\\)" "111")
                 "asda\n```\nvas@Backtraceasdasd\n```\nasd"))

  (should
   (string-equal (oai-block-tags--replace-last-regex-smart "asdasd@Backtraceasdasdasd" "@Backtrace")
                 "@Backtrace"))

  ;; search without replace
  (should (string-equal (oai-block-tags--replace-last-regex-smart
                         "foo `@Backtrace` bar `@Backtrace `@BacktraceX"
                         oai-block-tags--regexes-backtrace)
                        "@Backtrace"))

  (should (string-equal (oai-block-tags--replace-last-regex-smart
                         "foo `@Backtrace` bar `@Backtrace `@Backtrace`X"
                         oai-block-tags--regexes-backtrace)
                        "@Backtrace"))

  (should (string-equal (oai-block-tags--replace-last-regex-smart
                         "foo `@Backtrace` bar `@Backtrace `@B`X"
                         oai-block-tags--regexes-backtrace)
                        "@B"))

  (should
   (string-equal (oai-block-tags--replace-last-regex-smart
                  "foo `@Backtrace` bar `@Backtrace `@Backtrace`X"
                  oai-block-tags--regexes-backtrace
                  "REPLACED")
                 "foo `@Backtrace` bar `@Backtrace REPLACEDX"))
  ;; with space
  (should
   (equal (oai-block-tags--replace-last-regex-smart
           "foo `@Backtrace` bar `@Backtrace `@BacktraceX"
           oai-block-tags--regexes-backtrace
           "REPLACED")
          "foo `@Backtrace` bar REPLACED`@BacktraceX"))

  (should
   (equal (oai-block-tags--replace-last-regex-smart
           "foo `@Backtrace` bar `@B `@BacktraceX"
           oai-block-tags--regexes-backtrace
           "REPLACED")
          "foo `@Backtrace` bar REPLACED`@BacktraceX"))

  (should
   (equal (oai-block-tags--replace-last-regex-smart
           "foo `@/asd.txt` X"
           oai-block-tags--regexes-path
           "REPLACED")
          "foo REPLACED X"))

  (should
   (equal (oai-block-tags--replace-last-regex-smart "foo `@.` bar " oai-block-tags--regexes-path "REPLACED")
          "foo REPLACED bar "))

  (should
   (string-equal
    (oai-block-tags--replace-last-regex-smart "asd `@/tmp/t.txt` assd" oai-block-tags--regexes-path "path")
    "asd path assd")))

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

(ert-deftest oai-tests-block-tags--get-content-at-point-not-org3 ()
  ;; Test: paragraph
  (should
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
      (oai-block-tags--get-content-at-point-not-org)))))

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
  (should
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
        (oai-block-tags--get-content-at-point))))))


;; -=-= Test: oai-block-tags--markdown-fenced-code-body-get-range
(ert-deftest oai-tests-block-tags--markdown-fenced-code-body-get-range ()
  (should (equal '(39 42)
                 (with-temp-buffer
                   (org-mode)
                   (insert "#+NAME: asd\n#+begin_src text\n```elisp")
                   (let ((p (point)))
                     (insert "\naa\n```\n#+end_src\n")
                     (goto-char p)
                     (oai-block-tags--markdown-fenced-code-body-get-range))))))

;; -=-= Test: oai-block-tags--get-org-content-m-block
(ert-deftest oai-tests-block-tags--get-org-content-m-block ()
  (should
   (string-equal
    "\nBlock name: asd\n```elisp\naa\n```"
    (with-temp-buffer
      (org-mode)
      (insert "#+NAME: asd\n#+begin_src elisp\naa\n#+end_src\n")
      (goto-char 11)
      (oai-block-tags--get-org-content-m-block)))))

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
Here a.el:
```elisp
ss
```")))



;; -=-= provide
(provide 'oai-tests-block-tags)

;;; oai-tests-block-tags.el ends here
