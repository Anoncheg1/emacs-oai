;;; oai-tests-oai.el --- Tests -*- lexical-binding: t -*-
;; Copyright (c) 2025 github.com/Anoncheg1,codeberg.org/Anoncheg
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Author: <github.com/Anoncheg1,codeberg.org/Anoncheg>
;; Keywords: tools, async, callback
;; URL: https://github.com/Anoncheg1/async1

;; (eval-buffer)
;; (ert t)
;; emacs -Q --batch -l ert.el -l oai-debug.el -l oai-block.el -l oai-block-tags.el -l oai-timers.el -l oai-async1.el -l oai-restapi.el -l oai-prompt.el -l oai.el -l ./tests/oai-tests-oai.el -f ert-run-tests-batch-and-exit
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

(require 'oai)
;; (print (list "vvvvvvvvvvvvvvvvvvvvvv1" (bound-and-true-p debug)))
(require 'ert)
(defvar ert-enabled nil)
; org-links - is optional dependency


;; -=-= Test: oai-expand-block-1
(defvar oai-tests-oai--expand-block-string1 "#+begin_ai
asdas
[[11::asds]]

[me:] VV
#+end_ai

#+name: asds
#+begin_ai
[ME]: Please
[ai]: My output
#+end_ai
")

(when (require 'org-links nil 'noerror)
  (ert-deftest oai-tests-oai--expand-block-1 ()
    (with-temp-buffer
      (org-mode)
      (insert oai-tests-oai--expand-block-string1)
      (goto-char 1)
      ;; (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
      ;; (oai-expand-block nil))
      (let ((oai-restapi-con-token "token")
            res)
        (setq res (substring-no-properties (oai-expand-block nil)))
        (should (string-equal
                 res
                 "[ME]: asdas
Please

[ai]: My output

[ME]: VV"))
        ;; (print (setq res (oai-expand-block-deep)))))
        (print (setq res (oai-expand-block-deep)))
        (should (equal
                 (oai-expand-block-deep)
                 '("https://api.openai.com/v1/chat/completions" (("Content-Type" . "application/json") ("Authorization" . "Bearer token"))
                   ((messages . [(:role system :content "Be helpful.") (:role user :content "asdas\nPlease") (:role assistant :content "My output") (:role user :content "VV")]) (model . "gpt-4o-mini") (stream . t)))
        ))))))

;; -=-= Test: oai-expand-block-2
(defvar oai-tests-oai--expand-block-string "#+begin_ai :model nil
[[* tt1]]

[[* tt2]]
#+end_ai

* tt1
asd

* tt2
asd2")


(defvar oai-tests-oai--expand-block-2-shouldbe "```text
# tt1
asd


```




```text
# tt2
asd2
```")
(when (require 'org-links nil 'noerror)
  (ert-deftest oai-tests-oai--expand-block-2 ()
    (with-temp-buffer
      (org-mode)
      (insert oai-tests-oai--expand-block-string)
      (goto-char 1)
      (let ((oai-restapi-con-token "token")
            res)
        (setq res (substring-no-properties (oai-expand-block nil)))
        (should
         (string-equal
          (concat "[ME]: " oai-tests-oai--expand-block-2-shouldbe)
          res))
        (let ((messages (oai-block-tags-get-content-ai-messages (oai-block-p)
                                                                t  ; noweb-control
                                                                nil ; links-only-last
                                                                nil ; not-clear-properties
                                                                )))
          ;; messages)))
          ;; (vconcat (list (list :role 'user :content oai-tests-oai--expand-block-2-shouldbe))))))
          (should (equal messages
                         (vconcat (list (list :role 'user :content oai-tests-oai--expand-block-2-shouldbe)))))
          (should (equal
                   (oai-expand-block-deep)
'("https://api.openai.com/v1/chat/completions" (("Content-Type" . "application/json") ("Authorization" . "Bearer token")) ((messages . [(:role system :content "Be helpful.") (:role user :content "```text
# tt1
asd


```




```text
# tt2
asd2
```")]) (stream . t)))
        )))))))


;; -=-= Test: oai-debug--safe-format
(ert-deftest test-oai-debug--safe-format ()
  "Tests for `oai-debug--safe-format` with streamlined scenarios."
  (let ((cases '(
                 ("Hello %s!" ("World") "Hello! World\n")
                 ("%s %s" ("One" "Two") " One Two\n")
                 ("%s" ("Extra1" "Extra2") " Extra1 Extra2\n")
                 ("%s %s" ("First") " First\n")
                 ("No args here" () "No args here \n")
                 ("Empty %s" ("") "Empty \n")
                 )))
    (dolist (case cases)
      (let* ((fmt (nth 0 case))
             (args (nth 1 case))
             (expected (nth 2 case))
             (result (apply 'oai-debug--safe-format fmt args)))
        (should (equal result expected))))))
;; -=-= Test oai--expand-block-deep-masking-chat

(ert-deftest oai--expand-block-deep-masking-chat () ; for masking chat prefixes
  (let ((tmpfile (make-temp-file "masking-chat-prefix" nil ".org")))
    (with-temp-file tmpfile
      (insert "#+begin_ai\n")
      (let ((p1 (point)))
        (insert "\n#+end_ai")
        (goto-char p1))
      (insert "test\n\n[ai]:\nblabla\n\n[ME]: vv")
      )

    (with-temp-buffer
      (insert "#+begin_ai\n")
      (let ((p1 (point)))
        (insert "\n#+end_ai")
        (goto-char p1))
      (insert (concat "[[" tmpfile "]]"))
      ;; (print (buffer-substring-no-properties (point-min) (point-max))))
      (let ((oai-restapi-con-token "token"))
        (should (= 2 (length (cdr (car (nth 2 (oai-expand-block-deep))))))))
      (delete-file tmpfile))))

(ert-deftest oai--expand-block-deep-ai () ; for masking chat prefixes
  (let ((tmpfile (make-temp-file "masking-chat-prefix" nil ".ai")))
    (with-temp-file tmpfile
      (insert "test\n\n[ai]:\nblabla\n\n[ME]: vv")
      )

    (with-temp-buffer
      (insert "#+begin_ai\n")
      (let ((p1 (point)))
        (insert "\n#+end_ai")
        (goto-char p1))
      (insert (concat "[[" tmpfile "]]"))
      ;; (print (buffer-substring-no-properties (point-min) (point-max))))
      (let ((oai-restapi-con-token "token"))
        (should (= 4 (length (cdr (car (nth 2 (oai-expand-block-deep))))))))
      (delete-file tmpfile))))
;; -=-= provide
(provide 'oai-tests-oai)

;;; oai-tests-oai.el ends here
