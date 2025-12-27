;;; oai-tests-prompt.el --- Tests. -*- lexical-binding: t; -*-

;; Copyright (c) 2025 github.com/Anoncheg1,codeberg.org/Anoncheg
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Author: github.com/Anoncheg1,codeberg.org/Anoncheg

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
;;

(require 'ert)
(require 'oai-prompt)
(defvar ert-enabled nil)
;; (eval-buffer)
;; (ert t)
;; emacs -Q --batch -l ert.el -l oai-debug.el -l oai-block.el -l oai-block-tags.el -l oai-timers.el -l oai-async1.el -l oai-restapi.el -l oai-prompt.el -l ./tests/oai-tests-prompt.el -f ert-run-tests-batch-and-exit

;;; Code:

;; -=-= For `oai-prompt-collect-chat-research-steps-prompt'

(ert-deftest oai-tests-prompt--collect-chat-research-steps-prompt1 ()
  (should
   (equal
    (let ((oai-restapi-add-max-tokens-recommendation t)
          (max-tokens 200))
      (oai-prompt-collect-chat-research-steps-prompt oai-prompt-chain-list
                                             0
                                             (oai-block--collect-chat-messages-from-string
                                              "[ME:]How to make coffe?\n[AI]: IDK.")
                                             ""
                                             max-tokens))
      (vector (list :role 'system :content (concat (nth 0 oai-prompt-chain-list) " " (oai-restapi--get-lenght-recommendation 200)))
                    (list :role 'user :content "How to make coffe?")
                    (list :role 'assistant :content "IDK.")))))

(ert-deftest oai-tests-prompt--collect-chat-research-steps-prompt2 ()
  (should
   (equal
    (oai-prompt-collect-chat-research-steps-prompt oai-prompt-chain-list
                                                   1
                                                   (oai-block--collect-chat-messages-from-string "[ME:]How to make coffe?\n[AI]: IDK.")
                                                   "Be helpful.")
    (vector (list :role 'system :content (concat "Be helpful. " (nth 0 oai-prompt-chain-list)))
            (list :role 'user :content "How to make coffe?")
            (list :role 'assistant :content "IDK.")
            (list :role 'system :content (nth 1 oai-prompt-chain-list))))))

(ert-deftest oai-tests-prompt--collect-chat-research-steps-prompt3 ()
  (should
   (let (oai-restapi-add-max-tokens-recommendation)
     (equal
      (oai-prompt-collect-chat-research-steps-prompt oai-prompt-chain-list
                                                     2
                                                     (oai-block--collect-chat-messages-from-string (concat "[ME:]How to make coffe?\n[AI]: IDK.\n[SYS]: " (nth 1 oai-prompt-chain-list) "\n[AI]: IDK.")))
      (vector (list :role 'system :content (nth 0 oai-prompt-chain-list))
              (list :role 'user :content "How to make coffe?")
              (list :role 'assistant :content "IDK.")
              (list :role 'system :content (nth 1 oai-prompt-chain-list))
              (list :role 'assistant :content "IDK.")
              (list :role 'system :content (nth 2 oai-prompt-chain-list)))))))

(ert-deftest oai-tests-prompt--collect-chat-research-steps-prompt4 ()
  (should
   (let (oai-restapi-add-max-tokens-recommendation)
     (equal
      (oai-prompt-collect-chat-research-steps-prompt oai-prompt-chain-list
                                                     0
                                                     (oai-block--collect-chat-messages-from-string "[ME:]How to make coffe?\n[AI]: IDK."))
      (vector (list :role 'system :content (nth 0 oai-prompt-chain-list))
              (list :role 'user :content "How to make coffe?")
              (list :role 'assistant :content "IDK."))))))

;; -=-= provide
(provide 'oai-tests-prompt)

;;; oai-tests-prompt.el ends here
