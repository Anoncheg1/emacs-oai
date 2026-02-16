;;; oai-optional.el --- Useful functions that not enabled by default.  -*- lexical-binding: t; -*-

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

;; Configuration for usage:

;; (require 'oai-optional)
;; (add-hook 'oai-block-after-chat-insertion-hook
;;          #'oai-optional-remove-headers-hook-function)
;; (add-hook 'oai-block-after-chat-insertion-hook
;;          #'oai-optional-remove-distant-empty-lines-hook-function)

(require 'cl-lib) ; Ensure cl-lib is loaded for cl-defun and cl-destructuring-bind
(require 'org)
(require 'oai-debug)

;;; Code:

;; -=-= remove-distant-empty-lines hook
(defun oai-optional-remove-distant-empty-lines (beg)
  "Remove excessibe empty lines from BEG to current position.
Don't remove empty lines that have more than two lines in a row before
 tham."
  (forward-line -1) ; precaution
  ;; (beginning-of-line) ; required for loop
  (let ((empty-line)
        (cl 0))
    (while (< beg (point))
      (when (eolp) ; empty line
        (when (and (<= cl 2) (> cl 0) empty-line)
          (save-excursion
            (goto-char empty-line)
            (delete-char 1)))
        (setq cl 0)
        (setq empty-line (point)))
      (setq cl (1+ cl ))
      (forward-line -1))))

(defun oai-optional-remove-distant-empty-lines-hook-function (&optional type content before-pos stream)
  "Remove empty lines when there is too many of them.
Arguments TYPE CONTENT BEFORE-POS STREAM parameters described in
`oai-block-after-chat-insertion-hook' hook."
  (ignore content stream)
  (save-excursion
    (oai--debug "IN A HOOK oai-optional-remove-distant-empty-lines-hook-function: %s %s %s %s"
                before-pos
                (point)
                type
                (type-of type))
    (if stream
        (oai-optional-remove-distant-empty-lines (save-excursion (oai-block-previous-message)))
      ;; else - not stream
      (oai-optional-remove-distant-empty-lines before-pos))))

;; -=-= remove-headers hook
(defun oai-optional-remove-headers (beg-pos end-pos)
  "Remove Org mode header prefixes, like ^**.
You may require this, because  LLMs frequently uses markdown for headers
that recognized in Org mode as headers, that break blocks.
Works at every line between BEG-POS and END-POS in the current buffer.
Uses `org-outline-regexp-bol' to match headers, respecting
user-configured prefixes."
  (interactive "r")
  (replace-regexp-in-region org-outline-regexp-bol "" beg-pos end-pos))


(defun oai-optional-remove-headers-hook-function (type _content before-pos _stream)
  "Ready for usage in `oai-block-after-chat-insertion-hook'.
Remove Org headers between BEFORE-POS and current position in current
 buffer buffer by adding space before it.
TYPE _CONTENT BEFORE-POS BUF parameters described in
`oai-block-after-chat-insertion-hook' hook.
Should be added the last to be executed first."
  (oai--debug "IN A HOOK oai-optional-remove-headers-for-hook: %s %s %s %s"
              before-pos
              (point)
              type
              (type-of type))
  (when (member type '(text end))
    (save-excursion
      (let ((end (point-marker)))
        (goto-char before-pos)
        (while (re-search-forward org-outline-regexp-bol (marker-position end) t)
          (beginning-of-line)
          (insert " ") ; this effectively quote standard headers
          (end-of-line))))))

;;; provide
(provide 'oai-optional)
;;; oai-optional.el ends here
