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
      (when (eq (point) (line-end-position)) ; empty line
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
Arguments TYPE_CONTENT BEFORE-POS BUF parameters described in
`oai-block-after-chat-insertion-hook' hook."
  (ignore content stream)
  (save-excursion
    (oai--debug "IN A HOOK oai-optional-remove-distant-empty-lines-hook-function: %s %s %s %s"
                before-pos
                (point)
                type
                (type-of type))
    (if stream
        (oai-optional-remove-distant-empty-lines (save-excursion oai-block-previous-message)))
    ;; else - not stream
    (oai-optional-remove-distant-empty-lines (before-pos))))

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

;; (defcustom oai-optional-fill-paragraph-functions
;;   (list
;;    'org-fill-paragraph)
;;    "List of steps to perform in the `my/org-fill-paragraph' function.
;; Replace single `fill-paragraph-function' variable with list of
;; functions."
;;   :type '(repeat function)
;;   :group 'oai)

;; -=-= old: fill-paragraph (old, not used)
;; (defun oai-optional-fill-paragraph (&optional justify region)
;;   "Call functions until success.
;; Replace single `fill-paragraph-function' with list of functions.
;; Optional argument JUSTIFY is parameter of `fill-paragraph'.
;; Optional argument REGION ."
;;   (interactive (progn
;; 		 (barf-if-buffer-read-only)
;; 		 (list (when current-prefix-arg 'full) t)))
;;   ;; call in loop functions, untill one return true
;;   (seq-find (lambda(step)
;;                 ;; (message step) ; debug
;;                 (funcall step justify region))
;;             my/org-fill-paragraph-functions))

;; Usage:
;; (setq oai-optional-fill-paragraph-functions
;;           (append (list #'my/oai-fill-paragraph)
;;                   oai-optional-fill-paragraph-functions))

;; (keymap-set org-mode-map \"M-q\" #'oai-optional-fill-paragraph)


;; -=-= old: remove-distant-empty-lines (old version)
;; (cl-defun oai-optional-remove-distant-empty-lines (start end)
;;   "Remove empty lines in current buffer between START and END.
;; Removes an empty line only if another empty line is two lines above it.
;; An 'empty line' is blank or whitespace-only.

;; Example:
;;   Original:      Result:
;;   line 1         line 1
;;                  (empty A) (kept)  (empty A)
;;   line 2         line 2
;;                  (empty B) (removed)line 3
;;   line 3         line 4
;;                  (empty C) (removed)line 5
;;   line 4         (empty D)
;;   line 5         line 6
;;                  (empty D) (kept)
;;   line 6

;; Case ME:
;; line 1         line 1
;;                  (empty A) (kept)  (empty A)
;; line 2         line 2
;;                  (empty B) (not removed)
;; line 3 with [ME]:   line 4
;; "
;;   (interactive "r") ;; Usable interactively on a selected region.

;;   (let ((lines-to-delete-pos '()) ; Stores positions of lines to remove.
;;         (line-info-list '())      ; Stores (line-pos . is-blank-p) for all lines in region.
;;         (original-start start)    ; Store original region start for robustness.
;;         (original-end end))       ; Store original region end for robustness.

;;     (save-excursion ;; Preserve point and mark positions after execution.

;;       ;; Phase 1: Collect info about lines in the region.
;;       ;; Iterate through the region, recording each line's starting position
;;       ;; and whether it's blank (contains only whitespace).
;;       (goto-char start)
;;       (while (< (point) end)
;;         (push (cons (point) (string-blank-p (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
;;               line-info-list)
;;         (forward-line 1))
;;       ;; Reverse the list to get lines in document order (top to bottom).
;;       (setq line-info-list (nreverse line-info-list))

;;       ;; Phase 2: Identify lines for deletion.
;;       ;; Iterate through the collected line information, keeping track of the
;;       ;; blank status of the previous two lines to apply the condition.
;;       (let ((line-minus-1-info nil) ; Stores (pos . is-blank) for the line one step back.
;;             (line-minus-2-info nil)) ; Stores (pos . is-blank) for the line two steps back.
;;         (dolist (current-line-info line-info-list)
;;           (let* ((current-line-pos (car current-line-info))
;;                  (current-line-is-blank (cdr current-line-info)))

;;             ;; Condition for deletion: current line is blank AND the line two steps back existed
;;             ;; and was also blank.
;;             (when (and current-line-is-blank
;;                        line-minus-2-info             ; Check if line two steps back actually exists.
;;                        (cdr line-minus-2-info))      ; Check if that line was blank.
;;               ;; If condition met, mark current line for deletion.
;;               ;; `push` adds positions to the front, so the list will be in reverse order
;;               ;; of appearance (later lines appear first in the list), which is ideal for deletion.
;;               (push current-line-pos lines-to-delete-pos)))

;;           ;; Update history for the next iteration:
;;           ;; Old 'minus-1' becomes new 'minus-2'.
;;           ;; Current line becomes new 'minus-1'.
;;           (setq line-minus-2-info line-minus-1-info)
;;           (setq line-minus-1-info current-line-info))))

;;     ;; Phase 3: Delete the identified lines.
;;     ;; Iterate through `lines-to-delete-pos`. Since it's already ordered from
;;     ;; the largest position to the smallest, deletions occur safely from the
;;     ;; end of the region towards the beginning, preventing position invalidation.
;;     (save-excursion
;;       (dolist (pos lines-to-delete-pos)
;;         ;; Robustness check: Ensure the position is still within the original bounds.
;;         (when (and (>= pos original-start) (< pos original-end))
;;           (goto-char pos) ; Move point to the start of the line to be deleted.
;;           ;; Delete the current line, including its trailing newline.
;;           (delete-region (point) (progn (forward-line 1) (point))))))

;;     (message "Removed empty lines based on condition.")))
;;; provide
(provide 'oai-optional)
;;; oai-optional.el ends here
