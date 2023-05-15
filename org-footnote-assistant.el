;;; org-footnote-assistant.el --- summary -*- lexical-binding: t -*-

;; Author: Andrea Lazzarini
;; Maintainer: Andrea Lazzarini
;; Version: 0.01
;; Package-Requires: ((org "8.0.0"))
;; Homepage: https://github.com/lazzalazza/org-footnote-assistant/
;; Keywords: org-footnote, org-mode

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; This package provides additional functionality to Emacs Org mode for handling
;; footnotes. It defines functions that allow the user to navigate between
;; footnote references and their definitions, and to edit the definitions in a
;; separate buffer.

;; The 'org-footnote-assistant--show-definition' function narrows the buffer to
;; the region of the current footnote definition, if the point is currently at a
;; footnote reference. The 'org-footnote-assistant--create-editor-window'
;; function creates a new buffer or selects an existing buffer named
;; "*footnote-editor*", narrows it to the footnote definition region, and
;; switches to it. The 'org-footnote-assistant--goto-next-footnote' function
;; finds the next or previous footnote reference and opens the narrowed buffer.
;; The 'org-footnote-new-advice' function is an advice function that adds the
;; ability to automatically jump to the definition of a newly-created footnote
;; after it has been inserted. Finally, the
;; 'org-footnote-assistant--goto-definition' function moves the point to the
;; definition of the specified footnote label.

;;; Code:

(defconst org-footnote-reference-re "[^\n]\\[fn:\\(?:\\(?1:[-_[:word:]]+\\)?\\(:\\)\\|\\(?1:[-_[:word:]]+\\)\\]\\)")


(defun org-footnote-assistant--delete-editor-window ()
  "Kill the buffer if it already exists."
  (when (get-buffer "*footnote-editor*")
    (kill-buffer "*footnote-editor*"))
  )

(defun org-footnote-assistant--show-definition ()
  "Narrows the buffer to the region of the current footnote definition, if the point is currently at a footnote reference."
  (interactive)
  (when (org-footnote-at-reference-p)
    ;; Get the label of the current footnote.
    (let* ((label (org-element-property :label (org-element-context)))
           ;; Get the beginning and end of the footnote definition region.
           (definition-begin (nth 1 (org-footnote-get-definition label)))
           (definition-end (nth 2 (org-footnote-get-definition label))))
      (message "%d %d" definition-begin  definition-end)
      (org-footnote-assistant--create-editor-window definition-begin definition-end)
      )))

(defun org-footnote-assistant--create-editor-window (begin end)
  ;; First, we get the buffer named "*footnote-editor*" if it exists.
  (let ((buf (get-buffer "*footnote-editor*")))
    ;; If the buffer exists, we select it and narrow to the footnote definition region.
    (if buf
        (with-current-buffer buf
          ;; Then, we narrow to the footnote definition region.
          (narrow-to-region begin end)
          ;; Finally, we display the buffer.
          (pop-to-buffer (current-buffer) t)
          (beginning-of-buffer)
          )
      ;; If the buffer does not exist, we create a new indirect buffer and switch to it.
      (progn
        (switch-to-buffer-other-window (clone-indirect-buffer "*footnote-editor*" nil))
        ;; Narrow to the footnote definition region.
        (narrow-to-region begin end)
        (beginning-of-buffer)
        ))))

(defun org-footnote-assistant--goto-next-footnote (&optional backward)
  "Finds the next footnote and opens the narrowed buffer. If BACKWARD is non-nil, it finds the previous reference."
  (interactive)

  ;; If we're in the *footnote-editor* buffer...
  (if (string-equal (buffer-name) "*footnote-editor*")
      ;; If the base buffer is already open in another window, go to that window
      (if (get-buffer-window (buffer-name (buffer-base-buffer)))
          (select-window (get-buffer-window (buffer-name (buffer-base-buffer))))
        ;; Otherwise, switch to the base buffer
        (switch-to-buffer (buffer-base-buffer))))

  ;; If we're not in the *footnote-editor* buffer...
  (if (not backward)
      ;; If we're going forward, search for the next footnote reference and
      ;; narrow to its definition
      (progn
        (when (re-search-forward org-footnote-reference-re nil t)
          (backward-char)
          (org-footnote-assistant--show-definition)))
    ;; If we're going backward, search for the previous footnote reference and
    ;; narrow to its definition
    (progn
      (when (re-search-backward org-footnote-reference-re nil t)
        (forward-char)
        (org-footnote-assistant--show-definition)))))

(defun org-footnote-assistant--goto-previous-footnote ()
  "Searches previous footnote reference. A wrapper
for (org-footnote-assistant--goto-next-footnote t)"
  (interactive)
  (org-footnote-assistant--goto-next-footnote t)
  )

(defun org-footnote-new-advice (orig-fun &rest args)
  (save-excursion
  (apply orig-fun args))
  (org-footnote-assistant--show-definition)
  (search-forward org-footnote-reference-re nil t)
  (goto-char (match-end 0))
  )

(defun org-footnote-assistant--goto-definition (label &optional location)
  "Modified version of org-footnote-goto-definition that integrates
'org-footnote-assistant--create-editor-window' Move point to the
definition of the footnote LABEL.

LOCATION, when non-nil specifies the buffer position of the
definition.

Throw an error if there is no definition or if it cannot be
reached from current narrowed part of buffer.  Return a non-nil
value if point was successfully moved."
  (interactive "sLabel: ")
  (let* ((label (org-footnote-normalize-label label))
	       (def-start (or location (nth 1 (org-footnote-get-definition label))))
         (def-end (nth 2 (org-footnote-get-definition label))))
    (cond
     ((not def-start)
      (user-error "Cannot find definition of footnote %s" label))
     ((or (> def-start (point-max)) (< def-start (point-min)))
      (user-error "Definition is outside narrowed part of buffer")))
    ;; (org-mark-ring-push)
    (print (concat (number-to-string def-start) " " (number-to-string def-end)))
    (org-footnote-assistant--create-editor-window def-start def-end)
    (looking-at (format "\\[fn:%s[]:]" (regexp-quote label)))
    (goto-char (match-end 0))
    (org-fold-show-context 'link-search)
    (when (derived-mode-p 'org-mode)
      (message "%s" (substitute-command-keys
		     "Edit definition and go back with \
`\\[org-mark-ring-goto]' or, if unique, with `\\[org-ctrl-c-ctrl-c]'.")))
    t))


(defun org-footnote-assistant--add-advices ()
  (advice-add 'org-footnote-new :around #'org-footnote-new-advice)
  (advice-add 'org-footnote-goto-definition :override #'org-footnote-assistant--goto-definition)
  )

(defun org-footnote-assistant--remove-advices ()
  (advice-remove 'org-footnote-new 'org-footnote-new-advice)
  (advice-remove 'org-footnote-goto-definition 'org-footnote-assistant--goto-definition)
  )

;;; Minor mode

(define-minor-mode org-footnote-assistant-mode
  "Toggles org-footnote-assistant-mode"
  :init-value nil
  :global t
  :group 'org
  :lighter " ofa"

  (if org-footnote-assistant-mode
      (progn
        (message "org-footnote-assistant mode activated!")
        (org-footnote-assistant--add-advices)
        )
    (progn
      (org-footnote-assistant--remove-advices)
      (org-footnote-assistant--delete-editor-window)
      (message "org-footnote-assistant-mode deactivated!"))))

(provide 'org-footnote-assistant)

;;; org-footnote-assistant.el ends here



