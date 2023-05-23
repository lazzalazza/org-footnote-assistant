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
;; footnote references and their definitions, edit the definitions in a separate
;; buffer, and delete footnotes.

;; The 'org-footnote-assistant--show-definition' function narrows the buffer to
;; the region of the current footnote definition, if the point is currently at a
;; footnote reference. The 'org-footnote-assistant--create-editor-window'
;; function creates a new buffer or selects an existing buffer named
;; "*footnote-editor*", narrows it to the footnote definition region, and
;; switches to it. The 'org-footnote-assistant-goto-next-footnote' function
;; finds the next or previous footnote reference and opens the narrowed buffer.
;; The 'org-footnote-assistant--goto-definition' function moves the point to the
;; definition of the specified footnote label.
;; 'org-footnote-assistant-delete-footnote' prompts for confirmation before
;; deleting the footnote. However, you can customize this behavior by setting
;; the variable 'org-footnote-assistant-ask-before-delete' to nil.

;;; Code:

;; Dependencies
(require 'org)


;;; Constants and customization

(defconst org-footnote-reference-re "[^\n]\\[fn:\\(?:\\(?1:[-_[:word:]]+\\)?\\(:\\)\\|\\(?1:[-_[:word:]]+\\)\\]\\)")

(defcustom org-footnote-assistant-ask-before-delete t
  "Whether to ask for permission before deleting a footnote."
  :type 'boolean
  :group 'org-footnote-assistant-mode) ;; the org-footnote-assistant-mode is
                                       ;; automatically set by the
                                       ;; define-minor-mode command


;;; Getters

;; TODO This getter might be more useful: see if it can be used elsewhere or
;; rewrite the 'org-footnote-assistant-delete-footnote' function

(defun org-footnote-assistant--get-label ()
  "Return the footnote reference number at point.
   If the cursor is inside a footnote reference, return the reference number.
   If the cursor is inside a footnote definition, return the number of the definition.
   If the cursor is not inside a footnote, return nil."
  (cond
   ;; Check if cursor is inside a footnote reference
   ((org-footnote-at-reference-p)
    (car (org-footnote-at-reference-p)))
   ;; Check if cursor is inside a footnote definition
   ((org-footnote-at-definition-p)
    (car (org-footnote-at-definition-p)))
   ;; Cursor is not inside a footnote
   (t nil)))


;;; Footnote editor / base buffer management functions

(defun org-footnote-assistant--current-buffer-related-to-editor-p ()
  "Check if the current buffer is the indirect buffer
'*footnote-editor*' or is related to it."
  (let* ((cur-buf (current-buffer))
         (edit-buf (get-buffer "*footnote-editor*")))
  (with-current-buffer edit-buf
    (if (or (eq cur-buf edit-buf) (eq (buffer-base-buffer) cur-buf))
        t
      nil))))

(defun org-footnote-assistant--delete-editor-window ()
  "Kill the buffer if it already exists."
  (when (get-buffer "*footnote-editor*")
    (kill-buffer "*footnote-editor*"))
  )

(defun org-footnote-assistant--show-definition ()
  "Narrows the buffer to the region of the current footnote
definition, if the point is currently at a footnote reference."
  (interactive)
  (when (org-footnote-at-reference-p)
    ;; Get the label of the current footnote.
    (let* ((label (org-element-property :label (org-element-context)))
           ;; Get the beginning and end of the footnote definition region.
           (definition-begin (nth 1 (org-footnote-get-definition label)))
           (definition-end (nth 2 (org-footnote-get-definition label))))
      (org-footnote-assistant--create-editor-window definition-begin definition-end)
      )))

(defun org-footnote-assistant--create-editor-window (begin end)
  (let ((buf (get-buffer "*footnote-editor*")))
    (if buf
        (if (org-footnote-assistant--current-buffer-related-to-editor-p)
            ;; If the buffer exists and is related to the current buffer,
            ;; narrow to the footnote definition region and display the buffer.
            (with-current-buffer buf
              (narrow-to-region begin end)
              (pop-to-buffer buf t)
              (beginning-of-buffer))
          ;; If the buffer exists but is not related to the current buffer,
          ;; kill it and create a new indirect buffer.
          (progn
            (kill-buffer buf)
            (setq buf nil))))
    ;; If the buffer does not exist, create a new indirect buffer.
    (unless buf
      (setq buf (switch-to-buffer-other-window (clone-indirect-buffer "*footnote-editor*" nil))))

    ;; Narrow to the footnote definition region in the buffer.
    (with-current-buffer buf
      (narrow-to-region begin end))))


;;; Scrolling functions

;;;###autoload
(defun org-footnote-assistant-goto-next-footnote (&optional backward)
  "Finds the next footnote and opens the narrowed buffer. If
BACKWARD is non-nil, it finds the previous reference."
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

;;;###autoload
(defun org-footnote-assistant-goto-previous-footnote ()
  "Searches previous footnote reference. A wrapper
for (org-footnote-assistant-goto-next-footnote t)"
  (interactive)
  (org-footnote-assistant-goto-next-footnote t)
  )


;; Footnote editing/deleting functions

;;;###autoload
(defun org-footnote-assistant-delete-footnote ()
  "Deletes footnote reference and definition. Wrapper for org-footnote-delete."
  (interactive)
  (let* ((label (org-footnote-assistant--get-label)))
    (if (or (not org-footnote-assistant-ask-before-delete)
            (y-or-n-p (concat "Really delete footnote " label "? ")))
        (if (buffer-base-buffer)
            (with-current-buffer (buffer-base-buffer)
              (org-footnote-delete label)
              )
          (org-footnote-delete label)
          )
      )))


;;; Advised functions: they modify the behavior of org-footnote.el functions

(defun org-footnote-assistant--goto-definition (label &optional location)
  "Modified version of 'org-footnote-goto-definition' that integrates
'org-footnote-assistant--create-editor-window' Move point to the
definition of the footnote LABEL.

LOCATION, when non-nil specifies the buffer position of the
definition.

Throw an error if there is no definition or if it cannot b
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
    (org-footnote-assistant--create-editor-window def-start def-end)
    (looking-at (format "\\[fn:%s[]:]" (regexp-quote label)))
    (goto-char (match-end 0))
    (org-fold-show-context 'link-search)
    (when (derived-mode-p 'org-mode)
      (message "%s" (substitute-command-keys
		     "Edit definition and go back with \
`\\[org-mark-ring-goto]' or, if unique, with `\\[org-ctrl-c-ctrl-c]'.")))
    t))

;;;###autoload
(defun org-goto-previous-reference-advice (orig-fun &rest args)
  (if (eq (current-buffer) (get-buffer "*footnote-editor*"))
      (let ((base-buffer (buffer-base-buffer)))
        (if (get-buffer-window base-buffer)
            (select-window (get-buffer-window base-buffer))
          (switch-to-buffer-other-window base-buffer))))
  (apply orig-fun args))

;;; Advice management

(defun org-footnote-assistant--add-advices ()
  (advice-add 'org-footnote-goto-definition :override #'org-footnote-assistant--goto-definition)
  (advice-add 'org-footnote-goto-previous-reference :around #'org-goto-previous-reference-advice)
  )

(defun org-footnote-assistant--remove-advices ()
  (advice-remove 'org-footnote-goto-definition 'org-footnote-assistant--goto-definition)
  (advice-remove 'org-footnote-goto-previous-reference 'org-goto-previous-reference-advice)
 )


;;; Minor mode

(defvar org-footnote-assistant-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-n") 'org-footnote-assistant-goto-next-footnote)
    (define-key map (kbd "C-c C-p") 'org-footnote-assistant-goto-previous-footnote)
    map))

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



