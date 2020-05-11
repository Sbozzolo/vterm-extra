;;; vterm-extra.el --- Convenience functions for vterm -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Gabriele Bozzola

;; Author: Gabriele Bozzola <sbozzolator@gmail.com>
;; URL: https://github.com/sbozzolo/vterm-extra.git
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (vterm "0.0.1"))
;; Keywords: convenience
;; Prefix: vterm-extra

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://WWW.gnu.org/licenses/>.

;;; Commentary:

;; vterm-extra provides convenience functions for improving the user experience
;; with vterm.

;; Functions:

;; vterm-extra-dispatcher: Switch to a VTerm buffer or create a new one.

;; If there are no VTerm buffers, it creates a new one.
;; If the current buffer is the only VTerm buffer, it creates a new one.
;; If the current buffer is a VTerm buffer, and there are other,
;; it prompts the user for a new one to select, and switches to that buffer.
;; With prefix argument, it always creates a new VTerm.

;; vterm-extra-edit-command-in-new-buffer

;; This function allows to edit commands in a separate temporary buffer.
;; It kills the current command, create a temporary buffer.  When
;; `vterm-extra-edit-done' is called (bound to `C-c C-c` by default), the
;; content of the buffer is sent to the associated vterm.  This is done
;; line by line, so the file has to be a valid multi-line command or series of
;; commands.
;; Current limitation: partially input commands that span multiple lines are
;; not correctly transferred over the temporary buffer.

;;; Code:

(require 'vterm)

(defun vterm-extra--is-vterm-buffer-p (buffer)
  "Return t if buffer BUFFER is a VTerm buffer."
  (eq (buffer-local-value 'major-mode buffer) 'vterm-mode))

(defun vterm-extra--get-vterm-buffers ()
  "List all the (other) VTerm buffers.

List all the VTerm buffers, if the current one is a VTerm buffer,
exclude it."
  (seq-filter #'vterm-extra--is-vterm-buffer-p
              (delq (current-buffer) (buffer-list))))

(defun vterm-extra--create-or-switch (vterm-buffer-names)
  "Ask the user to pick one in VTERM-BUFFER-NAMES or to create New VTerm."
  (let ((user-choice (completing-read "Select VTerm: "
                                      (cons "New VTerm" vterm-buffer-names)
                                      nil t)))
    (if (string= user-choice "New VTerm")
        (vterm)
      (pop-to-buffer user-choice))))

;; This function was inspired by Mike Zamansky's eshell-switcher
;; https://cestlaz.github.io/post/using-emacs-66-eshell-elisp/
;;;###autoload
(defun vterm-extra-dispatcher (&optional arg)
  "Switch to a VTerm buffer or create a new one.

If there are no VTerm buffers, create a new one.
If the current buffer is the only VTerm buffer, create a new one.
If the current buffer is a VTerm buffer, and there are other,
prompt the user for a new one to select, and switch to that
buffer.

With prefix argument ARG, always create a new VTerm."
  (interactive "P")
  ;; If called with prefix, just create a new VTerm
  (if arg (vterm)
    ;; When there are no VTerm buffers and when the current one is the only one,
    ;; we want to create a new one. We eliminate from the list of all the buffers
    ;; the current one, if there are zero VTerm buffers it means that we have to
    ;; create a new one (this happens if there are no VTerm buffers or if the
    ;; current one is the only one).
    (let* ((vterm-buffers (vterm-extra--get-vterm-buffers))
           (vterm-buffer-names (mapcar #'buffer-name vterm-buffers))
           (num-vterm-buffers (length vterm-buffers)))
      (if (eq num-vterm-buffers 0)
          (vterm)
        (vterm-extra--create-or-switch vterm-buffer-names)))))

(defvar vterm-extra-edit-associated-buffer nil)

(defun vterm-extra-edit-done ()
  "Send command to the associated VTerm.

At the moment, it does not support newlines or prexisting input."
  (interactive)
  (let ((command (buffer-string))
        (associated-terminal vterm-extra-edit-associated-buffer))
    (kill-buffer (current-buffer))
    (pop-to-buffer associated-terminal)
    (vterm-send-string command)))

(defun vterm-extra-edit-done-delete-window ()
  "Send command to the associated VTerm, then delete current buffer.

At the moment, it does not support newlines or prexisting input."
  (interactive)
  (let ((command (buffer-string))
        (associated-terminal vterm-extra-edit-associated-buffer))
    (kill-buffer-and-window)
    (pop-to-buffer associated-terminal)
    (vterm-send-string command)))

(defvar vterm-extra-edit-mode-map nil)
(setq vterm-extra-edit-mode-map (make-sparse-keymap))

(define-key vterm-extra-edit-mode-map (kbd "C-c C-c")
  'vterm-extra-edit-done)

(define-derived-mode vterm-extra-edit-mode sh-mode "VTermEdit"
  "Vterm extra edit mode.

Edit vterm commands in a separate buffer.")

;; HACK: This is a hack-ish way to achieve the copy the command.
;; It doesn't work if the command spans more lines and if the user
;; rebinds keys in an unusual way.  It also requires the location
;; of the prompt.  This should be improved

(defun vterm-extra--kill-and-return-current-command ()
  "Return the command in the current line after killing it.

This is used to prepare the populate the buffer to edit commands."
  (interactive)
  (let ((command
         (buffer-substring-no-properties (vterm--get-prompt-point) (vterm--get-end-of-line))))
    (vterm-send-C-a)
    (vterm-send-C-k)
    command))

;;;###autoload
(defun vterm-extra-edit-command-in-new-buffer ()
  "Edit the current VTerm command in a new buffer.

If something is already on the prompt line, kill it and insert it
in the temporary buffer.

The temporary buffer will be sent back to the terminal with
\\<vterm-extra-edit-mode-map>`\\[vterm-extra-edit-done]' line by
line.  A trailing newline corresponds to sending return, hence to
running the command.

The local variable `vterm-extra-edit-associated-buffer' contains
the associated VTerm buffer where the command will be inserted."
  (interactive)
  (let ((original-buffer (get-buffer (current-buffer)))
        (command (vterm-extra--kill-and-return-current-command)))
    (pop-to-buffer
     (get-buffer-create (concat "*" (buffer-name) "*")))
    (vterm-extra-edit-mode)
    (insert command)
    (setq-local vterm-extra-edit-associated-buffer original-buffer)))

(provide 'vterm-extra)

;;; vterm-extra.el ends here
