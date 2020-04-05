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

(provide 'vterm-extra)

;;; vterm-extra.el ends here
