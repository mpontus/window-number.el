;;; window-number.el --- Select windows by numbers

;; Copyright (C) 2004 Johann "Myrkraverk" Oskarsson
;; <myrkraverk@users.sourceforge.net>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; Introduction
;; ============

;; This is a fork of original window-number developed by Johann
;; "Myrkraverk" Oskarsson <myrkraverk@users.sourceforge.net> in 2004.
;; This version contains semantic function changes, defaults to
;; controling windows with meta keys and contains various new commands
;; such as `window-number-swap' and `window-number-shuffle'.

;; ----------------------------------------------------------------------------

;; Code starts here.

;; ----------------------------------------------------------------------------

(require 'cl); for set-difference and loop

(defun window-number-list ()
  "Returns a list of the windows, in fixed order and the
minibuffer (even if not active) last."
  (let* ((walk-windows-start
          (car (set-difference
                (window-list (selected-frame) t)
                (window-list (selected-frame) 1))))
         (walk-windows-current walk-windows-start)
         list)
    (while (progn
             (setq walk-windows-current
                   (next-window walk-windows-current t))
             (setq list (cons walk-windows-current list))
             (not (eq walk-windows-current walk-windows-start))))
    (reverse (cons (car list) (cdr list)))))

(defun window-number-get-number (&optional window)
  (catch 'return
    (let ((curr (or window (selected-window)))
          (list (window-number-list)))
      (dolist (i (number-sequence 1 (length list)))
        (when (eq curr (pop list))
          (throw 'return i))))))

(defun window-number-get-state (window)
  (list (window-buffer window)
        (window-hscroll window)
        (window-point window)
        (window-start window)))

(defun window-number-set-state (window state)
  (set-window-buffer window (pop state))
  (set-window-hscroll window (pop state))
  (set-window-point window (pop state))
  (set-window-start window (pop state)))

(defun window-number-rotate (&rest windows)
  "Rotate the states between WINDOWS.

\(nth 1 windows) assumes the state of (nth 0 windows)
\(nth 2 windows) assumes the state of (nth 1 windows)
and so forth."
  (let ((tmp (window-number-get-state (car windows))))
    (while (cdr windows)
     (window-number-set-state (pop windows)
      (window-number-get-state (car windows))))
    (window-number-set-state (car windows) tmp)))

(defun window-number-arg ()
  "Returns the number from key sequence used to invoke this command."
  (let* ((key (event-basic-type last-input-event)))
    (while (not (and (<= ?1 key) (>= ?9 key)))
      (setq key (read-char "Window Number (1-9): ")))
    (- key ?0)))

(defun window-number-arg-window ()
  "Retruns the window corresponding to number from key sequence."
  (nth (1- (window-number-arg)) (window-number-list)))

;;;###autoload
(defun window-number-select (window)
  "Selects the nth window."
  (interactive (list (window-number-arg-window)))
  (select-window window))

;;;###autoload
(defun window-number-swap (window)
  "Swap contents of selected window and WINDOW."
  (interactive (list (window-number-arg-window)))
  (window-number-rotate window (selected-window)))

;;;###autoload
(defun window-number-shuffle (window)
  "Set WINDOW conectents to selected window ones and change
selected window buffer to next one"
  (interactive (list (window-number-arg-window)))
  (let ((next-buffer
         (save-window-excursion
           (bury-buffer)
           (current-buffer))))
    (window-number-rotate window  (selected-window))
    (switch-to-buffer next-buffer)))

(defun window-number-get-number (&optional window)
  "Returns the the number of the current window."
  (length
   (memq (or window (selected-window))
         (nreverse (window-number-list)))))

(defun window-number-string ()
  "Returns the string containing the number of the current window"
  (propertize
   (format " -%d-" (window-number-get-number))
   'face 'window-number-face))

(defun window-number-define-keys (keymap beg end def)
  "Defines key sequences BEG .. END as DEF."
  (unless (equal (butlast (listify-key-sequence beg))
		 (butlast (listify-key-sequence end)))
    (error "Different prefix for key sequences %S and %S."
	   (key-description beg) (key-description end)))
  (let ((prefix (butlast (listify-key-sequence beg)))
	(last-events (cons (last (listify-key-sequence beg))
			   (last (listify-key-sequence end)))))
    (mapcar (lambda (last-event)
	      (define-key keymap (vconcat prefix (list last-event)) def))
	    (number-sequence (caar last-events) (cadr last-events)))))

;;;###autoload
(define-minor-mode window-number-mode
  "Global minor mode that enables use of M- keys to control windows."
  :keymap
  (let ((keymap (make-sparse-keymap)))
    (window-number-define-keys keymap
      (kbd "M-1") (kbd "M-9") 'window-number-select)
    (window-number-define-keys keymap
      (kbd "C-x C-1") (kbd "C-x C-9") 'window-number-swap)
    (window-number-define-keys keymap
      (kbd "C-x M-1") (kbd "C-x M-9") 'window-number-shuffle)
    keymap)
  :init-value nil
  :lighter (:eval (window-number-string))
  :global t)

(defface window-number-face
  '((((type tty) (class color))
     (:background "red"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((type x w32 mac))
     (:foreground "red")))
  "The face used for the window number in the mode-line.")

(provide 'window-number)

;;; window-number.el ends here.
