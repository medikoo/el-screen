;; my-screen.el --- Screen manager for Emacs

;; Author:	Mariusz Nowak <mariusz+emacs.my-screen@medikoo.com>
;; Copyright (C) 2010 Mariusz Nowak <mariusz+emacs.my-screen@medikoo.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.	 See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; See README.

(require 'my/directory nil t)
(require 'my/frame nil t)
(require 'my/file nil t)
(require 'my/key nil t)
(require 'my/list nil t)

(defgroup my-screen nil "my-screen -- Screen manager for Emacs")

(defcustom my-screen-default-buffer-name "*scratch*"
	"*Default buffer name. It's loaded in new screen or in windows which have
	problems getting back their buffers."
	:tag "Name of Default Buffer"
	:type '(string :size 24)
	:group 'my-screen)

(defcustom my-screen-dir (concat (getenv "HOME") "/.emacs.d/.my/")
	"*Path at which configurations are saved."
	:tag "Configuration files path"
	:type '(string :size 2048)
	:group 'my-screen)

(defvar my-screen-map (make-sparse-keymap)
	"Keymap for my-screen")
(define-key my-screen-map "s" 'my-screen-switch)
(define-key my-screen-map "r" 'my-screen-rename)
(define-key my-screen-map "c" 'my-screen-unload)
(define-key my-screen-map "n" 'my-screen-print-current)

(defcustom my-screen-prefix-key "\C-z"
	"*Prefix key for my-screen commands."
	:tag "Prefix key of my-screen"
	:type '(string :size 10)
	:set (lambda (symbol value)
		(if (boundp 'my-screen-prefix-key)
			(my-screen-set-prefix-key value)
			(my-key-set value my-screen-map))
		(custom-set-default symbol value))
	:group 'my-screen)

(defvar my-screen-dir
	(concat (getenv "HOME") "/.emacs.d/.my/")
	"Directory to save screens in.")

(defvar my-screen-file-extension
	".myscreen"
	"File extension for screen configurations.")

(defvar my-screen-current
	nil
	"Name of current screen configuration.")

(defvar my-screen-init-hook nil
	"Hook that gets run on my-screen initialization.")

(defvar my-screen-load-hook nil
	"Hook that gets run when previously created screen is loaded.")

(defvar my-screen-new-hook nil
	"Hook that gets run when new screen is created.")

(defvar my-screen-save-hook nil
	"Hook that gets run when screen is saved.")

(defun my-screen-set-prefix-key (key)
	"Set KEY as prefix for my-screen bindings."
	(my-key-replace my-screen-prefix-key key my-screen-map)
	(setq my-screen-prefix-key key))

(defun my-screen-set-name (name)
	"Set NAME for current screen."
	(if my-screen-current
		(unless name
			(remove-hook 'auto-save-hook 'my-screen-save)
			(remove-hook 'kill-emacs-hook 'my-screen-save))
		(when name
			(add-hook 'auto-save-hook 'my-screen-save)
			(add-hook 'kill-emacs-hook 'my-screen-save)))
	(setq my-screen-current name))

(defun my-screen-save ()
	"Save current screen."
	(my-file-write
		(concat my-screen-dir
			my-screen-current my-screen-file-extension)
		(prin1-to-string (my-frame-serialize)))
	(run-hooks 'my-screen-save-hook))

(defun my-screen-save-as (name)
	"Save current screen under NAME."
	(my-screen-set-name name)
	(my-screen-save))

(defun my-screen-new (name)
	"Prepare new (blank) screen and save under NAME."
	(delete-other-windows)
	(switch-to-buffer my-screen-default-buffer-name)
	(run-hooks 'my-screen-new-hook)
	(my-screen-save-as name))

(defun my-screen-load (name)
	"Load NAME screen."
	(if name
		(let ((data (my-file-read (concat my-screen-dir
							name my-screen-file-extension) t)))
			(if data
				(progn
					(my-frame-unserialize (read data))
					(my-screen-set-name name)
					(run-hooks 'my-screen-load-hook))
				(my-screen-new name))
			(message "Screen loaded: %S" name))
		(message "Please provide name for a screen")))

(defun my-screen-list ()
	"Return list of screens sorted by modification date."
	(mapcar (lambda (file) (substring file 0
				(string-match my-screen-file-extension file)))
		(my-directory-files-sorted my-screen-dir
			'my-file-modification-date-sort nil
			(concat "\\" my-screen-file-extension "$"))))

(defun my-screen-switch ()
	"Switch to chosen screen."
	(interactive)
	(if my-screen-current
		(my-screen-save))
	(let ((list (my-screen-list)))
		(if my-screen-current
			(setq list (my-list-move-first-to-end list)))
		(my-screen-load (ido-completing-read "Name: " list))))

(defun my-screen-unload ()
	"Unloads screen.
	Preserves frame display, goes back to inital unloaded state."
	(interactive)
	(when my-screen-current
		(my-screen-save)
		(my-screen-set-name nil))
	(message "Screen unloaded"))

(defun my-screen-rename (name)
	"Rename current screen to NAME."
	(interactive "sNew name: ")
	(rename-file
		(concat my-screen-dir my-screen-current my-screen-file-extension)
		(concat my-screen-dir name my-screen-file-extension))
	(my-screen-set-name name))

;;;###autoload
(defun my-screen-init ()
	"Initialize."
	(run-hooks 'my-screen-init-hook))

(defun my-screen-print-current ()
	"Print name of current screen."
	(interactive)
	(message (if my-screen-current
			my-screen-current
			"No screen loaded")))

(provide 'my-screen/my-screen)

(add-hook 'my-screen-new-hook 'my-frame-reasonable-split)