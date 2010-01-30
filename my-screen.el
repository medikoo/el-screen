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

(require 'my/frame)
(require 'my/file)
(require 'my/key)

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
	""
	"Name of current screen configuration.")

(defvar my-screen-current-name-file
	".myscreen-current"
	"Filname of file that holds current screen configuration name.")

(defun my-screen-set-prefix-key (key)
	"Set KEY as prefix for my-screen bindings."
	(my-key-replace my-screen-prefix-key key my-screen-map)
	(setq my-screen-prefix-key key))

(defun my-screen-set-name (name)
	"Set NAME for current screen."
	(my-file-write
		(concat my-screen-dir my-screen-current-name-file) name)
	(setq my-screen-current name))

(defun my-screen-save ()
	"Save current screen."
	(my-file-write
		(concat my-screen-dir
			my-screen-current my-screen-file-extension)
		(prin1-to-string (my-frame-serialize)))
	(run-hooks 'my-screen-save-hook)
	(message "Screen saved: %S" my-screen-current))

(defun my-screen-save-as (name)
	"Save current screen under NAME."
	(my-screen-set-name name)
	(my-screen-save))

(defun my-screen-new (name)
	"Prepare new (blank) screen and save as NAME."
	(delete-other-windows)
	(set-window-buffer (selected-window) my-screen-default-buffer-name)
	(run-hooks 'my-screen-new-hook)
	(my-screen-save-as name))

(defun my-screen-load (name)
	"Load NAME screen."
	(let ((data (my-file-read (concat my-screen-dir
						name my-screen-file-extension) t)))
		(if data
			(progn
				(my-frame-unserialize (read data))
				(my-screen-set-name name)
				(run-hooks 'my-screen-load-hook))
			(my-screen-new name))
		(message "Screen loaded: %S" name)))

(defun my-screen-switch (name)
	"Switch to NAME screen"
	(interactive "sName: ")
	(my-screen-save)
	(my-screen-load name))

(defun my-screen-rename (name)
	"Rename current screen to NAME."
	(interactive "sNew name: ")
	(rename-file
		(concat my-screen-dir my-screen-current my-screen-file-extension)
		(concat my-screen-dir name my-screen-file-extension))
	(my-screen-set-name name))

(defun my-screen-init ()
	"Initialize. Load previously loaded screen. Add save hooks."
	(run-hooks 'my-screen-init-hook)
	(my-screen-load (or
			(my-file-read (concat my-screen-dir my-screen-current-name-file) t)
			my-screen-current))
	(add-hook 'auto-save-hook 'my-screen-save)
	(add-hook 'kill-emacs-hook 'my-screen-save))

(defun my-screen-print-current ()
	"Print name of current screen."
	(interactive)
	(message my-screen-current))

(provide 'my-screen/my-screen)

(add-hook 'my-screen-new-hook 'my-frame-reasonable-split)
(my-screen-init)
