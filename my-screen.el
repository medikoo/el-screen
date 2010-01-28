;; my-screen.el --- switchable screen configurations

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
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

(require 'my/frame)
(require 'my/file)

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

(defun my-screen-set-name (name)
	(my-file-write
		(concat my-screen-dir my-screen-current-name-file) name)
	(setq my-screen-current name))

(defun my-screen-save ()
	(my-file-write
		(concat my-screen-dir
			my-screen-current my-screen-file-extension)
		(prin1-to-string (my-frame-serialize)))
	(message "Screen saved: %S" my-screen-current))

(defun my-screen-save-as (name)
	(my-screen-set-name name)
	(my-screen-save))

(defun my-screen-new (name)
	(delete-other-windows)
	(set-window-buffer (selected-window) "*scratch*")
	(my-screen-new-configure)
	(my-screen-save-as name))

(defun my-screen-new-configure ())

(defun my-screen-load (name)
	(let ((data (my-file-read (concat my-screen-dir
						name my-screen-file-extension) t)))
		(if data
			(progn
				(my-frame-unserialize (read data))
				(my-screen-set-name name))
			(my-screen-new name))
		(message "Screen loaded: %S" name)))

(defun my-screen-switch (name)
	(interactive "sName: ")
	(my-screen-save)
	(my-screen-load name))

(defun my-screen-init ()
	(my-screen-load (or
			(my-file-read (concat my-screen-dir my-screen-current-name-file) t)
			my-screen-current))
	(add-hook 'auto-save-hook 'my-screen-save)
	(add-hook 'kill-emacs-hook 'my-screen-save))

(defun my-screen-print-current ()
	(interactive)
	(message my-screen-current))

(provide 'my-screen/my-screen)

(my-screen-init)
