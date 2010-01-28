;; my-tasks.el --- switchable screen configurations

;; Author:	Mariusz Nowak <mariusz+emacs.my-tasks@medikoo.com>
;; Copyright (C) 2010 Mariusz Nowak <mariusz+emacs.my-tasks@medikoo.com>

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

(defvar my-tasks-dir
	(concat (getenv "HOME") "/.emacs.d/.my/")
	"Directory to save tasks in.")

(defvar my-tasks-file-extension
	".mytask"
	"File extension for frame bookmarks.")

(defvar my-tasks-current
	""
	"Name of current frame configuration.")

(defvar my-tasks-current-name-file
	".mytask-current"
	"Filname of file that holds current frame configuration name.")

(defun my-tasks-set-name (name)
	(my-file-write
		(concat my-tasks-dir my-tasks-current-name-file) name)
	(setq my-tasks-current name))

(defun my-tasks-save ()
	(my-file-write
		(concat my-tasks-dir
			my-tasks-current my-tasks-file-extension)
		(prin1-to-string (my-frame-serialize)))
	(message "Task saved: %S" my-tasks-current))

(defun my-tasks-save-as (name)
	(my-tasks-set-name name)
	(my-tasks-save))

(defun my-tasks-new (name)
	(delete-other-windows)
	(set-window-buffer (selected-window) "*scratch*")
	(my-tasks-new-configure)
	(my-tasks-save-as name))

(defun my-tasks-new-configure ())

(defun my-tasks-load (name)
	(let ((data (my-file-read (concat my-tasks-dir
						name my-tasks-file-extension) t)))
		(if data
			(progn
				(my-frame-unserialize (read data))
				(my-tasks-set-name name))
			(my-tasks-new name))
		(message "Task loaded: %S" name)))

(defun my-tasks-switch (name)
	(interactive "sName: ")
	(my-tasks-save)
	(my-tasks-load name))

(defun my-tasks-init ()
	(my-tasks-load (or
			(my-file-read (concat my-tasks-dir my-tasks-current-name-file) t)
			my-tasks-current))
	(add-hook 'auto-save-hook 'my-tasks-save)
	(add-hook 'kill-emacs-hook 'my-tasks-save))

(defun my-tasks-print-current ()
	(interactive)
	(message my-tasks-current))

(provide 'my-tasks/my-tasks)

(my-tasks-init)
