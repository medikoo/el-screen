;; my-screen.el --- Window configurations manager for Emacs

;; Author:	Mariusz Nowak <mariusz+emacs.my-screen@medikoo.com>
;; Copyright (C) 2010, 2011 Mariusz Nowak <mariusz+emacs.my-screen@medikoo.com>

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

(defgroup my-screen nil "my-screen -- Window configurations manager for Emacs")

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

(defvar my-screen-key-map (make-sparse-keymap)
	"Keymap for my-screen")
(define-key my-screen-key-map "s" 'my-screen-switch)
(define-key my-screen-key-map "o" 'my-screen-open)
(define-key my-screen-key-map "r" 'my-screen-rename)
(define-key my-screen-key-map "d" 'my-screen-delete)
(define-key my-screen-key-map "c" 'my-screen-unload)
(define-key my-screen-key-map "n" 'my-screen-print-current)
(define-key my-screen-key-map "w" 'my-screen-wipe-interactive)
(define-key my-screen-key-map "h" 'my-screen-help)

(defcustom my-screen-prefix-key "\C-z"
	"*Prefix key for my-screen commands."
	:tag "Prefix key of my-screen"
	:type '(string :size 10)
	:set (lambda (symbol value)
		(if (boundp 'my-screen-prefix-key)
			(my-screen-set-prefix-key value)
			(my-key-set value my-screen-key-map))
		(custom-set-default symbol value))
	:group 'my-screen)

(defvar my-screen-file-extension
	".myscreen"
	"File extension for screen configurations.")

(defvar my-screen-help-file
	(concat (or (file-name-directory
				(or load-file-name (buffer-file-name) "")) "") "HELP")
	"Path to my-screen help file")

(defvar my-screen-frame-map
	()
	"Screen to frame mappings")

(defvar my-screen-wipe-hook nil
	"Hook that gets run when frame is cleared.")

(defun my-screen-set-prefix-key (key)
	"Set KEY as prefix for my-screen bindings."
	(my-key-replace my-screen-prefix-key key my-screen-key-map)
	(setq my-screen-prefix-key key))

(defun my-screen-set (name frame)
	"Assign NAME screen for given FRAME.
	If NAME is nil then unload screen from given FRAME."
	(let ((screen-assoc (rassoc frame my-screen-frame-map)))
		(if screen-assoc
			(if name
				(setcar screen-assoc name)
				(setq my-screen-frame-map
					(rassq-delete-all frame my-screen-frame-map))
				(unless my-screen-frame-map
					(remove-hook 'auto-save-hook 'my-screen-save-all)
					(remove-hook 'kill-emacs-hook 'my-screen-save-all)))
			(when name
				(setq screen-assoc (cons name frame))
				(if my-screen-frame-map
					(nconc my-screen-frame-map (list screen-assoc))
					(setq my-screen-frame-map (list screen-assoc))
					(add-hook 'auto-save-hook 'my-screen-save-all)
					(add-hook 'kill-emacs-hook 'my-screen-save-all))))
		screen-assoc))

(defun my-screen-unset (frame)
	"Unloads screen from current frame"
	(let ((screen-assoc (rassoc frame my-screen-frame-map)))
		(if screen-assoc
			(progn (my-screen-save screen-assoc)
				(my-screen-set nil frame)
				t)
			nil)))

(defun my-screen-get-name (frame)
	"Returns screen name for given FRAME."
	(car (rassoc frame my-screen-frame-map)))

(defun my-screen-get-current ()
	"Returns screen name for selected frame."
	(my-screen-get-name (selected-frame)))

(defun my-screen-save-all ()
	"Save all screens."
	(dolist (screen-assoc my-screen-frame-map)
		(my-screen-save screen-assoc)))

(defun my-screen-save-current ()
	"Save current screen."
	(let ((screen-assoc (rassoc (selected-frame) my-screen-frame-map)))
		(when screen-assoc
			(my-screen-save screen-assoc))))

(defun my-screen-save (screen-assoc)
	"Save given SCREEN-ASSOC."
	(my-file-write
		(concat my-screen-dir
			(symbol-name (car screen-assoc)) my-screen-file-extension)
		(prin1-to-string (my-frame-serialize (cdr screen-assoc)))))

(defun my-screen-wipe ()
	"Clears selected frame."
	(delete-other-windows)
	(switch-to-buffer my-screen-default-buffer-name)
	(run-hooks 'my-screen-wipe-hook))

(defun my-screen-load (name &optional frame)
	"Load NAME screen into FRAME.
	If no FRAME is given then load screen in new frame.
	If NAME screen is already loaded focus screen frame."
	(let ((screen-assoc (assoc name my-screen-frame-map)))
		(if screen-assoc
			(select-frame-set-input-focus (cdr screen-assoc))
			(let ((data (my-file-read (concat my-screen-dir
								(symbol-name name) my-screen-file-extension) t))
					(dowipe (or (my-screen-get-current) (not frame))))
				(unless frame
					(setq frame (make-frame-command)))
				(if data
					(progn (ignore-errors (my-frame-unserialize (read data) frame))
						(my-screen-save (my-screen-set name frame)))
					(if dowipe
						(my-screen-wipe))
					(my-screen-save (my-screen-set name (selected-frame))))
				(message (concat "Screen loaded: " (symbol-name name)))))))

(defun my-screen-list ()
	"Return list of saved screens sorted by modification date."
	(mapcar (lambda (file) (substring file 0
				(string-match my-screen-file-extension file)))
		(my-directory-files-sorted my-screen-dir
			'my-file-modification-date-sort nil
			(concat "\\" my-screen-file-extension "$"))))

(defun my-screen-ido-list ()
	"Return list of saved screens with current screen moved to end of list."
	(let ((screen-list (my-screen-list))
			(current-name (my-screen-get-current)))
		(when current-name
			(setq current-name (symbol-name current-name))
			(setq screen-list (delete current-name screen-list))
			(nconc screen-list (list current-name)))
		screen-list))

(defun my-screen-ido-read-name ()
	"Get screen name input from user."
	(let ((name (ido-completing-read "Name: " (my-screen-ido-list))))
		(if (and name (not (eq name "")))
			(intern name)
			(message "Please provide name for a screen")
			nil)))

(defun my-screen-switch ()
	"Load new or existing screen into current frame."
	(interactive)
	(let ((name (my-screen-ido-read-name)))
		(when name
			(my-screen-save-current)
			(my-screen-load name (selected-frame)))))

(defun my-screen-open ()
	"Load new or saved screen into new frame."
	(interactive)
	(let ((name (my-screen-ido-read-name)))
		(if name
			(my-screen-load name))))

(defun my-screen-unload ()
	"Unloads screen.
	Preserves frame display, goes back to inital unloaded state."
	(interactive)
	(if (my-screen-unset (selected-frame))
		(message "Screen unloaded")
		(message "No screen loaded")))

(defun my-screen-rename ()
	"Rename current screen."
	(interactive)
	(let ((screen-current (my-screen-get-current)))
		(if screen-current
			(let ((name (read-from-minibuffer "New name: ")))
				(if (and name (not (equal name "")))
					(if (member name (my-screen-list))
						(message "Screen with that name already exists")
						(rename-file
							(concat my-screen-dir (symbol-name screen-current)
								my-screen-file-extension)
							(concat my-screen-dir name my-screen-file-extension))
						(my-screen-set (intern name) (selected-frame))
						(message (concat "Screen renamed to: " name)))
					(message "Name must not be empty")))
			(message "No screen loaded"))))

(defun my-screen-delete ()
	"Deletes current screen. No new screen is loaded."
	(interactive)
	(let ((screen-current (my-screen-get-current)))
		(if screen-current
			(if (y-or-n-p (concat "Do you really want to delete '"
						(symbol-name screen-current) "' ? "))
				(progn (my-screen-set nil (selected-frame))
					(delete-file
						(concat my-screen-dir (symbol-name screen-current)
							my-screen-file-extension))
					(message
						(concat "Screen '" (symbol-name screen-current) "' deleted")))
				(message ""))
			(message "No screen loaded"))))

(defun my-screen-wipe-interactive ()
	"Interactive wrapper for `my-screen-wipe"
	(interactive)
	(when (y-or-n-p "Are you sure ? ")
		(my-screen-wipe)
		(my-screen-save-current))
	(message ""))

(defun my-screen-help ()
	"Shows help."
	(interactive)
	(with-help-window (help-buffer) (princ (my-file-read my-screen-help-file))))

;;;###autoload
(defun my-screen-init ()
	"Initialize."
	(add-hook 'my-screen-wipe-hook 'my-frame-reasonable-split)
	(if delete-frame-functions
		(nconc delete-frame-functions '(my-screen-unset))
		(setq delete-frame-functions '(my-screen-unset))))

(defun my-screen-print-current ()
	"Print name of current screen."
	(interactive)
	(let ((screen-current (my-screen-get-current)))
		(message (if screen-current
				(symbol-name screen-current)
				"No screen loaded"))))

(provide 'my-screen/my-screen)
