;; el-screen.el --- Window configurations manager for Emacs

;; Author:	Mariusz Nowak <medikoo+el-screen@medikoo.com>
;; Copyright (C) 2010, 2011 Mariusz Nowak <medikoo+el-screen@medikoo.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE. See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; See README.

(require 'el-kit/directory nil t)
(require 'el-kit/frame nil t)
(require 'el-kit/file nil t)
(require 'el-kit/key nil t)
(require 'el-kit/list nil t)
(require 'el-index/el-index nil t)

(defgroup el-screen nil "el-screen -- Window configurations manager for Emacs")

(defcustom el-screen-default-buffer-name "*scratch*"
	"*Default buffer name. It's loaded in new screen or in windows which have
	problems getting back their buffers."
	:tag "Name of default buffer"
	:type '(string :size 24)
	:group 'el-screen)

(defcustom el-screen-dir (concat (getenv "HOME") "/.emacs.d/.elscreen/")
	"*Path at which configurations are saved."
	:tag "Configuration files path"
	:type '(string :size 2048)
	:group 'el-screen)

(defvar el-screen-file-extension
	".elscreen"
	"File extension for screen configurations.")

(defvar el-screen-frame-title-cache nil
	"Cached method that sets frame title")

(defvar el-screen-frame-title
	'(:eval
		(let ((name (el-screen-get-current)))
			(if name
				(symbol-name name)
				"*none*")))
	"Method that sets frame title to screen name")

(defvar el-screen-key-map (make-sparse-keymap)
	"Keymap for el-screen")

(define-key el-screen-key-map "'" 'el-screen-switch)
(define-key el-screen-key-map "c" 'el-screen-switch)
(define-key el-screen-key-map "\C-c" 'el-screen-switch)

(define-key el-screen-key-map "o" 'el-screen-open)

(define-key el-screen-key-map "1" (lambda () (interactive)
		(el-screen-switch-to-number 1)))
(define-key el-screen-key-map "2" (lambda () (interactive)
		(el-screen-switch-to-number 2)))
(define-key el-screen-key-map "3" (lambda () (interactive)
		(el-screen-switch-to-number 3)))
(define-key el-screen-key-map "4" (lambda () (interactive)
		(el-screen-switch-to-number 4)))
(define-key el-screen-key-map "5" (lambda () (interactive)
		(el-screen-switch-to-number 5)))
(define-key el-screen-key-map "6" (lambda () (interactive)
		(el-screen-switch-to-number 6)))
(define-key el-screen-key-map "7" (lambda () (interactive)
		(el-screen-switch-to-number 7)))
(define-key el-screen-key-map "8" (lambda () (interactive)
		(el-screen-switch-to-number 8)))
(define-key el-screen-key-map "9" (lambda () (interactive)
		(el-screen-switch-to-number 9)))
(define-key el-screen-key-map "0" (lambda () (interactive)
		(el-screen-switch-to-number 10)))

(define-key el-screen-key-map "\"" 'el-screen-index)
(define-key el-screen-key-map "w" 'el-screen-index)
(define-key el-screen-key-map "\C-w" 'el-screen-index)

(define-key el-screen-key-map "\C-a" 'el-screen-toggle)

(define-key el-screen-key-map " " 'el-screen-switch-next)
(define-key el-screen-key-map "n" 'el-screen-switch-next)
(define-key el-screen-key-map "\C-n" 'el-screen-switch-next)

(define-key el-screen-key-map "p" 'el-screen-switch-previous)
(define-key el-screen-key-map "\C-p" 'el-screen-switch-previous)
(define-key el-screen-key-map "\C-h" 'el-screen-switch-previous)
(define-key el-screen-key-map (kbd "DEL") 'el-screen-switch-previous)

(define-key el-screen-key-map "i" 'el-screen-print-current)
(define-key el-screen-key-map "\C-i" 'el-screen-print-current)
(define-key el-screen-key-map "N" 'el-screen-print-current)

(define-key el-screen-key-map "A" 'el-screen-rename-interactive)

(define-key el-screen-key-map "C" 'el-screen-clear-interactive)

(define-key el-screen-key-map "d" 'el-screen-unload)
(define-key el-screen-key-map "\C-d" 'el-screen-unload)
(define-key el-screen-key-map "k" 'el-screen-unload)
(define-key el-screen-key-map "\C-k" 'el-screen-unload)
(define-key el-screen-key-map "\C-z" 'el-screen-unload)

(define-key el-screen-key-map "\C-\\" 'el-screen-unload-all)

(define-key el-screen-key-map "?" 'el-screen-help)

(defcustom el-screen-prefix-key "\C-z"
	"*Prefix key for el-screen commands."
	:tag "Prefix key of el-screen"
	:type '(string :size 10)
	:set (lambda (symbol value)
		(if (boundp 'el-screen-prefix-key)
			(el-screen-set-prefix-key value)
			(el-kit-key-set value el-screen-key-map))
		(custom-set-default symbol value))
	:group 'el-screen)

(defun el-screen-set-prefix-key (key)
	"Set KEY as prefix for el-screen bindings."
	(el-kit-key-replace el-screen-prefix-key key el-screen-key-map)
	(setq el-screen-prefix-key key))

(defvar el-screen-help-file
	(concat (or (file-name-directory
				(or load-file-name (buffer-file-name) "")) "") "HELP")
	"Path to el-screen help file.")

(defvar el-screen-initialized?
	nil
	"Whether el-screen was already initialized.")

(defvar el-screen-frame-map
	nil
	"Screen to frame mappings.")

(defvar el-screen-loaded-list
	nil
	"Configrations loaded during this session.
	In first loaded order.")

(defvar el-screen-loaded-ring
	nil
	"Configurations loaded during this session.
	In last visited order.")

(defvar el-screen-clear-hook nil
	"Hook that gets run when frame is cleared.")

(defvar el-screen-switch-before-hook nil
	"Hook that gets run before configuration is switched.")

(defun el-screen-set (name frame)
	"Assign NAME configuration for given FRAME.
	If NAME is nil then unload configuration from given FRAME."
	(let ((screen-assoc (rassoc frame el-screen-frame-map)))
		(if screen-assoc
			(if name
				(setcar screen-assoc name)
				(setq el-screen-frame-map
					(rassq-delete-all frame el-screen-frame-map))
				(unless el-screen-frame-map
					(remove-hook 'auto-save-hook 'el-screen-save-all)
					(remove-hook 'kill-emacs-hook 'el-screen-save-all)))
			(when name
				(setq screen-assoc (cons name frame))
				(if el-screen-frame-map
					(nconc el-screen-frame-map (list screen-assoc))
					(setq el-screen-frame-map (list screen-assoc))
					(add-hook 'auto-save-hook 'el-screen-save-all)
					(add-hook 'kill-emacs-hook 'el-screen-save-all))))
		(if name
			(el-screen-add-to-loaded-list name))
		screen-assoc))

(defun el-screen-delete-from-loaded-list (name)
	"Delete NAME configuration from ring and loaded list."
	(setq el-screen-loaded-ring
		(delq name el-screen-loaded-ring))
	(setq el-screen-loaded-list
		(delq name el-screen-loaded-list)))

(defun el-screen-add-to-loaded-list (name)
	"Add NAME configuration to loaded list and ring."
	(setq el-screen-loaded-ring (cons name (delq name el-screen-loaded-ring)))
	(if (not (memql name el-screen-loaded-list))
		(setq el-screen-loaded-list (nconc el-screen-loaded-list (list name)))))

(defun el-screen-unset (frame)
	"Unloads configuration from given FRAME."
	(let ((screen-assoc (rassoc frame el-screen-frame-map)))
		(if screen-assoc
			(progn (el-screen-save screen-assoc)
				(el-screen-set nil frame)
				(el-screen-delete-from-loaded-list (car screen-assoc))
				t)
			nil)))

(defun el-screen-get-name (frame)
	"Returns configuration name for FRAME."
	(car (rassoc frame el-screen-frame-map)))

(defun el-screen-get-current ()
	"Returns configuration name for selected frame."
	(el-screen-get-name (selected-frame)))

(defun el-screen-save-current ()
	"Save configuration for selected frame."
	(let ((screen-assoc (rassoc (selected-frame) el-screen-frame-map)))
		(when screen-assoc
			(el-screen-save screen-assoc))))

(defun el-screen-save (config-assoc)
	"Save CONFIG-ASSOC to file."
	(el-kit-file-write
		(concat el-screen-dir
			(symbol-name (car config-assoc)) el-screen-file-extension)
		(prin1-to-string (el-kit-frame-serialize (cdr config-assoc)))))

(defun el-screen-save-all ()
	"Save configurations for all open frames."
	(dolist (screen-assoc el-screen-frame-map)
		(el-screen-save screen-assoc)))

(defun el-screen-load (name &optional frame)
	"Load NAME configuration into FRAME.
	If no FRAME is given then load configuration in new frame.
	If configuration is already loaded focus its frame."
	(let ((screen-assoc (assoc name el-screen-frame-map)))
		(if screen-assoc
			(when (not (eq screen-assoc
						(rassoc (selected-frame) el-screen-frame-map)))
				(run-hooks 'el-screen-switch-before-hook)
				(select-frame-set-input-focus (cdr screen-assoc))
				(el-screen-add-to-loaded-list name))
			(run-hooks 'el-screen-switch-before-hook)
			(let ((data (el-kit-file-read (concat el-screen-dir
								(symbol-name name) el-screen-file-extension) t))
					(doclear (or (el-screen-get-current) (not frame))))
				(unless frame
					(setq frame (make-frame-command)))
				(if data
					(progn
						(ignore-errors (el-kit-frame-unserialize (read data) frame))
						(el-screen-save (el-screen-set name frame)))
					(if doclear
						(el-screen-clear))
					(el-screen-save (el-screen-set name (selected-frame)))))))
	(message (concat "Configuration loaded: " (symbol-name name))))

(defun el-screen-list ()
	"Return list of saved configurations sorted by modification date."
	(el-kit-directory-files el-screen-dir
		(concat "\\" el-screen-file-extension "$")
		'el-kit-file-modification-date-sort
		'el-kit-file-name-nondirectory-sans-extension))

(defun el-screen-ido-list ()
	"Return list of saved configurations with selected one moved to end of list."
	(let ((screen-list (el-screen-list))
			(current-name (el-screen-get-current)))
		(when current-name
			(setq current-name (symbol-name current-name))
			(setq screen-list (delete current-name screen-list))
			(nconc screen-list (list current-name)))
		screen-list))

(defun el-screen-ido-read-name ()
	"Get configuration name from user."
	(let ((name (ido-completing-read "Name: " (el-screen-ido-list))))
		(if (and name (not (eq name "")))
			(intern name)
			(message "Please provide name for a configuration")
			nil)))

(defun el-screen-switch ()
	"Load new or existing configuration into current frame."
	(interactive)
	(let ((name (el-screen-ido-read-name)))
		(when name
			(el-screen-save-current)
			(el-screen-load name (selected-frame)))))

(defun el-screen-open ()
	"Load new or saved configuration into new frame."
	(interactive)
	(let ((name (el-screen-ido-read-name)))
		(if name
			(el-screen-load name))))

(defun el-screen-switch-by-func (method)
	"Switch to configuration returned by method."
	(if el-screen-loaded-list
		(let ((current (el-screen-get-current)) index)
			(el-screen-load
				(if current
					(funcall method current)
					(car el-screen-loaded-list))
				(selected-frame)))
		(message "No configuration loaded")))

(defun el-screen-switch-next ()
	"Switch to next configuration."
	(interactive)
	(el-screen-switch-by-func (lambda (current)
			(el-kit-list-next el-screen-loaded-list current t))))

(defun el-screen-switch-previous ()
	"Switch to previous configuration."
	(interactive)
	(el-screen-switch-by-func (lambda (current)
			(el-kit-list-previous el-screen-loaded-list current t))))

(defun el-screen-switch-to-number (num)
	"Switch to configuration number NUM."
	(let ((name (nth (- num 1) el-screen-loaded-list)))
		(if name
			(el-screen-load name (selected-frame))
			(el-screen-switch))))

(defun el-screen-toggle ()
	"Toggle to configuration displayed previously."
	(interactive)
	(if el-screen-loaded-ring
		(if (el-screen-get-current)
			(if (> (length el-screen-loaded-ring) 1)
				(el-screen-load (second el-screen-loaded-ring) (selected-frame)))
			(el-screen-load (car el-screen-loaded-ring) (selected-frame)))))

(defun el-screen-unload ()
	"Unload configuration.
	Preserves frame display, goes back to inital unloaded state."
	(interactive)
	(if (el-screen-unset (selected-frame))
		(message "Configuration unloaded")
		(message "No configuration loaded")))

(defun el-screen-unload-all ()
	"Unload all loaded configurations."
	(interactive)
	(while el-screen-frame-map
		(el-screen-unset (cdr (car el-screen-frame-map))))
	(setq el-screen-loaded-list ())
	(setq el-screen-loaded-ring ())
	(message "All configurations unloaded"))

(defun el-screen-clear ()
	"Clear selected frame."
	(delete-other-windows)
	(switch-to-buffer el-screen-default-buffer-name)
	(run-hooks 'el-screen-clear-hook))

(defun el-screen-clear-interactive ()
	"Interactive wrapper for `el-screen-clear'."
	(interactive)
	(when (y-or-n-p "Are you sure ? ")
		(el-screen-clear)
		(el-screen-save-current))
	(message ""))

(defun el-screen-rename (oldname newname)
	"Rename OLDNAME configuration with NEWNAME."
	(if (and newname (not (equal newname "")))
		(if (member newname (el-screen-list))
			(message "Configuration with that name already exists")
			(rename-file
				(concat el-screen-dir (symbol-name oldname) el-screen-file-extension)
				(concat el-screen-dir newname el-screen-file-extension))
			(el-kit-list-replace el-screen-loaded-list oldname (intern newname))
			(el-kit-list-replace el-screen-loaded-ring oldname (intern newname))
			(let ((frame (cdr (assoc oldname el-screen-frame-map))))
				(if frame
					(el-screen-set (intern newname) frame)))
			t)
		(message "Name must not be empty")
		nil))

(defun el-screen-rename-interactive ()
	"Rename current configuration."
	(interactive)
	(let ((screen-current (el-screen-get-current)))
		(if screen-current
			(let ((name (read-from-minibuffer
							(concat "New name for '" (symbol-name screen-current) "': "))))
				(if (el-screen-rename screen-current name)
					(message (concat "Configuration renamed to: " name))))
			(message "No configuration loaded"))))

(defun el-screen-delete (name)
	"Delete NAME configuration."
	(delete-file
		(concat el-screen-dir (symbol-name name)
			el-screen-file-extension))
	(el-screen-delete-from-loaded-list name)
	(let ((frame (cdr (assoc name el-screen-frame-map))))
		(if frame (el-screen-set nil frame))))

(defun el-screen-delete-interactive ()
	"Delete current configuration."
	(interactive)
	(let ((screen-current (el-screen-get-current)))
		(if screen-current
			(if (y-or-n-p (concat "Do you really want to delete '"
						(symbol-name screen-current) "' ? "))
				(progn (el-screen-set nil (selected-frame))
					(el-screen-delete screen-current)
					(message
						(concat "Configuration '" (symbol-name screen-current)
							"' deleted")))
				(message ""))
			(message "No configuration loaded"))))

(defun el-screen-print-current ()
	"Print name of current configuration."
	(interactive)
	(let ((screen-current (el-screen-get-current)))
		(message (if screen-current
				(symbol-name screen-current)
				"No configuration loaded"))))

(defun el-screen-index ()
	"Shows screen index."
	(interactive)
	(el-index-display "el-screen index"
		(el-screen-index-list) el-screen-index-actions)
	(local-set-key "o" 'el-screen-index-open))

(defun el-screen-index-open ()
	"Open selected configuration in new frame"
	(interactive)
	(el-index-funcall-with-quit-window 'el-screen-load))

(defun el-screen-index-list ()
	"Return list of loaded and saved configuarations."
	(let ((saved-list (mapcar (lambda (name) (intern name)) (el-screen-list))))
		(dolist (name el-screen-loaded-list)
			(setq saved-list (delete name saved-list)))
		(list
			(cons "Loaded" el-screen-loaded-list)
			(cons "Saved" saved-list))))

(defvar el-screen-index-actions
	'((write . (lambda (name)
				(let ((index (el-kit-list-index-of el-screen-loaded-list name)))
					(if index
						(insert (if (< index 10)
								(concat (int-to-string (mod (+ 1 index) 10))) " ") " "))
					(insert (symbol-name name)))))
		(select . (lambda (name)
				(el-screen-save-current)
				(el-screen-load name (selected-frame))))
		(rename . el-screen-rename)
		(delete . el-screen-delete)
		(reload . el-screen-index))
	"Actions that are called from index buffer.")

(defun el-screen-help ()
	"Shows help."
	(interactive)
	(with-help-window (help-buffer)
		(princ (el-kit-file-read el-screen-help-file))))

(defun el-screen-set-frame-title ()
	"Sets frame title format to current screen name."
	(unless el-screen-frame-title-cache
		(setq el-screen-frame-title-cache frame-title-format)
		(setq frame-title-format el-screen-frame-title)))

(defun el-screen-unset-frame-title ()
	"Reverts frame title format to previous setting."
	(when el-screen-frame-title-cache
		(setq frame-title-format el-screen-frame-title-cache)
		(setq el-screen-frame-title-cache nil)))

;;;###autoload
(defun el-screen-init ()
	"Initialize."
	(unless el-screen-initialized?
		(setq el-screeen-initialized? t)
		(add-hook 'el-screen-clear-hook 'el-kit-frame-reasonable-split)
		(add-hook 'el-screen-switch-before-hook 'save-some-buffers)
		(setq delete-frame-functions
			(nconc delete-frame-functions '(el-screen-unset)))))

(provide 'el-screen/el-screen)
