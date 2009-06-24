;;; icons.el --- Org-mode icons

;; Copyright (C) 2009  Nicolas Girard

;; Author: Nicolas Girard <girard.nicolas@gmail.com>
;; Keywords: faces

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This program adds pretty icons to org-mode buffers. The icons
;; replace certain org-mode constructs onscreen with color and shape
;; coded icons. The icons' color scheme follows the Tango scheme (see
;; link below.)

;; Github repository: 
;; http://github.com/ngirard/org-icons/tree/master

;; Tango icon theme info: 
;; http://tango.freedesktop.org/Tango_Icon_Theme_Guidelines

;;; Code:

;; Emacs21.3 or earlier does not have locate-file.
(if (fboundp 'locate-file)
    (defalias 'org-locate-file 'locate-file)
  (defun org-locate-file (filename path)
    (locate-library filename t path)))

(setq org-icons-default-directory "~/.soft/elisp/org-icons/icons/png")

(setq org-status-icons 
      '(("PROJECT" . "project.png")
        ("TODO" . "todo.png")))

(defun org-get-icon (icon-name)
  "Returns the name of the icon file for ICON-NAME."
  (concat (file-name-as-directory org-icons-default-directory) icon-name))

(defun linkd-file-icon (file-name)
  "Choose an appropriate icon for FILE-NAME based on the name or extension.
Returns the file-name to the icon image file."
  (let* ((dir (file-name-as-directory linkd-icons-directory))
         (icon (concat dir "linkd-file-" (file-name-extension file-name) ".xpm")))
    (if (file-exists-p icon)
        icon
      (concat dir "linkd-file-generic.xpm"))))

(defun set-icon (beg end icon)
  (add-text-properties beg end (list 'display icon)))
;  (add-text-properties beg end (list 'display icon)))

;-------------------
(defun org-font-lock-add-todo-state-faces (limit)
  "Add the todo state faces."
  (let (
	(re-heading (concat "^\\(\\**\\)\\(\\*[ \t]+\\)" org-todo-regexp "\\(.*\\|$\\)"))
	(i-todo-blue (org-get-icon "rect-blue.png"))
	(i-todo-chocolate (org-get-icon "rect-chocolate.png"))
	(i-todo-green (org-get-icon "rect-green.png"))
	(i-todo-grey1 (org-get-icon "rect-grey1.png"))
	(i-todo-grey2 (org-get-icon "rect-grey2.png"))
	(i-todo-orange (org-get-icon "rect-orange.png"))
	(i-todo-scarlet (org-get-icon "rect-scarlet.png"))
	(i-todo-violet (org-get-icon "rect-violet.png"))
	(i-todo-yellow (org-get-icon "rect-yellow.png"))
	(i-project-blue (org-get-icon "project-blue.png"))
	(i-project-green (org-get-icon "project-green.png"))
	(i-project-grey1 (org-get-icon "project-grey1.png"))
	)
    (while (re-search-forward re-heading limit t)
      (let* ((state (match-string 3))
	     (tags (org-get-tags-at))
	     ;(has-tag (lambda (tag) (if (member tag tags) t nil)))
	     (icon (cond
		   ((equal state "PROJECT") i-project-blue)
		   ((equal state "PROJDONE") i-project-green)
		   ((equal state "SOMEDAY") i-project-grey1)
		   ((equal state "TODO") i-todo-grey1)
		   ((equal state "NEXT") i-todo-blue)
		   ((equal state "DONE") i-todo-green)
		   ((equal state "WAITING") i-todo-scarlet)
		   ; This works
		   ;((and (member "NEXT" (org-get-tags-at)) (equal state "NA")) i-todo-star)
		   ; This doesn't. I must be missing something, possibly a funcall/apply somewhere
		   ; for has-tag to work
		   ;((and (has-tag "NEXT") (equal state "NA")) i-todo-star)
		   (t nil))))
	(when icon
	  ; I'd prefer the leading '*' to be hidden, using
	  ;   (match-beginning 2)
	  ; but it leads to misalignment of drawers and body text.
	  ;(set-text-properties 
	  ; (match-beginning 3) (match-end 3)
	  ; (list 'display (create-image icon nil nil :ascent 'center))
	  (set-icon (match-beginning 2) (match-end 3) 
		    (create-image icon nil nil :ascent 'center)
	   )
	  )))))

		       
(defun org-font-lock-add-drawer-faces (limit)
  "Add the drawer faces."
  (let (
	(re-drawer "^[ \t]*\\(:LOGBOOK:\\|:END:\\)[ \t]*\n?")
	(i-drawer-end (org-get-icon "drawer-end.png"))
	(i-logbook (org-get-icon "logbook.png"))
	(i-properties (org-get-icon "properties.png"))
	)
    (progn
      (save-excursion
	(while (re-search-forward org-drawer-regexp limit t)
	  ;(add-text-properties 
	  ; (match-beginning 0) (match-end 0) (list 'face 'org-special-keyword 'font-lock-fontified t))
	  (let* ((name (match-string 1))
		 (tags (org-get-tags-at))
		 (icon (cond
			((equal name "LOGBOOK") i-logbook)
			((equal name "PROPERTIES") i-properties)
			(t nil))))
	    (when icon
	      (set-icon (1-(match-beginning 1)) (1+ (match-end 1)) 
			(create-image icon nil nil :ascent 'center :margin '(0 . 0))))
      ))))
      (while (re-search-forward "^[ \t]*\\(:END:\\)[ \t]*\n?" limit t)
	      (set-icon (match-beginning 1) (match-end 1)
			(create-image i-drawer-end nil nil :ascent 'center :margin '(0 . 0))))))


(defun org-font-lock-add-special-keyword-faces (limit)
  (let (
	(i-scheduled (org-get-icon "scheduled-ok.png"))
	(i-deadline  (org-get-icon "deadline-ok.png"))
	(i-closed  (org-get-icon "closed.png"))
	)
  (progn
    (save-excursion
      (while (re-search-forward (concat "\\<" org-scheduled-string) limit t)
	(set-icon (match-beginning 0) (match-end 0)
		  (create-image i-scheduled nil nil :ascent 'center :margin '(0 . 0)))))
    (save-excursion
      (while (re-search-forward (concat "\\<" org-deadline-string) limit t)
	(set-icon (match-beginning 0) (match-end 0)
		  (create-image i-deadline nil nil :ascent 'center :margin '(0 . 0)))))
    (save-excursion
      (while (re-search-forward (concat "\\<" org-closed-string) limit t)
	(set-icon (match-beginning 0) (match-end 0)
		  (create-image i-closed nil nil :ascent 'center :margin '(0 . 0)))))
)))
;org-closed-string
;org-clock-string

(defun org-font-lock-add-priority-faces (limit)
  "Add the special priority faces."
  (let (
	(i-prio-a (org-get-icon "prio-a.png"))
	(i-prio-b (org-get-icon "prio-b.png"))
	(i-prio-c (org-get-icon "prio-c.png"))
	(i-prio-nil (org-get-icon "prio-nil.png"))
	)
  (while (re-search-forward "\\[#\\([A-Z0-9]\\)\\]" limit t)
      (let* ((pri (match-string 1))
	     (icon (cond
		   ((equal pri "A") i-prio-a)
		   ((equal pri "B") i-prio-b)
		   ((equal pri "C") i-prio-c)
		   (t nil))))
	(when icon (set-icon (1-(1-(match-beginning 1))) (1+(match-end 1)) 
			     (create-image icon nil nil :ascent 'center)))))))




(provide 'icons)
;;; icons.el ends here

