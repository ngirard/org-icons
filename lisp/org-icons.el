;;; org-icons.el --- Org-mode icons

;; Copyright (C) 2009, 2010  Nicolas Girard

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

;; This org-mode add-on lets you replace certain org-mode constructs
;; onscreen with color and shape coded icons. The content of the org
;; buffer is not affected. The included icons' color scheme follows
;; the Tango specification (see link below).

;; Github repository: 
;; http://github.com/ngirard/org-icons/tree/master

;; Tango icon theme info: 
;; http://tango.freedesktop.org/Tango_Icon_Theme_Guidelines

;;; Code:

;;;; Compatibility 

;; Emacs21.3 or earlier does not have locate-file.

(if (fboundp 'locate-file)
    (defalias 'org-locate-file 'locate-file)
  (defun org-locate-file (filename path)
    (locate-library filename t path)))

;;;; Finding icon files

(defvar org-icons-default-directory
  (expand-file-name
   "png"
   (expand-file-name
    "icons"
    (expand-file-name
     ".." (file-name-directory (or load-file-name buffer-file-name)))))
  "Directory to search for icons.
Set this to where you have installed the accompanying org icons.")

(defun org-get-icon (icon-name)
  "Returns the name of the icon file for ICON-NAME."
  (expand-file-name icon-name (file-name-as-directory org-icons-default-directory)))

;;;; List of icons

(defvar org-icon-alist
  '(("state-next" . "rect-blue")
    ("state-done" . "rect-green")
    ("state-someday" . "rect-grey1")
    ("state-todo" . "rect-grey2")
    ("state-waiting" . "rect-scarlet")
    ("state-project" . "project-blue")
    ("state-project-done" . "project-green")
    ("drawer-logbook" . "logbook")
    ("drawer-properties" . "properties")
    ("drawer-end" . "drawer-end")
    ("priority-a" . "prio-a")
    ("priority-b" . "prio-b")
    ("priority-c" . "prio-c")
    ("priority-nil" . "prio-nil")
    ("keyword-closed" . "closed")
    ("keyword-deadline" . "deadline-ok")
    ("keyword-scheduled" . "scheduled-ok")
    )
  "Alist of icons.
The car of each element is a string, denoting the icon. The cdr is either nil or the name of the file containing the icon, minus the extension.")



(setq org-icon-hash (make-hash-table :test 'equal))

(let ((oia org-icon-alist) a name file)
  (while (setq a (pop oia))
    (let ((name (car a)) (file (cdr a)))
      (puthash name 
	       (if file 
		   (create-image
		    (org-get-icon (concat (cdr a) ".png")) 
		    nil nil :ascent 'center :margin '(0 . 0))
		 nil)
		 org-icon-hash)
      )))

;;;; Drawing icons in a buffer

(defun draw-icon (beg end icon)
  (add-text-properties beg end (list 'display icon)))

(defun org-draw-icon (beg end name)
  (draw-icon beg end (gethash name org-icon-hash)))

;;;; High-level functions

(defun org-todo-state-icon-at (state tags)
  (progn (cond
   ((equal  "PROJECT"  state) "state-project")
   ((equal  "PROJDONE" state) "state-project-done")
   ((member "SOMEDAY"  tags ) "state-someday")
   ((equal  "TODO"     state) "state-todo")
   ((equal  "NEXT"     state) "state-next")
   ((equal  "DONE"     state) "state-done")
   ((equal  "WAITING"  state) "state-waiting")
   (t nil))))

(defun org-priority-icon-at (pri)
  (progn (cond
   ((equal pri "A") "priority-a")
   ((equal pri "B") "priority-b")
   ((equal pri "C") "priority-c")
   (t nil))))

(defun org-drawer-icon-at (name drawer-end)
  (progn (cond
   ((equal name "LOGBOOK") "drawer-logbook")
   ((equal name "PROPERTIES") "drawer-properties")
   (drawer-end  "drawer-end")
   (t nil))))

(defun org-special-keyword-icon-at (keyword)
  (progn (cond
   ((equal keyword org-closed-string) "keyword-closed")
   ((equal keyword org-deadline-string) "keyword-deadline")
   ((equal keyword org-scheduled-string) "keyword-scheduled")
   (t nil))))


;;;; Low-level functions

(defun org-font-lock-add-todo-state-faces (limit)
  "Add the todo state faces."
  (let ((re-heading (concat "^\\(\\**\\)\\(\\*[ \t]+\\)" org-todo-regexp "\\(.*\\|$\\)")))
    (when (re-search-forward re-heading limit t)
      (let* ((state (match-string 3))
	     (tags (org-get-tags-at))
	     (icon (org-todo-state-icon-at state tags)))
	(when icon
	  (org-draw-icon (match-beginning 2) (match-end 3) icon)		    
	  )))))

(defun org-font-lock-add-drawer-faces (limit)
  "Add the drawer faces."
  (let ((re-drawer "^[ \t]*\\(:LOGBOOK:\\|:END:\\)[ \t]*\n?"))
    (save-excursion
	(while (re-search-forward org-drawer-regexp limit t)
	  (let* ((name (match-string 1))
		 (tags (org-get-tags-at))
		 (icon (org-drawer-icon-at name nil)))
	    (when icon
	      (org-draw-icon (1-(match-beginning 1)) (1+ (match-end 1)) icon))
      )))
    (while (re-search-forward "^[ \t]*\\(:END:\\)[ \t]*\n?" limit t)
      (org-draw-icon (match-beginning 1) (match-end 1) 
		     (org-drawer-icon-at nil t)))))

(defun org-font-lock-add-priority-faces (limit)
  "Add the special priority faces."
  (when (re-search-forward "\\[#\\([A-Z0-9]\\)\\]" limit t)
      (let* ((pri (match-string 1))
	     (icon (org-priority-icon-at pri)))
	(when icon 
	  (org-draw-icon (1-(1-(match-beginning 1))) (1+(match-end 1)) icon)))))

;;;; Minor mode
(define-minor-mode org-icons-mode
  "When active, replace certain org-mode constructs
  onscreen with color and shape coded icons. The content of the org
  buffer is not affected."
  t " Icons" nil (if org-icons-mode (org-icons-enable) (org-icons-disable)))

(defun org-icons-enable ()
  "Enable Org-Icons mode.")

(defun org-icons-disable ()
  "Disable Org-Icons mode.")


;;;; Unused code

;; FIXME this function is not used?
(defun linkd-file-icon (file-name)
  "Choose an appropriate icon for FILE-NAME based on the name or extension.
Returns the file-name to the icon image file."
  (let* ((dir (file-name-as-directory linkd-icons-directory))
         (icon (concat dir "linkd-file-" (file-name-extension file-name) ".xpm")))
    (if (file-exists-p icon)
        icon
      (concat dir "linkd-file-generic.xpm"))))

(provide 'org-icons)
;;; org-icons.el ends here

