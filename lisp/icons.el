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

;; 

;;; Code:



(provide 'icons)
;;; icons.el ends here

;; Emacs21.3 or earlier does not have locate-file.
(if (fboundp 'locate-file)
    (defalias 'org-locate-file 'locate-file)
  (defun org-locate-file (filename path)
    (locate-library filename t path)))

(setq org-status-icons-default-directory "~/icons/org-mode/")

(setq org-status-icons 
      '(("PROJECT" . "project.png")
        ("TODO" . "todo.png")))

(defun org-get-icon (name)
  (create-image (org-locate-file org-status-icons-default-directory name)))

(defun l-org-get-icon (icon-name)
  "Returns the name of the icon file for ICON-NAME."
  (concat (file-name-as-directory org-status-icons-default-directory) icon-name))

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
	(i-project (l-org-get-icon "project-crop.png"))
	(i-project-done (l-org-get-icon "project-crop-done.png"))
	(i-todo-blue (l-org-get-icon "rect-blue2.png"))
	(i-todo-next (l-org-get-icon "todo-next.png"))
	(i-todo-star (l-org-get-icon "todo-biggerstar.png"))
	(i-todo-ok (l-org-get-icon "todo-ok.png"))
	(i-todo-bad (l-org-get-icon "todo-bad.png"))
	(i-todo-normal (l-org-get-icon "todo-normal.png"))
	(i-drawer-end (l-org-get-icon "drawer-end.png"))
	(i-logbook (l-org-get-icon "logbook.png"))
	(i-rect-blue (l-org-get-icon "rect-blue.png"))
	(i-birect-blue (l-org-get-icon "birect-blue.png"))
	(i-rect-chocolate (l-org-get-icon "rect-chocolate.png"))
	(i-rect-green (l-org-get-icon "rect-green.png"))
	(i-birect-green (l-org-get-icon "birect-green.png"))
	(i-rect-grey1 (l-org-get-icon "rect-grey1.png"))
	(i-rect-grey2 (l-org-get-icon "rect-grey2.png"))
	(i-rect-orange (l-org-get-icon "rect-orange.png"))
	(i-rect-scarlet (l-org-get-icon "rect-scarlet.png"))
	(i-rect-violet (l-org-get-icon "rect-violet.png"))
	(i-rect-yellow (l-org-get-icon "rect-yellow.png"))
	)
    (while (re-search-forward re-heading limit t)
      (let* ((state (match-string 3))
	     (tags (org-get-tags-at))
	     ;(has-tag (lambda (tag) (if (member tag tags) t nil)))
	     (icon (cond
		   ((equal state "PROJECT") i-birect-blue)
		   ((equal state "PROJDONE") i-birect-green)
		   ((equal state "TODO") i-rect-grey1)
		   ((equal state "NEXT") i-rect-blue)
		   ((equal state "DONE") i-rect-green)
		   ((equal state "WAITING") i-rect-scarlet)
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
	  ))
      )
))

		       
(defun org-font-lock-add-drawer-faces (limit)
  "Add the drawer faces."
  (let (
	(re-drawer "^[ \t]*\\(:LOGBOOK:\\|:END:\\)[ \t]*\n?")
	(i-drawer-end (l-org-get-icon "drawer-end.png"))
	(i-logbook (l-org-get-icon "logbook3.png"))
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
			(t nil))))
	    (when icon
	      (set-icon (1-(match-beginning 1)) (1+ (match-end 1)) 
			(create-image icon nil nil :ascent 'center :margin '(0 . 0))))
      ))))
      (while (re-search-forward "^[ \t]*\\(:END:\\)[ \t]*\n?" limit t)
	      (set-icon (match-beginning 1) (match-end 1)
			(create-image i-drawer-end nil nil :ascent 'center :margin '(0 . 0))))))

;;   (let (
;; 	(re-drawer "^[ \t]*\\(:LOGBOOK:\\|:END:\\)[ \t]*\n?")
;; 	(i-drawer-end (l-org-get-icon "drawer-end.png"))
;; 	(i-logbook (l-org-get-icon "logbook3.png"))
;; 	)
;;     (while (re-search-forward re-drawer limit t)
;;       (let* ((name (match-string 1))
;; 	     (tags (org-get-tags-at))
;; 	     (icon (cond
;; 		   ((equal name ":LOGBOOK:") i-logbook)
;; 		   ((equal name ":END:") i-drawer-end)
;; 		   (t nil))))
;; 	(when icon
;; 	  (set-icon (match-beginning 1) (match-end 1) 
;; 		    (create-image icon nil nil :ascent 'center :margin '(0 . 0))))
;;       ))
;; ))


(defun org-font-lock-add-special-keyword-faces (limit)
  (let (
	(i-scheduled (l-org-get-icon "array-a-green.png"))
	)
  (progn
    (save-excursion
      (while (re-search-forward (concat "\\<" org-deadline-string) limit t) t))
    (save-excursion
      (while (re-search-forward (concat "\\<" org-scheduled-string) limit t)
	(set-icon (match-beginning 0) (match-end 0)
		  (create-image i-scheduled nil nil :ascent 'center :margin '(0 . 0))))))))


(defun org-font-lock-add-special-keyword-faces (limit)
  (progn
    (save-excursion 
      (while (re-search-forward (concat "\\<" org-deadline-string) limit t)
	(add-text-properties (match-beginning 0) (match-end 0) 
			     (list 'face 'org-special-keyword 'font-lock-fontified t))))
    (save-excursion 
      (while (re-search-forward (concat "\\<" org-scheduled-string) limit t)
	(add-text-properties (match-beginning 0) (match-end 0) 
			     (list 'face 'org-special-keyword 'font-lock-fontified t))))
    (save-excursion 
      (while (re-search-forward (concat "\\<" org-closed-string) limit t)
	(add-text-properties (match-beginning 0) (match-end 0) 
			     (list 'face 'org-special-keyword 'font-lock-fontified t))))
    (save-excursion 
      (while (re-search-forward (concat "\\<" org-clock-string) limit t)
	(add-text-properties (match-beginning 0) (match-end 0) 
			     (list 'face 'org-special-keyword 'font-lock-fontified t))))
))
