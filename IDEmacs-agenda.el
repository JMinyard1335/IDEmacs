;;; IDEmacs-agenda.el --- Agent for IDEmacs -*- lexical-binding: t -*-

;; Author: Jachin Minyard
;; Maintainer: Jachin Minyard
;; Version: 0.0.1
;; Package-Requires: ((doct) (org-super-agenda) (org-modern))
;; Homepage:

;;; Commentary:

;; This package is designed to implement an agenda system for the IDEmacs package.
;; This system is built off of they systems created by other great programmers.
;; Packages like org-super-agenda, org-modern, and org-agenda are used to create this system.

;;; Code:
;required packages and set up
(require 'org-capture)
(require 'IDEmacs-faces)
(use-package org
  :init (setq inhibit-compacting-font-caches t)
  :hook
  (org-mode . org-bullets-mode)
  (org-mode . visual-line-mode)
  :config
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
  (setq org-agenda-window-setup 'current-window)
  (setq org-log-into-drawer t)
  (setq org-log-done '('time))
  (setq org-todo-keywords
	'((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")
	  (sequence "ASSIGNED(A)" "CURRENT(C)" "|" "TURNEDIN(T)")))
  (setq org-todo-keywords-for-agenda '("TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "ASSIGNED(A)" "CURRENT(C)"))
  (setq org-done-keywords-for-agenda '("DONE(d)" "CANCELED(c)" "TURNEDIN(T)"))
  (setq org-deadline-warning-days 7)
  (setq org-popup-calendar-for-date-prompt t)
  (setq org-agenda-hide-tags-regexp ".*")
  (setq org-adapt-indentation t)
  (setq org-hide-drawer-startup t))
(use-package olivetti
  :ensure t
  :hook
  (org-agenda-mode . olivetti-mode))
(use-package org-super-agenda :ensure t
  :hook (org-agenda-mode . org-super-agenda-mode))
(use-package org-modern :ensure t)
(use-package doct
  :ensure t
  :commands (doct))
(use-package org-bullets
  :ensure t
  :after org
  :custom
  (org-bullets-bullet-list
   '("◉" "○" "●" "○" "●" "○" "●")))

; Group Declarations
(defgroup IDEmacs-Agenda nil
  "Holds all groups related to org agenda and its customizations."
  :group 'IDEmacs)

(org-link-set-parameters
 ;; This is a custom link type for linking files to tasks in the agenda.
 ;; When this file is opened with `org-agenda-open-link', The file is opened.
 ;; If the file does not exist, the user is prompted to create it.
 "IDEmacs_file"
 :follow #'idemacs/agenda-follow-link)

(defun idemacs/agenda-follow-link (file)
  "This function is used to follow links of the type `IDEmacs_file'.
If the file does not exist, the user is prompted to create it.
The file is opened in the current buffer."
  (if (idemacs-sidebar-mode)
      (progn
	(other-window 1)
	(idemacs/agenda-create-link-file file))
    (idemacs/agenda-create-link-file file)))

(defun idemacs/agenda-create-link-file (file)
  "This function is used to create a file with the name (FILE)."
  (if (not (file-exists-p file))
      (if (y-or-n-p (format "File %s does not exist. Create it?" file))
	  (progn
	    (find-file file)
	    (switch-to-buffer (current-buffer)))
	(message "%s Not created." file))
    (find-file file)))


;; Org agenda views
(defun idemacs/daily-quest ()
  "Hold the daily quest view for the agenda."
  '("d" "Daily Quest"
    ((agenda
      ""
      ((org-agenda-files idemacs-agenda-file-list)
       (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
       (org-agenda-span 'day)
       (org-todo-keywords
	'((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")
	  (sequence "ASSIGNED(A)" "CURRENT(C)" "|" "TURNEDIN(T)")))
       (org-todo-keywords-for-agenda '("TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "ASSIGNED(A)" "CURRENT(C)"))
       (org-done-keywords-for-agenda '("DONE(d)" "CANCELED(c)" "TURNEDIN(T)"))
       (org-agenda-time-grid nil)
       (org-agenda-overriding-header "           Daily Quest           ")
       (org-agenda-deadline-leaders '("Deadline:" "In %1d d." "%1d d. ago"))
       (org-agenda-prefix-format '((agenda . "  %i  %s ")))
       (org-super-agenda-groups
       '((:name " 󰼈   Quests  󰼈  " 
		:todo ("TODO" "ASSIGNED")
		:order 1)
	 (:name " 󰼆  Tracked  󰼆  "
		:todo ("CURRENT")
		:order 3)
	 (:name " 󱋻  Completed  󱋻  "
		:regexp "CLOSED:"
		:order 4)
	 (:name "    Side Quests     "
		:todo nil
		:order 2)
	 (:discard (:anything t)))))))))

(defun idemacs/school-quest ()
  "Hold the school quest view for the agenda."
  '("s" "School Agenda"
    ((agenda
      ""
      ((org-agenda-files (list idemacs-school-path))
       (org-agenda-span 1)
       (org-todo-keywords
	'((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")
	  (sequence "ASSIGNED(A)" "CURRENT(C)" "|" "TURNEDIN(T)")))    
       (org-todo-keywords-for-agenda '("TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "ASSIGNED(A)" "CURRENT(C)"))
       (org-done-keywords-for-agenda '("DONE(d)" "CANCELED(c)" "TURNEDIN(T)"))
       (org-agenda-time-grid nil)
       (org-agenda-overriding-header "        󰞟    School Agenda   󰞟          ")
       (org-agenda-deadline-leaders '("Due:" "In %1d d." "%1d d. ago"))
       (org-agenda-prefix-format '((agenda . "  %i %s %t ")))
       (org-super-agenda-groups
	'((:name " 󱉟  Projects  󱉟  "
		 :tag "project")
	  (:name "   Assignments    "
		 :tag "homework")
	  (:name "   Labs    "
		 :tag "lab")
	  (:name " 󰄥   Exams  󰄥   "
		 :tag "exam")
	  (:name " 󰋀  Classes 󰋀   "
		 :tag "class")
	 (:discard (:anything t)))))))))

(defun idemacs/agenda-view ()
  "Hold the idemacs agenda view for the agenda."
  '("a" "IDEmacs Agenda"
    ((agenda
      ""
      ((org-agenda-files idemacs-agenda-file-list)
       (org-agenda-span 'day)
       (org-todo-keywords
	'((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")
	  (sequence "ASSIGNED(A)" "CURRENT(C)" "|" "TURNEDIN(T)")))
       (org-todo-keywords-for-agenda '("TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "ASSIGNED(A)" "CURRENT(C)"))
       (org-done-keywords-for-agenda '("DONE(d)" "CANCELED(c)" "TURNEDIN(T)"))
       (org-todo-keywords
	'((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELED")
	  (sequence "ASSIGNED" "CURRENT" "|" "TURNEDIN")))
       (org-agenda-time-grid nil)
       (org-agenda-overriding-header "          IDEmacs Agenda         ")
       (org-agenda-prefix-format '((agenda . "  %i %s %t ")))
       (org-super-agenda-unmatched-name " Misc  ")
       (org-super-agenda-groups
	'((:name "    Overdue     "
		:deadline past
		:order 0)
	 (:name " 󱈸  Today  󱈸  "
		:deadline today
		:order 1)
	 (:name " 󱆀   School  󱆀   "
		:tag "school"
		:order 2)
	 (:name "    Home     "
	       :tag "home"
	       :order 2)
	 (:name "    Personal     "
		:tag "personal"
		:order 2)
	(:name " 󰝮   Random  󰝮   "
	       :not (:and (:tag ("school" "home" "personal"))))
	(:discard (:anything t)))))))))

(defun idemacs/view-daily-quest ()
  "This function is used to view the daily quest."
  (interactive)
  (let ((org-agenda-custom-commands (list (idemacs/daily-quest))))
    (org-agenda nil "d")
    (idemacs/faces-agenda-apply)))

(defun idemacs/view-school-agenda ()
  "This function is used to view the school quest."
  (interactive)
  (let ((org-agenda-custom-commands (list (idemacs/school-quest))))
    (org-agenda nil "s")
    (idemacs/faces-agenda-apply)))

(defun idemacs/view-agenda ()
  "This function is used to view the idemacs agenda."
  (interactive)
  (let ((org-agenda-custom-commands (list (idemacs/agenda-view))))
    (org-agenda nil "a")
    (idemacs/faces-agenda-apply)))


;; Org agenda capture templates
(defun idemacs/capture-school-class ()
  "This function is used to capture class information."
  (interactive)
  (let ((org-capture-templates (doct (list (idemacs/capture-school-template)))))
    (org-capture nil "sc")))

(defun idemacs/capture-school-template ()
  "A list to hold the capture templates for school tasks."
  '("School Work" :keys "s"
    :file idemacs-school-path
    :template
    ("* %{todo-state} %{task} %{tags}:school:%^G"
     "%{time-stamp}"
     ":PROPERTIES:"   
     ":Class: %^{class}"
     ":Assigned: %U"
     "%{link}"
     ":END:")
    :children
    (("Assignment" :keys "a"
     :headline "Assignments"
     :todo-state "ASSIGNED"
     :task "%^{Assignment}"
     :time-stamp "DEADLINE: %(idemacs/agenda-set-deadline)"
     :link ":Assignment: %(idemacs/agenda-link-file)"
     :tags ":homework")
     ("Lab" :keys "l"
      :headline "Labs"
      :todo-state "ASSIGNED"
      :task "%^{Lab}"
      :time-stamp "DEADLINE: %(idemacs/agenda-set-deadline)"
      :link ":Lab: %(idemacs/agenda-link-file)"
      :tags ":lab")
     ("Exams" :keys "e"
      :headline "Exams"
      :todo-state ""
      :task "%^{Exam}"
      :time-stamp "DEADLINE: %(idemacs/agenda-set-deadline)"
      :link ":Review: %(idemacs/agenda-link-file)"
      :tags ":exam")
     ("Class" :keys "c"
      :headline "Classes"
      :template
      ("* TODO %^{Class} :school:class:"
       "%(idemacs/agenda-set-class-time) %(idemacs/agenda-set-class-time)"
       ":PROPERTIES:"
       ":Course_Number: %^{Course Number}"
       ":Section_Number: %^{Section Number}"
       ":Instructor: %^{Instructor}"
       ":Location: %^{Location}"
       ":Class_Directory: %(idemacs/agenda-link-file)"
       ":END:")))))
     
(defun idemacs/capture-project-template ()
 "A list to hold the capture templates for projects."
  '("Projects" :keys "p"
    :template
    ("* %{todo-state} %^{Task} %{tags}:project:%^G"
     "%{time-stamp}"
     ":PROPERTIES:"
     ":Description: %?"
     "%{created}"
     "%{link}"
     "%{project}"
     ":END:")
    :children
    (("School Projects" :keys "s"
      :file idemacs-school-path
      :todo-state "ASSIGNED"
      :tags ":school"
      :time-stamp "DEADLINE: %(idemacs/agenda-set-deadline)"
      :created ":Assigned: %U"
      :children
      (("Project" :keys "p"
	:headline "Projects"
	:link ":Project: %(idemacs/agenda-link-file)"
	:project "%^{What is the name of the project?}")
       ("Sub Task" :keys "t"
	:function idemacs/agenda-school-project-capture
	:link ":Task: %(idemacs/agenda-link-file)"
	:project "%^{What is the name of the project? }")))
     ("Personal Projects" :keys "p"
      :file idemacs-personal-path
      :todo-state "TODO"
      :tags ":personal"
      :time-stamp "DEADLINE: %(idemacs/agenda-set-deadline)"
      :created ":Created: %U"
      :children
      (("Project" :keys "p"
	:headline "Projects"
	:link ":Personal_Docs: %(idemacs/agenda-link-file)"
	:project ":Project: %^{What is the name of the project?}")
       ("Sub Task" :keys "t"
	:function idemacs/agenda-personal-project-capture
	:link ":Task: %(idemacs/agenda-link-file)"
	:project ":Project: %^{What is the name of the project? }")))
     ("Home Projects" :keys "h"
      :file idemacs-home-path
      :todo-state "TODO"
      :tags ":home"
      :time-stamp "%(idemacs/agenda-set-deadline)"
      :created ":Created: %U"
      :children
      (("Project" :keys "p"
	:headline "Projects"
	:link ":Home_Docs: %(idemacs/agenda-link-file)"
	:project ":Project: %^{What is the name of the project?}")
       ("Sub Task" :keys "t"
	:function idemacs/agenda-home-project-capture
	:link ":Task: %(idemacs/agenda-link-file"
	:project ":Project: %^{What is the name of the project? "))))
     ))

(defun idemacs/capture-home-template ()
 "A list to hold the capture templates for home tasks."
  '("Home" :keys "h"
    :file idemacs-home-path
    :template
    ("* TODO %^{Task} %{tags}%^g"
     "SCHEDULED: %(idemacs/agenda-set-deadline)"
     ":PROPERTIES:"
     ":DATE: %U"
     ":END:")
    :children
    (("Chores" :keys "c"
      :headline "Chores"
      :tags ":home:errand:"
      )
     ("Errands" :keys "e"
      :headline "Errands"
      :tags ":home:errand:"))))

(defun idemacs/capture-personal-template ()
  "A list to hold the capture templates for personal tasks."
  '("Personal" :keys "i"
    :file idemacs-personal-path
    :children
    (("Reminders" :keys "r"
      :headline "Reminders"
      :template
      ("* TODO %^{Reminder} :personal:reminders:%^g"
       "DEADLINE: %(idemacs/agenda-set-deadline"))
     ("Goals" :keys "g"
      :headline "Goals"
      :template
      ("* %^{Goal} :personal:goals:%^g"
       "%?"
       ":PROPERTIES:"
       ":DATE: %U"
       ":REASON: %^{Why is this a goal?}"
       ":END:"))
     ("Family" :keys "f"
      :headline "Family"
      :template
      ("* TODO %^{Activity} :personal:family:%^g"
       ":PROPERTIES:"
       ":DATE: %(idemacs/agenda-set-deadline)"
       ":LOCATION: %^{Location}"
       ":END:")))))

(defun idemacs/capture-sidebar-entries ()
  "This function is used to create the capture templates for the sidebar."
  '("Sidebar" :keys "b"
    :file idemacs-sidebar-file
    :children
    (("header" :keys "h"
      :template ("** %^{Header}"))
     ("link" :keys "l"
      :template ("*** %(idemacs/sidebar-format-capture-link)")))))


;; Agenda Tags
(defun idemacs/agenda-update-org-tag-alist ()
  "Update `org-tag-alist` with the tags from `my-org-tags-list`."
  (dolist (tag idemacs-agenda-tag-list)
    (unless (assoc tag org-tag-alist)
      (add-to-list 'org-tag-alist tag))))

(defun idemacs/agenda-set-tag-alist (symbol value)
  "Used to set the tag alist for the agenda.
\(SYMBOL) and (VALUE) are stored."
  (set-default symbol value)
  (idemacs/agenda-update-org-tag-alist))

(defcustom idemacs-agenda-tag-list nil
  "The variable can be set in your config with the following code:
\(`setq' `idemacs-agenda-tag-list'
      '((\"tag1\" . ?1)
        (\"tag2\" . ?2)
	(\"tag3\" . ?3)))"
  :type '(repeat 'string)
  :set 'idemacs/agenda-set-tag-alist
  :group 'IDEmacs-Agenda)


;; Helper Functions
(defun idemacs/agenda-link-file ()
  "Prompts the user if they would like to insert a link to a file.
This link is of the type `IDEmacs_file' and is used to link files to tasks.
If the user selects yes, they will be prompted to select a file to link.
If the user selects no, `nil' is inserted in its place."
  (if (y-or-n-p "Would you like to link a file to this task? ")
      (format "[[IDEmacs_file:%s]]" (read-file-name "File to link: "))
    (format "NA" )))

(defun idemacs/agenda-set-deadline ()
  "This function creates a deadline timestamp.
This function will prompt a date, time, repeat interval, and a warning period."
  (idemacs/helper-replace-time-fields
   (format "<%s%s%s%s>"
	   (idemacs/agenda-get-date)
	   (idemacs/agenda-get-time)
	   (idemacs/agenda-get-repeat)
	   (idemacs/agenda-get-warning))))

(defun idemacs/agenda-set-class-time ()
  "This function is used to set the time for a class."
  (idemacs/helper-replace-time-fields
   (format "<%s%s%s>"
	   (idemacs/agenda-get-date)
	   (idemacs/agenda-get-time)
	   (idemacs/agenda-get-repeat))))

(defun idemacs/agenda-get-date ()
  "This function is to be used as part of a function to create a date format string.
It prompts the user to enter a date using the built-in `org-read-date'."
  (let ((date (org-read-date nil t nil "Enter the date ")))
    (message (format-time-string "%Y-%m-%d" date))))

(defun idemacs/agenda-get-time ()
  "This function is used to get a time from the user."
  (if (y-or-n-p "Would you like to set a time? ")
      (let ((time (read-string "Enter the time: ")))
	(if (not (idemacs/helper-validate-time time))
	    (setq time (read-string "Use the format HH:MM:SS or HH:MM"))
	  (format " %s" time)))
    (format "" )))

(defun idemacs/agenda-get-repeat ()
  "Ask the user if they want the time stamp to include a repeat value.
This value is of the for +nc where n is an integer and c is a choice between
'(Year Month Week Day Hour). The user must pick the interval type and the number"
  (if (y-or-n-p "Would you like to set a repeat interval? ")
      (let ((interval (idemacs/agenda-repeat-interval))
	    (number (read-number "Enter the number of intervals: ")))
	(format " +%d%s" number interval))
    (format " +0d")))

(defun idemacs/agenda-repeat-interval ()
  "This function is used to get and return the char needed to set a repeat interval.
This function allows the user to pick between several options '(Year Month Day Hour).
Depending on the value picked by the user a character is returned '(y m d h)."
  (let ((choice (completing-read "Select repeat interval: " '("Year" "Month" "Day" "Hour" "Week") nil t)))
    (cond ((string= choice "Year") "y")
	  ((string= choice "Month") "m")
	  ((string= choice "Day") "d")
	  ((string= choice "Hour") "h")
	  ((string= choice "Week") "w"))))

(defun idemacs/agenda-get-warning ()
  "Ask the user if they want the time stamp to include a warning value.
This value is of the for -nc where n is an integer and c is a choice between
'(Year Month Week Day Hour). The user must pick the interval type and the number"
  (if (y-or-n-p "Would you like to set a warning? ")
      (let ((interval (idemacs/agenda-repeat-interval))
	    (number (read-number "Enter the number of intervals: ")))
	(format " -%d%s" number interval))
    (format "")))

(defun idemacs/agenda-select-capture-path (&optional file-path)
  "This function is used to find a 'file-path' specified by the user. It then looks through that file for
org-headlines. It then prompts the user to select one of the headlines. After a selection is made
the function moves to that location in preperation for inserting a task with org capture."
  (let ((fpath (or file-path (read-file-name "Enter the org file: ")))
	(heading (idemacs/helper-select-header)))
    (find-file fpath)
    (goto-char (point-min))
    (search-forward heading)))

(defun idemacs/agenda-school-project-capture ()
  "This function is used to find the `idemacs-school-path' file.
Once the file is located the user is prompted to select either the
headline 'projects' or one of the sub-headlines. This will only allow
the user to enter task to projects of the file."
  (let ((projects '())
	(choice ""))
    (find-file idemacs-school-path)
    (goto-char (org-find-exact-headline-in-buffer "Projects"))
    (org-narrow-to-subtree)
    (setq projects (idemacs/helper-get-headers 2))
    (widen)
    (setq choice (completing-read "Select project: " projects))
    (goto-char (point-min))
    (search-forward choice)))

(defun idemacs/agenda-home-project-capture ()
  "This function is used to find the `idemacs-home-path' file.
Once the file is located the user is prompted to select either the
headline 'projects' or one of the sub-headlines. This will only allow
the user to enter task to projects of the file."
  (let ((projects '())
	(choice ""))
    (find-file idemacs-home-path)
    (goto-char (org-find-exact-headline-in-buffer "Projects"))
    (org-narrow-to-subtree)
    (setq projects (idemacs/helper-get-headers 2))
    (widen)
    (setq choice (completing-read "Select project: " projects))
    (goto-char (point-min))
    (search-forward choice)))

(defun idemacs/agenda-personal-project-capture ()
  "This function is used to find the `idemacs-personal-path' file."
  (let ((projects '())
	(choice ""))
    (find-file idemacs-personal-path)
    (goto-char (org-find-exact-headline-in-buffer "Projects"))
    (org-narrow-to-subtree)
    (setq projects (idemacs/helper-get-headers 2))
    (widen)
    (setq choice (completing-read "Select project: " projects))
    (goto-char (point-min))
    (search-forward choice)))

(defun idemacs/upadate-time-stamps ()
  "This is going to be a long.
So i need a function that will take the current org item in the agenda at my point and mark it as done.
Not only does it need to mark it as done it needs to look through the org item and update the timestamps.
There will be two timestamps and only one should be updated at a time.
The time stamp that will be updated will be determined by the current time.
If none of the timestamps match the current day then the oldest timestamp will be updated."
  (interactive)
  (let ((time-stamp '()) ; list to hold the time-stamps
	(current-day (format-time-string "%Y-%m-%d" (current-time))) ; gets the current time
	(heading nil))
    ;; the command must be run from an agenda view.
    (if (get-buffer "*Org Agenda*")
	(with-current-buffer "*Org Agenda*"
	  (org-agenda-goto) ; Jump to the heading in its org file
	  (org-narrow-to-subtree) ; Narrow the view of the org-file to the heading
	  (setq heading (org-get-heading t t t t)) ; Get the heading of the org item
	  ;; Parse the narrowed buffer and store the time-stamps in the list
	  (setq time-stamp (org-element-map (org-element-parse-buffer) 'timestamp
			     (lambda (timestamp)
			       (org-element-property :raw-value timestamp))))
	  (dolist (ts time-stamp)
	    ;;extract the data from the ts

	    ;; If you update on the day of the event
	    (if (string-match current-day ts)
		;; This is the time-stamp that needs to be updated
		(message "The time stamp matches the current day %s = %s" current-day ts)
	      ;; If the update is on a different day.
	      ;; update the oldest time stamp
	      (message "time stamps do not match %s != %s" current-day ts))))
      (message "Not in an org buffer"))
    (message "%s Current Day:%s, Time Code:%s Time Stamp:%s" heading current-day (current-time) time-stamp)))
  
 
(define-key org-agenda-mode-map (kbd "RET") 'org-agenda-open-link)

(provide 'IDEmacs-agenda)
;;; IDEmacs-agenda.el ends here
