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
  (if (not (file-exists-p file))
      (if (y-or-n-p (format "File %s does not exist. Create it?" file))
	  (find-file file)
	(message "%s Not created." file))
    (find-file file)))

;;;###autoload
(define-minor-mode idemacs-agenda-mode
  "A minor mode for IDEmacs agenda."
  :lighter " IDEA"
  :init nil
  (if idemacs-agenda-mode
      (progn
	(unless (get-buffer "*Org Agenda*")
	  (if (not (idemacs/sidebar-open-p))
	      (idemacs/sidebar-open))))
    (idemacs/sidebar-kill)))

; idemacs/personal
(defgroup IDEmacs-Personal nil
  "Holds all the variables for the agenda's personal files."
  :group 'IDEmacs-Agenda)

(defconst idemacs-personal-default-path
  "~/.emacs.d/IDEmacs/OrgFiles/Personal.org"
  "Hold the default path for the personal org file.
This should be treated as a constant and never changed.")

(defconst idemacs-personal-default-template
  "#+CATEGORY: Personal\n\n* Projects\n* Family\n* Reminders\n* Goals\n"
  "Default string to be written to the file `idemacs-personal-path'.
This should be treated as a constant and never changed.")

(defcustom idemacs-personal-path idemacs-personal-default-path
  "The path to an org file for personal tasks."
  :type 'string
  :group 'IDEmacs-Personal)

(defcustom idemacs-personal-template idemacs-personal-default-template
  "A format string that will be written to the personal file."
  :type 'string
  :group 'IDEmacs-Personal)

(defun idemacs/personal-set-path (file)
  "Set `idemacs-personal-path' to (FILE)."
  (interactive "FSelect a path: ")
  (if (not (file-exists-p file))
      (my-create-file file idemacs-personal-template))
  (setq idemacs-personal-path file)
  (message "Personal file set to: %s" idemacs-personal-path))

(defun idemacs/personal-reformat-file ()
  "Clears the file at `idemacs-personal-path'.
Writes the `idemacs-personal-template' to the file."
  (interactive)
  (if (y-or-n-p (format "Would you like to reformat %s? " idemacs-personal-path))
      (progn (with-temp-buffer
	       (write-region idemacs-personal-template nil idemacs-personal-path)
	       (save-buffer)
	       (kill-buffer))
	     (message "%s reformated sucessfully" idemacs-personal-path))
      (message "%s was not reformated" idemacs-personal-path)))

;; idemacs/school
(defgroup IDEmacs-School nil
  "Holds all variables related to the agenda's school files."
  :group 'IDEmacs-Agenda)

(defconst idemacs-school-default-path
  "~/.emacs.d/IDEmacs/OrgFiles/School.org"
 "Hold the default path for the school org file.
This should be treated as a constant and never changed.")

(defconst idemacs-school-default-template
  "#+CATEGORY: School\n\n* Classes\n* Assignments\n* Projects\n* Exams\n* Labs\n"
  "Default string to be written to the file `idemacs-school-path'.
This should be treated as a constant and never changed.")

(defcustom idemacs-school-path idemacs-school-default-path
 "The path to an org file for school tasks."
  :type 'string
  :group 'IDEmacs-School)

(defcustom idemacs-school-template idemacs-school-default-template
  "A format string that will be written to the school file."
  :type 'string
  :group 'IDEmacs-School)

(defun idemacs/school-set-path (file)
 "Set `idemacs-school-path' to (FILE)."
  (interactive "FSelect a path: ")
  (if (not (file-exists-p file))
      (idemacs/helper-create-file file idemacs-school-template))
  (setq idemacs-school-path file)
  (message "School file set to %s" idemacs-school-path))

(defun idemacs/school-reformat-file ()
  "Clears the file at `idemacs-school-path'.
Writes the `idemacs-school-template' to the file."
  (interactive)
  (if (y-or-n-p (format "Would you like to reformat %s? " idemacs-school-path))
      (progn (with-temp-buffer
	       (write-region idemacs-school-template nil idemacs-school-path)
	       (save-buffer)
	       (kill-buffer))
	     (message "%s was reformated" idemacs-school-path))
    (message "%s was not reformated" idemacs-school-path)))

;; idemacs/home
(defgroup IDEmacs-Home nil
  "Holds all variables related to the agenda's home files."
  :group 'IDEmacs-Agenda)

(defconst idemacs-home-default-path
  "~/.emacs.d/IDEmacs/OrgFiles/Home.org"
  "Hold the default path for the home org file.
This should be treated as a constant and never changed.")

(defconst idemacs-home-default-template
  "#+CATEGORY: Home\n\n* Projects\n* Errands\n* Chores\n"
  "Default string to be written to the file `idemacs-home-path'.
This should be treated as a constant and never changed.")

(defcustom idemacs-home-path idemacs-home-default-path
  "The path to an org file for home tasks."
  :type 'string
  :group 'IDEmacs-Home)

(defcustom idemacs-home-template idemacs-home-default-template
  "A format string that will be written to the home file."
  :type 'string
  :group 'IDEmacs-Home)

(defun idemacs/home-set-path (file)
  "Set `idemacs-home-path' to (FILE)."
  (interactive "FSelect a path: ")
  (if (not (file-exists-p file))
      (idemacs/helper-create-file file idemacs-home-template))
  (setq idemacs-home-path file)
  (message "Home path set to: %s" idemacs-home-path))

(defun idemacs/home-reformat-file ()
  "Clears the file at `idemacs-home-path'.
Writes the `idemacs-home-template' to the file."
  (interactive)
  (if (y-or-n-p (format "Would you like to reformat %s? " idemacs-home-path))
      (progn (with-temp-buffer
	       (write-region idemacs-home-template nil idemacs-home-path)
	       (save-buffer)
	       (kill-buffer))
	     (message "%s was reformated" idemacs-home-path))
      (message "%s was not reformated" idemacs-home-path)))

;; idemacs/refile
(defgroup IDEmacs-Refile nil
  "This is a subgroup of `IDEmacs' it contains all info regarding home tasks."
  :group 'IDEmacs)

(defconst idemacs-refile-default-path
  "~/.emacs.d/IDEmacs/OrgFiles/Refile.org"
  "The default path for the refile org file.
This should be treated as a constant and never changed.")

(defconst idemacs-refile-default-template
  "#+CATEGORY: Completed\n\n* School\n* Home\n* Personal\n* Work\n* Other\n"
  "The default template for the refile org file.
This should be treated as a constant and never changed.")

(defcustom idemacs-refile-path idemacs-refile-default-path
  "The path to the refile org file."
  :type 'string
  :group 'IDEmacs-Refile)

(defcustom idemacs-refile-template idemacs-refile-default-template
  "A format string that will be written to the refile file."
  :type 'string
  :group 'IDEmacs-Refile)

(defun idemacs/refile-set-path (file)
  "Set `idemacs-refile-path' to (FILE)."
  (interactive "FSelect a path: ")
  (if (not (file-exists-p file))
      (my-create-file file idemacs-refile-template))
  (setq idemacs-refile-path file)
  (message "Home path set to: %s" idemacs-refile-path))

(defun idemacs/refile-reformat-file ()
  "Clears the file at `idemacs-refile-path'.
Writes the `idemacs-refile-template' to the file."
  (interactive)
  (if (y-or-n-p "Would you like to reformat the Home file? ")
      (progn (with-temp-buffer
	       (write-region idemacs-refile-template nil idemacs-refile-path)
	       (save-buffer)
	       (kill-buffer))
	     (message "Refile file reformated"))
      (message "Refile file was not reformated")))

(defcustom idemacs-agenda-file-list (list idemacs-home-path idemacs-personal-path idemacs-school-path)
  "List of files to be used as agenda files."
  :type (list 'string)
  :group 'IDEmacs-Agenda)

;; Org agenda views
(defun idemacs/daily-quest ()
  "Hold the daily quest view for the agenda."
  '("d" "Daily Quest"
    ((agenda
      ""
      ((org-agenda-files idemacs-agenda-file-list)
       (org-agenda-span 'day)
       (org-todo-keywords-for-agenda '("TODO" "ASSIGNED" "CURRENT"))
       (org-done-keywords-for-agenda '("DONE" "TURNED-IN"))
       (org-agenda-time-grid nil)
       (org-agenda-overriding-header "           Daily Quest           ")
       (org-agenda-deadline-leaders '("Deadline:" "In %1d d." "%1d d. ago"))
       (org-agenda-prefix-format '((agenda . "  %i  %s ")))
       (org-super-agenda-groups
	'((:name " 󰟙  Quests  󰟙  "
		 :todo ("TODO" "ASSIGNED")
		 :order 1)
	  (:name " 󰼆  Tracked  󰼆  "
		:todo ("CURRENT")
		:order 3)
	  (:name " 󱋻  Completed  󱋻  "
		 :regexp "CLOSED:"
		 :order 4)
	  (:name " 󰼈  Side Quests  󰼈  "
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
       (org-todo-keywords-for-agenda '("TODO" "ASSIGNED" "CURRENT"))
       (org-done-keywords-for-agenda '("DONE" "TURNED-IN"))
       (org-agenda-time-grid nil)
       (org-agenda-overriding-header "        󰞟    School Agenda   󰞟          ")
       (org-agenda-deadline-leaders '("Due:" "In %1d d." "%1d d. ago"))
       (org-agenda-prefix-format '((agenda . "  %i %s %t ")))
       (org-super-agenda-groups
	'((:name "󱉟  Projects  󱉟  "
		 :tag "project")
	  (:name "  Assignments    "
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
       (org-todo-keywords-for-agenda '("TODO" "ASSIGNED" "CURRENT"))
       (org-done-keywords-for-agenda '("DONE" "TURNED-IN"))
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
    (org-agenda nil "d")))

(defun idemacs/view-school-agenda ()
  "This function is used to view the school quest."
  (interactive)
  (let ((org-agenda-custom-commands (list (idemacs/school-quest))))
    (org-agenda nil "s")))

(defun idemacs/view-agenda ()
  "This function is used to view the idemacs agenda."
  (interactive)
  (let ((org-agenda-custom-commands (list (idemacs/agenda-view))))
    (org-agenda nil "a")))

;; Org agenda capture templates
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

(defun idemacs/capture-school-class ()
  "This function is used to capture class information."
  (interactive)
  (let ((org-capture-templates (doct (list (idemacs/school-capture-template)))))
    (org-capture nil "sc")))

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

;; Interactive Functions






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

(defun idemacs/agenda-keybinds ()
  "This holds the keybinds that interact with 'org-mode' and 'org-agenda'.
This should be loaded after 'org-mode'."  
  (define-key org-agenda-mode-map (kbd "RET") 'org-agenda-open-link)
  (define-key global-map (kbd "M-I a d") 'idemacs/view-daily-quest)
  (define-key global-map (kbd "M-I a s") 'idemacs/view-school-agenda)
  (define-key global-map (kbd "M-I a a") 'idemacs/view-agenda))

(provide 'IDEmacs-agenda)
;;; IDEmacs-agenda.el ends here
