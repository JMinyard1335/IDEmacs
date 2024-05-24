;;; IDEmacs-orgfiles.el --- Agenda configuration for IDEmacs

;; Author: Jachin Minyard
;; Maintainer: Jachin Minyard
;; version: 0.0.1

;;; Commentary:

;; This file contains all the configuration for the agenda files in IDEmacs.

;;; Code:

;;idemacs/personal
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
      (idemacs/helper-create-file file idemacs-personal-template))
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
      (idemacs/helper-create-file file idemacs-refile-template))
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

(provide 'IDEmacs-orgfiles)
;;; IDEmacs-orgfiles.el ends here
