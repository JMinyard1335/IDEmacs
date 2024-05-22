;;; idemacs-user.el --- User information -*- lexical-binding: t -*-

;;; Commentary:

;; This file contains functions and variables that are used to store user

;;; Code:
(defcustom idemacs-user-info '(:name nil :age nil :email nil :job nil)
  "Holds the user info."
  :type '(plist
	  :inline t
	  :options (:name :age :email :job)
          :tag "User Info"
          :doc "Stores the name and age of the user"
          (string :tag "Name" :key :name)
          (integer :tag "Age" :key :age)
	  (string :tag "Email" :key :email)
	  (string :tag "Job" :key :job))
  :group 'IDEmacs)

(defun idemacs/user-set (name &optional age email job)
  "Set the `idemacs-user-info' variable to the user's (NAME), (AGE), (EMAIL), and (JOB).
This function can be called interactively or programmatically. when called
interactively, the user will be prompted to enter their (NAME). This is required
after that the function will prompt the user if they want to enter the additional
infromation.

When called programmatically, the user must pass in a (NAME).
The user can also pass in an (AGE), (EMAIL), and (JOB).
Example:
  (idemacs/user-set \"John Doe\" 25 \"example.email@host.com\" \"Wizard\")"
  (interactive "sEnter your name: ")
  (plist-put idemacs-user-info :name name)
  (if (called-interactively-p 'interactive)
      (progn
	(idemacs/user-set-age)
	(idemacs/user-set-email)
	(idemacs/user-set-job))
    (when age (plist-put idemacs-user-info :age age))
    (when email (plist-put idemacs-user-info :email email))
    (when job (plist-put idemacs-user-info :job job)))
  (message "User info set to: %s" idemacs-user-info))

(defun idemacs/reset-user ()
  "Reset the user information."
  (interactive)
  (setq idemacs-user-info '(:name nil :age nil :email nil :job nil))
  (message "User info reset."))

(defun idemacs/user-set-age ()
  "Sets the user's age. This function was designed to be called from `idemacs/user-set'.
This function is called whenever the user has called the `idemacs/user-set' function.
Interactively, this function will prompt the user to set their age."
  (if (y-or-n-p "Do you want to set your age? ")
      (let ((age (read-number "Enter your age: ")))
	(plist-put idemacs-user-info :age age))
    (message "Age not set.")))

(defun idemacs/user-set-email ()
  "Sets the user's email. This function was designed to be called from `idemacs/user-set'.
This function is called whenever the user has called the `idemacs/user-set' function.
Interactively, this function will prompt the user to set their email."
  (if (y-or-n-p "Do you want to set your email? ")
      (let ((email (read-string "Enter your email: ")))
	(plist-put idemacs-user-info :email email))
    (message "Email not set.")))

(defun idemacs/user-set-job ()
  "Sets the user's job. This function was designed to be called from `idemacs/user-set'.
This function is called whenever the user has called the `idemacs/user-set' function.
Interactively, this function will prompt the user to set their job."
  (if (y-or-n-p "Do you want to set your job? ")
      (let ((job (read-string "Enter your job: ")))
	(plist-put idemacs-user-info :job job))
    (message "Job not set.")))

(provide 'IDEmacs-user)
;;; idemacs-user.el ends here
