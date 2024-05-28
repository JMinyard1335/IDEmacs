;;; IDEmacs-faces.el --- Faces for IDEmacs -*- lexical-binding: t -*-

;; Author: Jachin Minyard
;; Maintainer: Jachin Minyard
;; Keywords: faces

;;; Commentary:

;; Faces for IDEmacs.

;;; Code:
;; Define the faces here. I am currently beign lazy and just using the default faces.

;(grey10 "#1a1a1a")
;(grey20 "#333333")
(defgroup IDEmacs-faces nil
  "Faces for IDEmacs."
  :group 'IDEmacs)

(defvar idemacs-green "#ACE1AF")
(defvar idemacs-purple "#dac4f7")
(defvar idemacs-violet "#d291e4")
(defvar idemacs-red "#FF8080")
(defvar idemacs-beige "#ebd2b4")
(defvar idemacs-blue "#acecf7")
(defvar idemacs-orange "#FCAE7C")
(defvar idemacs-yellow "#FFEA61")
(defvar idemacs-pink "#FF90BB")
(defvar idemacs-brown "#B99470")

(defface IDEmacs-default
  '((t :inherit default))
  "Default face for IDEmacs."
  :group 'IDEmacs-faces)

(defface IDEmacs-highlight
  '((t :inherit highlight))
  "Highlight face for IDEmacs."
  :group 'IDEmacs-faces)

(defface IDEmacs-sidebar-default
  '((t :inherit IDEmacs-default))
  "Default face for the sidebar."
  :group 'IDEmacs-faces)

(defface IDEmacs-sidebar-link
  '((t :inherit link))
  "Link face for the sidebar."
  :group 'IDEmacs-faces)

(defface IDEmacs-sidebar-drawer
  '((t :inherit IDEmacs-sidebar-default))
  "Drawer face for the sidebar."
  :group 'IDEmacs-faces)

(defface sidebar-lvl-1
  '((t :inherit org-level-1 :height 130))
  "Level 1 face for the sidebar."
  :group 'IDEmacs-faces)

(defface sidebar-lvl-2
  '((t :inherit sidebar-lvl-1 :underline t))
  "Level 2 face for the sidebar."
  :group 'IDEmacs-faces)

(defface sidebar-lvl-3
  '((t :inherit sidebar-lvl-1 :underline t))
  "Level 3 face for the sidebar."
  :group 'IDEmacs-faces)

(defface sidebar-lvl-4
 '((t :inherit sidebar-lvl-1 :underline t))
  "Level 4 face for the sidebar."
  :group 'IDEmacs-faces)

(defface sidebar-lvl-5
  '((t :inherit sidebar-lvl-1))
  "Level 5 face for the sidebar."
  :group 'IDEmacs-faces)

(defface sidebar-lvl-6
  '((t :inherit sidebar-lvl-1))
  "Level 6 face for the sidebar."
  :group 'IDEmacs-faces)

(defface sidebar-lvl-7
  '((t :inherit sidebar-lvl-1))
  "Level 7 face for the sidebar."
  :group 'IDEmacs-faces)

(defface sidebar-lvl-8
  '((t :inherit sidebar-lvl-1))
  "Level 8 face for the sidebar."
  :group 'IDEmacs-faces)

(defface idemacs-agenda-default
  '((t :inherit IDEmacs-default))
  "Default agenda face"
  :group 'IDEmacs-Agenda)

(defface idemacs-agenda-structure
  '((t :foreground "#dac4f7" :box (:line-width 2)))
  "Agenda structure face"
  :group 'IDEmacs-Agenda)

(defface idemacs-todays-date
  '((t :foreground "#acecf7" :box (:line-width 2)))
  "Face for today's date in the agenda"
  :group 'IDEmacs-Agenda)

(defface idemacs-done-keys
  '((t :foreground "#ACE1AF" :inverse-video t :bold t :box (:line-width 2 :color "#ACE1AF")))
  "Face for done tasks in the agenda"
  :group 'IDEmacs-Agenda)
(defface idemacs-done-task
  '((t :foreground "#ACE1AF" :bold t :box (:line-width 2)))
  "Face for done tasks in the agenda"
  :group 'IDEmacs-Agenda)

(defface idemacs-todo-keys
  '((t :foreground "#FF8080" :inverse-video t :bold t :box (:line-width 2 :color "#FF8080")))
  "Face for todo tasks in the agenda"
  :group 'IDEmacs-Agenda)
(defface idemacs-todo-task
  '((t :foreground "#FF8080" :bold t :box (:line-width 2 :color)))
  "Face for todo tasks in the agenda"
  :group 'IDEmacs-Agenda)

(defface idemacs-imminent-deadline
  '((t :foreground "#FF8080" :bold t :box (:line-width 2)))
  "Face for tasks with an imminent deadline in the agenda"
  :group 'IDEmacs-Agenda)
(defface idemacs-upcoming-deadline
  '((t :foreground "#FCAE7C" :box (:line-width 2)))
  "Face for tasks with an upcoming deadline in the agenda"
  :group 'IDEmacs-Agenda)
(defface idemacs-distant-deadline
  '((t :foreground "#FDFD96" :bold t :box (:line-width 2)))
  "Face for tasks with a distant deadline in the agenda"
  :group 'IDEmacs-Agenda)

(defface idemacs-calender-event
  '((t :foreground "#ebd2b4" :box (:line-width 2)))
  "Face for calender events in the agenda"
  :group 'IDEmacs-Agenda)

(defun idemacs/faces-sidebar-apply ()
  "Apply the sidebar faces."
  (interactive)
  (face-remap-add-relative 'org-link 'IDEmacs-sidebar-link)
  (face-remap-add-relative 'org-drawer 'IDEmacs-sidebar-drawer)
  (face-remap-add-relative 'default 'IDEmacs-sidebar-default)
  (face-remap-add-relative 'org-level-1 'sidebar-lvl-1)
  (face-remap-add-relative 'org-level-2 'sidebar-lvl-2)
  (face-remap-add-relative 'org-level-3 'sidebar-lvl-3)
  (face-remap-add-relative 'org-level-4 'sidebar-lvl-4)
  (face-remap-add-relative 'org-level-5 'sidebar-lvl-5)
  (face-remap-add-relative 'org-level-6 'sidebar-lvl-6)
  (face-remap-add-relative 'org-level-7 'sidebar-lvl-7)
  (face-remap-add-relative 'org-level-8 'sidebar-lvl-8))

(defun idemacs/faces-agenda-apply ()
  "Apply the agenda faces."
  (interactive)
  (face-remap-add-relative 'org-agenda-structure 'idemacs-agenda-structure)
  (face-remap-add-relative 'default 'idemacs-agenda-default)
  (face-remap-add-relative 'org-agenda-date-today 'idemacs-todays-date)
  (face-remap-add-relative 'org-modern-done 'idemacs-done-keys)
  (face-remap-add-relative 'org-agenda-done 'idemacs-done-task)
  (face-remap-add-relative 'org-modern-todo 'idemacs-todo-keys)
  (face-remap-add-relative 'org-imminent-deadline 'idemacs-imminent-deadline)
  (face-remap-add-relative 'org-upcoming-deadline 'idemacs-upcoming-deadline)
  (face-remap-add-relative 'org-upcoming-distant-deadline 'idemacs-distant-deadline)
  (face-remap-add-relative 'org-agenda-calendar-event 'idemacs-calender-event)
)

(provide 'IDEmacs-faces)
;;; IDEmacs-faces.el ends here
