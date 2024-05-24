;;; IDEmacs-faces.el --- Faces for IDEmacs -*- lexical-binding: t -*-

;; Author: Jachin Minyard
;; Maintainer: Jachin Minyard
;; Keywords: faces

;;; Commentary:

;; Faces for IDEmacs.

;;; Code:
;; Define the faces here. I am currently beign lazy and just using the default faces.
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
(defvar idemacs-yellow "#F9FFB5")
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
  '((t :inherit org-level-1))
  "Level 1 face for the sidebar."
  :group 'IDEmacs-faces)

(defface sidebar-lvl-2
  '((t :inherit org-lvl-2))
  "Level 2 face for the sidebar."
  :group 'IDEmacs-faces)

(defface sidebar-lvl-3
  '((t :inherit org-lvl-3))
  "Level 3 face for the sidebar."
  :group 'IDEmacs-faces)

(defface sidebar-lvl-4
 '((t :inherit org-lvl-4))
  "Level 4 face for the sidebar."
  :group 'IDEmacs-faces)

(defface sidebar-lvl-5
  '((t :inherit org-lvl-5))
  "Level 5 face for the sidebar."
  :group 'IDEmacs-faces)

(defface sidebar-lvl-6
  '((t :inherit org-level-6))
  "Level 6 face for the sidebar."
  :group 'IDEmacs-faces)

(defface sidebar-lvl-7
  '((t :inherit org-level-7))
  "Level 7 face for the sidebar."
  :group 'IDEmacs-faces)

(defface sidebar-lvl-8
  '((t :inherit org-level-8))
  "Level 8 face for the sidebar."
  :group 'IDEmacs-faces)

(defun idemacs/faces-sidebar-apply ()
  "Apply the sidebar faces."
  (idemacs/helper-make-face 'IDEmacs-sidebar-default :foreground idemacs-beige :height 100)
  (idemacs/helper-make-face 'IDEmacs-sidebar-link :foreground idemacs-yellow :height 100)
  (idemacs/helper-make-face 'IDEmacs-sidebar-drawer :foreground idemacs-red :height 100)
  (idemacs/helper-make-face 'sidebar-lvl-1 :foreground idemacs-blue :height 110)
  (idemacs/helper-make-face 'sidebar-lvl-2 :inherit 'sidebar-lvl-1)
  (idemacs/helper-make-face 'sidebar-lvl-3 :inherit 'sidebar-lvl-1)
  (idemacs/helper-make-face 'sidebar-lvl-4 :inherit 'sidebar-lvl-1)
  (idemacs/helper-make-face 'sidebar-lvl-5 :inherit 'sidebar-lvl-1)
  (idemacs/helper-make-face 'sidebar-lvl-6 :inherit 'sidebar-lvl-1)
  (idemacs/helper-make-face 'sidebar-lvl-7 :inherit 'sidebar-lvl-1)
  (idemacs/helper-make-face 'sidebar-lvl-8 :inherit 'sidebar-lvl-1)
  (face-remap-add-relative 'default 'IDEmacs-sidebar-default)
  (face-remap-add-relative 'org-link 'IDEmacs-sidebar-link)
  (face-remap-add-relative 'org-drawer 'IDEmacs-sidebar-drawer)
  (face-remap-add-relative 'org--level-1 'sidebar-lvl-1)
  (face-remap-add-relative 'org--level-2 'sidebar-lvl-2)
  (face-remap-add-relative 'org--level-3 'sidebar-lvl-3)
  (face-remap-add-relative 'org--level-4 'sidebar-lvl-4)
  (face-remap-add-relative 'org--level-5 'sidebar-lvl-5)
  (face-remap-add-relative 'org--level-6 'sidebar-lvl-6)
  (face-remap-add-relative 'org--level-7 'sidebar-lvl-7)
  (face-remap-add-relative 'org--level-8 'sidebar-lvl-8)
  )




(provide 'IDEmacs-faces)
;;; IDEmacs-faces.el ends here
