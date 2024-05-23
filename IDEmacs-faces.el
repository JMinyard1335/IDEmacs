;;; IDEmacs-faces.el --- Faces for IDEmacs -*- lexical-binding: t -*-

;; Author: Jachin Minyard
;; Maintainer: Jachin Minyard
;; Keywords: faces

;;; Commentary:

;; Faces for IDEmacs.

;;; Code:
(defgroup IDEmacs-faces nil
  "Faces for IDEmacs."
  :group 'IDEmacs)

(defface IDEmacs-sidebar-default
  '((t :inherit default))
  "Face for the default sidebar buffer.")

(defface IDEmacs-sidebar-buffer-link
  '((t :foreground "white" :background "black"))
  "Face for links in the sidebar buffer."
  :group 'IDEmacs-faces)

(provide 'IDEmacs-faces)
;;; IDEmacs-faces.el ends here
