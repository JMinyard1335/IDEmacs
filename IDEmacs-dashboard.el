;;; IDEmacs-dashboard.el --- Dashboard config for IDEmacs -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'subr-x)
(require 'ob-shell)
(use-package dashboard :ensure t)


;; (defvar idemacs-orange "#FCAE7C")
;; (defvar idemacs-yellow "#F9FFB5")
;; (defvar idemacs-pink "#FF90BB")
;; (defvar idemacs-brown "#B99470")
;; (defvar idemacs-purple "#dac4f7")
;; (defvar idemacs-viloet "#d291e4")
;; (defvar idemacs-red "#FF8080")
;; (defvar idemacs-beige "#ebd2b4")


(defgroup IDEmacs-dashboard nil
  "Dashboard config for IDEmacs."
  :group 'IDEmacs)

(defface idemacs-dashboard-welcome-message
  '((t :foreground "#F9FFB5" :height 1.2))
  "my custom face") 

(defface idemacs-dashboard-heading
  '((t :foreground "#acecf7" :height 1.1))
  "my custom face")

(defface idemacs-dashboard-subtext
  '((t :foreground "#d291e4" :height 1.0))
  "my custom face")

(defface idemacs-dashboard-default
  '((t :foreground "#ebd2b4" :height 1.0))
  "my custom face")

(defun idemacs/dashboard-apply-faces ()
  "Apply faces for dashboard."
  (face-remap-add-relative 'dashboard-banner-logo-title 'idemacs-dashboard-welcome-message)
  (face-remap-add-relative 'dashboard-heading 'idemacs-dashboard-heading)
  (face-remap-add-relative 'font-lock-comment-face 'idemacs-dashboard-subtext)
  (face-remap-add-relative 'default 'idemacs-dashboard-default)
  )

(add-hook 'dashboard-mode-hook 'idemacs/dashboard-apply-faces)



(provide 'IDEmacs-dashboard)
;;; IDEmacs-dashboard.el ends here
			    
