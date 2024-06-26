;;; IDEmacs.el --- Emacs configuration for IDE-like features -*- lexical-binding: t -*-

;; Author: Jachin Minyard
;; Maintainer: Jachin Minyard
;; Version: 0.0.1

;;; Commentary:

;; This is a configuration for Emacs that provides IDE-like features.

;;; Code:
;;required packages
(require 'org)
(require 'org-agenda)
(require 'org-capture)


;; Load the subpackages
(add-to-list 'load-path (file-name-directory "~/Documents/IDEmacs/"))
(require 'IDEmacs-helpers)
(require 'IDEmacs-faces)
(require 'IDEmacs-gui-setup)
(require 'IDEmacs-orgfiles)
(require 'IDEmacs-user)
(require 'IDEmacs-agenda)
(require 'IDEmacs-sidebar)

;; Define The Main group in the package
(defgroup IDEmacs nil
  "This is my custom group"
  :group 'convenience)

(defvar IDEmacs-mode-map (make-sparse-keymap)
  "Keymap for IDEmacs mode.")

(defvar idemacs-previous-theme nil
  "The previous theme that was used.")

(defvar idemacs-loading-theme nil
  "Whether or not we're loading the theme.")

(defun IDEmacs--enable-theme ()
  (setq idemacs-loading-theme t)
  (when idemacs-previous-theme
    (let ((custom--inhibit-theme-enable nil))
      (disable-theme (car custom-enabled-themes))))
  (let ((custom--inhibit-theme-enable nil))
    (load-theme IDEmacs-theme t))
  (setq idemacs-loading-theme nil))

(defun IDEmacs--disable-theme ()
  (setq idemacs-loading-theme t)
  (when idemacs-previous-theme
    (let ((custom--inhibit-theme-enable nil)
	  (temp-theme nil))
      (load-theme idemacs-previous-theme t)))
  (setq temp-theme IDEmacs-theme)
  (let ((custom--inhibit-theme-enable nil))
    (disable-theme IDEmacs-theme))
  (setq IDEmacs-theme temp-theme)
  (setq idemacs-loading-theme nil))

(defun IDEmacs--inital-display ()
  (when IDEmacs-agenda-on-startup
    (when (get-buffer "*Org Agenda*")
      (kill-buffer "*Org Agenda*")
      (iemacs/view-agenda))
    (idemacs/view-agenda))
  (when IDEmacs-sidebar-on-startup
    (unless (idemacs/sidebar-open-p)
      (idemacs/sidebar-open))))

(defun IDEmacs--revert-display ()
  (when (get-buffer "*Org Agenda*")
    (kill-buffer "*Org Agenda*"))
  (when (idemacs/sidebar-open-p)
    (idemacs/sidebar-toggle)))

(defun IDEmacs--capture-setup ()
  "Set up the initial configuration for IDEmacs."
  (setq org-capture-templates
	(doct
	 (list
	  (idemacs/capture-project-template)
	  (idemacs/capture-school-template)
	  (idemacs/capture-personal-template)
	  (idemacs/capture-home-template)
	  (idemacs/capture-sidebar-entries)))))

(defun IDEmacs--agenda-setup ()
  "Set up the initial configuration for the agenda."
  (setq org-custom-commands
	(list (idemacs/daily-quest)
	      (idemacs/school-quest)
	      (idemacs/agenda-view))))
 
(defun IDEmacs--keybimds ()
  "Set up the keybindings for IDEmacs."
  (define-key IDEmacs-mode-map (kbd "C-c C-s") 'idemacs/sidebar-toggle)
  (define-key IDEmacs-mode-map (kbd "C-c C-a") 'idemacs/view-agenda)
  (define-key IDEmacs-mode-map (kbd "C-c C-t") 'org-capture)
  (define-key IDEmacs-mode-map (kbd "C-;") 'hs-hide-block)
  (define-key IDEmacs-mode-map (kbd "C-'") 'hs-show-block)
  (define-key IDEmacs-mode-map (kbd "C-:") 'hs-hide-all)
  (define-key IDEmacs-mode-map (kbd "C-\"") 'hs-show-all)
  (define-key IDEmacs-mode-map (kbd "C-c s") 'flyspell-correct-word-before-point))

;;;###autoload
(define-minor-mode idemacs-mode
  "Toggle IDEmacs mode."
  :init-value nil
  :lighter " IDEmacs"
  :keymap IDEmacs-mode-map
  :global t
  (unless idemacs-loading-theme
    (if idemacs-mode
	(progn
	  ;; Set up the mode
	  (setq idemacs-previous-theme (car custom-enabled-themes))
	  (message "Welcome to IDEmacs!")
	  (IDEmacs--capture-setup)
	  (IDEmacs--agenda-setup)
	  (IDEmacs--gui-setup)
	  (IDEmacs--keybimds)
	  (idemacs/faces-sidebar-apply)
	  ;; Set up the theme
	  (IDEmacs--enable-theme)
	  ;; Set up the Default view
	  (IDEmacs--inital-display))
      ;; Set up closing the mode
      (progn
	(message "Goodbye IDEmacs!")
	;; Restore the previous theme
	(IDEmacs--disable-theme)
	;; Close the agenda windows and the sidebar
	(IDEmacs--revert-display)
	)
      (setq idemacs-mode nil)
      )))

(add-hook 'after-capture-finalize-hook 'idemacs/helper-kill-calendar)


(define-key global-map [f2] 'idemacs-mode)

(provide 'IDEmacs)
;;; IDEmacs.el ends here
