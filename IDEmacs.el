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
(require 'IDEmacs-orgfiles)
(require 'IDEmacs-user)
(require 'IDEmacs-agenda)
(require 'IDEmacs-sidebar)

;; Define The Main group in the package
(defgroup IDEmacs nil
  "This is my custom group"
  :group 'convenience)

(defun my-package-theme-list ()
  "Return a list of all the themes available in the package."
  (mapcar #'symbol-name
          (custom-available-themes)))

(defcustom IDEmacs-theme 'wombat
  "The theme to use for IDEmacs."
  :type `(choice ,@(mapcar (lambda (theme) `(const ,theme)) (my-package-theme-list)))
  :group 'IDEmacs)

(defcustom IDEmacs-font-size 120
  "The font size to use for IDEmacs."
  :type 'integer
  :group 'IDEmacs)

(defcustom IDEmacs-line-numbers t
  "Whether or not to display line numbers in Programing modes."
  :type 'boolean
  :group 'IDEmacs)

(defcustom IDEmacs-sidebar-on-startup t
  "Whether or not to open the sidebar on startup."
  :type 'boolean
  :group 'IDEmacs)

(defcustom IDEmacs-agenda-on-startup t
  "Whether or not to open the agenda on startup."
  :type 'boolean
  :group 'IDEmacs)

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

(defun IDEmacs--gui-setup ()
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (global-tab-line-mode 1)
  (display-time-mode 1)
  (display-battery-mode 1)
  
  (set-face-attribute 'default nil :height IDEmacs-font-size)
  
  (when IDEmacs-line-numbers
    (dolist (mode '(prog-mode-hook))
      (add-hook mode (lambda () (display-line-numbers-mode 1))))))

(defun IDEmacs--keybimds ()
  "Set up the keybindings for IDEmacs."
  (define-key IDEmacs-mode-map (kbd "C-c C-s") 'idemacs/sidebar-open)
  (define-key IDEmacs-mode-map (kbd "C-c C-q") 'idemacs/sidebar-toggle)
  (define-key IDEmacs-mode-map (kbd "C-c C-a") 'idemacs/view-agenda)
  (define-key IDEmacs-mode-map (kbd "C-c C-t") 'org-capture)
  (define-key IDEmacs-mode-map (kbd "C-;") 'hs-hide-block)
  (define-key IDEmacs-mode-map (kbd "C-'") 'hs-show-block)
  (define-key IDEmacs-mode-map (kbd "C-:") 'hs-hide-all)
  (define-key IDEmacs-mode-map (kbd "C-\"") 'hs-show-all))

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


(defun in-capture-buffer-p ()
  "Check if we're in a capture buffer."
  (string-prefix-p "CAPTURE" (buffer-name)))
	    



(provide 'IDEmacs)
;;; IDEmacs.el ends here
