;;; IDEmacs-gui-setup.el --- GUI setup for IDEmacs -*- lexical-binding: t -*-

;; Author: Jachin Minyard
;; Maintainer: Jachin Minyard
;; Version: 0.0.1

;;; Commentary:

;; This file sets up the IDEmacs gui variables. Most of this is just me assigning variables.
;; Most of these variables are built in to Emacs I am just setting them to my preferences.
;; Having these variables allows me to easily change the look of the GUI.
;; It also allows me to easily change the look back to its original view before my mode was ran.

;;; Code:
(defgroup IDEmacs-gui nil
  "GUI settings for IDEmacs."
  :group 'IDEmacs)

(defcustom IDEmacs-theme 'wombat
  "The theme to use for IDEmacs."
  :type 'symbol
  :group 'IDEmacs-gui)

(defcustom IDEmacs-font-name "VictorMono Nerd Font"
  "The font to use for IDEmacs."
  :type 'string
  :group 'IDEmacs-gui)

(defcustom IDEmacs-font-size 120
  "The font size to use for IDEmacs."
  :type 'integer
  :group 'IDEmacs-gui)

(defcustom IDEmacs-line-numbers t
  "Whether or not to display line numbers in Programing modes."
  :type 'boolean
  :group 'IDEmacs)

(defcustom IDEmacs-menu-bar -1
  "Whether or not to display the menu bar."
  :type '(choice (const :tag "Turn Menu bar off" -1)
		(const :tag "Turn menu bar on" 1))
  :group 'IDEmacs-gui)

(defcustom IDEmacs-tool-bar -1
  "Whether or not to display the tool bar."
  :type '(choice (const :tag "Turn Tool bar off" -1)
		(const :tag "Turn Tool bar on" 1))
  :group 'IDEmacs-gui)

(defcustom IDEmacs-scroll-bar -1
  "Whether or not to display the scroll bar."
  :type '(choice (const :tag "Turn Scroll bar off" -1)
		(const :tag "Turn Scroll bar on" 1))
  :group 'IDEmacs-gui)

(defcustom IDEmacs-tab-bar 1
  "Whether or not to display the tab bar."
  :type '(choice (const :tag "Turn Tab bar off" -1)
		(const :tag "Turn Tab bar on" 1))
  :group 'IDEmacs-gui)

(defcustom IDEmacs-display-battery -1
  "Whether or not to display the battery in the mode line.
This is off by default since desktops don't have batteries.
Turn this on if you are using a laptop and wnat to see the battery status."
  :type '(choice (const :tag "Turn Battery mode on" 1)
		 (const :tag "Turn Battery mode off" -1))
  :group 'IDEmacs-gui)

(defcustom IDEmacs-display-time 1
  "Whether or not to display the time in the mode line."
  :type '(choice (const :tag "Turn Time on" 1)
		(const :tag "Turn Time off" -1))
  :group 'IDEmacs-gui)

(defcustom IDEmacs-sidebar-on-startup t
  "Whether or not to open the sidebar on startup."
  :type 'boolean
  :group 'IDEmacs-gui)

(defcustom IDEmacs-agenda-on-startup t
  "Whether or not to open the agenda on startup."
  :type 'boolean
  :group 'IDEmacs-gui)

(defun IDEmacs--gui-setup ()
  (menu-bar-mode IDEmacs-menu-bar)
  (tool-bar-mode IDEmacs-tool-bar)
  (scroll-bar-mode IDEmacs-scroll-bar)
  (global-tab-line-mode IDEmacs-tab-bar)
  (display-time-mode IDEmacs-display-time)
  (display-battery-mode IDEmacs-display-battery)
  (set-face-attribute 'default nil :family IDEmacs-font-name :height IDEmacs-font-size)
  (when IDEmacs-line-numbers
    (dolist (mode '(prog-mode-hook))
      (add-hook mode (lambda () (display-line-numbers-mode 1))))))

(provide 'IDEmacs-gui-setup)
;;; IDEmacs-gui-setup.el ends here
