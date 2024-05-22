;;; IDEmacs.el --- Emacs configuration for IDE-like features -*- lexical-binding: t -*-

;;; Commentary:

;; This is a configuration for Emacs that provides IDE-like features.

;;; Code:

(add-to-list 'load-path (file-name-directory "~/Documents/IDEmacs/"))
;; Load the required packages
(use-package dashboard :ensure t)
(use-package olivetti
  :ensure t
  :hook
  (org-agenda-mode . olivetti-mode))
(use-package org-super-agenda :ensure t)
(use-package org-modern :ensure t)
(use-package doct
  :ensure t
  :commands (doct))

(defun idemacs-setup-org-agenda ()
  "This function is used to setup the org-agenda."
  (setq org-agenda-files idemacs-agenda-file-list)
  (setq org-agenda-window-setup 'only-window)
  (setq org-log-done '('time))
  (setq org-log-into-drawer t)
  (setq org-deadline-warning-days 7)
  (setq org-popup-calendar-for-date-prompt nil)
  (setq org-agenda-hide-tags-regexp ".*"))
(use-package org
  :init (setq inhibit-compacting-font-caches t)
  :hook
  (org-agenda-mode . idemacs-agenda-mode)
  (org-agenda-mode . org-super-agenda-mode)
  :config
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
  (idemacs-setup-org-agenda))


;; Define The Main group in the package
(defgroup IDEmacs nil
  "This is my custom group"
  :group 'convenience)

;; Load the subpackages
(require 'IDEmacs-helpers)
(require 'IDEmacs-user)
(require 'IDEmacs-agenda)
(require 'IDEmacs-dashboard)
(require 'IDEmacs-sidebar)


;; Start up functions
(idemacs/agenda-keybinds)

(provide 'IDEmacs)
;;; IDEmacs.el ends here
