(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("melpa" . "http://melpa.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t)

;;Disables the startup message
(setq inhibit-startup-message t)

;; Allows you to customize the size and width to great precision and which frame to modify.
;; (selected-frame) can be changed to the desired frame.
;; the two parameters after the frame selection are height and width respectivly
;; width is in characters and height is in lines.  
;;(if (window-system) (set-frame-size (selected-frame) 124 40))

;; If you dont care about tweaking the height and width the following commands will work just fine
;; uncomment this linestart the initial frame maximized
;;(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; uncomment this line to start every frame maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(set-face-attribute 'default nil :height 150)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 150)
;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 170 :weight 'regular)

;;Enables Global line numbers on the left side of the screen
(global-display-line-numbers-mode)

;; adds line numbers to the specified modes below. DO NOT USE IN CONJUNCTION WITH GLOBAL LINE MODE AND THE EXCLUSION LIST. pick one method or the other
;;(dolist (mode '(c-mode-hook
  ;;              c++-mode-hook
  ;;              java-mode-hook
  ;;              python-mode-hook))
  ;;(add-hook mode (lambda () (display-line-numbers-mode 1))))

;;Creates Exclusion to remove certin modes from being effected by (global-display-line-numbers-mode)
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Disables tool bar
(tool-bar-mode -1)

;;Disables menu bar
(menu-bar-mode -1)

;;Disables scroll Bar
(scroll-bar-mode -1)

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :custom
  ((doom-modeline-height 40)))

;;Installs the doom color themes
(use-package doom-themes)

;;Sets the color theme
(load-theme 'doom-rouge t)

;;colors delimiters for easy matching
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(defun IDEmacs-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  ;;you can change the numerical value to change the size of the geading levels.
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-ofixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun IDEmacs-org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (company-mode 1)
  (flyspell-mode 1))

;;Org-mode package and its built in configs
(use-package org
  :hook 
  (org-mode . IDEmacs-org-mode-setup)
  (org-mode . IDEmacs-org-babel-tangle-config)
  :config
  (setq org-ellipsis " ")
  ;; User defined functions found below add to make the set up code look cleaner
  (IDEmacs-org-agenda-setup)
  (IDEmacs-org-custom-keywords)
  (IDEMacs-org-habits)
  (IDEmacs-org-task-refiling)
  (IDEmacs-org-capture-templates)
  (IDEmacs-font-setup))

(defun IDEmacs-org-custom-keywords ()
      (setq org-todo-keywords
            '((sequence "CURRENT(c)" " TODO(t)" "NEXT(n)" "|" "DONE(d!)")))

      ;;Creates a custom list of tags
      (setq org-tag-alist
            '((:startgroup)
              ;; Put mutually exclusive tags here
              (:endgroup)
              ("@errand" . ?E)
              ("@home" . ?H)
              ("@work" . ?W)
              ("agenda" . ?a)
              ("planning" . ?p)
              ("publish" . ?P)
              ("batch" . ?b)
              ("note" . ?n)
              ("idea" . ?i))))

(defun IDEmacs-org-agenda-setup ()
      (setq org-agenda-files
            '("~/Documents/Projects/Code/Org-Mode/Schedule/Task.org")
            '("~/Documents/School/AssignmentSchedule.org"))
      (setq org-deadline-warning-days 7)
      (setq org-agenda-start-with-log-mode t)
      (setq org-log-done 'time)
      (setq org-log-into-drawer t)

      ;; Configure custom agenda views
      (setq org-agenda-custom-commands
            '(("d" "Dashboard"
               ((agenda "" ((org-deadline-warning-days 7)))
                (todo "NEXT"
                      ((org-agenda-overriding-header "Next Tasks")))
                (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))
	    
              ("n" "Next Tasks"
               ((todo "NEXT"
                      ((org-agenda-overriding-header "Next Tasks")))))
	    
              ("W" "Work Tasks" tags-todo "+work-email")

              ;; Low-effort next actions
              ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
               ((org-agenda-overriding-header "Low Effort Tasks")
                (org-agenda-max-todos 20)
                (org-agenda-files org-agenda-files))))))

;;Enables habit mode
(defun IDEMacs-org-habits ()
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60))

;;Allows you to move completed task from the task file to the archive file
(defun IDEmacs-org-task-refiling ()
  (setq org-refile-targets
        '(("Archive.org" :maxlevel . 1)
          ("Tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers))

(defun IDEmacs-org-capture-templates ()
  (setq org-capture-templates
        `(("t" "Tasks / Projects")
          ("tt" "Task" entry (file+olp "~/Documents/Projects/Code/Org-Mode/Schedule/Task.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

          ("j" "Journal Entries")
          ("jj" "Journal" entry
           (file+olp+datetree "~/Documents/Projects/Code/Org-Mode/Schedule/Journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
          ("jm" "Meeting" entry
           (file+olp+datetree "~/Documents/Projects/Code/Org-Mode/Schedule/Journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

          ("w" "Workflows")
          ("we" "Checking Email" entry (file+olp+datetree "~/Documents/Projects/Code/Org-Mode/Schedule/Journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1))))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (C . t)
   (java .t)))

;; Automatically tangle our Emacs.org config file when we save it
(defun IDEmacs-org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
  (expand-file-name "~/Documents/Projects/Code/Org-Mode/IDEmacs/IDEmacs.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'IDEmacs-org-babel-tangle-config)))

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("j" . "src java"))

(use-package which-key
  :init
  (which-key-mode)
  :diminish
  which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package flycheck
  :hook
  (prog-mode . flycheck-mode))

(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                emacs-lisp-mode-hook))
  (add-hook mode (lambda () (flycheck-mode 0))))

(use-package company
  :hook
  (prog-mode . company-mode)
  (org-mode . company-mode)
  :config
  (setq company-idle-delay 0.1)  
  (setq company-minimum-prefix-length 1))

(use-package company-box
  :hook
  (company-mode . company-box-mode))

;;(dolist (mode '())
  ;;(add-hook mode (lambda () (company-mode 0))))

(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (treemacs-git-mode 'extended)
  (setq treemacs-width 20)
  (setq treemacs-indention 3)
  :bind
  ;; global map to pull up the currently displayed project.
  (:map global-map
        ("C-c q" . treemacs)))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package projectile
  :diminish
  projectile-mode
  :config
  ;; Turns on global projectile
  (projectile-mode)
  :custom
  ((projectile-completion-system 'company))
  :bind-keymap
  ;; Sets C-c p to open projectile commands
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Documents/Projects/Code")
    (setq projectile-project-search-path '("~/Documents/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package magit)

(use-package lsp-mode
  :commands
  (lsp lsp-deferred)
  :hook
  (c-mode . lsp-mode)
  (c++-mode . lsp-mode)
  (python-mode . lsp-mode)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui)

(use-package lsp-treemacs
:after lsp
:config
(lsp-treemacs-sync-mode 1)
(setq lsp-treemacs-width 20)
:bind (:map global-map ("C-c t" . lsp-treemacs-symbols)))

(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))
(use-package helm-lsp)
(use-package helm
  :config (helm-mode))

(use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
(use-package dap-java :ensure nil)

;;This is a useful keybind that allows ESC to exit out of prompts.
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Sets up Unique keybinds for Org-Mode  
(define-key global-map (kbd "C-c j")
    (lambda () (interactive) (org-capture nil "jj")))
(define-key global-map (kbd "C-c l")
    (lambda () (interactive) (org-capture nil "tt")))
(define-key global-map (kbd "C-c a")
  (lambda () (interactive) (org-agenda-list "a")))
