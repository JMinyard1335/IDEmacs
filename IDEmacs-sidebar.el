;;; IDEmacs-sidebar.el --- Sidebar for org-mode files -*- lexical-binding: t -*-

;; Author: Jachin Minyard
;; Maintainer: Jachin Minyard
;; Version: 0.0.1

;;; Commentary:

;; This package provides a sidebar for org-mode files.

;;; Code:
(require 'subr-x)
(require 'ob-shell)

(defgroup IDEmacs-org-sidebar nil
  "Sidebar for org-mode files."
  :group 'IDEmacs)

(defcustom idemacs-sidebar-file "~/.emacs.d/IDEmacs/OrgFiles/Sidebar.org"
  "The file to use for the sidebar."
  :type 'string
  :group 'IDEmacs-org-sidebar)

(defcustom idemacs-sidebar-link-name "sidebar"
  "Default link name"
  :type 'string
  :group 'IDEmacs-org-sidebar)

(defcustom idemacs-sidebar-width 40
  "Width of the sidebar."
  :type 'integer
  :group 'IDEmacs-org-sidebar)

(defcustom idemacs-sidebar-lighter " SB"
  "Lighter for the sidebar."
  :type 'string
  :group 'IDEmacs-org-sidebar)

(org-link-set-parameters
 idemacs-sidebar-link-name
 :follow #'idemacs/sidebar-follow-link)

(defvar idemacs-sidebar-prev-local-keymap nil
  "Buffer local var to store the previous keymap.")

(make-variable-buffer-local 'idemacs-sidebar-prev-local-keymap)

(defvar idemacs-sidebar--async-update-in-progress nil
  "Flag to indicate that an async update is in progress.")

(make-variable-buffer-local 'idemacs-sidebar--async-update-in-progress)

;;;###autoload
(define-minor-mode idemacs-sidebar-mode
  "A minor mode for the sidebar"
  :lighter " sb"
  :init-value nil
  (if idemacs-sidebar-mode
      (progn
	(setq buffer-read-only t)
	;; Switch the keymap
	(setq idemacs-sidebar-prev-local-keymap (current-local-map))
	(use-local-map (make-composed-keymap (idemacs/sidebar-parse-keymap) (current-local-map)))
	;; If we are in the sidebar assign "RET" to `org-open-at-point'
	(if (string= (buffer-file-name) (expand-file-name idemacs-sidebar-file))
	    (local-set-key (kbd "RET") #'org-open-at-point))
	;; Updates the sidebar
	(idemacs/sidebar-update))
    (if idemacs-sidebar--async-update-in-progress
	(user-error "Update in progress, please wait."))
    ;; Return to the previous keymap 
    (use-local-map idemacs-sidebar-prev-local-keymap)
    ;; Turn off read only
    (setq buffer-read-only nil)))

(defun idemacs/sidebar-open ()
  "If the file exisits, open it, and run the minor mode on it."
  (if (file-exists-p idemacs-sidebar-file)
      (let ((sidebar-window (idemacs/sidebar-open-p)))
	(unless sidebar-window
	  (setq sidebar-window
		(display-buffer-in-side-window
		 (find-file-noselect idemacs-sidebar-file)
		 (list
		  (cons 'side 'left)
		  (cons 'window-width idemacs-sidebar-width)
		  (cons 'window-parameters
			(list
			 (cons 'no-delete-other-windows t)
			 (cons 'no-other-window  t)
			 (cons 'mode-line-format  nil)))))))
	(select-window sidebar-window)
	(idemacs-sidebar-mode))
    (message (format " %s does not exisit." idemacs-sidebar-file))))

(defun idemacs/sidebar-kill ()
  "Find any sidbar buffer and kill it."
  (let ((sidebar-window (idemacs/sidebar-open-p)))
    (when sidebar-window
      (delete-window sidebar-window)
      (kill-buffer (get-file-buffer idemacs-sidebar-file)))))

(defun idemacs/sidebar-toggle ()
  "Toggle the sidebar.
Open the sidebar if it is not open, close it if it is open.
Bound by default to `M-I s'."
  (interactive)
  (let ((sidebar-window (idemacs/sidebar-open-p)))
    (if sidebar-window
	(progn 
	  (idemacs/sidebar-kill)
	  (idemacs-sidebar-mode -1))
      (idemacs/sidebar-open))))

(defun idemacs/sidebar-open-p ()
  "Check if the sidebar is open."
  (let ((buffer (get-file-buffer idemacs-sidebar-file)))
    (when buffer
      (catch 'found
	(dolist (frame (frame-list))
	  (dolist (window (window-list frame))
	    (when (equal (window-buffer window) buffer)
	      (throw 'found window))))))))


(defun idemacs/sidebar-show-matches (query file header)
  "Shows the (HEADER) in the (FILE) that match the (QUERY)"
  (let ((org-agenda-overriding-header (concat header "\n"))
	(org-agenda-files file))
    (progn
      (kill-buffer "*Org Agenda(a)*")
      (org-tags-view nil query)
      (rename-buffer "*Org Agenda(a)*"))))

(defun idemacs/sidebar-count-items (query files)
  "Count the number of items the (QUERY) in the (FILES)."
  (length (org-map-entries nil query files)))

(defun idemacs/sidebar-agenda-day-view ()
  "Change to the other window close it and open `IDEmacs/view-daily-quest'."
  (if (get-buffer "*Org Agenda*")
      (progn
	(other-window 1)
	(kill-buffer "*Org Agenda*")
	(idemacs/view-daily-quest))
    (other-window 1)
    (idemacs/view-daily-quest)))

(defun idemacs/sidebar-agenda-school-view ()
  "Change to the other window close it and open `IDEmacs/view-school-agenda'."
  (if (get-buffer "*Org Agenda")
      (progn
	(other-window 1)
	(kill-buffer "*Org Agenda*")
	(idemacs/view-school-agenda))
    (other-window 1)
    (idemacs/view-school-agenda)))

(defun idemacs/sidebar-follow-link (path)
  (let* ((link (org-element-context))
	 (link-name (org-element-property :raw-link link))
	 (fields (split-string path "|"))
	 (query (nth 0 fields))
	 (files (nth 1 fields))
	 (fmt (nth 2 fields)))
    (message "query: %s, files: %s, fmt: %s" query files fmt)
    (cond
     ((string-equal query "daily-agenda")
      (idemacs/sidebar-agenda-day-view))
     ((string-equal query "school-agenda")
      (idemacs/sidebar-agenda-school-view))
     )))
      

  ;; (let* ((link (org-element-context))
  ;; 	 (query (string-trim (nth 0 (split-string path "[]|]"))))
  ;; 	 (files (nth 1 (split-string path "[]|]")))
  ;; 	 (files (if (> (length files) 0) (split-string files) org-agenda-files))
  ;; 	 (fmt (nth 2 (split-string path "[]|]")))
  ;; 	 (description (buffer-substring
  ;; 		       (org-element-property :contains-begin (org-element-context))
  ;; 		       (org-element-property :contains-end (org-element-context)))))
  ;;   (message "query: %s, files: %s, fmt: %s, description: %s" query files fmt description)
  ;;   (cond
  ;;    ((string-equal query "daily-agenda")
  ;;     (idemacs/sidebar-agenda-day-view))
  ;;    ((not fmt)
  ;;     (other-window 1)
  ;;     (org-agenda-show-matches query files description))
  ;;    ((and fmt (> (length fmt) 0))
  ;;     (idemacs/sidebar-update-link link)))))

(defun idemacs/sidebar-update-link (link)
  (let* ((path (org-element-property :path link))
	 (query (string-trim (nth 0 (split-string path "|"))))
	 (files (nth 1 (split-string path "|")))
	 (files (if (> (length files) 0) (split-string files) org-agenda-files))
	 (fmt (nth 2 (split-string path "|")))
	 (beg (org-element-property :contents-begin link))
	 (end (org-element-property :contents-end link))
	 (size (- end beg)))
    (if (and fmt (> (length fmt) 0))
	(let* ((output (idemacs/sidebar-count-items query files))
	       (output (format fmt output)))
	  (let ((modified (buffer-modified-p))
		(inhibit-read-only t))
	    (save-excursion
	      (delete-region beg end)
	      (goto-char beg)
	      (insert (if (<=(length output) size) output
			(make-string size ?+))))
	    (set-buffer-modified-p modified))))))

(defun idemacs/sidebar-update-all ()
  (idemacs/sidebar-clear-all)
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (when (string= (org-element-property :type link) idemacs-sidebar-link-name)
	(idemacs/sidebar-update-link link))))
  (redisplay t))

(defun idemacs/sidebar-clear-link (link)
  (let* ((path (org-element-property :path link))
	 (files (nth 1 (split-string path "|")))
	 (files (if (> (length files) 0) (split-string files) org-agenda-files))
	 (fmt (nth 2 (split-string path "|")))
	 (beg (org-element-property :contents-begin link))
	 (end (org-element-property :contents-end link))
	 (size (- end beg)))
    (if (and fmt (> (length fmt) 0))
	(let ((modified (buffer-modified-p))
	      (inhibit-read-only t))
	  (save-excursion
	    (delete-region beg end)
	    (goto-char beg)
	    (insert (format "(%s)" (make-string (max (- size 2) 0) ?-))))
	  (set-buffer-modified-p modified)))))

(defun idemacs/sidebar-clear-all ()
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (when (string= (org-element-property :type link) idemacs-sidebar-link-name)
	(idemacs/sidebar-clear-link link)))))

(defun idemacs/sidebar-update ()
  (interactive)
  (dolist (buffer (buffer-list (current-buffer)))
    (with-current-buffer buffer
      (if (bound-and-true-p IDEmacs-sidebar-mode)
	  (org-agenda-dashboard-update-all)))))

(defun idemacs/sidebar-parse-keymap ()
  (let ((map (make-sparse-keymap)))
    (org-element-map (org-element-parse-buffer) 'keyword
      (lambda (keyword)
	(when (string= (org-element-property :key keyword) "KEYMAP")
          (let* ((value (org-element-property :value keyword))
		 (key   (string-trim (nth 0 (split-string value "|"))))
		 (call  (string-trim (nth 1 (split-string value "|")))))
            (define-key map
	      (kbd key)
	      `(lambda () (interactive) ,(car (read-from-string (format "(%s)" call)))))
            (message "org-agenda-dashboard: binding %s to %s"
		     key
		     (format "(lambda () (interactive) (%s))" call))))))
    map))

(define-key global-map (kbd "M-I s") #'idemacs/sidebar-toggle)

(provide 'IDEmacs-sidebar)
;;; IDEmacs-sidebar.el ends here
