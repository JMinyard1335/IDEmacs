;;; IDEmacs-sidebar.el --- Sidebar for org-mode files -*- lexical-binding: t -*-

;; Author: Jachin Minyard
;; Maintainer: Jachin Minyard
;; Version: 0.0.1

;;; Commentary:

;; This package provides a sidebar for IDEmacs.

;;; Code:
(require 'subr-x)
(require 'ob-shell)
(require 'IDEmacs-faces)

(defgroup IDEmacs-sidebar nil
  "Sidebar for org-mode files."
  :group 'IDEmacs)

(defcustom idemacs-sidebar-file "~/.emacs.d/IDEmacs/OrgFiles/Sidebar.org"
  "The file to use for the sidebar."
  :type 'string
  :group 'IDEmacs-sidebar)

(defcustom idemacs-sidebar-template
  "* Schedules:\n [[sidebar:daily-agenda][Daily Quest]]\n [[sidebar:school-agenda][School Agenda]]\n\n* Config\n [[IDEmacs_file:~/.emacs.d/init.el][Init File]]\n"
  "The template to be writen to the idemacs sidebar file on creation."
  :type 'string
  :group 'IDEmacs-sidebar)

(defcustom idemacs-sidebar-link-name "sidebar"
  "Default link name"
  :type 'string
  :group 'IDEmacs-org-sidebar)

(defcustom idemacs-sidebar-width 30
  "Width of the sidebar."
  :type 'integer
  :group 'IDEmacs-org-sidebar)

(defcustom idemacs-sidebar-lighter " SB"
  "Lighter for the sidebar."
  :type 'string
  :group 'IDEmacs-org-sidebar)

(defvar idemacs-sidebar-prev-local-keymap nil
  "Buffer local var to store the previous keymap.")

(defvar idemacs-sidebar--async-update-in-progress nil
  "Flag to indicate that an async update is in progress.")

(make-variable-buffer-local 'idemacs-sidebar--async-update-in-progress)

(make-variable-buffer-local 'idemacs-sidebar-prev-local-keymap)

(org-link-set-parameters
 idemacs-sidebar-link-name
 :follow #'idemacs/sidebar-follow-link)

;;;###autoload
(define-minor-mode idemacs-sidebar-mode
  "A minor mode for the sidebar"
  :lighter " sb"
  :init-value nil
  (if idemacs-sidebar-mode
      (progn
	(org-cycle '(2))
	(setq buffer-read-only t)
	;; Switch the keymap
	(setq idemacs-sidebar-prev-local-keymap (current-local-map))
	(use-local-map (make-composed-keymap (idemacs/sidebar-parse-keymap) (current-local-map)))
	;; If we are in the sidebar assign "RET" to `org-open-at-point'
	(if (string= (buffer-file-name) (expand-file-name idemacs-sidebar-file))
	    (local-set-key (kbd "RET") #'org-open-at-point))
	;; Updates the sidebar
	(idemacs/sidebar-update))
    ;; Return to the previous keymap 
    (use-local-map idemacs-sidebar-prev-local-keymap)
    ;; Turn off read only
    (setq buffer-read-only nil)))

(defun idemacs/sidebar-set-path (file)
  "Set `idemacs-personal-path' to (FILE)."
  (interactive "FSelect a path: ")
  (if (not (file-exists-p file))
      (idemacs/helper-create-file file idemacs-sidebar-template))
  (setq idemacs-sidebar-file file)
  (message "Sidebar file set to %s" idemacs-sidebar-file))

(defun idemacs/sidebar-reformat-file ()
  "Reformat the sidebar file."
  (interactive)
  (if (y-or-n-p (format "Would you like to reformat %s? " idemacs-sidebar-file))
      (progn (with-temp-buffer
	       (write-region idemacs-sidebar-template nil idemacs-sidebar-file)
	       (save-buffer)
	       (kill-buffer))
	     (message "%s reformated sucessfully" idemacs-sidebar-file))
      (message "%s was not reformated" idemacs-sidebar-file)))

(defun idemacs/sidebar-open ()
  "If the file exisits, open it, and run the minor mode on it."
  (interactive)
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
			 (cons 'no-other-window nil)
			 (cons 'tab-line-format 'none)
			 (cons 'mode-line-format 'none)
			 (cons 'header-line-format 'none)))))))
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
  (if (get-buffer "*Org Agenda*")
      (progn
	(other-window 1)
	(kill-buffer "*Org Agenda*")
	(idemacs/view-school-agenda))
    (other-window 1)
    (idemacs/view-school-agenda)))

(defun idemacs/sidebar-format-capture-link ()
  "Creates the link to store in the sidebar with a capture template."
  (format "[[%s:%s][%s]]"
	  (idemacs/sidebar-get-file-type)
	  (read-file-name "File: ")
 	  (read-string "Label ")))
	  
(defun idemacs/sidebar-get-file-type ()
  "This function is used to get the file type for the link."
  (let ((file-type (completing-read "Select file type: "
				    (list '"IDEmacs_file" idemacs-sidebar-link-name))))
    (cond ((string= file-type "IDEmacs_file") "IDEmacs_file")
	  ((string= file-type idemacs-sidebar-link-name) idemacs-sidebar-link-name))))

(defun idemacs/sidebar-insert-link ()
  "Insert a link into the sidebar."
  (interactive)
  (let ((org-capture-templates (doct (list (idemacs/capture-sidebar-entries))))
	(window (get-buffer-window (current-buffer))))
    (setq buffer-read-only nil)
    (setq buffer-read-only nil)
    (set-window-parameter window 'window-side nil)
    (org-capture 0 "bl")
    (set-window-parameter window 'window-side 'left)
    (setq buffer-read-only t)))

(defun idemacs/sidebar-count-items (query files)
  "Count the number of items the (QUERY) in the (FILES)."
  (length (org-map-entries nil query files)))

(defun idemacs/sidebar-show-matches (query file header)
  "Shows the (HEADER) in the (FILE) that match the (QUERY)"
  (let ((org-agenda-overriding-header (format " %s " header))
	(org-agenda-files (list file)))
    (message "%s %s" query file)
    (progn
      (when (eq (current-buffer) (get-buffer "Sidebar.org"))
	(other-window 1))
      (when (get-buffer "*Org Agenda*")
	(kill-buffer "*Org Agenda*"))
      (org-tags-view nil query)
      (rename-buffer "*Org Agenda*"))))

(defun idemacs/sidebar-follow-link (path)
  "Follow the link at (PATH).
The path should be in the form of '[[sidebar:QUERY|FILES|FMT][DESCRIPTION]]'
where QUERY is the org-agenda query, FILES is the list of files to search."
  (let* ((link (org-element-context))
	 (link-name (org-element-property :raw-value path))
      	 (fields (split-string path "|"))
	 (query (nth 0 fields))
	 (files (nth 1 fields))
	 (fmt (nth 2 fields))
	 (description
	  (buffer-substring
 	   (org-element-property :contents-begin (org-element-context))
   	   (org-element-property :contents-end (org-element-context)))))
    (cond
     ((string-equal query "daily-agenda")
      (idemacs/sidebar-agenda-day-view))
     ((string-equal query "school-agenda")
      (idemacs/sidebar-agenda-school-view))
     ((not fmt)
      (other-window 1)
      (idemacs/sidebar-show-matches query files description))
     ((and fmt (> (length fmt) 0))
      (idemacs/sidebar-update-link link)))))

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
  "Updates all links in the buffer.
This will update the count if the link has a format string."
  (idemacs/sidebar-clear-all)
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (when (string= (org-element-property :type link) idemacs-sidebar-link-name)
	(idemacs/sidebar-update-link link))))
  (redisplay t))
;; -------------------------------------------------------------------

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
      (if (bound-and-true-p idemacs-sidebar-mode)
	  (idemacs/sidebar-update-all)))))

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

(provide 'IDEmacs-sidebar)
;;; IDEmacs-sidebar.el ends here
