(defun dont-make-backup-files () (setq make-backup-files nil))

(defun npat-add-dir-to-PATH (dir)
  "Prepend DIR to execution PATH, if not already there."
  (interactive "DDirectory: ")
  (let* ((real-dir (expand-file-name dir))
	 (path (getenv "PATH"))
	 (path-list (split-string path ":")))
    (cond ((string= "" path) (setenv "PATH" real-dir))
          ((not (member real-dir path-list))
           (setenv "PATH" (concat real-dir ":" path))))))

(defun npat-add-dir-to-PYTHONPATH (dir)
  "Prepend DIR to PYTHONPATH, if not already there."
  (interactive "DDirectory: ")
  (let* ((real-dir (expand-file-name dir))
	 (path (getenv "PYTHONPATH"))
	 (path-list (split-string path ":")))
    (cond ((string= "" path) (setenv "PYTHONPATH" real-dir))
          ((not (member real-dir path-list))
           (setenv "PYTHONPATH" (concat real-dir ":" path))))))


(defun npat-find-library (lib &optional path)
  "Find library LIB in PATH, or in load-path, if PATH is not given."
  (interactive "sLibrary: ")
  (unless path (setq path load-path))
  (let ((found (catch 'win
		 (while (consp path)
		   (let* ((prefix (concat (car path) "/" lib))
			  (try (lambda (&optional suffix)
				 (let ((name (concat prefix suffix)))
				   (if (file-exists-p name)
				       (throw 'win name))))))
		     (funcall try ".elc")		     
		     (funcall try ".el")
		     (funcall try))
		   (setq path (cdr path)))
		 nil)))
    (when (interactive-p) (message "%s" found))
    found))

(defun npat-enlarge-or-shrink-window (cols)
  "Enlarge or shring the current window to make it COLS columns wide."
  (condition-case nil
      (let ((dx (- (window-width) cols)))
	(cond ((> dx 0) (shrink-window dx t))
	      ((< dx 0) (enlarge-window (- dx) t)))
	t)
    (error nil))
  (if (eq (window-width) cols) t nil))

(defun npat-force-window-width (&optional cols)
  "Make current window COLS columnts wide, spliting it if necessary."
  (interactive)
  (unless cols (setq cols 80))
  (unless (npat-enlarge-or-shrink-window cols)
    (split-window-horizontally)
    (npat-enlarge-or-shrink-window cols)))

(fset 'npat-toggle-frame-size
      (lexical-let ((x 'iso-std))
	(lambda ()
	  "Toggle frame size between standard (80x25) and tall (80x50)"
	  (interactive)
	  (when (display-graphic-p)
	    (cond ((eq x 'iso-std)
		   (set-frame-size (selected-frame) 80 27)
		   (setq x 'iso-tall))
		  ((eq x 'iso-tall)
		   (set-frame-size (selected-frame) 80 52)
		   (setq x 'iso-std)))))))

;;(defun npat-toggle-maximize-frame-vertically
;;  "Toggle the frame's vertical size between maximum and previous value."
;;  (interactive)
  
;; still needs work
(defun npat-toggle-maximize-frame-vertically ()
  (let* ((frame-list (get 'npat-toggle-maximize-frame-vertically 
			 'frame-list))
	 (frame (selected-frame))
	 (old-frame-metrics (plist-get frame-list frame))
	 (maximize-frame (lambda (frame)
			   (set-frame-position 
			    frame (frame-parameter frame 'left) 0)
			   (set-frame-height frame 52)))
	 (restore-frame (lambda (frame frame-metrics)
			  nil))
	 (get-frame-metrics (lambda (frame)
			      nil)))
    (if old-frame-metrics 
	(funcall restore-frame frame old-frame-metrics)
      (setq frame-list 
	    (plist-put frame-list  frame 
		       (funcall get-frame-metrics frame)))
      (put 'npat-toggle-maximize-frame-vertically 'frame-list
	   frame-list)
      (funcall maximize-frame frame))))

(defun npat-rfc (num)
  "Show RFC NUM in a buffer."
  (interactive "nRFC (0 for index): ")
  (let ((url (if (zerop num)
		 "https://www.rfc-editor.org/in-notes/rfc-index.txt"
               (format "https://www.rfc-editor.org/rfc/rfc%i.txt" num)))
        (buf (get-buffer-create "*RFC*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (delete-region (point-min) (point-max))
        (let ((proc (start-process "wget" buf "wget" "-q" "-O" "-" url)))
          (set-process-sentinel proc 'npat-rfc-sentinel))
        (message "Getting RFC %i..." num)))))

(defun npat-rfc-sentinel (proc event)
  "Sentinel for `rfc'."
  (with-current-buffer (process-buffer proc)
    (goto-char (point-min))
    (view-mode 1)
    (when (fboundp 'rfcview-mode)
      (rfcview-mode)))
  (display-buffer (process-buffer proc)))

(defun npat-assoc-rexp (key alist &optional default)
  (let ((l alist))
    (catch 'match 
      (while (consp l)
	(if (string-match (caar l) key)
	    (throw 'match (cdar l)))
	(setq l (cdr l)))
      default)))

(defun npat-locale-charmap ()
  "Return the charmap of the selected locale (downcase'd)."
  (downcase (substring (shell-command-to-string "locale charmap") 0 -1)))

(defun npat-kill-emacs-server ()
  (save-some-buffers t)
  (mapcar 'server-delete-client server-clients)
  (kill-emacs))
