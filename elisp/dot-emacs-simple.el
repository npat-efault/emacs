;;
;; Simple dot-emacs config file
;;
;; by Nick Patavalis (npat@efault.net)
;;

;; Translate `C-h' to <DEL> and vice-versa.
;;(keyboard-translate ?\C-h ?\C-?)
;;(keyboard-translate ?\C-? ?\C-h)

;; Visible bell
(setq visible-bell 't)

;; Next line (down arrow) at the end of a line does not
;; add a new line to the file.
(setq next-line-add-newlines 'nil)

;; Automatic parenthesis highlighting
(show-paren-mode 1)

;; Text mode to switch automatically to auto-fill.
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; upcase enabled. we 're not sissies!
(put 'upcase-region 'disabled nil)

;; narrow-mode enabled. we 're not sissies!
(put 'narrow-to-region 'disabled nil)

;; no splash screen please
(setq inhibit-startup-message t)

; no am and pm here
(setq display-time-24hr-format t)

;; if you can't tell, use text-mode
(setq default-major-mode 'text-mode)

;; open gziped files
(auto-compression-mode 1)

;; lines and columns
(column-number-mode 1)
(line-number-mode 1)

;; no backup files
(setq make-backup-files nil)

;;
;; Keys
;;
(global-set-key [?\C-x ?l] 'goto-line)

;;
;; Modes
;;

;; I want fontification wherever possible
(global-font-lock-mode 1)

;; Dired related settings
(setq dired-recursive-deletes t)
(define-key global-map "\C-x\C-j" 'dired-jump)
(add-hook 'dired-load-hook
	  (lambda ()
	    (load "dired-x")
	    ;; Set dired-x global variables here.  For example:
	    ;; (setq dired-guess-shell-gnutar "gtar")
	    ;; (setq dired-x-hands-off-my-keys nil)
	    ))
(add-hook 'dired-mode-hook
	  (lambda ()
	    ;; Set dired-x buffer-local variables here.  For example:
	    ;; (setq dired-omit-files-p t)
	    ))
(setq dired-guess-shell-alist-user
      (list
       (list "\\.pdf$" "exec mupdf * >/dev/null 2>&1 &")
       (list "\\.ps$" "gv" "lpr")
       (list "\\.dvi$" "xdvi" "dvips")
;       (list "\\.bar$";; rule with condition test
;	     '(if CONDITION
;		  "BAR-COMMAND-1"
;		"BAR-COMMAND-2"))
       ))

;;
;; Python mode
;;

(autoload 'python-mode "python-mode" "Major mode to edit Python sources." t)
(setq auto-mode-alist
      (append
       (list '("\\.py" . python-mode))
       auto-mode-alist))

