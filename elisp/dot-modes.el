;;
;; Customization for EMACS modes
;;

;;
;; Autoinsert
;;

(require 'autoinsert)
(add-hook 'find-file-hooks 'auto-insert)
(setq auto-insert-directory "~/lib/emacs/autoins/")
(setq auto-insert-alist '((c-mode . "c-template")))

;;
;; Font-lock mode
;;

;; I want fontification wherever possible
(global-font-lock-mode 1)
;; Minimal fontification for C / C++ code.
(setq font-lock-maximum-decoration
      '((c-mode . 1) (c++-mode . 1)))
;; No fontification for ReST
(setq font-lock-global-modes '(not rst-mode ...))
;; Strike these keywords out!
(font-lock-add-keywords 
 'c-mode (list '("\\<\\(FIXME\\):" 1 font-lock-warning-face t)
	       '("\\<\\(TODO\\):" 1 font-lock-warning-face t)))
(font-lock-add-keywords
 'go-mode (list '("\\<\\(BUG([^)]*)\\)" 1 font-lock-warning-face t)
		'("\\<\\(FIXME([^)]*)\\)" 1 font-lock-warning-face t)
		'("\\<\\(NOTE([^)]*)\\)" 1 font-lock-warning-face t)
	       '("\\<\\(TODO([^)]*)\\)" 1 font-lock-warning-face t)))

;;
;; Dired related settings
;;

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
       (list "\\.pdf$" "fork acroread")
       (list "\\.ps$" "fork gv" "lpr")
       (list "\\.dvi$" "fork xdvi" "dvips")
;       (list "\\.bar$";; rule with condition test
;	     '(if CONDITION
;		  "BAR-COMMAND-1"
;		"BAR-COMMAND-2"))
       ))

;;
;; C mode customizations
;;

;; Set tab-width and basic-offset 4 char whenever entering c-mode.
(add-hook 'c-mode-hook 
	  (lambda () 
	    (setq tab-width 4)
	    (setq c-basic-offset 4)))
;; For java use the "java" indentation style.
;; For all other c-like dialects use the "linux" style
(setq c-default-style 
      (list '(java-mode . "java") 
	    '(other . "linux")))
;; Define command for applying settings for linux identation style
(defun linux-c-mode ()
  "C mode with adjusted defaults for use with the Linux kernel."
  (interactive)
  (c-mode)
  (c-set-style "linux")
  (setq tab-width 8))
;; Arange for 'linux-c-mode' to be called when editing linux source
(setq auto-mode-alist 
      (append (list '(".*/linux.*/.*\\.[ch]$" . linux-c-mode)
		    '(".*/.*linux/.*\\.[ch]$" . linix-c-mode))
	      auto-mode-alist))

;;
;; Disable Assembler mode
;;

(setq auto-mode-alist
      (append
       (list
	'("\\.s" . fundamental-mode)
	'("\\.S" . fundamental-mode))
       auto-mode-alist))

;;
;; Open ".cal" (calc-calculator) files in C mode
;;

(setq auto-mode-alist
      (append
       (list '("\\.cal" . c-mode)) 
       auto-mode-alist))

;;
;; Python mode
;;

; (autoload 'python-mode "python-mode" "Major mode to edit Python sources." t)
; (setq auto-mode-alist
;      (append
;       (list '("\\.py" . python-mode))
;       auto-mode-alist))

;;
;; For the dictionary package
;;

(autoload 'dictionary-search "dictionary"
  "Ask for a word and search it in all dictionaries" t)
(autoload 'dictionary-match-words "dictionary"
  "Ask for a word and search all matching words in the dictionaries" t)
(autoload 'dictionary-lookup-definition "dictionary"
  "Unconditionally lookup the word at point." t)
(autoload 'dictionary "dictionary"
  "Create a new dictionary buffer" t)
(autoload 'dictionary-mouse-popup-matching-words "dictionary"
  "Display entries matching the word at the cursor" t)
(autoload 'dictionary-popup-matching-words "dictionary"
  "Display entries matching the word at the point" t)
(autoload 'dictionary-tooltip-mode "dictionary"
  "Display tooltips for the current word" t)
(autoload 'global-dictionary-tooltip-mode "dictionary"
  "Enable/disable dictionary-tooltip-mode for all buffers" t)
;; server
(setq dictionary-server 
      (cond ((string-match (system-name) "azure.priv.inaccessnetworks.com")
	     "localhost")
	    ((string-match (system-name) "inaccessnetworks.com")
	     "orion.priv.inaccessnetworks.com")
	    (t 
	     "dict.org")))
;; key bindings
(global-set-key "\C-cs" 'dictionary-search)
(global-set-key "\C-cm" 'dictionary-match-words)

;;
;; PSGML Mode
;;
;; (add-priv-dir-to-path "psgml")
;; (autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)
;; (autoload 'xml-mode "psgml" "Major mode to edit XML files." t)
;; (setq sgml-always-quote-attributes t)
;; (setq sgml-auto-activate-dtd t)

;; (setq sgml-indent-step 2)
;; (setq sgml-indent-data t)
;; (setq sgml-set-face t)

;; (setq sgml-markup-faces 
;;       (list '(start-tag . font-lock-keyword-face)
;; 	    '(end-tag . font-lock-keyword-face)
;; 	    '(comment . font-lock-comment-face)
;; 	    '(pi . bold)
;; 	    '(sgml . bold)
;; 	    '(doctype . font-lock-function-name-face)
;; 	    '(entity . font-lock-constant-face)
;; 	    '(shortref . font-lock-variable-name-face)))

;; (setq auto-mode-alist
;;       (append
;;        (list
;; 	'("\\.xml?" . xml-mode)
;; 	'("\\.sgml?" . sgml-mode))
;;        auto-mode-alist))

;;
;; PCL-CVS mode
;;

(setq cvs-program "iancvs")

;;
;; PSVN subversion mode
;;

(require 'psvn)

;;
;; reStructuredText mode (takes-over text-mode)
;;

(require 'rst)
(setq auto-mode-alist
      (append '(("\\.txt$" . rst-mode)
                ("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode)) auto-mode-alist))

;;
;; ascii-chart mode
;;
;(require 'ascii)

;;
;; Mail mode (nor RMail)
;;
(setq user-mail-address 
      (if (string-match "inaccessnetworks\.com" (system-name))
	  "npat@inaccessnetworks.com"
	"npat@efault.net"))
(setq mail-self-blind 't)


;;
;; Inferior lisp mode
;;
(setq inferior-lisp-program "clisp -K full -I")


;;
;; CSS mode
;;
(setq cssm-indent-function #'cssm-c-style-indenter)

;;
;; Taskjuggler mode
;;
(require 'taskjuggler-mode)

;;
;; SLIME mode
;;
;(defun npat-slime-config ()
;  (setq slime-description-autofocus t))  
;(add-hook 'slime-mode-hook 'npat-slime-config)
(setq slime-description-autofocus t)


;;
;; Go mode
;;
(require 'go-mode-autoloads)
(setq gofmt-command "goimports")
(setq auto-mode-alist
      (append '(("\\.gox$" . go-mode)) auto-mode-alist))
(add-hook 'go-mode-hook 
	  (lambda ()
	    (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
	    (local-set-key (kbd "C-c i") 'go-goto-imports)
	    (local-set-key (kbd "M-.") 'godef-jump)
	    (if (not (string-match "go" compile-command))
		(set (make-local-variable 'compile-command)
		     "go generate && go install -v && go test -v && go vet"))))
(add-hook 'before-save-hook 'gofmt-before-save)
(defun go-run ()
  "run current buffer"
  (interactive)
  (compile (concat "go run " (buffer-file-name))))
(load-file "~/src/golang.org/x/tools/cmd/oracle/oracle.el")
(load-file "~/src/golang.org/x/tools/refactor/rename/rename.el")

;;
;; Markdown Mode
;;

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
