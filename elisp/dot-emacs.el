;;
;; .emacs
;;
;; emacs config file
;;
;; by Nick Patavalis (npat@efault.net)
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "\nLoading lib/emacs/dot-emacs/dot-emacs.el...\n")

;;
;; Add directories to load-path
;;

;; Common lisp is nice to have
(require 'cl)

;; I keep all my emacs-related stuff under ~/emacs
(defvar emacs-root 
  (expand-file-name "~/lib/emacs/")
  "The root of my private emacs load-path.")

;; add all the elisp directories under emacs-root to my load path
(defun add-priv-dir-to-path (dir)
  "Prepend DIR to load-path, if not already there."
  (interactive "DDirectory: ")
  (add-to-list 'load-path (concat emacs-root dir)))

(add-priv-dir-to-path "elisp")       ;; customizations, functions and stuff
(add-priv-dir-to-path "third-party") ;; various stuff found on the net
(add-priv-dir-to-path "tmp")         ;; for fooling around

(require 'path-util)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Load stuff found in ~/lib/emacs/xxx
;;

(load-library "functions")
(load-library "dot-misc")
(load-library "dot-greek")
(load-library "dot-keys")
(load-library "dot-modes")
(load-library "dot-w3m")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set by interactive customizer
;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default-frame-alist (quote ((vertical-scroll-bars) (height . 40) (width . 80))))
 '(global-edit-server-edit-mode t)
 '(initial-frame-alist (quote ((menu-bar-lines . 0) (tool-bar-lines . 0) (width . 80) (height . 40))))
 '(menu-bar-mode nil)
 '(minibuffer-frame-alist (quote ((width . 80) (height . 1))))
 '(scroll-bar-mode nil)
 '(select-active-regions t)
 '(tool-bar-mode nil)
 '(transient-mark-mode nil)
 '(winner-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "gray90" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 105 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(cursor ((t (:background "blue"))))
 '(font-lock-builtin-face ((t nil)))
 '(font-lock-comment-face ((t (:foreground "steel blue"))))
 '(font-lock-constant-face ((t nil)))
 '(font-lock-doc-face ((t nil)))
 '(font-lock-function-name-face ((t nil)))
 '(font-lock-keyword-face ((t nil)))
 '(font-lock-preprocessor-face ((t nil)))
 '(font-lock-string-face ((t (:foreground "dark olive green"))))
 '(font-lock-type-face ((t nil)))
 '(font-lock-variable-name-face ((t nil)))
 '(font-lock-warning-face ((t (:inherit error))))
 '(fringe ((t (:background "gray85" :foreground "red")))))
(put 'erase-buffer 'disabled nil)
