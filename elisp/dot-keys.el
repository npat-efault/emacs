;;
;; Custom EMACS key bindings
;;
;; by Nick Patavalis (npat@inaccessnetworks.com)
;;

(global-set-key [?\C-x ?l] 'goto-line)
(global-set-key [f2] 'npat-force-window-width)
(global-set-key [f4] 'other-window)
;;(global-set-key [C-f4] 'make-frame-command)
(global-set-key [menu] 'menu-bar-mode)
(global-set-key [?\C-x ?g] 'magit-status)

(global-set-key [?\C-=] 'text-scale-increase)
(global-set-key [?\C-+] 'text-scale-increase)
(global-set-key [?\C--] 'text-scale-decrease)

(global-set-key [f7] 'winner-undo)
(global-set-key [C-f7] 'winner-redo)

(global-unset-key "\C-z")
(global-set-key "\C-z" 'undo)
