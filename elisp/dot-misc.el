;;
;; General EMACS settings
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

;; I want fontification wherever possible
(global-font-lock-mode 1)

;; Automatic parenthesis highlighting
(show-paren-mode 1)

;; Scroll compilation window by default
(setq compilation-scroll-output 't)

;; Text mode to switch automatically to auto-fill.
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; upcase abd downcase enabled. we 're not sissies!
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; narrow-mode enabled. we 're not sissies!
(put 'narrow-to-region 'disabled nil)

;; special buffer frames
;(setq same-window-buffer-names
;      '("*info*" "*mail*"))
(setq same-window-buffer-names
      '("*info*" "*mail*" "*shell*" "*Buffer List*"))
;;(setq special-display-buffer-names
;;      '("*Calendar*" "*compilation*"))

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

;; no TABS in files
(setq ident-tabs-mode nil)

;; no shift-select, to transient selects (we' re not winblows!)
(when (>= emacs-major-version 23)
  (setq shift-select-mode nil)
  (transient-mark-mode nil))

;; Start server to be used by "Edit with Emacs" google-chrome 
;; extension (not to be confused with running emacs in daemon mode).
;; See:
;;   lib/emacs/misc/edit-server.el
;;   http://www.emacswiki.org/emacs/Edit_with_Emacs
(require 'edit-server)
(edit-server-start)
(require 'edit-server-htmlize)
(add-hook 'edit-server-start-hook 'edit-server-maybe-dehtmlize-buffer)
(add-hook 'edit-server-done-hook  'edit-server-maybe-htmlize-buffer)

;;
;; Gmail
;;
;; Requires gnutls-bin to be installed (apt get gnutls-bin)
(setq send-mail-function 'smtpmail-send-it)
(setq smtpmail-smtp-server "smtp.gmail.com")
(setq smtpmail-smtp-service 25)


