;;
;; Web-browsing related stuff
;;

(defun npat-choose-browser (url &rest args)
  (interactive "sURL: ")
  (if (y-or-n-p "Use external browser? ")
      (browse-url-mozilla url)
    (w3m-browse-url url t)))

(setq browse-url-browser-function 'npat-choose-browser)

(defun google (what)
  "Use google to search for WHAT."
  (interactive "sSearch: ")
  (w3m-browse-url (concat "http://www.google.com/search?q="
                          (w3m-url-encode-string what)) t))

(setq webjump-sites
      '(("npat" . "http://npat.efault.net/")
	("blog" . "http://npat.livejournal.com/")
	("ian" . "http://www.priv.inaccessnetworks.com/")
	("t3doc" . "http://draco.priv.inaccessnetworks.com/tel3/t3doc")
	("lists" . "http://dmz.inaccessnetworks.com/mailman/listinfo")
	("mailman" . "http://dmz.inaccessnetworks.com/mailman/admin")
        ("emacs" . "http://www.emacswiki.org/cgi-bin/wiki/RecentChanges")
	("hyperspec" . "http://www.lisp.org/HyperSpec/FrontMatter/index.html")
	("lisp" . "http://www.lisp.org/")
        ("google" . "http://www.google.de/")
        ("css" . "http://www.w3.org/TR/CSS21/")
	("notawiki" . "http://draco.priv.inaccessnetworks.com/notawiki")))

(defun npat-w3m-setup ()
  "Setup a w3m buffer."
  (mapc
   (lambda (mapping)
     (apply #'define-key w3m-mode-map mapping))
   '(([(control ?c) (control ?@)] lui-track-next-buffer)
     ([(control ?c) ?c] npat-copy-url)
     ([(down)]  next-line)
     ([(up)]    previous-line)
     ([(right)] forward-char)
     ([(left)]  backward-char)
     ([(control left)] w3m-view-previous-page)
     ([(control right)] w3m-view-next-page)
     ([(control ?x) ?b] switch-to-buffer))))

(defun npat-remove-cr ()
  "Remove all occurrences of ^M in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\r" nil t)
      (replace-match ""))))

(defun npat-copy-url (n)
  "Copy the current URL to the kill ring, or the current anchor URL if
a prefix argument is given."
  (interactive "p")
  (let ((url (if (= n 1)
                 w3m-current-url
               (w3m-anchor))))
    (if url
        (kill-new url)
      (error "No url."))))

(add-hook 'w3m-mode-hook 'npat-w3m-setup)
(add-hook 'w3m-form-input-textarea-mode-hook 'npat-remove-cr)

(setq w3m-use-cookies t
      w3m-cookie-accept-bad-cookies t)
;; This might help in saving cookies
(eval-after-load "w3m"
  '(add-hook 'kill-emacs-hook
             (lambda ()
               (w3m-quit t))))









 