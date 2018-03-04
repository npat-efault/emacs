;;
;; Automatically install EMACS packages
;;
;; See also: https://stackoverflow.com/questions/10092322/
;;

(require 'package)

;; GNU Emacs Lisp Package Archive (http://elpa.gnu.org)
;; (included by default)
;(add-to-list 'package-archives
;	     '("elpa" . "https://elpa.gnu.org/packages/"))

;; Milkypostman’s Emacs Lisp Package Archive (http://melpa.org)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; Marmalade-repo (https://marmalade-repo.org/)
;(add-to-list 'package-archives
;	     '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if
it’s not. Return a list of installed packages or nil for every
skipped package."
  (mapcar
   (lambda (package)
     ;; (package-installed-p 'evil)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;;
;; Put packages you wish installed here
;;
(ensure-package-installed 'go-mode)
(ensure-package-installed 'go-playground)
(ensure-package-installed 'go-dlv)
(ensure-package-installed 'magit)
(ensure-package-installed 'ws-butler)
(ensure-package-installed 'multiple-cursors)

;; activate installed packages
(package-initialize)
