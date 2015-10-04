;;
;; Stuff for working with Greek
;;
;; by Nick Patavalis (npat@efault.net)
;;

(defvar npat-charmap-encoding-alist
  '(("iso[_-]?8859[_-]?1" . iso-8859-1)
    ("iso[_-]?8859[_-]?7" . iso-8859-7)
    ("utf[_-]?8" . utf-8))
  "Associates a locale charmap with an emacs encoding symbol.")

(defvar npat-default-encoding
  'utf-8
  "Default to use if encoding cannot be inffered from the charmap.")

;; Language environment to greek
;(set-language-environment 'Greek)

;; I want greek even on a terminal
(set-terminal-coding-system 
 (npat-assoc-rexp (npat-locale-charmap)
		  npat-charmap-encoding-alist
		  npat-default-encoding))
