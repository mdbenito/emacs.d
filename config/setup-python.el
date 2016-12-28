;;
;;
(add-hook 'python-mode-hook #'electric-pair-mode)

;;
;;
(add-hook 'python-mode-hook #'anaconda-mode)
(eval-after-load "company"
  '(add-to-list 'company-backends 'company-anaconda))

(custom-set-variables
 '(smart-tab-completion-functions-alist
   (quote
    ((emacs-lisp-mode . lisp-complete-symbol)
     (text-mode . dabbrev-completion)
     (anaconda-mode . anaconda-mode-complete)))))
