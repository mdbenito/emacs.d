;;
(add-hook 'python-mode-hook #'electric-pair-mode)

(add-hook 'python-mode-hook #'anaconda-mode)

(eval-after-load "company"
  '(add-to-list 'company-backends 'company-anaconda))

(eval-after-load 'python-mode
  (define-key python-mode-map (kbd "s-d") #'pdb)
  (define-key python-mode-map (kbd "<f10>")  ; ugly...
    (lambda () (interactive) (gud-call "pp locals()"))))

;; anaconda-mode-show-doc and -find-definitions take no arguments...
(with-eval-after-load 'anaconda-mode
  (define-key anaconda-mode-map (kbd "M-<mouse-1>")
    (mdb-my-func-mouse (lambda (x) (anaconda-mode-show-doc))))
  (define-key anaconda-mode-map (kbd "s-<mouse-1>")
    (mdb-my-func-mouse (lambda (x) (anaconda-mode-find-definitions)))))
