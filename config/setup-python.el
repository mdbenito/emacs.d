;;
(add-hook 'python-mode-hook #'electric-pair-mode)
(add-hook 'python-mode-hook #'anaconda-mode)

(eval-after-load "company"
  '(add-to-list 'company-backends 'company-anaconda))

(with-eval-after-load "python"
  (define-key python-mode-map (kbd "s-d") #'realgud:pdb))
;; (load-library "realgud")

(with-eval-after-load "realgud"  
  (puthash "print" "pp %s" realgud:pdb-command-hash)

  (defun realgud:cmd-print (arg)
    "Pretty print the value of an expression."
    (interactive `(,(read-string (format "Print (%s): " (symbol-at-point))
                                 nil nil (symbol-name (symbol-at-point)))))
    (realgud:cmd-run-command arg "print"))
  
  (defun realgud:pdb-cmd-print-locals ()
    "Prints the value of all local variables."
    (interactive)
    ; FIXME: should make sure that this is only called for python debuggers
    (realgud:cmd-run-command "locals()" "print"))

  (defun realgud-short-key-mode-help ()
    (interactive)
    ;; placeholder
    (describe-minor-mode 'realgud-short-key-mode))
  
  (define-key realgud:shortkey-mode-map "?" #'realgud-short-key-mode-help)
  (define-key realgud:shortkey-mode-map  "p" #'realgud:cmd-print)
  (define-key realgud:shortkey-mode-map "P" #'realgud:pdb-cmd-print-locals)
  (define-key realgud:shortkey-mode-map (kbd "<double-mouse-1>")
    (mdb-my-func-mouse realgud:cmd-print)))

(with-eval-after-load 'anaconda-mode
  (define-key anaconda-mode-map (kbd "M-<mouse-1>")
    (mdb-my-func-mouse (lambda (x) (anaconda-mode-show-doc))))
  (define-key anaconda-mode-map (kbd "s-<mouse-1>")
    (mdb-my-func-mouse (lambda (x) (anaconda-mode-find-definitions)))))
