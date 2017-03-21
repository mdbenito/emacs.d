;; Automatically load paredit when editing a lisp file
;; More at http://www.emacswiki.org/emacs/ParEdit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; eldoc-mode shows documentation in the minibuffer when writing code
;; http://www.emacswiki.org/emacs/ElDoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(define-key emacs-lisp-mode-map (kbd "C-S-<return>") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "s-S-<return>") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "C-<return>") 'eval-last-sexp)
(define-key emacs-lisp-mode-map (kbd "s-<return>") 'eval-last-sexp)
(define-key lisp-interaction-mode-map (kbd "C-S-<return>") 'eval-buffer)
(define-key lisp-interaction-mode-map (kbd "s-S-<return>") 'eval-buffer)
(define-key lisp-interaction-mode-map (kbd "C-<return>") 'eval-last-sexp)
(define-key lisp-interaction-mode-map (kbd "s-<return>") 'eval-last-sexp)

(define-key emacs-lisp-mode-map (kbd "M-<mouse-1>")
  (mbd-my-func-mouse (lambda (s) (describe-symbol (intern-soft s)))))
(define-key emacs-lisp-mode-map (kbd "M-<f1>") #'describe-symbol)
