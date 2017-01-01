;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MBD: custom config. Recall to add the following to init.el:
;;(setq custom-file "~/custom.el")
;;(load custom-file)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 4)
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("2601b6cb623a44e906f61d42685dfc1ad2d06c7906088856b5e780af3a1b5036" "97d039a52cfb190f4fd677f02f7d03cf7dbd353e08ac8a0cb991223b135ac4e6" "4e753673a37c71b07e3026be75dc6af3efbac5ce335f3707b7d6a110ecb636a3" "9e54a6ac0051987b4296e9276eecc5dfb67fdcd620191ee553f40a9b6d943e78" "fad38808e844f1423c68a1888db75adf6586390f5295a03823fa1f4959046f81" "a433b4f6e0f8a1fe7cc8411419a708b6ca911320f34e07e6c73e37bb98a710d2" "06dbcfac3705aaaa79e1a3264c6fd44ef0cf86ef5ed67930e4007e63a8c1e8ee" "9f3181dc1fabe5d58bbbda8c48ef7ece59b01bed606cfb868dd147e8b36af97c" "92c0e015d523f3b3a23ed0b84235978d3033e31822cdd8f0bb63e2c81eb899db" default)))
 '(global-smart-tab-mode t)
 '(package-selected-packages
   (quote
    (realgud delight anaconda-mode company-anaconda bash-completion powerline 4clojure magit magit-popup neotree page-break-lines popup projectile rtags popup-complete popwin cider ac-cider company company-shell conda ido-at-point ido-completing-read+ magit-filenotify magit-find-file magit-gh-pulls magit-gitflow magithub rotate cython-mode multiple-cursors paradox smart-tab nlinum cmake-ide cmake-mode dockerfile-mode all-the-icons doom-themes tabbar markdown-mode markdown-preview-eww markdown-toc sr-speedbar smooth-scrolling workgroups2 tagedit smex rainbow-delimiters paredit move-text modern-cpp-font-lock ido-ubiquitous flycheck-irony flycheck-cython flycheck-clojure flycheck-clangcheck exec-path-from-shell darcula-theme cpputils-cmake company-statistics company-irony-c-headers company-irony company-c-headers clojure-mode-extra-font-locking mode-icons)))
 '(paradox-automatically-star nil)
 '(smart-tab-completion-functions-alist
   (quote
    ((emacs-lisp-mode . company-complete)
     (text-mode . company-complete)
     (python-mode . company-complete)
     (anaconda-mode . company-complete)
     (c-mode . company-complete)
     (c++-mode . company-complete)
     (clojure-mode . ac-complete)
     (lisp-interaction-mode . company-complete)
     (inferior-emacs-lisp-mode . company-complete)
     (sh-mode . company-complete))))
 '(tabbar-mode t nil (tabbar))
 '(tabbar-separator (quote (0.5))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)
