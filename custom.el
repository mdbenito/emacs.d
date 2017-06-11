;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MBD: custom config. Recall to add the following to init.el:
;;(setq custom-file "~/custom.el")
;;(load custom-file)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(coffee-tab-width 2)
 '(column-number-mode t)
 '(compilation-scroll-output (quote first-error))
 '(confirm-kill-emacs (quote y-or-n-p))
 '(custom-safe-themes
   (quote
    ("5cf7b364c947f0fa56dfc55ce841641b805e8f8a5301d9c90b0db5f7ea77c54b" "6b1e6953a08acf12843973ec25d69dbfa1a53d869f649dc991a56fbdf0d7eb9e" "84f35ac02435aa65aef82f510756ab21f173624fcb332dd81e3c9f2adaf6b85b" "4b3c24a1b13f29c6c6926c194eb8aa76e4ddab7a487cd171043b88ac1f3b4481" "363de9fd1194546e7461bdb766793b1442c222376faa8254b8eafaf25afe48dc" "552b25f8e7c6ba6d33f4f3a41e12569881fba6ffa1f264c1d511a7916eb91fe9" "e91ca866d6cbb79786e314e0466f4f1b8892b72e77ed702e53bf7565e0dfd469" "63b822ccd7a1928a7cbc88037dddf7b74b2f8a507e1bccd7281f20646f72cd0a" "6bde11b304427c7821b72a06a60e8d079b8f7ae10b407d8af37ed5e5d59b1324" "227e2c160b0df776257e1411de60a9a181f890cfdf9c1f45535fc83c9b34406b" "f63adec7dee4f849b29fea17b7abc9d9b70cae91fe70ac510ca9408b5bdb7ab5" "7f4b67cb8aff9eb76ef818b3e41ed5f03581799f8e31899c93ec85b0ef049ceb" "ead76c417365064889c6552678e62a3982f9c6b359888dd7b2ba62efb9422b96" "ad1c2abad40e11d22156fe3987fd9b74b9e1c822264a07dacb24e0b3133aaed1" "2601b6cb623a44e906f61d42685dfc1ad2d06c7906088856b5e780af3a1b5036" "97d039a52cfb190f4fd677f02f7d03cf7dbd353e08ac8a0cb991223b135ac4e6" "4e753673a37c71b07e3026be75dc6af3efbac5ce335f3707b7d6a110ecb636a3" "9e54a6ac0051987b4296e9276eecc5dfb67fdcd620191ee553f40a9b6d943e78" "fad38808e844f1423c68a1888db75adf6586390f5295a03823fa1f4959046f81" "a433b4f6e0f8a1fe7cc8411419a708b6ca911320f34e07e6c73e37bb98a710d2" "06dbcfac3705aaaa79e1a3264c6fd44ef0cf86ef5ed67930e4007e63a8c1e8ee" "9f3181dc1fabe5d58bbbda8c48ef7ece59b01bed606cfb868dd147e8b36af97c" "92c0e015d523f3b3a23ed0b84235978d3033e31822cdd8f0bb63e2c81eb899db" default)))
 '(dabbrev-case-fold-search nil)
 '(debug-on-error t)
 '(desktop-auto-save-timeout 30)
 '(docker-tramp-docker-executable "/usr/local/bin/docker")
 '(doom-one-brighter-modeline t)
 '(doom-one-comment-bg nil)
 '(ensime-sem-high-faces
   (quote
    ((var :foreground "#9876aa" :underline
          (:style wave :color "yellow"))
     (val :foreground "#9876aa")
     (varField :slant italic)
     (valField :foreground "#9876aa" :slant italic)
     (functionCall :foreground "#a9b7c6")
     (implicitConversion :underline
                         (:color "#808080"))
     (implicitParams :underline
                     (:color "#808080"))
     (operator :foreground "#cc7832")
     (param :foreground "#a9b7c6")
     (class :foreground "#4e807d")
     (trait :foreground "#4e807d" :slant italic)
     (object :foreground "#6897bb" :slant italic)
     (package :foreground "#cc7832")
     (deprecated :strike-through "#a9b7c6"))))
 '(fci-rule-color "#5D656B")
 '(global-smart-tab-mode t)
 '(jdee-db-active-breakpoint-face-colors (cons "#181e26" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#181e26" "#7bc275"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#181e26" "#3D3D48"))
 '(line-number-mode t)
 '(markdown-command "pandoc")
 '(neo-autorefresh t)
 '(neo-theme (quote arrow) t)
 '(org-ellipsis "  ")
 '(org-fontify-done-headline t)
 '(org-fontify-quote-and-verse-blocks t)
 '(org-fontify-whole-heading-line t)
 '(package-selected-packages
   (quote
    (expand-region spinner solaire-mode flx flx-ido flx-isearch ein matlab-mode easy-hugo clojure-mode company-rtags flycheck flycheck-rtags company-irony company-c-headers web-mode yaml-mode rtags anaconda-mode docker-tramp realgud delight company-anaconda bash-completion powerline 4clojure magit magit-popup neotree page-break-lines popup projectile popup-complete popwin cider ac-cider company company-shell conda ido-at-point ido-completing-read+ magit-filenotify magit-find-file magit-gh-pulls magit-gitflow magithub rotate cython-mode multiple-cursors paradox smart-tab nlinum cmake-ide cmake-mode dockerfile-mode all-the-icons doom-themes tabbar markdown-mode markdown-preview-eww markdown-toc sr-speedbar smooth-scrolling workgroups2 tagedit smex rainbow-delimiters paredit move-text modern-cpp-font-lock ido-ubiquitous flycheck-irony flycheck-cython flycheck-clojure flycheck-clangcheck exec-path-from-shell darcula-theme company-statistics company-irony-c-headers clojure-mode-extra-font-locking mode-icons)))
 '(paradox-automatically-star nil)
 '(realgud-bp-fringe-indicator-style (quote (realgud-bp-filled . realgud-bp-hollow)))
 '(safe-local-variable-values
   (quote
    ((c-default-style . "linux")
     (easy-hugo-url . "https://paperwhy.aerobatic.io")
     (easy-hugo-basedir . "~/Devel/web/paperwhy")
     (easy-hugo-previewtime . "300")
     (pythonic-environment . "~/Applications/miniconda3/envs/py3"))))
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
 '(tabbar-separator (quote (" ")))
 '(tramp-verbose 1)
 '(vc-annotate-background "#181e26")
 '(vc-annotate-color-map
   (quote
    ((20 . "#98be65")
     (40 . "#b4be6c")
     (60 . "#d0be73")
     (80 . "#ECBE7B")
     (100 . "#e6ab6a")
     (120 . "#e09859")
     (140 . "#da8548")
     (160 . "#d38079")
     (180 . "#cc7cab")
     (200 . "#c678dd")
     (220 . "#d974b7")
     (240 . "#ec7091")
     (260 . "#ff6c6b")
     (280 . "#d6696a")
     (300 . "#ad6769")
     (320 . "#836468")
     (340 . "#5B6268")
     (360 . "#5B6268"))))
 '(vc-annotate-very-old-color nil)
 '(which-function-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(realgud-backtrace-number ((t (:foreground "white" :weight bold))))
 '(realgud-bp-line-disabled-face ((t (:background "gray29"))))
 '(realgud-overlay-arrow1 ((t (:foreground "yellow" :weight bold))))
 '(realgud-overlay-arrow2 ((t (:foreground "yellow3"))))
 '(realgud-overlay-arrow3 ((t (:foreground "yellow4" :weight bold))))
 '(which-func ((((class color) (min-colors 257)) (:foreground "#51afef")) (((class color) (min-colors 256)) (:foreground "#51afef")) (((class color) (min-colors 16)) (:foreground "brightblue")))))
