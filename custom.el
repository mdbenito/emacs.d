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
 '(cmake-ide-cmake-opts "-DCMAKE_BUILD_TYPE=Debug")
 '(cmake-ide-rdm-rc-path "~/.rdmrc")
 '(coffee-tab-width 2)
 '(column-number-mode t)
 '(compilation-scroll-output (quote first-error))
 '(confirm-kill-emacs (quote y-or-n-p))
 '(custom-safe-themes
   (quote
    ("b81bfd85aed18e4341dbf4d461ed42d75ec78820a60ce86730fc17fc949389b2" "f89b15728948b1ea5757a09c3fe56882c2478844062e1033a29ffbd2ed0e0275" "12e2aee98f651031d10fd58af76250fa8cab6f28b3e88f03b88b7524c9278549" "f67652440b66223b66a4d3e9c0ddeddbf4a6560182fa38693bdc4d940ce43a2e" "8d737627879eff1bbc7e3ef1e9adc657207d9bf74f9abb6e0e53a6541c5f2e88" "0eef522d30756a80b28333f05c7eed5721f2ba9b3eaaff244ea4c6f6a1b8ac62" "0f0022c8091326c9894b707df2ae58dd51527b0cf7abcb0a310fb1e7bda78cd2" "5310b88333fc64c0cb34a27f42fa55ce371438a55f02ac7a4b93519d148bd03d" "7b76cd2a96395f980e02399ea011e34fc3dda761feeb30a160debb84756ea5f0" "93cc9b3596db95d0a80ef5c6c4c28bdf535cd9dd6110159075c250b6a2418a4f" "5cf7b364c947f0fa56dfc55ce841641b805e8f8a5301d9c90b0db5f7ea77c54b" "6b1e6953a08acf12843973ec25d69dbfa1a53d869f649dc991a56fbdf0d7eb9e" "84f35ac02435aa65aef82f510756ab21f173624fcb332dd81e3c9f2adaf6b85b" "4b3c24a1b13f29c6c6926c194eb8aa76e4ddab7a487cd171043b88ac1f3b4481" "363de9fd1194546e7461bdb766793b1442c222376faa8254b8eafaf25afe48dc" "552b25f8e7c6ba6d33f4f3a41e12569881fba6ffa1f264c1d511a7916eb91fe9" "e91ca866d6cbb79786e314e0466f4f1b8892b72e77ed702e53bf7565e0dfd469" "63b822ccd7a1928a7cbc88037dddf7b74b2f8a507e1bccd7281f20646f72cd0a" "6bde11b304427c7821b72a06a60e8d079b8f7ae10b407d8af37ed5e5d59b1324" "227e2c160b0df776257e1411de60a9a181f890cfdf9c1f45535fc83c9b34406b" "f63adec7dee4f849b29fea17b7abc9d9b70cae91fe70ac510ca9408b5bdb7ab5" "7f4b67cb8aff9eb76ef818b3e41ed5f03581799f8e31899c93ec85b0ef049ceb" "ead76c417365064889c6552678e62a3982f9c6b359888dd7b2ba62efb9422b96" "ad1c2abad40e11d22156fe3987fd9b74b9e1c822264a07dacb24e0b3133aaed1" "2601b6cb623a44e906f61d42685dfc1ad2d06c7906088856b5e780af3a1b5036" "97d039a52cfb190f4fd677f02f7d03cf7dbd353e08ac8a0cb991223b135ac4e6" "4e753673a37c71b07e3026be75dc6af3efbac5ce335f3707b7d6a110ecb636a3" "9e54a6ac0051987b4296e9276eecc5dfb67fdcd620191ee553f40a9b6d943e78" "fad38808e844f1423c68a1888db75adf6586390f5295a03823fa1f4959046f81" "a433b4f6e0f8a1fe7cc8411419a708b6ca911320f34e07e6c73e37bb98a710d2" "06dbcfac3705aaaa79e1a3264c6fd44ef0cf86ef5ed67930e4007e63a8c1e8ee" "9f3181dc1fabe5d58bbbda8c48ef7ece59b01bed606cfb868dd147e8b36af97c" "92c0e015d523f3b3a23ed0b84235978d3033e31822cdd8f0bb63e2c81eb899db" default)))
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
 '(gud-tooltip-mode t)
 '(hippie-expand-try-functions-list
   (quote
    (company-complete try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol)))
 '(jdee-db-active-breakpoint-face-colors (cons "#181e26" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#181e26" "#7bc275"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#181e26" "#3D3D48"))
 '(line-number-mode t)
 '(magit-log-arguments (quote ("-n256" "--graph" "--decorate" "--color")))
 '(magit-log-margin (quote (t "%b %d %H:%M " magit-log-margin-width t 18)))
 '(markdown-code-lang-modes
   (quote
    (("ocaml" . tuareg-mode)
     ("elisp" . emacs-lisp-mode)
     ("ditaa" . artist-mode)
     ("asymptote" . asy-mode)
     ("dot" . fundamental-mode)
     ("sqlite" . sql-mode)
     ("calc" . fundamental-mode)
     ("C" . c-mode)
     ("cpp" . c++-mode)
     ("C++" . c++-mode)
     ("screen" . shell-script-mode)
     ("shell" . sh-mode)
     ("bash" . sh-mode)
     ("python" . python-mode))))
 '(markdown-command "pandoc")
 '(markdown-enable-math t)
 '(markdown-fontify-code-blocks-natively t)
 '(markdown-header-scaling t)
 '(markdown-header-scaling-values (quote (1.9 1.5 1.3 1.1 1.0 1.0)))
 '(markdown-hide-urls nil)
 '(mode-icons
   (quote
    (("\\` ?Abbrev\\'" 61761 FontAwesome)
     ("\\`CSS\\'" 61756 FontAwesome)
     ("\\`Coffee\\'" "coffee" xpm-bw)
     ("\\`Compil.*\\'" 61573 FontAwesome)
     ("\\`Emacs-Lisp\\'" "emacs" xpm)
     ("IELM" "emacs" xpm)
     ("\\`Lisp Interaction\\'" "emacs" xpm)
     ("\\`HTML\\'" 61755 FontAwesome)
     ("\\`Haml\\'" "haml" xpm)
     ("\\`Image\\[imagemagick\\]\\'" 61502 FontAwesome)
     ("\\`Inf-Ruby\\'" "infruby" xpm)
     ("\\`Java[Ss]cript\\'" 59461 devicon)
     ("\\`Lisp\\'" "cl" xpm)
     ("\\`Lua\\'" "Lua-Logo_16x16" png)
     ("\\`nXML\\'" "xml" xpm)
     ("\\`Org\\'" "org" xpm)
     ("\\`PHP\\(\\|/.*\\)\\'" 60273 devicon)
     ("\\`Projectile Rails Server\\'" 60323 devicon)
     ("\\`Python\\'" 60297 devicon)
     ("\\` Emmet\\'" "emmet" xpm)
     ("\\`Ruby\\'" 60362 devicon)
     ("\\`EnhRuby\\'" "ruby" xpm)
     ("\\`ESS\\[S\\]\\'" "R" xpm)
     ("\\`ESS\\[SAS\\]\\'" "sas" xpm)
     ("\\`ESS\\[BUGS\\]\\'" 61832 FontAwesome)
     ("\\`iESS\\'" "R" xpm)
     ("\\`SCSS\\'" "sass" xpm)
     ("\\`Sass\\'" "sass" xpm)
     ("\\`Scheme" "scheme" xpm-bw)
     ("\\`Shell-script\\[.*\\]\\'" 61728 FontAwesome)
     ("Shell" 61728 FontAwesome)
     ("\\`Slim" "slim" xpm-bw)
     ("\\`Snippet" "yas" xpm)
     ("\\`Term\\'" 61728 FontAwesome)
     ("\\`Web\\'" 61755 FontAwesome)
     ("\\`XML\\'" 61729 FontAwesome)
     ("\\`YAML\\'" "yaml" xpm)
     ("\\` ?YASnippet\\'" "yas" xpm)
     ("\\` ?yas\\'" "yas" xpm)
     ("hs" 61552 FontAwesome)
     ("\\`Markdown\\'" 61641 github-octicons)
     ("\\`GFM\\'" 61641 github-octicons)
     ("\\`Scala\\'" 61787 font-mfizz)
     ("\\`Magit\\'" 61907 FontAwesome)
     ("\\` Pulls\\'" 61586 FontAwesome)
     ("\\`Zip-Archive\\'" 61894 FontAwesome)
     ("\\` ARev\\'" 61473 FontAwesome)
     ("\\`Calc\\(ulator\\)?\\'" 61932 FontAwesome)
     ("\\`Debug.*\\'" 61832 FontAwesome)
     ("\\`Calendar\\'" 61555 FontAwesome)
     ("\\`Help\\'" 61529 FontAwesome)
     ("\\`WoMan\\'" 61530 FontAwesome)
     ("\\`C\\(/.*\\|\\)\\'" 58936 devicon)
     ("\\`Custom\\'" 61613 FontAwesome)
     ("\\`Go\\'" "go" xpm)
     ("\\` ?Rbow\\'" "rainbow" xpm)
     ("\\` ?ivy\\'" "ivy" xpm)
     ("\\` ?ICY\\'" "icy" xpm)
     ("\\` ?Golden\\'" "golden" xpm-bw)
     ("\\`BibTeX\\'\\'" "bibtex" xpm-bw)
     ("\\`C[+][+]\\(/.*\\|\\)\\'" 58932 devicon)
     ("\\`C[#]\\(/.*\\|\\)\\'" 58935 devicon)
     ("\\`Elixir\\'" 61717 font-mfizz)
     ("\\`Erlang\\'" 61718 font-mfizz)
     ("\\`Haskell\\'" 61734 font-mfizz)
     ("\\`Clojure\\'" 61707 font-mfizz)
     ("\\`Java\\(/.*\\|\\)\\'" 61739 font-mfizz)
     ("\\`C?Perl\\'" 61768 font-mfizz)
     ("\\`Octave\\'" "octave" xpm)
     ("\\`AHK\\'" "autohotkey" xpm)
     ("\\`Info\\'" 61530 FontAwesome)
     ("\\` ?Narrow\\'" 61542 FontAwesome)
     ("\\`Dockerfile\\'" 58911 devicon)
     ("\\`Spacemacs buffer\\'" "spacemacs" png)
     ("\\` ?emoji\\'" "emoji" png)
     ("\\`Org-Agenda" 61510 FontAwesome)
     ("\\`PS\\'" "powershell" xpm)
     (mode-icons-powershell-p "powershell" xpm)
     (mode-icons-cmd-p "cmd" xpm-bw)
     (mode-icons-msys-p "msys" xpm)
     (mode-icons-cygwin-p "cygwin" xpm)
     (read-only 61475 FontAwesome)
     (writable 61596 FontAwesome)
     (save 61639 FontAwesome)
     (saved "" nil)
     (modified-outside 61553 FontAwesome)
     (steal 61979 FontAwesome)
     (apple 61817 FontAwesome)
     (win 61818 FontAwesome)
     (unix 61820 FontAwesome)
     (undecided 61820 FontAwesome)
     ("Paredit" 59677 IcoMoon-Free)
     ("Text" 61686 FontAwesome)
     ("company" 61687 FontAwesome)
     ("Special" 61476 FontAwesome)
     ("Process Menu" 61614 FontAwesome)
     ("\\` ?AC\\'" 61838 FontAwesome)
     ("\\` ?FlyC[!]*\\'" 59922 IcoMoon-Free)
     ("\\` ?Ergo" 61724 FontAwesome)
     ("\\` ?drag\\'" 61511 FontAwesome)
     ("\\` ?Helm\\'" "helm" xpm-bw)
     ("\\`Messages\\'" 62075 FontAwesome)
     ("\\`Conf" 61918 FontAwesome)
     ("\\`Fundamental\\'" 61614 FontAwesome)
     ("\\`Javascript-IDE\\'" "js" xpm)
     ("\\` Undo-Tree\\'" ":palm_tree:" emoji)
     ("\\`LaTeX\\'" "tex" ext)
     ("\\`Image\\[xpm\\]\\'" "xpm" ext)
     ("\\`Image\\[png\\]\\'" "png" ext)
     ("\\` ?AI\\'" 61500 FontAwesome)
     ("\\` ?Isearch\\'" 61442 FontAwesome)
     (default 61529 FontAwesome)
     ("\\` ?\\(?:ElDoc\\|Anzu\\|SP\\|Guide\\|PgLn\\|Undo-Tree\\|Ergo.*\\|,\\|Isearch\\|Ind\\)\\'" nil nil))))
 '(mode-icons-change-mode-name nil)
 '(mode-icons-mode t)
 '(neo-autorefresh t)
 '(neo-theme (quote arrow) t)
 '(org-ellipsis "  ")
 '(org-fontify-done-headline t)
 '(org-fontify-quote-and-verse-blocks t)
 '(org-fontify-whole-heading-line t)
 '(package-selected-packages
   (quote
    (dash-at-point expand-region spinner solaire-mode flx flx-ido flx-isearch ein matlab-mode easy-hugo clojure-mode company-rtags flycheck flycheck-rtags company-irony company-c-headers web-mode yaml-mode rtags anaconda-mode docker-tramp realgud delight company-anaconda bash-completion powerline 4clojure magit magit-popup neotree page-break-lines popup projectile popup-complete popwin cider ac-cider company company-shell conda ido-at-point ido-completing-read+ magit-filenotify magit-find-file magit-gh-pulls magit-gitflow magithub rotate cython-mode multiple-cursors paradox nlinum cmake-ide cmake-mode dockerfile-mode all-the-icons doom-themes tabbar markdown-mode markdown-preview-eww markdown-toc sr-speedbar smooth-scrolling workgroups2 tagedit smex rainbow-delimiters paredit move-text modern-cpp-font-lock ido-ubiquitous flycheck-irony flycheck-cython flycheck-clojure flycheck-clangcheck exec-path-from-shell darcula-theme company-statistics company-irony-c-headers clojure-mode-extra-font-locking mode-icons)))
 '(paradox-automatically-star nil)
 '(realgud-bp-fringe-indicator-style (quote (realgud-bp-filled . realgud-bp-hollow)))
 '(safe-local-variable-values
   (quote
    ((c-default-style . "linux")
     (easy-hugo-url . "https://paperwhy.aerobatic.io")
     (easy-hugo-basedir . "~/Devel/web/paperwhy")
     (easy-hugo-previewtime . "300")
     (pythonic-environment . "~/Applications/miniconda3/envs/py3"))))
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
 '(magit-log-author ((t (:foreground "#F2C38F" :height 0.9))))
 '(magit-log-date ((t (:foreground "#83AFE5" :height 0.9))))
 '(markdown-code-face ((((class color) (min-colors 257)) (:background "#2e3138")) (((class color) (min-colors 256)) (:background "#303030")) (((class color) (min-colors 16)) (:background "brightblack"))))
 '(mode-line ((t (:height 0.85 :family "Lato"))))
 '(mode-line-inactive ((t (:height 0.85 :family "Lato"))))
 '(realgud-backtrace-number ((t (:foreground "white" :weight bold))))
 '(realgud-bp-line-disabled-face ((t (:background "gray29"))))
 '(realgud-overlay-arrow1 ((t (:foreground "yellow" :weight bold))))
 '(realgud-overlay-arrow2 ((t (:foreground "yellow3"))))
 '(realgud-overlay-arrow3 ((t (:foreground "yellow4" :weight bold))))
 '(tabbar-default ((t (:background "#3d4c55" :foreground "gray72" :height 0.9))))
 '(tabbar-highlight ((t (:inherit tabbar-default :inverse-video t))))
 '(tabbar-selected ((t (:inherit tabbar-default :background "#3d4c55" :foreground "#c5c8c6" :weight bold))))
 '(tabbar-unselected ((t (:inherit tabbar-default :foreground "gray72"))))
 '(which-func ((((class color) (min-colors 257)) (:foreground "#51afef")) (((class color) (min-colors 256)) (:foreground "#51afef")) (((class color) (min-colors 16)) (:foreground "brightblue")))))
