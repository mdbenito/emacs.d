(add-hook 'c-mode-common-hook
          (lambda ()
            (if (derived-mode-p 'c-mode 'c++-mode)
                (cppcm-reload-all)
              )))

; Avoid typing full path when starting gdb
(global-set-key (kbd "C-c C-g")
                '(lambda ()(interactive)
                   (gud-gdb (concat "gdb --fullname "
                                    (cppcm-get-exe-path-current-buffer)))))

;; Specify extra flags forwarded to compiler
;(setq cppcm-extra-preprocss-flags-from-user '("-I/usr/src/linux/include" "-DNDEBUG"))

;; Enable rtags to run over tramp
(require 'rtags)
(setq rtags-tramp-enabled t)
(setq rtags-autostart-diagnostics t)
(setq rtags-completions-enabled t)
;(require 'company)
;(global-company-mode)
(add-to-list 'company-backends 'company-rtags)

(cmake-ide-setup)

(define-key c-mode-base-map (kbd "s-<mouse-1>")
  (mbd-my-func-mouse rtags-find-symbol-at-point))
(define-key c-mode-base-map (kbd "M-.") #'rtags-find-symbol-at-point)


