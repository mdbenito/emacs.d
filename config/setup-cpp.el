
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode)
                ;; (cppcm-reload-all)
                (flycheck-mode)
                (irony-mode)
                (flycheck-select-checker 'rtags))))

; Avoid typing full path when starting gdb
;; (global-set-key (kbd "C-c C-g")
;;                 '(lambda ()(interactive)
;;                    (gud-gdb (concat "gdb --fullname "
;;                                     (cppcm-get-exe-path-current-buffer)))))

;; Specify extra flags forwarded to compiler
;(setq cppcm-extra-preprocss-flags-from-user '("-I/usr/src/linux/include" "-DNDEBUG"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rtags kicks ass

(setq rtags-tramp-enabled t) ;; Enable rtags to run over tramp
(setq rtags-autostart-diagnostics t)
;(setq rtags-completions-enabled t)

(eval-after-load 'company
  '(progn
     (add-to-list 'company-backends 'company-irony)
     ;(add-to-list 'company-backends 'company-rtags)
     ; ensure we don't use company-clang by mistake
     (setq company-backends (delete 'company-clang company-backends))
     (setq company-backends (delete 'company-semantic company-backends))))

(define-key c-mode-base-map (kbd "s-<mouse-1>")
  (mbd-my-func-mouse rtags-find-symbol-at-point))
(define-key c-mode-base-map (kbd "M-.") #'rtags-find-symbol-at-point)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cmake-ide should make my life easier... But it doesn't
(cmake-ide-setup)
(setq cmake-ide-build-pool-dir "~/Devel/builds")
(setq cmake-ide-build-pool-use-persistent-naming t)
