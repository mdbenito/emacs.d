;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic setup

(require 'flycheck-rtags)
(require 'irony)
(require 'company-irony)
(require 'company-c-headers)
(require 'rtags)
(require 'cc-mode)

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode)
                (irony-mode)
                (flycheck-mode)
                (flycheck-select-checker 'rtags))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rtags kicks ass

(setq rtags-tramp-enabled t) ;; Enable rtags to run over tramp
(setq rtags-autostart-diagnostics t)
;(setq rtags-completions-enabled t)

(eval-after-load 'company
  '(progn
     (add-to-list 'company-backends 'company-irony)
     (add-to-list 'company-backends 'company-c-headers)
     ;(add-to-list 'company-backends 'company-rtags)  ; doesn't work that well
     ; ensure we don't use some company-whatever by mistake
     (setq company-backends (delete 'company-clang company-backends))
     (setq company-backends (delete 'company-semantic company-backends))))

(eval-after-load 'c-mode
  '(progn
     (define-key c-mode-base-map (kbd "s-<mouse-1>")
       (mbd-my-func-mouse rtags-find-symbol-at-point))
     (define-key c-mode-base-map (kbd "M-.") #'rtags-find-symbol-at-point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cmake-ide should make my life easier... But it doesn't

(cmake-ide-setup)
(setq cmake-ide-build-pool-dir "~/Devel/builds")
(setq cmake-ide-build-pool-use-persistent-naming t)
