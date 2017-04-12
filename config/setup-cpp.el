;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic setup
;; 
;; * cmake-ide should be able to invoke cmake so that it creates a json file
;;   with build commands for rtags, irony-server
;;     - build products go to "~/Devel/builds"
;;     - I need to run cmake-ide-run-cmake once when opening a new project
;;
;; * rtags uses the json in order to provide jump-to-definition
;;     - REMEMBER TO UPDATE rdm/rc after updating the melpa package!!!
;;       In particular check whether homebrew's version isn't behind.
;;
;; * flycheck uses rtags to provide syntax checks. Works, sort of.
;;
;; * irony-server uses it to provide completions. BUT:
;;   - I need to restart irony-server for each buffer!?!?!
;;   - I need to manually reset irony-cdb-json-select
;;

(require 'flycheck-rtags)
(require 'irony)
(require 'company-irony)
(require 'company-c-headers)
(require 'rtags)
(require 'cc-mode)

(defun remove-from-list (l what)
  (setq l (delete what l)))

(defun mbd--c-mode-common-config ()
  (when (derived-mode-p 'c-mode 'c++-mode)
    ;; irony doesn't support tramp (yet? 04.2017) Only use it on local files
    ;; it provides better completions than rtags
    (if (tramp-tramp-file-p (buffer-file-name (current-buffer)))
        (progn
          (remove-from-list company-backends 'company-irony)
          (remove-from-list company-backends 'company-c-headers)
          (add-to-list 'company-backends 'company-rtags)
          (setq rtags-completions-enabled t))
      (progn
        (add-to-list 'company-backends 'company-irony)
        (add-to-list 'company-backends 'company-c-headers)
        (remove-from-list company-backends 'company-rtags)
        (setq rtags-completions-enabled nil)
        (irony-mode)))
    (flycheck-mode)
    (flycheck-select-checker 'rtags)))

(add-hook 'c-mode-common-hook #'mbd--c-mode-common-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rtags kicks ass

(setq rtags-tramp-enabled t) ;; Enable rtags to run over tramp
(setq rtags-autostart-diagnostics t)
(rtags-diagnostics)

(eval-after-load 'company
  '(progn
     ; ensure we don't use some company-whatever by mistake
     (setq company-backends (delete 'company-clang company-backends))
     (setq company-backends (delete 'company-semantic company-backends))))

(define-key c-mode-base-map (kbd "s-<mouse-1>")
  (mbd-my-func-mouse rtags-find-symbol-at-point))
(define-key c-mode-base-map (kbd "M-.") #'rtags-find-symbol-at-point)
(define-key c-mode-base-map (kbd "M-?") #'rtags-find-references-at-point)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cmake-ide should make my life easier... But it doesn't

(cmake-ide-setup)
(setq cmake-ide-build-pool-dir "~/Devel/builds")
(setq cmake-ide-build-pool-use-persistent-naming t)
