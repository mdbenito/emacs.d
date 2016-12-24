;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc stuff

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remote access
(defun mbd-string-chomp (str)
  "Chomp leading and tailing whitespace from STR.
From: https://www.emacswiki.org/emacs/ElispCookbook"
  (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
                                    (: (* (any " \t\n")) eos)))
                            ""
                            str))
(defun mbd-string-from-file (file-path)
  "Return filePath's file content.
Pascal J Bourguignon and TheFlyingDutchman <zzbba…@aol.com>"
  (with-temp-buffer
    (insert-file-contents file-path)
    (mbd-string-chomp (buffer-string))))

(eval-after-load 'tramp
  (setq tramp-default-method "ssh"))
(eval-after-load 'paradox
  (setq paradox-github-token (mbd-string-from-file "~/.emacs.d/private/paradox-token")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit config

;; Better colors for blame mode
(with-eval-after-load "magit-blame"
  (setq doom-one-modeline-activated-fg-color "#BBB9A7")
  (setq doom-one-bg-color "#282C34")
  (set-face-attribute 'magit-blame-heading nil
		      :background doom-one-bg-color
		      :foreground doom-one-modeline-activated-fg-color
		      :slant 'italic
                      :weight 'regular
		      :height 0.9))

;; Some nice shortcuts
;; FIXME: the global shortcuts won't load after magit is started once!
(with-eval-after-load "magit-mode"
  (global-unset-key (kbd "s-m"))
  (global-set-key (kbd "s-m s") #'magit-status)
  (global-set-key (kbd "s-m l") #'magit-log)
  (global-set-key (kbd "s-m f") #'magit-log-buffer-file)
  (global-set-key (kbd "s-m b") #'magit-blame)
  ;; hide and show sections using the same keys as for HideShow
  (define-key magit-mode-map (kbd "S-C-M-<left>")
    (lambda () (interactive)
      (magit-section-hide-children magit-root-section)))
  (define-key magit-mode-map (kbd "S-C-M-<right>")
    (lambda () (interactive)
      (magit-section-show-children magit-root-section))))
