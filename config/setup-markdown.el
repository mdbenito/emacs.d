;; Keymap conflicts
(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "M-<left>") nil)
  (define-key markdown-mode-map (kbd "M-<right>") nil)
  (define-key markdown-mode-map (kbd "M-S-<right>") nil)
  (define-key markdown-mode-map (kbd "M-S-<left>") nil)
  (define-key markdown-mode-map (kbd "M-n") nil) ;; M-n is ~ in german Apple keyboard
  )
