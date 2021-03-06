;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frame and window config

;; "Winner mode is a global minor mode that records the changes in
;; the window configuration (i.e. how the frames are partitioned
;; into windows) so that the changes can be "undone" using the
;; command ‘winner-undo’.  By default this one is bound to the key
;; sequence ‘C-c <left>’.  If you change your mind (while undoing),
;; you can press ‘C-c <right>’ (calling ‘winner-redo’)."
;;;;; No need for winner-mode if using workgroups2
(winner-mode 1)
(global-set-key (kbd "C-c C-z") #'winner-undo)
(global-set-key (kbd "C-c C-S-z") #'winner-redo)
(desktop-save-mode 1)

(global-unset-key (kbd "s-w"))   ;; was delete-frame

;; (require 'workgroups2)
;; (setq wg-session-file     "~/.emacs.d/workgroups"
;;       wg-prefix-key                   (kbd "s-w")
;;       wg-emacs-exit-save-behavior           'ask ; 'save 'ask or nil
;;       wg-workgroups-mode-exit-save-behavior 'ask ; 'save 'ask or nil
;;       wg-mode-line-display-on                nil)

;; (define-key wg-prefixed-map (kbd "z") #'wg-undo-wconfig-change)
;; (define-key wg-prefixed-map (kbd "Z") #'wg-redo-wconfig-change)
;; (workgroups-mode 1)

;; Full path in title bar
(setq-default frame-title-format "%b (%f)")

;; Turn off the menu bar at the top of each frame because it's distracting
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

; Don't show the ugly tool bar at the top
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Don't show native OS scroll bars for buffers because they're redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Show line and column numbers
(column-number-mode 1)
;(add-hook 'prog-mode-hook #'linum-mode) ; Don't!!! linum is slooooow!
(require 'nlinum)  ; nlinum caches line numbers for a while to be much faster
(add-hook 'prog-mode-hook #'nlinum-mode)
;; FIXME: This is only available with nlinum v1.7 but elpa has 1.6
(setq nlinum-highlight-current-line t)

;; Draw hrulers instead of ^L
(require 'page-break-lines)
(global-page-break-lines-mode 1)

;; Customizations for tabbar
(load "tabbar-custom.el")

;; Display elisp doc for things in the minibuffer, in the modeline
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pinning windows. See:
;; http://stackoverflow.com/questions/43765/pin-emacs-buffers-to-windows-for-cscope
(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not.
  Code by Frank Klotz."
  (interactive)
  (message 
   (if (let (window (get-buffer-window (current-buffer)))      
         (set-window-dedicated-p window 
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

(global-set-key (kbd "<f12>") #'toggle-window-dedicated)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; popwin lets us have any special buffers (specified in
;; popwin:special-display-config) to always show in a popup
;; window. One can close it by typing C-g or selecting other windows.
(require 'popwin)
(popwin-mode 1)
;; Add obnoxious buffers to those managed by popwin:
(dolist (name '("xref" "Anaconda" "anaconda-mode" "anaconda-response"
                "gud" "Completions"))
  (add-to-list 'popwin:special-display-config
	       (apply #'concat `("*" ,name "*"))))
(add-to-list 'popwin:special-display-config
             '("*RTags*" :stick t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Neotree
(require 'all-the-icons)
(global-set-key (kbd "<f8>") #'neotree-toggle)
(customize-set-variable 'neo-theme (if window-system 'icons 'arrow))
;; Make neotree's root follow the current buffer (this has to be set to some
;; value or an obnoxious dialog will pop up) 
(setq neo-force-change-root t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme tweaks

(require 'solaire-mode)
(require 'doom-themes)

(defun mbd--load-theme-before (theme &optional no-confirm no-enable)
  "Advice for before load-theme."
  ;; This helps cleaning up the mess that custom themes leave behind
  ;; when another one loads. See:
  ;; http://emacs.stackexchange.com/questions/3112/how-to-reset-color-theme
  (mapcar #'disable-theme custom-enabled-themes)
  (when (equal theme 'doom-one)
    ;; Brighter modeline to tell windows apart
    (setq doom-enable-brighter-comments t)))

(defun mbd--remove-hook-test (hook fun)
  "Removes FUN from a HOOK and returns t if we did anything."
  (let* ((funs-before (remove-hook hook nil))
         (funs-after (remove-hook hook fun)))
    (and (member fun funs-before) (not (member fun funs-after)))))

(defun mbd--load-theme-after (theme &optional no-confirm no-enable)
  "Advice for after load-theme.
Removes hooks which could conflict with other themes, etc."
  (cond ((not (member theme '(doom-one doom-molokai doom-one-light)))
         (when (and (mbd--remove-hook-test 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
                    (mbd--remove-hook-test 'after-change-major-mode-hook #'turn-on-solaire-mode))
           (message "Removed doom-one hooks"))
           ;; Remove ourselves from load-theme.
           ;; BUT THEN: we need to call advice-add upon loading the theme...
           ;; (advice-remove 'load-theme #'mbd--load-theme-after)
           ;; do more stuff...
           ;; HACK: this face was added by the hook and should be removed
           ;; (set-face-background 'doom-minibuffer-active "#ffffff")
           ;; UPDATE: Instead use disable-enabled-themes below
           )
        ((equal theme 'doom-one)
         (message "Adding doom-one hooks")
         ;; Set doom-buffer-mode for all open buffers
         (mapc (lambda (b) (with-current-buffer b (turn-on-solaire-mode))) (buffer-list))
         (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
         (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
         (custom-theme-set-faces
          'doom-one
          `(mode-line ((t (:foreground "#bbc2cf" :background "#444455"))))
          `(mode-line-inactive ((t (:foreground "#bbc2cf"
                                                :background "#333333"))))))))

(advice-add 'load-theme :before #'mbd--load-theme-before)
(advice-add 'load-theme :after #'mbd--load-theme-after)


(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

(load-theme 'doom-one t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font tweaks

;; No effect (var is read only?):
;; (custom-reevaluate-setting 'minibuffer-prompt-properties)

; Height is in 10ths of pt.
(cond ((string-prefix-p "PelBook" system-name) 
       (set-face-attribute 'default nil :family "Menlo" :height 120
                                        :weight 'regular))
      ((string-prefix-p "PelMac" system-name)
       (set-face-attribute 'default nil :family "Menlo" :height 160))
      ((string-prefix-p "ingwer" system-name)
       (set-face-attribute 'default nil :family "Ubuntu mono" :height 140))
      ((string-prefix-p "HPel620" system-name)
       (set-face-attribute 'default nil :family "Inconsolata" :height 110))
      ((string-prefix-p "hpelux" system-name)
       (set-face-attribute 'default nil :family "Inconsolata" :height 150)))

(blink-cursor-mode 0)

; Minibuffer
;;(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup)
;;(defun my-minibuffer-setup ()
;;       (set (make-local-variable 'face-remapping-alist)
;;           '((default :height 1.2))))

;; (setq initial-frame-alist '((top . 0) (left . 0) (width . 177) (height . 53)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc

(setq
 ;; makes killing/yanking interact with the clipboard
 x-select-enable-clipboard t
 ;; Non-nil means cutting and pasting uses the primary selection
 x-select-enable-primary t
 ;; Save clipboard strings into kill ring before replacing them.
 ;; When one selects something in another program to paste it into Emacs,
 ;; but kills something in Emacs before actually pasting it,
 ;; this selection is gone unless this variable is non-nil
 save-interprogram-paste-before-kill t
 ;; Shows all options when running apropos. For more info,
 ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
 apropos-do-all t
 ;; Mouse yank commands yank at point instead of at click.
 ;; mouse-yank-at-point t
 ;; no bell
 ring-bell-function 'ignore)

;; Useless keys
(global-unset-key (kbd "s-t"))   ; don't pop up font menu
(global-unset-key (kbd "<f10>")) ; don't open the menu bar

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; Remove useless minor mode names from modeline
(delight '((doom-buffer-mode "")
           (workgroups-mode "")
           (smart-tab-mode "")
           (hs-minor-mode "")
           (page-break-lines-mode "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mouse config

;; Scrolling:
(setq mouse-wheel-scroll-amount '(2 ((shift) . 2))) ;; two lines at a time    
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(global-set-key (kbd "<wheel-right>")
                (lambda () (interactive) (scroll-left 2)))
(global-set-key (kbd "<double-wheel-right>")
                (lambda () (interactive) (scroll-left 4)))
(global-set-key (kbd "<triple-wheel-right>")
                (lambda () (interactive) (scroll-left 8)))
(global-set-key (kbd "<wheel-left>")
                (lambda () (interactive) (scroll-right 2)))
(global-set-key (kbd "<double-wheel-left>")
                (lambda () (interactive) (scroll-right 4)))
(global-set-key (kbd "<triple-wheel-left>")
                (lambda () (interactive) (scroll-right 8)))

;; Browsing code with xref:
(global-unset-key (kbd "C-<down-mouse-1>"))
(global-unset-key (kbd "M-<down-mouse-1>"))

;; yuk!
(global-set-key (if (string-prefix-p "HPel620" system-name) (kbd "C-<mouse-1>")
                  (kbd "s-<mouse-1>"))
                (mbd-my-func-mouse xref-find-definitions))
