;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autocompletion
;; FIXME: completion sometimes happens in a popup, sometimes in a buffer
;; There seems to be some conflict between packages

;; Key binding to use "hippie expand" for text autocompletion
;; http://www.emacswiki.org/emacs/HippieExpand
;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
      '(company-complete
        try-expand-dabbr
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

(autoload 'bash-completion-dynamic-complete "bash-completion"
  "BASH completion hook")
(add-hook 'shell-dynamic-complete-functions
          #'bash-completion-dynamic-complete)

                                        ; Global complete any
(require 'company)
(add-hook 'after-init-hook #'global-company-mode)

;; Smart-Tab: automagically determines whether we want to indent or
;; autocomplete AND FAILS MISERABLY FOR C++ AND OTHER LANGUAGES. It is
;; activated and configured in custom.el. NOTE: Need to add/fix config
;; for all relevant major modes in custom.el The fail is because it
;; uses (looking-at "\\_>") to decide whether point is at a place
;; where completions are needed and this is of course not enough for
;; e.g. python or c++ where we actually want
;; (thing-at-point-looking-at "\\_>\."). So I'm disabling it.
;;


;; FIXME: this works poorly at best
(defun mbd--complete-or-indent ()
  (interactive)
  (if (use-region-p)
      (indent-region (region-beginning) (region-end))
    (if (minibufferp)
        (dabbrev-expand nil)
      (when (looking-back "[a-zA-Z\.>_-]")  ;; HACK: \s_ doesn't work (?)
        (company-complete)))))

(define-key prog-mode-map (kbd "<tab>") #'mbd--complete-or-indent)
(eval-after-load "c++-mode"
  '(define-key c++-mode-map (kbd "<tab>") #'mbd--complete-or-indent))

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc

(global-set-key (kbd "S-SPC") #'just-one-space)

(when (eq system-type 'darwin)
  (global-set-key (kbd "S-<f1>") #'dash-at-point))

;; Highlights matching parenthesis
(show-paren-mode 1)
(set-face-background 'show-paren-match "#ddd")

;; Pressing a key with the selection on deletes it
(delete-selection-mode t) 

;; Highlight current line
;;(global-hl-line-mode 1)

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)

;; Multiple cursors magic
                                        ;(require 'multiple-cursors)  ; in custom.el
(global-set-key (kbd "<f6>") #'mc/mark-all-dwim)
(global-set-key (kbd "<f7>") #'mc/edit-lines)

;; TO DO: Finish this
(defun mdb--isearch ()
  (let ((s (symbol-at-point)))
    (isearch-yank-string (or (and s (symbol-name s)) ""))))

;; (global-set-key (kbd "s-f") #'mdb--isearch)

                                        ; From https://www.emacswiki.org/emacs/UnfillRegion
(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
    logical line.  This is useful, e.g., for use with
    `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

;; Handy key definition
(global-set-key (kbd "C-c M-q") #'unfill-region)

(require 'expand-region)
(global-set-key (kbd "M-SPC") 'er/expand-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commenting lines and regions

(defun mdb--comment-really-dwim ()
  "Comment or uncomment current line / region"
  (interactive)
  (if (region-active-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (comment-or-uncomment-region beg end)
        ;; FIXME: do something here to restore the region as it was...
        ;; (and set-mark is not it...)
        )
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))
(global-set-key (kbd "C-;") #'mdb--comment-really-dwim)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yay rainbows!
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; use 4 spaces for tabs
(defun die-tabs ()
  (interactive)
  (set-variable 'tab-width 4)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

;; fix weird os x kill error
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))

                                        ;(setq electric-indent-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Files and backups

;; Point goes to the last place when visiting a file
(require 'saveplace)
(setq-default save-place t)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; Put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Activate WhichFunction
(add-hook 'prog-mode-hook #'which-function-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standard shortcuts
;; UPD: I've disabled most of these since they make me twist my left hand

;; Interactive search key bindings. By default, C-s runs
;; isearch-forward, so this swaps the bindings.
;; (global-set-key (kbd "C-s") #'isearch-forward-regexp)
;; (global-set-key (kbd "C-r") #'isearch-backward-regexp)

;; (global-set-key (kbd "s-f") #'isearch-forward)
;; (global-set-key (kbd "M-s-f") #'isearch-backward)
;; (global-key-binding (kbd "s-g") #'isearch-repeat-forward)
;; (global-key-binding (kbd "M-s-g") #'isearch-repeat-backward)

(global-set-key (kbd "s-F") #'rgrep)
;; (global-set-key (kbd "s-G") #'next-error)
;; (global-set-key (kbd "M-s-G") #'previous-error)

;; (global-set-key (kbd "s-r") #'query-replace)

;; (global-unset-key (kbd "s-d"))  ; used to be isearch-repeat-forward (?)

;; ESC ESC ESC is bound to keyboard-escape-quit by default which
;; happens to kill all other windows and I hate it. This is handier:
(global-set-key (kbd "<escape> <escape> <escape>") #'keyboard-quit)

;; Jump whole symbols instead of words with alt+left/right. YESSS!!
(defun backward-symbol (arg)
  "Move point to the previous beginning of a symbol by calling
 `forward-symbol` with ARG negated."
  (interactive "p")
  (forward-symbol (- arg)))

(define-key prog-mode-map (kbd "M-<left>") #'backward-symbol)
(define-key prog-mode-map (kbd "M-<right>") #'forward-symbol)
;; Are these are ignored in c++? See init-cpp
(define-key prog-mode-map (kbd "C-M-<left>") #'backward-sentence)
(define-key prog-mode-map (kbd "C-M-<right>") #'forward-sentence)

(global-set-key (kbd "M-p") #'fill-paragraph)


;; Mac shortcuts for linux:
(when (eq system-type 'gnu/linux) 
  (setq x-select-enable-clipboard t)
  (global-set-key (kbd "s-x")
                  (lambda () (interactive) (clipboard-kill-region nil nil t)))
  (global-set-key (kbd "s-c") #'clipboard-kill-ring-save)
  (global-set-key (kbd "s-v") #'yank)
  (global-set-key (kbd "s-y") #'yank)   ; easier on the hand

  (global-set-key (kbd "s-z") #'undo)
  ;; (global-set-key (kbd "s-s") #'save-buffer)

  (global-set-key (kbd "s-f") #'isearch-forward))

;;;; Common sense stuff which also isn't too hard on my hands...
(when (eq system-type 'darwin)
  ;; Right option is used to type []{} in german mac keyboard,
  ;; We leave it as meta for emacs but retain the keystrokes we need
  ;; These are the output of each row of the keyboard, then with alt,
  ;; then with shift+alt
  
  ;; ^1234567890ß´
  ;; „¡“¶¢[]|{}≠¿'
  ;; “¬”#£ﬁ^\~·¯˙˚

  ;; qwertzuiopü+
  ;; «∑€®†Ω¨⁄øπ•±
  ;; »„‰¸˝ˇÁÛØ∏°

  ;; asdfghjklöä#
  ;; å‚∂ƒ©ªº∆@œæ‘
  ;; ÅÍ™ÏÌÓıˆﬂŒÆ’

  ;; <yxcvbnm,.-
  ;; ≤¥≈ç√∫~µ∞…–
  ;; ≥‡ÙÇ◊‹›˘˛÷—
  
  (when nil  ;; TODO: define has-mac-keyboard-p
    (global-set-key (kbd "M-5") "[")
    (global-set-key (kbd "M-6") "]")
    (global-set-key (kbd "M-7") "|")
    (global-set-key (kbd "M-/")
                    ;; For some reason, I cannot just use "\\"
                    (lambda () (interactive) (insert "\\")))  ;used to be dabbrev-expand
    (global-set-key (kbd "M-8") "{")
    (global-set-key (kbd "M-9") "}")
    (global-set-key (kbd "M-l") "@")
    (global-set-key (kbd "M-n") "~"))

  ;; (global-set-key (kbd "s-y") #'yank)   ; easier on the hand
 
  (setq mac-right-option-modifier 'meta)
  ;; I don't have a right control key. Use right command.

  (setq mac-right-command-modifier 'control)
  ;; (global-unset-key (kbd "C-z"))  ;; was suspend-frame
  (global-set-key (kbd "C-z") #'undo))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Moving (things) around

(require 'move-lines)
(move-lines-binding)  ;; c-<up> and c-<down> move lines/regions

(global-set-key (kbd "s-<left>") #'move-beginning-of-line)
(global-set-key (kbd "s-<right>") #'move-end-of-line)
(global-set-key (kbd "<home>") #'move-beginning-of-line)
(global-set-key (kbd "<end>") #'move-end-of-line)
(global-set-key (kbd "s-<up>") #'beginning-of-buffer)
(global-set-key (kbd "s-<down>") #'end-of-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HideShow rebindings (shows/hides code blocks)
(require 'hideshow)
(eval-after-load #'hs-minor-mode
  (progn
    (define-key hs-minor-mode-map (kbd "S-C-M-<right>") #'hs-show-all)
    (define-key hs-minor-mode-map (kbd "S-C-M-<left>") #'hs-hide-level)
    ;; Conflict with mark ring browsing and toggling is better anyway:
    ;; (define-key hs-minor-mode-map (kbd "C-M-<right>") #'hs-show-block)
    ;; (define-key hs-minor-mode-map (kbd "C-M-<left>") #'hs-hide-block)
    ;; Conflict with word selection:
    ;;(define-key hs-minor-mode-map (kbd "<double-mouse-1>") #'hs-toggle-hiding)
    (define-key hs-minor-mode-map (kbd "<S-return>") #'hs-toggle-hiding)))

(add-hook 'prog-mode-hook
          (lambda ()
            (unless (eq major-mode 'web-mode)
              (hs-minor-mode))))

;;;; Global keymaps
;; (defmacro defkbalias (new old)
;;   `(define-key (current-global-map) ,new
;;      (lookup-key (current-global-map) ,old)))

; sloppy...
;; (defmacro swap-keys (a b)
;;   `(let ((b_key (lookup-key (current-global-map) ,b))
;;          (a_key (lookup-key (current-global-map) ,a)))
;;      (progn
;;        (define-key (current-global-map) ,b a_key)
;;        (define-key (current-global-map) ,a b_key))))

;; now "s-<mouse-1>" equals "M-."
;;(defkbalias (kbd "s-<mouse-1>") (kbd "M-."))
 
