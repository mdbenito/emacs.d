;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc stuff

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

;; I use this elsewhere
(defmacro mbd-my-func-mouse (func)
  "Calls FUNC with the symbol under a mouse click."
  `(lambda (event)
    (interactive "e")
    (progn  ;; if we save-excursion this won't work with anaconda-mode-*
      (goto-char (posn-point (event-end event)))
      (let ((sy (symbol-at-point)))
        (,func (symbol-name sy))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hide the first lines of grep and rgrep
;; redisplay them with C-x n w

(defmacro with-grep-buffer-writable (&body body)
  `(save-excursion
    (with-current-buffer grep-last-buffer
      (setq buffer-read-only nil)
      ,@body
      (setq buffer-read-only t))))

; Original idea at http://stackoverflow.com/a/16133543
(defun mbd--hide-grep-header ()
  (with-grep-buffer-writable
   (goto-line 5)
   (narrow-to-region (point) (point-max))
   (insert-text-button "(...)"
                       'action #'mbd--click-show-grep-button)
   (insert "\n")))

(defun mbd--click-hide-grep-button (button)
  (with-grep-buffer-writable
   (button-put button 'action #'mbd--click-show-grep-button)
   (narrow-to-region (button-start button) (point-max))))

(defun mbd--click-show-grep-button (button)
  (with-grep-buffer-writable
   (button-put button 'action #'mbd--click-hide-grep-button)
   (widen)
   (goto-line 1)))

(advice-add 'grep :after #'mbd--hide-grep-header)
(advice-add 'rgrep :after #'mbd--hide-grep-header)
;; (advice-remove 'rgrep #'mbd--hide-grep-header)
;; (advice-remove 'grep #'mbd--hide-grep-header)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debugger(s)
;; Not necessary if using realgud
;; (global-set-key (kbd "<f8>") #'gud-step)
;; (global-set-key (kbd "<f9>") #'gud-next)
;; (global-set-key (kbd "s-b") #'gud-break)

(with-eval-after-load "realgud"
  ;; These need to be set before loading! (e.g. in custom.el) but do they??
  ;; (custom-set-variables
  ;;  '(realgud-bp-fringe-indicator-style (quote (realgud-bp-filled . realgud-bp-hollow))))
  ;; (custom-set-faces
  ;;  '(realgud-backtrace-number ((t (:foreground "white" :weight bold))))
  ;;  '(realgud-bp-line-disabled-face ((t (:background "gray29"))))
  ;;  '(realgud-overlay-arrow1 ((t (e:foreground "yellow" :weight bold))))
  ;;  '(realgud-overlay-arrow2 ((t (:foreground "yellow3"))))
  ;;  '(realgud-overlay-arrow3 ((t (:foreground "yellow4" :weight bold)))))
  
  ;; Fix for TRAMP conections (should make a PR)
  ;; MBD FIXME: I don't know if this works or even makes sense anymore...
  (defun BOGUS-realgud-expand-format (fmt-str &optional opt-str opt-buffer)
    "Expands commands format characters inside FMT-STR.
OPT-STR is an optional string (used with %p and %s).  Values are
taken from current buffer, or OPT-BUFFER if non-nil.  Some
%-escapes in the string arguments are expanded.  These are:

  %f -- Name without directory of current source file.
  %F -- Name without directory or extension of current source file.
  %x -- Name of current source file.
  %X -- Expanded name of current source file.
  %d -- Directory of current source file.
  %l -- Number of current source line.
  %c -- Fully qualified class name derived from the expression
        surrounding point.
  %p -- Value of OPT-STR, converted to string using `int-to-string'
  %s -- Value of OPT-STR.

%p and %s are replaced by an empty string if OPT-STR is nil."
    (let* ((buffer (or opt-buffer (current-buffer)))
           (srcbuf (realgud-get-srcbuf buffer))
           (srcfname (and srcbuf (buffer-file-name srcbuf)))
           (tramp-vec (and srcfname (tramp-dissect-file-name srcfname)))
           (src-file-name (if tramp-vec (tramp-file-name-localname tramp-vec)
                            srcfname))
           ;; (src-file-name (and srcbuf (buffer-file-name srcbuf)))
           result)
      (while (and fmt-str
                  (let ((case-fold-search nil))
                    (string-match "\\([^%]*\\)%\\([dfFlpxXs]\\)" fmt-str)))
        (let* ((key-str (match-string 2 fmt-str))
               (key (string-to-char key-str)))
          (setq result
                (concat
                 result (match-string 1 fmt-str)
                 (cond
                  ((cdr (assq key realgud-expand-format-overrides)))
                  ((eq key ?d)
                   (or (and src-file-name
                            (file-name-directory src-file-name))
                       "*source-file-not-found-for-%d"))
                  ((eq key ?f)
                   (or (and src-file-name
                            (file-name-nondirectory src-file-name))
                       "*source-file-not-found-for-%f*"))
                  ((eq key ?F)
                   (or (and src-file-name
                            (file-name-sans-extension
                             (file-name-nondirectory src-file-name)))
                       "*source-file-not-found-for-%F"))
                  ((eq key ?l)
                   (if srcbuf
                       (with-current-buffer srcbuf
                         (int-to-string
                          (save-restriction
                            (widen)
                            (+ (count-lines (point-min) (point))
                               (if (bolp) 1 0)))))
                     "source-buffer-not-found-for-%l"))
                  ((eq key ?x)
                   (or (and src-file-name src-file-name)
                       "*source-file-not-found-for-%x"))
                  ((eq key ?X)
                   (or (and src-file-name (expand-file-name src-file-name))
                       "*source-file-not-found-for-%X"))
                  ;; ((eq key ?e)
                  ;;  (gud-find-expr))
                  ;; ((eq key ?a)
                  ;;  (gud-read-address))
                  ;; ((eq key ?c)
                  ;;   (gud-find-class srcbuf))
                  ((eq key ?p) (if opt-str (int-to-string opt-str) ""))
                  ((eq key ?s) (or opt-str ""))
                  (t key)))))
        (setq fmt-str (substring fmt-str (match-end 2))))
      ;; There might be text left in FMT-STR when the loop ends.
      (concat result fmt-str)))
)

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

(eval-after-load 'paradox
  (setq paradox-github-token
        (mbd-string-from-file "~/.emacs.d/private/paradox-token")))

(eval-after-load 'tramp
  (setq tramp-default-method "docker"))

(setq enable-remote-dir-locals t)

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
(global-unset-key (kbd "s-m"))
(global-set-key (kbd "s-m s") #'magit-status)
(global-set-key (kbd "s-m l") #'magit-log)
(global-set-key (kbd "s-m f") #'magit-log-buffer-file)
(global-set-key (kbd "s-m b") #'magit-blame)
(with-eval-after-load "magit-mode"
  ;; hide and show sections using the same keys as for HideShow
  (define-key magit-mode-map (kbd "S-C-M-<left>")
    (lambda () (interactive)
      (magit-section-hide-children magit-root-section)))
  (define-key magit-mode-map (kbd "S-C-M-<right>")
    (lambda () (interactive)
      (magit-section-show-children magit-root-section))))

;; Disable emacs' VC for git repos
(setq vc-handled-backends (delq 'Git vc-handled-backends))
