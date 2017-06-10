;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make repeating buffer names unique by prepending path chunks as needed
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Turn on recent file mode so that you can more easily switch to
;; recently edited files when you first start emacs
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 40)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido-mode allows navigation of choices in the minibuffer
;; http://www.emacswiki.org/emacs/InteractivelyDoThings
(ido-mode t)

;; Use flx-ido for better, fuzzier matching
(flx-ido-mode t)
(setq ido-enable-flex-matching t)  ;; disable ido faces to see flx highlights
(setq ido-use-faces nil)
;; Above this many competions fall back to (faster) ido-flex
(setq flx-ido-threshold 8000)

;; Turn this behavior off because it's annoying
(setq ido-use-filename-at-point nil)

;; Don't try to match file across all "work" directories; only match files
;; in the current directory displayed in the minibuffer
(setq ido-auto-merge-work-directories-length -1)

;; Includes buffer names of recently open files, even if they're not open now
(setq ido-use-virtual-buffers t)

;; Enable ido in all contexts where it could be useful, not just buffer names
(ido-ubiquitous-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smex provides a filterable list of possible commands for M-x
;; http://www.emacswiki.org/emacs/Smex
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projectile

;; projectile everywhere! NO! it's suddenly become very slow...
;; (projectile-global-mode)

;; change root automatically when changing projectile project
(with-eval-after-load "projectile"
  (setq projectile-switch-project-action 'neotree-projectile-action))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer and window navigation
(global-set-key (kbd "M-<tab>") (lambda () (interactive) (other-window 1)))
(global-set-key (kbd "M-S-<tab>") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "M-s-<right>") #'switch-to-next-buffer)
(global-set-key (kbd "M-s-<left>") #'switch-to-prev-buffer)
(global-set-key (kbd "C-x C-k") #'kill-this-buffer)
(global-set-key (kbd "s-k") #'kill-this-buffer)
(global-set-key (kbd "s-K") #'kill-buffer-and-window)
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "s-b") #'ibuffer)
; Do I need this?
;(global-set-key (kbd "s-k")
                ;; kill-this-buffer fails sometimes under OSX
                ;; see e.g. https://github.com/syl20bnr/spacemacs/issues/4929
;                (lambda () (interactive) (kill-buffer (current-buffer))))


(setq
 ;; Keep popping the mark from the right after C-u C-SPC 
 set-mark-command-repeat-pop t
 ;; Keep at most these many marks in the buffer's mark ring
 mark-ring-max 32)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FIXME: Sane navigation of the global mark ring.
;; Adapted from http://stackoverflow.com/a/27661338
;;

(setq mdb--cycle-marks nil)

(defun mdb--marker-is-point-p (marker)
  "Test if marker is current point"
  (and (eq (marker-buffer marker) (current-buffer))
       (= (marker-position marker) (point))))

(defun mdb--push-mark-maybe () 
  "Push mark onto `global-mark-ring' if mark head or tail is not current location"
  (if (not global-mark-ring) (error "global-mark-ring empty")
    (unless (or (mdb--marker-is-point-p (car global-mark-ring))
                (mdb--marker-is-point-p (car (reverse global-mark-ring))))
      (push-mark))))

(defun mdb-backward-global-mark () 
  "Use `pop-global-mark', pushing current point if not on ring."
  (interactive)
  (mdb--push-mark-maybe)
  (when (mdb--marker-is-point-p (car global-mark-ring))
    (call-interactively #'pop-global-mark))
  (call-interactively #'pop-global-mark))

(defun mdb-forward-global-mark ()
  "Hack `pop-global-mark' to go in reverse, pushing current point if not on ring."
  (interactive)
  (mdb--push-mark-maybe)
  (setq global-mark-ring (nreverse global-mark-ring))
  (when (mdb--marker-is-point-p (car global-mark-ring))
    (call-interactively #'pop-global-mark))
  (call-interactively #'pop-global-mark)
  (setq global-mark-ring (nreverse global-mark-ring)))

(defun mdb--done-with-local-mark-ring ()
  "FIXME: this is WRONG."
  (or (equal mdb--cycle-marks mark-ring)
      (> (length mdb--cycle-marks) (length mark-ring))))

(defun mdb-backward-mark-dwi ()
  "TODO: Pop the buffer's mark until we hit the end of the ring,
then use the global mark ring."
  (interactive)
  ;; FIXME: this is bogus
  (if (mdb--done-with-local-mark-ring)
      (progn (message "jumping to previous global mark")
             (mdb-backward-global-mark)
             (setq mdb--cycle-marks nil))
    (progn (message "cycling locally (%d)" (length mdb--cycle-marks))
           (setq mdb--cycle-marks (cons (mark-marker) mdb--cycle-marks))
           (pop-to-mark-command))))

(defun mdb-forward-mark-dwi ()
  "TODO: Reverse `mdb-backward-mark-dwi' "
  (interactive)
  )

(global-set-key (kbd "C-S-<left>") #'mdb-backward-mark-dwi)
(global-set-key (kbd "C-S-<right>") #'mdb-forward-mark-dwi)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Undoing kill-buffer. Adapted from http://stackoverflow.com/a/2227692
;;
;; FIXME: the mark-ring for the closing buffer is already empty when
;; the hook is called, so it makes no sense to reimplement what
;; recentf does in recentf-track-closed-file

;; (defvar mdb--killed-buffers (list))

;; (defun mdb--track-killed-buffer ()
;;   (message buffer-file-name)
;;   (and buffer-file-name ;mark-ring
;;        (add-to-list 'mdb--killed-buffers
;;                     (cons buffer-file-name (car mark-ring)))))

;; (defun mdb--last-killed-buffers ()
;;   (interactive)
  
;;   (find-file (ido-completing-read "Last closed: "
;;                                   (mapcar 'car mdb--killed-buffers))))

;; (add-hook 'kill-buffer-hook 'mdb--track-killed-buffer)

;; ;; Borrowed from https://www.emacswiki.org/emacs/RecentFiles
;; (defun mdb--undo-kill-buffer (arg)
;;   "Re-open the last buffer killed.  With ARG, re-open the nth buffer."
;;   (interactive "p")
;;   (let ((recently-killed-list (mapcar 'car mdb--killed-buffers))
;; 	 (buffer-files-list
;; 	  (delq nil (mapcar (lambda (buf)
;; 			      (when (buffer-file-name buf)
;; 				(expand-file-name (buffer-file-name buf))))
;;                             (buffer-list))))
;;          (n (if arg arg 0)))
;;     ; Remove open buffers from recently-killed-list
;;     (mapc
;;      (lambda (buf-file)
;;        (setq recently-killed-list
;; 	     (delq buf-file recently-killed-list)))
;;      buffer-files-list)
;;     (let ((file (nth n recently-killed-list))
;;           (marker (cdr (assoc file mdb--killed-buffers))))
;;       (find-file file)
;;       (if marker (goto-char (marker-position marker))))))

;; Borrowed from https://www.emacswiki.org/emacs/RecentFiles
(defun undo-kill-buffer (arg)
  "Re-open the last buffer killed.  With ARG, re-open the nth buffer."
  (interactive "p")
  (let ((recently-killed-list (copy-sequence recentf-list))
	 (buffer-files-list
	  (delq nil (mapcar (lambda (buf)
			      (when (buffer-file-name buf)
				(expand-file-name (buffer-file-name buf))))
                            (buffer-list)))))
    (mapc
     (lambda (buf-file)
       (setq recently-killed-list
	     (delq buf-file recently-killed-list)))
     buffer-files-list)
    (find-file
     (if arg (nth arg recently-killed-list)
       (car recently-killed-list)))))

(global-set-key (kbd "s-W") #'undo-kill-buffer)
