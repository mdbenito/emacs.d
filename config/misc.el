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
