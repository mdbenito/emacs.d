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
(defun get-string-from-file (file-path)
;; thanks to Pascal J Bourguignon and TheFlyingDutchman <zzbba…@aol.com>
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(setq tramp-default-method "ssh")
(setq paradox-github-token (get-string-from-file "paradox-token"))
