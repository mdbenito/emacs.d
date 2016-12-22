;; System specific stuff

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sets up exec-path-from shell so that Emacs has the right environment vars
;; https://github.com/purcell/exec-path-from-shell

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(if (eq system-type 'darwin)
    (progn
      (require 'exec-path-from-shell)
      (when (memq window-system '(mac ns))
        (exec-path-from-shell-initialize)
        (exec-path-from-shell-copy-envs '("PATH")))))

;; Note that your shell will inherit Emacssenvironment variables when it is run
;; to avoid surprises your config files should therefore set the environment
;; variables to their exact desired final values, i.e. don't do this:
;;     export PATH=/usr/local/bin:$PATH
;; but instead do this:
;;     export PATH=/usr/local/bin:/usr/bin:/bin

