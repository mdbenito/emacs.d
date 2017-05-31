;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages

;; Define package repositories
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; Activate emacs packages (reads autoloads). This also sets the load path.
(package-initialize)

;; Add dirs to load path for (load "blah")
(add-to-list 'load-path "~/.emacs.d/config")
(add-to-list 'load-path "~/.emacs.d/standalone")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration

;; Customized variables
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Whatever
(load "misc.el")

;; OSX / Linux specific stuff (other than shortcuts)
(load "system.el")

;; Navigate files, switch buffers, and choose options from the minibuffer
(load "navigation.el")

;; Change some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Language-specific
(load "setup-elisp.el")
(load "setup-clojure.el")
(load "setup-js.el")
(load "setup-cpp.el")
(load "setup-python.el")
(load "setup-markdown.el")
(load "setup-web.el")
(load "tm-mode.el")
