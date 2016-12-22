;;; modern-cpp-font-lock-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (modern-c++-font-lock-global-mode modern-c++-font-lock-mode)
;;;;;;  "modern-cpp-font-lock" "modern-cpp-font-lock.el" (22497 14461
;;;;;;  938837 218000))
;;; Generated autoloads from modern-cpp-font-lock.el

(autoload 'modern-c++-font-lock-mode "modern-cpp-font-lock" "\
Provides font-locking as a Minor Mode for Modern C++

\(fn &optional ARG)" t nil)

(defvar modern-c++-font-lock-global-mode nil "\
Non-nil if Modern-C++-Font-Lock-Global mode is enabled.
See the command `modern-c++-font-lock-global-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `modern-c++-font-lock-global-mode'.")

(custom-autoload 'modern-c++-font-lock-global-mode "modern-cpp-font-lock" nil)

(autoload 'modern-c++-font-lock-global-mode "modern-cpp-font-lock" "\
Toggle Modern-C++-Font-Lock mode in all buffers.
With prefix ARG, enable Modern-C++-Font-Lock-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Modern-C++-Font-Lock mode is enabled in all buffers where
`(lambda nil (when (apply (quote derived-mode-p) (quote (c++-mode))) (modern-c++-font-lock-mode 1)))' would do it.
See `modern-c++-font-lock-mode' for more information on Modern-C++-Font-Lock mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("modern-cpp-font-lock-pkg.el") (22497
;;;;;;  14462 69578 992000))

;;;***

(provide 'modern-cpp-font-lock-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; modern-cpp-font-lock-autoloads.el ends here
