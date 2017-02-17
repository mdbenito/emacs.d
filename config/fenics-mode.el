;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; First steps towards a minor fenics-mode 
;; Create your scripts with a line at the top like
;; # -*- fenics/dependencies: ("ffc" "ufl") -*-

(require 'realgud)

(defvar fenics/output-buffer "*fenics-build*")

(make-variable-buffer-local
 (defvar fenics/dependencies '()
   "A list of strings with the fenics dependencies 
(e.g. '(\"ffc\" \"ufl\")) for a buffer."))

(defun fenics/tramp-current-host-safe ()
  "HACK: Check whether the current buffer is inside the fenics container via tramp."
  (when (tramp-tramp-file-p (buffer-file-name))
    (tramp-file-name-real-host (tramp-dissect-file-name (buffer-file-name)))))

(defun fenics/remote-file-p ()
  (equal "fenics" (fenics/tramp-current-host-safe)))

(defun fenics/build-dependencies (ignored)
  "HACK!"
  ;; Note: TRAMP runs commands remotely, so we don't need to invoke docker
  (when (fenics/remote-file-p)
    (with-temp-message
        (concat "Bulding " (mapconcat #'indentity nfenics/dependencies " ")
                "...")
      (save-some-buffers #'fenics/remote-file-p)
      (let* ((bash-cmd (concat ". /home/fenics/fenics.env.conf && /home/fenics/bin/fenics-build "
                               (mapconcat #'indentity fenics/dependencies " "))))
        (shell-command bash-cmd fenics/output-buffer fenics/output-buffer)))))

(defvar fenics-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s-d") #'realgud:pdb)
    map)
  "Keymap for fenics minor mode.")

;;;###autoload
(define-minor-mode fenics-minor-mode
  "Minor mode for fenics + tramp + docker + ...
Key bindings:
\\{fenics-minor-mode-map}"
  :lighter " fen"
  :keymap fenics-minor-mode-map
  (if fenics-minor-mode
      (advice-add #'realgud:pdb :before #'fenics/build-dependencies)
    (advice-remove #'realgud:pdb #'fenics/build-dependencies)))

(provide 'fenics-minor-mode)
