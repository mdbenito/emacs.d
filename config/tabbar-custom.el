;; Tabbar
(require 'tabbar)
(tabbar-mode 1)

;; Change padding of the tabs
;; we also need to set separator to avoid overlapping tabs by highlighted tabs
(custom-set-variables
 '(tabbar-separator (quote (" "))))

(defun mdb--tabbar-buffer-tab-label (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar."
  (let ((label (if tabbar--buffer-show-groups
                   (format "[%s]" (tabbar-tab-tabset tab))
                 (format "%s" (tabbar-tab-value tab)))))
    ;; Unless the tab bar auto scrolls to keep the selected tab
    ;; visible, shorten the tab label to keep as many tabs as possible
    ;; in the visible area of the tab bar.
    (if tabbar-auto-scroll-flag
        label
      (tabbar-shorten
       label (max 1 (/ (window-width)
                       (length (tabbar-view
                                (tabbar-current-tabset)))))))))

(defun mdb--tabbar-buffer-help-on-tab (tab)
  "Return the help string shown when mouse is onto TAB."
  (let* ((tabset (tabbar-tab-tabset tab))
         (tab (tabbar-selected-tab tabset))
         (buf (tabbar-tab-value tab))
;         (name (buffer-name buf))
         (file (buffer-file-name buf)))
    (if tabbar--buffer-show-groups
        (format "%s\nmouse-1: switch to this buffer in group [%s]"
                file tabset)
      (format "%s\nmouse-1: switch to this buffer\n\
mouse-2: pop to buffer\nmouse-3: delete other windows"
              file))))

; I'd like to use all-the-icons here, e.g. (all-the-icons-faicon "caret-left")
(setq tabbar-use-images            nil
      ; (selected . deselected), where (de)selected = (str . image)
      tabbar-scroll-left-button    (cons (cons " " nil) (cons " =" nil))
      ; (selected . deselected), where (de)selected = (str . image)
      tabbar-scroll-right-button   (cons (cons " " nil) (cons "= " nil))
      tabbar-tab-label-function    #'mdb--tabbar-buffer-tab-label
      tabbar-help-on-tab-function  #'mdb--tabbar-buffer-help-on-tab)
 
(require 'popup)
(defun mbd--tabbar-press-home ()
  "Diplay a popup with the tab groups to choose from."
  (interactive)
  (tabbar-buffer-show-groups t)
  (let* ((groups (tabbar-view (tabbar-current-tabset t)))
         (make-item (lambda (x) (popup-make-item (symbol-name (cdr x)) :value x)))
         (choice (popup-menu* (mapcar make-item groups) :point (max 1 (- (window-start) 1)))))
    (tabbar-click-on-tab choice)
    (tabbar-buffer-show-groups f)))

(define-key tabbar-mode-map (kbd "C-<escape>") #'mbd--tabbar-press-home)
(define-key tabbar-mode-map (kbd "C-<tab>") #'tabbar-forward-tab)
(define-key tabbar-mode-map (kbd "C-S-<tab>") #'tabbar-backward-tab)


;; Disable redundant information in the mode line
(setq-default
 mode-line-buffer-identification-bak mode-line-buffer-identification
 mode-line-buffer-identification     (if header-line-format nil
                                         mode-line-buffer-identification-bak))

;; (tabbar-mode 1)


