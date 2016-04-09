(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")))

;(smartparens-global-mode t)
;(show-paren-mode 1)
;(require 'smartparens-config)


(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))

(load-theme 'alect-dark t)

;;;(set-face-attribute 'default nil :height 100)

(set-face-attribute 'default nil :font  "Droid Sans Mono-al-12")
(set-frame-font   "Droid Sans Mono-al-12" nil t)

(tool-bar-mode -1)

;;; path matching etc
;;;(require 'helm-config)
;;;(helm-mode 1)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)


(setq default-tab-width 2)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

;;;find things fast
(require 'find-things-fast)
(global-set-key "\C-c\C-f" 'ftf-find-file)
(global-set-key '[f2] 'ftf-grepsource)
(global-set-key '[f4] 'ftf-gdb)
(global-set-key '[f5] 'ftf-compile)

;;neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))



;;; Frame/Window management
(defun make-frame-80-columns ()
  (interactive)
  (set-frame-width (selected-frame) 80))

(defun small-frame ()
  (interactive)
  (set-frame-size (selected-frame) 80 90))

(defun fullscreen-frame ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

(defun other-frame-right ()
  (interactive)
  (other-frame 1))

(defun other-frame-left ()
  (interactive)
  (other-frame -1))


;;; Global minor modes / mode line behavior
(windmove-default-keybindings)
;;;(ido-mode 1)
;;;(tool-bar-mode 0)
;;;(scroll-bar-mode 0)
(show-paren-mode 1)
;;;(transient-mark-mode 1)
;;;(column-number-mode 1)
;;;(subword-mode 1)
;;;(global-auto-revert-mode 1)
;;;(display-time)
;;;(global-auto-complete-mode)
;;;(setq-default ac-sources '(ac-source-words-in-same-mode-buffers))


;; alarm bell control

 (defun unfocused-ding-blink ()
    "Flash the screen when Emacs is focused, ring the bell (ding) if not."
    (setq ring-bell-function `(lambda ()
      (setq visible-bell (fsm-frame-x-active-window-p (selected-frame)))
      (ding)))
  
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Frame-related functions ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; All from http://www.emacswiki.org/emacs-pt/rcircDbusNotification
  
    (require 'dbus)
  
    (defun fsm-x-active-window ()
      "Return the window ID of the current active window in X, as
  given by the _NET_ACTIVE_WINDOW of the root window set by the
  window-manager, or nil if not able to"
      (if (eq (window-system) 'x)
          (let ((x-active-window (x-window-property "_NET_ACTIVE_WINDOW" nil "WINDOW" 0 nil t)))
            (string-to-number (format "%x00%x" (car x-active-window) (cdr x-active-window)) 16))
        nil))
  
    (defun fsm-frame-outer-window-id (frame)
      "Return the frame outer-window-id property, or nil if FRAME not of the correct type"
      (if (framep frame)
          (string-to-number
           (frame-parameter frame 'outer-window-id))
        nil))
  
    (defun fsm-frame-x-active-window-p (frame)
      "Check if FRAME is is the X active windows
  Returns t if frame has focus or nil if"
      (if (framep frame)
          (progn
            (if (eq (fsm-frame-outer-window-id frame)
                    (fsm-x-active-window))
                t
              nil))
        nil))
  )
  (unfocused-ding-blink)

;  (setq coffee-tab-width n) ; coffeescript
 ; (setq javascript-indent-level n) ; javascript-mode
 ; (setq js-indent-level n) ; js-mode
 ; (setq js2-basic-offset n) ; js2-mode, in latest js2-mode, it's alias of js-indent-level
 ; (setq web-mode-markup-indent-offset n) ; web-mode, html tag in html file
 ; (setq web-mode-css-indent-offset n) ; web-mode, css in html file
 ; (setq web-mode-code-indent-offset n) ; web-mode, js code in html file
 ; (setq css-indent-offset n) ; css-mode
  
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(standard-indent 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
