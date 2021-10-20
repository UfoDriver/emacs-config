;;; package --- Emacs init file
;;; Commentary:
;;; my custom Emacs initialization file
;;; Code:
(setq custom-file "~/.emacs.d/custom.el")
(load "~/.emacs.d/custom.el")

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/")

(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
;; (setq use-package-always-ensure t)

;; -----------------------------------------------------------------------------
;; GLOBAL SETTINGS
;; -----------------------------------------------------------------------------

;; http://cachestocaches.com/2015/8/getting-started-use-package/
;; https://github.com/jwiegley/use-package
;; (eval-when-compile
;;   ;;  Following line is not needed if use-package.el is in ~/.emacs.d
;;   ;; (add-to-list 'load-path "<path where use-package is installed>")
;;   ;; (package-activate 'use-package)
;;   (require 'use-package))

;; -----------------------------------------------------------------------------
;; CUSTOM FUNCTIONS
;; -----------------------------------------------------------------------------

(defun my:set-window-width (x)
  "Set current window width to X chars."
  (adjust-window-trailing-edge (selected-window)
                               (- x (window-width))
                               t))

(defun my:set-80-columns ()
  "Set current window to 80 columns width."
  (interactive)
  (my:set-window-width 81))

(defun my:setup-frame-name ()
  "Set up frame name with projectile support."
  (set-frame-name
   (if (string= (projectile-project-name) "-")
       (progn (message "Projectile project not found")
              default-directory)
     (capitalize (projectile-project-name)))))

;; (advice-add 'kill-buffer :around (lambda (orig-func &rest args)
;;                                    (notify "Emacs" "Before")
;;                                    (let ((helm-display-function 'helm-default-display-buffer))
;;                                      (apply orig-func args)
;;                                      (notify "Emacs" "After")
;;                                      )))

(defun my:notes ()
  "Open index org file."
  (interactive)
  (find-file "~/Documents/Org-files/index.org"))

; Based on https://www.reddit.com/r/emacs/comments/7rho4f/now_you_can_use_helm_with_frames_instead_of/
(defun my:helm-display-frame-center (buffer &optional resume)
  "Display `helm-buffer' in a separate frame which centered in parent frame."
  (if (not (display-graphic-p))
      ;; Fallback to default when frames are not usable.
      (helm-default-display-buffer buffer)
    (setq helm--buffer-in-new-frame-p t)
    (let* ((parent (selected-frame))
           (frame-pos (frame-position parent))
           (parent-left (car frame-pos))
           (parent-top (cdr frame-pos))
           (width (/ (frame-width parent) 2))
           (height (- (frame-height parent) 10))
           tab-bar-mode
           (default-frame-alist
             (if resume
                 (buffer-local-value 'helm--last-frame-parameters
                                     (get-buffer buffer))
               `((parent . ,parent)
                 (width . ,width)
                 (height . ,height)
                 (left-fringe . 0)
                 (right-fringe . 0)
                 (tool-bar-lines . 0)
                 (line-spacing . 0)
                 (desktop-dont-save . t)
                 (no-special-glyphs . t)
                 (inhibit-double-buffering . t)
                 (tool-bar-lines . 0)
                 (left . ,(+ parent-left (/ (* (frame-char-width parent) (frame-width parent)) 4)))
                 (top . ,(+ parent-top (/ (* (frame-char-width parent) (frame-height parent)) 5)))
                 (title . "Helm")
                 (vertical-scroll-bars . nil)
                 (menu-bar-lines . 0)
                 (fullscreen . nil)
                 (visible . ,(null helm-display-buffer-reuse-frame))
                 (skip-taskbar . t)
                 (undecorated . t)
                 (internal-border-width . 1))))
           display-buffer-alist)
      (set-face-background 'internal-border (face-foreground 'default))
      (helm-display-buffer-popup-frame buffer default-frame-alist))
    (helm-log-run-hook 'helm-window-configuration-hook)))

;; -----------------------------------------------------------------------------
;; PACKAGES
;; -----------------------------------------------------------------------------

(use-package diminish :ensure t)

(use-package meson-mode
  :defer t)

(use-package emacs
  :config
  (global-set-key (kbd "M-u") 'upcase-dwim)
  (global-set-key (kbd "M-l") 'downcase-dwim)
  (global-unset-key (kbd "C-x C-u"))
  (global-unset-key (kbd "C-x C-l"))

  ; https://karthinks.com/software/batteries-included-with-emacs/
  (defun pulse-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))
  (dolist (command '(scroll-up-command scroll-down-command
                                       recenter-top-bottom other-window))
    (advice-add command :after #'pulse-line))

  (define-advice kill-ring-save (:before (start end &rest rest))
    (pulse-momentary-highlight-region start end)))

(use-package whitespace
  :diminish)

(use-package asm-mode
  :defer t
  :hook
  (asm-mode .
            (lambda ()
              (setq-local
               indent-tabs-mode t
               tab-width 10))))


;; https://www.reddit.com/r/emacs/comments/audffp/tip_how_to_use_a_stable_and_fast_environment_to/
(use-package lsp-mode
  :hook (prog-mode . lsp)
  :config
  (setq read-process-output-max 8192)
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))

;; (use-package lsp-ui
;;   :defer t
;;   :hook
;;   (lsp-mode . lsp-ui-mode))

(use-package lsp-ui
  :defer t)

(use-package company-lsp
  :defer t)

(use-package helm-lsp
  :commands helm-lsp-workspace-symbol)

;; (use-package dap-mode)

;; Next form is copypaste and should be cleaned
(use-package ccls
  :defer t
  :after projectile
;  :ensure-system-package ccls
  :custom
  (ccls-args nil)
  ;; (ccls-executable (executable-find "/home/alex/tmp/ccls/Release/ccls"))
  )

(use-package slime
  :defer t
  :init
  (setq-default inferior-lisp-program "/usr/bin/sbcl")
  :config
  (load (expand-file-name "~/.local/lib/quicklisp/slime-helper.el"))
  (slime-setup '(slime-company slime-repl inferior-slime slime-fancy slime-presentations
                               slime-presentation-streams helm-slime))
  (let ((hyperspec-dir (expand-file-name (concat user-emacs-directory "/HyperSpec/"))))
    (if (file-directory-p hyperspec-dir)
        (setq-default common-lisp-hyperspec-root hyperspec-dir))))

(use-package drag-stuff
  :diminish
  :config
  (drag-stuff-global-mode t)
  (drag-stuff-define-keys))

(use-package magit
  :defer t
  :init
  (setq-default magit-last-seen-setup-instructions "1.4.0"))

(use-package company
  :diminish
  :config
  (global-company-mode))

(use-package flycheck
  :defer t
  :init
  (global-flycheck-mode))

(use-package feature-mode
  :defer t)

(use-package jinja2-mode
  :defer t)

(use-package hy-mode
  :mode ("\\.hy\\'" . hy-mode))

(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :bind ((:map projectile-command-map)
         ("s s" . helm-projectile-ag))
  :config
  (my:setup-frame-name))

(use-package helm
  :diminish
  :bind
  (("M-x" . helm-M-x)
   ("M-y" . helm-show-kill-ring)
   ("C-x C-b" . helm-mini)
   ("C-x C-f" . helm-find-files))
  :init
  (helm-mode 1)
;  :config
;  (setq helm-display-function 'my:helm-display-frame-center)
)

(use-package helm-projectile
  :config
  (helm-projectile-on))

(use-package which-key
  :diminish
  :config
  (which-key-mode))

(use-package geiser
  :defer t
  ;; :commands (geiser-eval-last-sexp)
  ;; :bind-keymap ("C-c" . geiser-map)
  )

;; -----------------------------------------------------------------------------
;; ENABLED DISABLED BY DEFAULT COMMANDS
;; -----------------------------------------------------------------------------

(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; -----------------------------------------------------------------------------
;; CUSTOM KEYBINDINGS
;; -----------------------------------------------------------------------------

;; Enter for newline-and-indent in programming modes
(add-hook 'prog-mode-hook '(lambda ()
  (local-set-key (kbd "RET") 'newline-and-indent)))

;; -----------------------------------------------------------------------------
;; TO SORT OUT
;; -----------------------------------------------------------------------------

;; Yay, nothing is here

;; -----------------------------------------------------------------------------
;; LOCAL INITIALIZATION
;; -----------------------------------------------------------------------------

(load-file "~/.emacs.d/icons-in-terminal.el")
(load-file "~/.emacs.d/icons-in-terminal-local.el")

(if (file-exists-p "~/.emacs.d/local-init.el")
    (load-file "~/.emacs.d/local-init.el")
  (message "No local-init.el found"))

;; (my:setup-frame-name)

(require 'color)

(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

;;; init.el ends here
