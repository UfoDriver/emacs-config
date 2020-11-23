;;; package --- Emacs init file
;;; Commentary:
;;; my custom Emacs initialization file
;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(column-number-mode t)
 '(company-box-icons-alist 'company-box-icons-icons-in-terminal)
 '(company-minimum-prefix-length 1)
 '(company-tooltip-idle-delay 0)
 '(css-indent-offset 2)
 '(custom-enabled-themes '(tango-dark))
 '(custom-safe-themes
   '("3448e3f5d01b39ce75962328a5310438e4a19e76e4b691c21c8e04ca318a5f62" default))
 '(delete-selection-mode t)
 '(ecb-options-version "2.40")
 '(electric-pair-mode t)
 '(fill-column 80)
 '(flycheck-gcc-language-standard "c++11")
 '(gc-cons-threshold 1000000)
 '(geiser-active-implementations '(guile))
 '(geiser-default-implementation 'guile)
 '(geiser-guile-binary "guile2.2")
 '(global-company-mode t)
 '(global-whitespace-mode t)
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-prefix-key "-cg")
 '(helm-gtags-pulse-at-cursor t)
 '(helm-gtags-suggested-key-mapping t)
 '(helm-gtags-use-input-at-cursor t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(jedi:complete-on-dot t)
 '(js-indent-level 2)
 '(js-switch-indent-offset 2)
 '(js2-basic-offset 2)
 '(lsp-enable-snippet nil)
 '(lsp-pyls-server-command '("pyls" "-v"))
 '(magit-push-always-verify nil)
 '(markdown-command "markdown2 -x tables -x smarty-pants -x strike")
 '(markdown-command-needs-filename t)
 '(menu-bar-mode nil)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount '(1 ((control)) ((shift) . 5)))
 '(package-archives
   '(("marmalade" . "http://marmalade-repo.org/packages/")
     ("melpa" . "http://melpa.org/packages/")
     ("original" . "http://tromey.com/elpa/")
     ("gnu" . "https://elpa.gnu.org/packages/")
     ("org" . "https://orgmode.org/elpa/")))
 '(package-selected-packages
   '(helm-lsp which-key diminish typescript-mode vue-mode svg company-box yasnippet-snippets helm-slime docker-tramp hy-mode nasm-mode flycheck-mypy queue cider fsm jabber jabber-otr helm-unicode iedit use-package lsp-ui ccls company-lsp scad-mode geiser ediprolog ctags liso-theme json-navigator company-ansible flycheck-pycheckers json-mode elisp-slime-nav slime-company notify slime drag-stuff helm-gtags rpm-spec-mode company-qml qml-mode graphviz-dot-mode stickyfunc-enhance dockerfile-mode cython-mode feature-mode helm-emmet helm-package yaml-mode xmlgen scss-mode request-deferred python-pep8 pymacs pyflakes pycomplete pos-tip multi-web-mode marmalade markdown-mode+ magit jsx-mode json-rpc jinja2-mode jade-mode helm-projectile helm-css-scss helm-ag fuzzy fill-column-indicator fabric emmet-mode discover-js2-refactor company-tern company-jedi))
 '(projectile-completion-system 'helm)
 '(projectile-globally-ignored-directories
   '(".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "env" ".env" ".mypy_cache"))
 '(projectile-project-root-files-top-down-recurring '("compile_commands.json" ".ccls" ".svn" "CVS" "Makefile") nil nil "Customized with use-package ccls")
 '(projectile-switch-project-action 'helm-projectile-find-file)
 '(safe-local-variable-values
   '((whitespace-line-column . 120)
     (whitespace-line-column . 100)))
 '(scheme-program-name "guile2.2")
 '(scroll-bar-mode nil)
 '(scroll-step 1)
 '(show-paren-mode t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style 'post-forward nil (uniquify))
 '(uniquify-separator ":")
 '(use-package-verbose t)
 '(whitespace-display-mappings '((space-mark 32) (newline-mark 13) (tab-mark 9)))
 '(whitespace-line-column 100)
 '(whitespace-style
   '(face trailing tabs spaces lines-tail newline empty indentation space-after-tab space-before-tab space-mark tab-mark newline-mark)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#2e3436" :foreground "#eeeeec" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "PfEd" :family "Iosevka"))))
 '(company-scrollbar-bg ((t (:background "#458d4e9f51a5"))))
 '(company-scrollbar-fg ((t (:background "#39c6414f43d2"))))
 '(company-tooltip ((t (:inherit default :background "#32b539533b87"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
 '(font-lock-comment-face ((t (:foreground "#73d216" :slant oblique))))
 '(font-lock-keyword-face ((t (:foreground "#b4fa70" :weight bold))))
 '(magit-diff-add ((t (:inherit nil :foreground "green"))))
 '(magit-diff-del ((t (:inherit diff-removed :foreground "red"))))
 '(magit-item-highlight ((t (:inherit highlight-))))
 '(whitespace-empty ((t (:background "DarkGoldenrod4" :foreground "firebrick"))))
 '(whitespace-indentation ((t (:background "gray15" :foreground "firebrick"))))
 '(whitespace-space-after-tab ((t (:background "gray15" :foreground "firebrick"))))
 '(whitespace-space-before-tab ((t (:background "gray15" :foreground "firebrick"))))
 '(whitespace-tab ((t (:background "gray18")))))

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
  (lsp-headerline-breadcrumb-mode)
  (setq read-process-output-max 8192))

(use-package lsp-ui
  :defer t
  :hook
  (lsp-mode . lsp-ui-mode))

(use-package company-lsp
  :defer t)

(use-package company-box
  :diminish
  :hook (company-mode . company-box-mode))

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

;; (use-package geiser
;;   :defer t
;;   ;; :commands (geiser-eval-last-sexp)
;;   :bind-keymap ("C-c" . geiser-map))

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
