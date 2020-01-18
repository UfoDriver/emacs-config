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
 '(css-indent-offset 2)
 '(custom-enabled-themes (quote (tango-dark)))
 '(custom-safe-themes
   (quote
    ("3448e3f5d01b39ce75962328a5310438e4a19e76e4b691c21c8e04ca318a5f62" default)))
 '(ecb-options-version "2.40")
 '(fill-column 80)
 '(flycheck-gcc-language-standard "c++11")
 '(geiser-active-implementations (quote (guile)))
 '(geiser-default-implementation (quote guile))
 '(geiser-guile-binary "guile2.2")
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-prefix-key "-cg")
 '(helm-gtags-pulse-at-cursor t)
 '(helm-gtags-suggested-key-mapping t)
 '(helm-gtags-use-input-at-cursor t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(jedi:complete-on-dot t)
 '(js-switch-indent-offset 2)
 '(js2-basic-offset 2)
 '(lsp-enable-snippet nil)
 '(magit-push-always-verify nil)
 '(markdown-command "markdown2 -x tables -x smarty-pants -x strike")
 '(markdown-command-needs-filename t)
 '(menu-bar-mode nil)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (1 ((control)) ((shift) . 5))))
 '(package-archives
   (quote
    (("marmalade" . "http://marmalade-repo.org/packages/")
     ("melpa" . "http://melpa.milkbox.net/packages/")
     ("original" . "http://tromey.com/elpa/")
     ("gnu" . "https://elpa.gnu.org/packages/"))))
 '(package-selected-packages
   (quote
    (hy-mode nasm-mode flycheck-mypy queue cider fsm jabber jabber-otr helm-unicode iedit use-package lsp-ui ccls company-lsp scad-mode geiser ediprolog ctags liso-theme json-navigator company-ansible flycheck-pycheckers json-mode elisp-slime-nav slime-company notify slime drag-stuff helm-gtags rpm-spec-mode company-qml qml-mode graphviz-dot-mode stickyfunc-enhance dockerfile-mode cython-mode feature-mode helm-emmet helm-package yaml-mode xmlgen scss-mode request-deferred python-pep8 pymacs pyflakes pycomplete pos-tip multi-web-mode marmalade markdown-mode+ magit jsx-mode json-rpc jinja2-mode jade-mode helm-projectile helm-css-scss helm-ag fuzzy fill-column-indicator fabric emmet-mode discover-js2-refactor company-tern company-jedi)))
 '(projectile-completion-system (quote helm))
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "env" ".env" ".mypy_cache")))
 '(projectile-project-root-files-top-down-recurring
   (quote
    ("compile_commands.json" ".ccls" ".svn" "CVS" "Makefile")) nil nil "Customized with use-package ccls")
 '(projectile-switch-project-action (quote helm-projectile-find-file))
 '(safe-local-variable-values
   (quote
    ((whitespace-line-column . 120)
     (whitespace-line-column . 100))))
 '(scheme-program-name "guile2.2")
 '(scroll-bar-mode nil)
 '(scroll-step 1)
 '(show-paren-mode t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(whitespace-display-mappings (quote ((space-mark 32) (newline-mark 13) (tab-mark 9))))
 '(whitespace-line-column 100)
 '(whitespace-style
   (quote
    (face trailing tabs spaces lines-tail newline empty indentation space-after-tab space-before-tab space-mark tab-mark newline-mark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#2e3436" :foreground "#eeeeec" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 70 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(magit-diff-add ((t (:inherit nil :foreground "green"))))
 '(magit-diff-del ((t (:inherit diff-removed :foreground "red"))))
 '(magit-item-highlight ((t (:inherit highlight-))))
 '(whitespace-empty ((t (:background "DarkGoldenrod4" :foreground "firebrick"))))
 '(whitespace-indentation ((t (:background "gray15" :foreground "firebrick"))))
 '(whitespace-space-after-tab ((t (:background "gray15" :foreground "firebrick"))))
 '(whitespace-space-before-tab ((t (:background "gray15" :foreground "firebrick"))))
 '(whitespace-tab ((t (:background "gray18")))))

(add-to-list 'load-path "~/.emacs.d/site-lisp/")

(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)
;; (require 'use-package)

;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

;; -----------------------------------------------------------------------------
;; GLOBAL SETTINGS
;; -----------------------------------------------------------------------------

;; http://cachestocaches.com/2015/8/getting-started-use-package/
;; https://github.com/jwiegley/use-package
(eval-when-compile
  ;;  Following line is not needed if use-package.el is in ~/.emacs.d
  ;; (add-to-list 'load-path "<path where use-package is installed>")
  ;; (package-activate 'use-package)
  (require 'use-package))

(setq use-package-verbose t)

(global-whitespace-mode)
;; Selection now like in other editors
(delete-selection-mode t)
;; Electric pairs are useful when gets used
(electric-pair-mode)

;; Asm mode tabs and tab width
(add-hook 'asm-mode-hook
          (lambda ()
            (set (make-local-variable 'indent-tabs-mode) t)
            (set (make-local-variable 'tab-width) 10)))


;; -----------------------------------------------------------------------------
;; PACKAGES
;; -----------------------------------------------------------------------------

;; https://www.reddit.com/r/emacs/comments/audffp/tip_how_to_use_a_stable_and_fast_environment_to/
(use-package lsp-mode
  :hook (prog-mode . lsp))

(use-package lsp-ui
  :defer t)

(use-package company-lsp
  :defer t)

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
  (slime-setup '(slime-company slime-repl inferior-slime slime-fancy slime-presentations
                               slime-presentation-streams))
  (let ((hyperspec-dir (expand-file-name (concat user-emacs-directory "/HyperSpec/"))))
    (if (file-directory-p hyperspec-dir)
        (setq-default common-lisp-hyperspec-root hyperspec-dir))))

(use-package drag-stuff
  :config
  (drag-stuff-global-mode t)
  (drag-stuff-define-keys))

(use-package magit
  :defer t
  :init
  (setq-default magit-last-seen-setup-instructions "1.4.0"))

(use-package company
  :init
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

(use-package uniquify
  :init
  (setq-default uniquify-buffer-name-style 'post-forward uniquify-separator ":"))

(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :bind ((:map projectile-command-map)
         ("s s" . helm-projectile-ag)))

(use-package helm
  :bind
  (("M-x" . helm-M-x)
   ("M-y" . helm-show-kill-ring)
   ("C-x C-b" . helm-mini)
   ("C-x C-f" . helm-find-files))
  :init
  (helm-mode 1))

(use-package helm-projectile
  :config
  (helm-projectile-on))

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
;; CUSTOM FUNCTIONS
;; -----------------------------------------------------------------------------

;; Set window width
(defun my:set-window-width (x)
  "Set current window width to X chars."
  (adjust-window-trailing-edge (selected-window)
                               (- x (window-width))
                               t))

;; Set window width to 80 columns
(defun my:set-80-columns ()
  "Set current window to 80 columns width."
  (interactive)
  (my:set-window-width 81))


;; Setting frame name if projectile project found
(defun my:setup-frame-name ()
  "Set up frame name with projectile support."
  (set-frame-name
   (if (string= (projectile-project-name) "-")
       (progn (message "Projectile project not found")
              default-directory)
     (capitalize (projectile-project-name)))))

;; -----------------------------------------------------------------------------
;; TO SORT OUT
;; -----------------------------------------------------------------------------

;; Yay, nothing is here

;; -----------------------------------------------------------------------------
;; LOCAL INITIALIZATION
;; -----------------------------------------------------------------------------

(if (file-exists-p "~/.emacs.d/local-init.el")
    (load-file "~/.emacs.d/local-init.el")
  (message "No local-init.el found"))

(my:setup-frame-name)
;;; init.el ends here
