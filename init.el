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
 '(js-switch-indent-offset 2)
 '(js2-basic-offset 2)
 '(magit-push-always-verify nil)
 '(markdown-command "markdown2-3 -x tables -x smarty-pants -x strike")
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
    (queue cider fsm jabber jabber-otr helm-unicode iedit use-package lsp-ui ccls company-lsp scad-mode geiser csv-mode ediprolog ctags liso-theme json-navigator company-ansible flycheck-pycheckers json-mode elisp-slime-nav slime-company notify slime drag-stuff helm-gtags rpm-spec-mode company-qml qml-mode graphviz-dot-mode stickyfunc-enhance dockerfile-mode cython-mode feature-mode helm-emmet helm-package yaml-mode xmlgen scss-mode rfringe request-deferred python-pep8 pymacs pyflakes pycomplete pos-tip multi-web-mode marmalade markdown-mode+ magit jsx-mode json-rpc jinja2-mode jade-mode helm-projectile helm-css-scss helm-ag fuzzy flymake-python-pyflakes flymake-less flymake-json flymake-jshint flymake-cursor fill-column-indicator fabric emmet-mode discover-js2-refactor company-tern company-jedi)))
 '(projectile-completion-system (quote helm))
 '(projectile-switch-project-action (quote helm-projectile-find-file))
 '(safe-local-variable-values
   ((js2-basic-offset . 2)
    (whitespace-line-column . 120)
    (eval progn
          (helm-mode 1)
          (projectile-mode))
    (css-indent-offset . 2)
    (whitespace-line-column . 100)
    (indent-tabs-mode t)))
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
  ;; (package-activate 'lsp-ui)
  ;; (package-activate 'company-lsp)
  (require 'use-package))


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
;; PACKAGES AND SOURCES
;; -----------------------------------------------------------------------------

;; https://www.reddit.com/r/emacs/comments/audffp/tip_how_to_use_a_stable_and_fast_environment_to/
(use-package lsp-mode
  :hook (prog-mode . lsp))

(use-package lsp-ui)

(use-package company-lsp)
(setq lsp-enable-snippet nil)

(use-package ccls
  :after projectile
;  :ensure-system-package ccls
  :custom
  (ccls-args nil)
  (ccls-executable (executable-find "/home/alex/tmp/ccls/Release/ccls"))
  (projectile-project-root-files-top-down-recurring
   (append '("compile_commands.json" ".ccls")
           projectile-project-root-files-top-down-recurring))
  :config (push ".ccls-cache" projectile-globally-ignored-directories))

(use-package slime
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
  :init
  (setq-default magit-last-seen-setup-instructions "1.4.0"))

(use-package company
  :init
  (global-company-mode))

(use-package flycheck
  :init
  (global-flycheck-mode))

(use-package feature-mode)

(use-package uniquify
  :init
  (setq-default uniquify-buffer-name-style 'post-forward uniquify-separator ":"))


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
;; MODULES AND EXTENSIONS
;; -----------------------------------------------------------------------------
;;(load-file "/home/alex/.emacs.d/emacs-for-python/epy-init.el")
; (require 'epy-setup)      ;; It will setup other loads, it is required!
; (require 'epy-python)     ;; If you want the python facilities [optional]
; (require 'epy-completion) ;; If you want the autocompletion settings [optional]
; (require 'epy-editing)    ;; For configurations related to editing [optional]

;;(epy-setup-checker "pyflakes %f")
;;(epy-django-snippets)
;;(epy-setup-ipython)

;; ;; Jinja2 support
;; (require 'jinja2-mode)

;; ;; Less CSS
;; (require 'less-css-mode)

;; ;; Fill column indicator
;; (require 'fill-column-indicator)
;; (add-hook 'python-mode-hook 'fci-mode)
;(setq fci-rule-width 1)
;(setq fci-rule-color "darkblue")
;fci-rule-character
;(add-hook 'after-change-major-mode-hook 'fci-mode)

;; JS hint mode
;; (require 'flymake-jshint)
;; (flymake-jshint-load)
;; (add-hook 'js-mode-hook
;;      (lambda () (progn
;;                   (setq js-indent-level 2)
;;                   (flymake-mode t))))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . js2-jsx-mode))
(add-hook 'js2-jsx-mode-hook
          (lambda ()
            (setq-local sgml-basic-offset js2-basic-offset)))

;; Right fringe (gutter)
;; (require 'rfringe)
;; (remove-hook 'after-change-major-mode-hook 'rfringe-mode)

;; -----------------------------------------------------------------------------
;; TO SORT OUT
;; -----------------------------------------------------------------------------

;; SASS mode
;;(autoload 'scss-mode "scss-mode")
;;(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

; Django mode
;;(require 'django-html-mode)
;;(require 'django-mode)
;;(yas/load-directory "/home/alex/tmp/emacs/django-mode")

; Emacs code browser
;;(add-to-list 'load-path
;;             "~/tmp/emacs/ecb/")
;;(require 'ecb)

;; (add-to-list 'auto-mode-alist '("\\.jsp$") . web-mode)


;; Org mode todo states
;; (setq org-todo-keywords '("TODO" "BLOCKED" "DONE"))
;; (setq org-todo-keyword-faces
;;       '(("BLOCKED" . org-warning)))


;; Tern - javascript refactoring library settings
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))

;; Helm framework
(require 'helm-config)

;; Projectile and helm integration
(projectile-mode t)
(define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
(helm-projectile-on)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-b") 'helm-mini)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(require 'helm-gtags)
;; Enable helm-gtags-mode
;; (add-hook 'dired-mode-hook 'helm-gtags-mode)
;; (add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
;; (add-hook 'asm-mode-hook 'helm-gtags-mode)

;; (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
;; (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
;; (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
;; (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)


;; -----------------------------------------------------------------------------
;; LOCAL INITIALIZATION
;; -----------------------------------------------------------------------------

(if (file-exists-p "~/.emacs.d/local-init.el")
    (load-file "~/.emacs.d/local-init.el")
  (message "No local-init.el found"))


(defun my:python-mode-hook ()
  "My custom hook for python mode."
  (add-to-list 'company-backends 'company-jedi)
  (jedi:setup))
(add-hook 'python-mode-hook 'my:python-mode-hook)


(setq jedi:complete-on-dot t)

(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(add-to-list 'company-backends 'company-tern)

;; Dirty hack to set jedi:environment-root to active virtualenv (if any)
(let ((python-path (executable-find "python"))
      env-path)
  (unless (or (string-prefix-p "/usr" python-path)
              (string-prefix-p "/bin" python-path))
    ;; Next line is ugly as hell
    (setq env-path
          (file-name-directory
           (directory-file-name
            (file-name-directory python-path))))
    (message (concat "Python virtual environment detected: " env-path))
    (setq jedi:environment-root env-path)))

(my:setup-frame-name)
;;; init.el ends here
