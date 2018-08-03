;  Emacs init file

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(css-indent-offset 2)
 '(custom-enabled-themes (quote (tango-dark)))
 '(ecb-options-version "2.40")
 '(fill-column 80)
 '(flycheck-gcc-language-standard "c++11")
 '(inhibit-startup-screen t)
 '(js-switch-indent-offset 2)
 '(js2-basic-offset 2)
 '(magit-push-always-verify nil)
 '(markdown-command "markdown2-3 -x tables -x smarty-pants -x strike")
 '(markdown-command-needs-filename t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (ediprolog ctags liso-theme json-navigator company-ansible flycheck-pycheckers json-mode elisp-slime-nav slime-company notify slime drag-stuff helm-gtags rpm-spec-mode company-qml qml-mode graphviz-dot-mode stickyfunc-enhance dockerfile-mode cython-mode feature-mode helm-emmet helm-package yaml-mode xmlgen scss-mode rfringe request-deferred pythonic python-pep8 pymacs pyflakes pycomplete pos-tip multi-web-mode marmalade markdown-mode+ magit karma jsx-mode json-rpc jinja2-mode jade-mode helm-projectile helm-css-scss helm-ag fuzzy flymake-python-pyflakes flymake-less flymake-json flymake-jshint flymake-cursor fill-column-indicator fabric emmet-mode discover-js2-refactor company-tern company-jedi)))
 '(safe-local-variable-values
   ((js2-basic-offset . 2)
    (whitespace-line-column . 120)
    (eval progn
          (helm-mode 1)
          (projectile-mode))
    (css-indent-offset . 2)
    (whitespace-line-column . 100)
    (indent-tabs-mode t)))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(whitespace-line-column 100))
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


;; -----------------------------------------------------------------------------
;; GLOBAL SETTINGS
;; -----------------------------------------------------------------------------

;(setq-default show-trailing-whitespace t)

;; Let's see trailing whitespaces and no nonprintable symbols
(setq whitespace-display-mappings
      '((space-mark 32)
        (space-mark 160)
        (newline-mark 10 [10])
        (tab-mark 9)))
(setq whitespace-style
      '(face tabs spaces trailing lines-tail space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark))
(global-whitespace-mode)

;; Indenting with spaces and tab is equal to 4 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Disabling the auto-pairing of parenthesis
;(setq skeleton-pair nil)

;; Line delimiter
(setq default-buffer-file-coding-system 'utf-8-unix)

;; Scrolling tweaks
(setq scroll-step 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't)

;; Unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward uniquify-separator ":")

;; HTML indent
(setq sgml-basic-offset 4)
;; To make this change specific only to html-mode, you can use the following
;; code:
(add-hook 'html-mode-hook
          (lambda ()
            ;; Default indentation is usually 2 spaces, changing to 4.
            (set (make-local-variable 'sgml-basic-offset) 2)
            (emmet-mode)))

;; Smarter minibuffer autocompletion
(require 'ido)
(ido-mode t)

;; Asm mode tabs and tab width
(add-hook 'asm-mode-hook
          (lambda ()
            ;; Default indentation is usually 2 spaces, changing to 4.
            (set (make-local-variable 'indent-tabs-mode) t)
            (set (make-local-variable 'tab-width) 8)))

;; Better buffer list
;; (global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("JavaScript" (or
                              (mode . javascript-mode)
                              (mode . js-mode)
                              (mode . js2-mode)))
               ("Web" (or
                       (mode . web-mode)
                       (mode . html-mode)
                       (name . "^.*\\.jspf$")))
               ("Emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))
               ("Services" (name . "^\\*.*\\*$"))
               ("Rubies" (name . "^.*\\.rb$"))
               ("Pythons" (name . "^.*\\.py$"))
               ("Emacs Lisp" (name . "^.*\\.el$"))
               ("Dired" (mode . dired-mode))
               ("CSS" (or
                       (mode . css-mode)
                       (mode . less-css-mode)))))))


;; Selection now like in other editors
(delete-selection-mode t)

;; Electric pairs are useful when gets used
(electric-pair-mode)

;; -----------------------------------------------------------------------------
;; PACKAGES AND SOURCES
;; -----------------------------------------------------------------------------

(require 'package)
(setq package-archives
      '(("original"  . "http://tromey.com/elpa/")
        ("gnu"       . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa"     . "http://melpa.milkbox.net/packages/")))

(package-initialize)

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
  "Set current window width"
  (adjust-window-trailing-edge (selected-window)
                               (- x (window-width))
                               t))

;; Set window width to 80 columns
(defun my:set-80-columns ()
  "Set current window to 80 columns width"
  (interactive)
  (my:set-window-width 81))


;; Setting frame name if projectile project found
(defun my:setup-frame-name ()
  (set-frame-name
   (if (string= (projectile-project-name) "-")
       (progn (message "Projectile project not found")
              default-directory)
     (capitalize (projectile-project-name)))))

;; -----------------------------------------------------------------------------
;; MODULES AND EXTENSIONS
;; -----------------------------------------------------------------------------
(global-company-mode)
(global-flycheck-mode)

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
(require 'rfringe)
;; (remove-hook 'after-change-major-mode-hook 'rfringe-mode)

;; Feature mode (cucumber, lettuce)
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

;; Magit - git integration
(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")

(drag-stuff-global-mode t)
(drag-stuff-define-keys)

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

; YAML mode
;;(require 'yaml-mode)
;;(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))


; Emacs code browser
;;(add-to-list 'load-path
;;             "~/tmp/emacs/ecb/")
;;(require 'ecb)


; MozRepl
;;(require 'moz)

;;; Usage
;; Run M-x moz-reload-on-save-mode to switch moz-reload on/off in the
;; current buffer.
;; When active, saving the buffer triggers Firefox
;; to reload its current page.

;;(define-minor-mode moz-reload-on-save-mode
;;  "Moz Reload On Save Minor Mode"
;;  nil " Reload" nil
;;  (if moz-reload-on-save-mode
      ;; Edit hook buffer-locally.
;;      (add-hook 'after-save-hook 'moz-firefox-reload nil t)
;;    (remove-hook 'after-save-hook 'moz-firefox-reload t)))

;;(defun moz-firefox-reload ()
;;  (comint-send-string (inferior-moz-process) "BrowserReload();"))


;; (add-to-list 'auto-mode-alist '("\\.jsp$") . web-mode)


;; Org mode todo states
(setq org-todo-keywords '("TODO" "BLOCKED" "DONE"))
(setq org-todo-keyword-faces
      '(("BLOCKED" . org-warning)))


;; Tern - javascript refactoring library settings
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))

;; Karma runner loading
(require 'karma)

;; Helm framework
(require 'helm-config)

;; Slime setup
(slime-setup '(slime-company slime-repl inferior-slime slime-fancy slime-presentations
                             slime-presentation-streams))
(setq inferior-lisp-program "/usr/bin/sbcl")
(let ((hyperspec-dir (expand-file-name (concat user-emacs-directory "/HyperSpec/"))))
  (if (file-directory-p hyperspec-dir)
      (setf common-lisp-hyperspec-root hyperspec-dir)))


;; Projectile and helm integration
(projectile-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(projectile-mode)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-b") 'helm-mini)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; Helm gtags setup
(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t)

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
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)


(defun my:python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi)
  (jedi:setup))
(add-hook 'python-mode-hook 'my:python-mode-hook)


(setq jedi:complete-on-dot t)

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
