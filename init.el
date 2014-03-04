;  Emacs init file

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes (quote (tango-dark)))
 '(ecb-options-version "2.40")
 '(fill-column 80)
 '(inhibit-startup-screen t)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 78 :width normal))))
 '(magit-diff-add ((t (:inherit nil :foreground "green"))))
 '(magit-diff-del ((t (:inherit diff-removed :foreground "red"))))
 '(magit-item-highlight ((t (:inherit highlight-))))
 '(whitespace-empty ((t (:background "DarkGoldenrod4" :foreground "firebrick")))))

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
(global-whitespace-mode)

;; Indenting with spaces and tab is equal to 4 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Disabling the auto-pairing of parenthesis
;(setq skeleton-pair nil)

;; Line delimiter
(setq default-buffer-file-coding-system 'utf-8-unix)


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
            (set (make-local-variable 'sgml-basic-offset) 2)))

;; Smarter minibuffer autocompletion
(require 'ido)
(ido-mode t)

;; Better buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)
(setq ibuffer-save-filter-groups
      (quote (("default"
               ("JavaScript" (or
                              (mode . javascript-mode)
                              (mode . js-mode)))
               ("Web" (or
                       (mode . web-mode)
                       (mode . html-mode)
                       (name . "^.*\\.jspf$")))
               ("Emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))
               ("Services" (name "^\\*.*\\*^"))
               ("Rubies" (name . "^.*\\.rb$"))
               ("Pythons" (name . "^.*\\.py$"))
               ("CSS" (or
                       (mode . css-mode)
                       (mode . less-css-mode)))))))


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


;; package-activated-list
;;  (setq additional-packages
;;     '(magit moe-theme yasnippet yaml-mode ruby-block ruby-electric ruby-end inf-ruby yari haml-mode haskell-mode ghc))

(setq additional-packages
      '(multi-web-mode auto-complete feature-mode fill-column-indicator
      flymake-jshint flymake-json flymake-easy flymake-less less-css-mode magit
      marmalade furl popup projectile pkg-info epl dash pymacs rfringe s
      zencoding-mode pyflakes jinja2-mode))

;; fetch the list of packages available
(when (not package-archive-contents)
  (package-refresh-contents))

;; install the missing packages
(dolist (package additional-packages)
  (when (not (package-installed-p package))
    (package-install package)))


;; -----------------------------------------------------------------------------
;; CUSTOM KEYBINDINGS
;; -----------------------------------------------------------------------------

;; C-d for commenting and uncommenting
(global-set-key (kbd "C-d") 'comment-or-uncomment-region)
;(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
;(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
;(global-set-key (kbd "S-C-<down>") 'shrink-window)
;(global-set-key (kbd "S-C-<up>") 'enlarge-window)


;; -----------------------------------------------------------------------------
;; CUSTOM FUNCTIONS
;; -----------------------------------------------------------------------------

;; Set window width
(defun set-window-width (x)
  "Set current window width"
  (adjust-window-trailing-edge (selected-window)
                               (- x (window-width))
                               t))

;; Set window width to 80 columns
(defun set-80-columns ()
  "Set current window to 80 columns width"
  (interactive)
  (set-window-width 81))


;; -----------------------------------------------------------------------------
;; MODULES AND EXTENSIONS
;; -----------------------------------------------------------------------------

;; Autocomplete Support
(require 'auto-complete)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
(require 'auto-complete-config)
(ac-config-default)


;;(load-file "/home/alex/.emacs.d/emacs-for-python/epy-init.el")
; (require 'epy-setup)      ;; It will setup other loads, it is required!
; (require 'epy-python)     ;; If you want the python facilities [optional]
; (require 'epy-completion) ;; If you want the autocompletion settings [optional]
; (require 'epy-editing)    ;; For configurations related to editing [optional]

;;(epy-setup-checker "pyflakes %f")
;;(epy-django-snippets)
;;(epy-setup-ipython)


;; Zencoding
;; (require 'zencoding-mode)
;; (add-hook 'sgml-mode-hook 'zencoding-mode)

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
(require 'flymake-jshint)
(add-hook 'js-mode-hook
     (lambda () (flymake-mode t)))

;; Right fringe (gutter)
(require 'rfringe)
;; (remove-hook 'after-change-major-mode-hook 'rfringe-mode)

;; RHTML mode
(require 'rhtml-mode)
(add-hook 'rhtml-mode-hook
          (lambda () (rinari-launch)))

;; Feature mode (cucumber, lettuce)
(require 'feature-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

;; Magit - git integration
(require 'magit)


;; -----------------------------------------------------------------------------
;; TO SORT OUT
;; -----------------------------------------------------------------------------

;; SASS mode
;;(autoload 'scss-mode "scss-mode")
;;(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

; Packages
;(package-initialize)
;(add-to-list 'package-archives
;             '("marmalade" . "http://marmalade-repo.org/packages/"))


; nXhtml - web related stuff
;(load-file "/home/alex/.emacs.d/nxhtml/autostart.el")


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

; Rinari (ROR support)
;;(add-to-list 'load-path "~/tmp/emacs/rinari")
;;(require 'rinari)


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

