;;; package --- Emacs init file
;;; Commentary:
;;; Everyone should have his/her own init.el
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

;; -----------------------------------------------------------------------------
;; CUSTOM FUNCTIONS
;; -----------------------------------------------------------------------------
(load (expand-file-name (concat user-emacs-directory "custom-functions")))

;; -----------------------------------------------------------------------------
;; PACKAGES
;; -----------------------------------------------------------------------------

(use-package diminish
  :ensure t
  :config
  (diminish 'hs-minor-mode)
  (diminish 'global-whitespace-mode))

(use-package meson-mode
  :defer t)

(use-package helm-posframe
  :defer t
  :init
  (helm-posframe-enable))

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

(defvar major-modes-without-lsp '(lisp-mode emacs-lisp-mode scheme-mode))
(defun my:lsp-hook ()
  "Check if we want to start LSP for this mode."
  (if (member major-mode major-modes-without-lsp)
      (message "LSP is disabled for %s, check major-modes-without-lsp varialbe" major-mode)
    (lsp)))

(use-package lsp-mode
  :hook (prog-mode . my:lsp-hook)
  :bind-keymap ("C-c l" . lsp-command-map)
  :config
  (setq read-process-output-max 8192)
  :bind-keymap
  ("C-c l" . lsp-command-map)
  :hook
  (lsp-mode . lsp-enable-which-key-integration))

(use-package lsp-ui
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
  (drag-stuff-define-keys)
  (setf drag-stuff-except-modes '(org-mode)))

(use-package magit
  :defer t
  :init
  (setq-default magit-last-seen-setup-instructions "1.4.0"))

(use-package company
  :diminish
  :config
  (global-company-mode)
  (setq company-format-margin-function #'company-detect-icons-margin))

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
  :diminish t
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

(use-package which-key
  :diminish
  :config
  (which-key-mode))

(use-package geiser
  :defer t
  ;; :commands (geiser-eval-last-sexp)
  ;; :bind-keymap ("C-c" . geiser-map)
  )

(use-package all-the-icons
  :if (display-graphic-p))

(use-package ligature
  ;; :load-path "path-to-ligature-repo"
(use-package paredit
  :defer t)

(use-package fold-dwim
  :defer t
  :bind (("C-c <tab>" . fold-dwim-toggle)))
  :config
  ;; Enable the "www" ligature in every possible major mode
  ;; (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  ;; (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode
                          '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                            ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                            "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                            "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                            "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                            "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                            "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                            "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                            ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                            "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                            "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                            "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                            "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

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


(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

;;; init.el ends here
