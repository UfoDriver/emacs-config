;;; package --- Emacs init file
;;; Commentary:
;;; Everyone should have his/her own init.el
;;; Code:
(load (expand-file-name "custom.el" user-emacs-directory))
;;(require 'compat)
(require 'package)
(package-initialize)

;; -----------------------------------------------------------------------------
;; GLOBAL SETTINGS
;; -----------------------------------------------------------------------------

;; -----------------------------------------------------------------------------
;; CUSTOM FUNCTIONS
;; -----------------------------------------------------------------------------
(load (expand-file-name "custom-functions" user-emacs-directory))

;; -----------------------------------------------------------------------------
;; PACKAGES
;; -----------------------------------------------------------------------------

(use-package diminish
  :ensure t
  :config
  (diminish 'hs-minor-mode)
  (diminish 'global-whitespace-mode)
  (diminish 'eldoc-mode)
  (diminish 'auto-revert-mode))

(use-package meson-mode
  :defer t)

(use-package emacs
  :config
  (global-set-key (kbd "M-u") 'upcase-dwim)
  (global-set-key (kbd "M-l") 'downcase-dwim)

  (when (display-graphic-p)
    (require 'pixel-scroll)
    (pixel-scroll-precision-mode t)
    (bind-keys :package pixel-scroll-precision-mode
               ([remap scroll-up-command]   . #'pixel-scroll-interpolate-down)
               ([remap scroll-down-command] . #'pixel-scroll-interpolate-up)))

  ; https://karthinks.com/software/batteries-included-with-emacs/
  (defun pulse-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))
  (dolist (command '(scroll-up-command scroll-down-command recenter-top-bottom other-window))
    (advice-add command :after #'pulse-line))

  (define-advice kill-ring-save (:before (start end &rest rest))
    (pulse-momentary-highlight-region start end))
  (global-display-line-numbers-mode)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode)))

(use-package asm-mode
  :defer t
  :hook
  (asm-mode . (lambda ()
                (setq-local
                 indent-tabs-mode t
                 tab-width 10))))

;; https://www.reddit.com/r/emacs/comments/audffp/tip_how_to_use_a_stable_and_fast_environment_to/
(defvar major-modes-without-lsp '(lisp-mode emacs-lisp-mode scheme-mode))
(defun my:lsp-hook ()
  "Check if we want to start LSP for this mode."
  (if (member major-mode major-modes-without-lsp)
      (message "LSP is disabled for %s, check `major-modes-without-lsp` variable" major-mode)
    (lsp)))

(use-package lsp-mode
  :defer t
  :hook (prog-mode . my:lsp-hook)
  :bind-keymap ("C-c l" . lsp-command-map)
  :config
  (setq read-process-output-max 8192)
  ;; https://github.com/emacs-lsp/lsp-mode/issues/4838#issuecomment-3198461412
  ;; https://github.com/emacs-lsp/lsp-mode/issues/4838#issuecomment-3198461412
  (advice-add 'lsp-typescript-javascript-tsx-jsx-activate-p :around
              (lambda (orig-fn filename &rest args)
                (message "Checking activation for: %s" filename) ; Debug message
                (or (string-match-p "\\.vue\\'" filename)
                    (apply orig-fn filename args))))
  :hook
  (lsp-mode . lsp-enable-which-key-integration))

(use-package lsp-ui
  :defer t)

(use-package helm-lsp
  :commands helm-lsp-workspace-symbol)

(use-package slime
  :if
  (or (executable-find "sbcl") (executable-find "ecl") (executable-find "clisp"))
  :defer t
  :init
  (setq-default inferior-lisp-program "/usr/bin/sbcl")
  :config
  (load (expand-file-name "~/.local/share/common-lisp/slime-helper.el"))
  (slime-setup '(slime-company slime-repl inferior-slime slime-fancy
                               slime-presentations
                               slime-presentation-streams helm-slime))
  (let ((hyperspec-dir (expand-file-name (concat user-emacs-directory "/HyperSpec/"))))
    (if (file-directory-p hyperspec-dir)
        (setq-default common-lisp-hyperspec-root hyperspec-dir))))

(use-package drag-stuff
  :diminish nil
  :config
  (drag-stuff-global-mode t)
  (drag-stuff-define-keys)
  (setf drag-stuff-except-modes '(org-mode)))

(use-package magit
  :defer t
  :init
  (setq-default magit-last-seen-setup-instructions "1.4.0"))

(use-package company
  :diminish nil
  :config
  (global-company-mode)
  ;;(company-quickhelp-mode)
  )
  ;; (setq company-format-margin-function #'company-detect-icons-margin))

(use-package flycheck
  :defer t
  :init
  (global-flycheck-mode))

(use-package feature-mode
  :defer t)

(use-package jinja2-mode
  :defer t)

(use-package hy-mode
  :mode ("\\.hy\\'" . hy-mode)
  :after lsp-mode
  :config
  (setq hy-jedhy--enable? nil)
  (lsp-register-client (make-lsp-client
                        :new-connection (lsp-stdio-connection "hyuga")
                        :activation-fn (lsp-activate-on "hy")
                        :server-id 'hyuga)))

(use-package projectile
  :defer t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :bind
  (:map projectile-command-map)
  ("s s" . helm-projectile-ag)
  :config
  (my:setup-frame-name))

(use-package helm
  :defer t
  :diminish nil
  :bind
  ("M-x" . helm-M-x)
  ("M-y" . helm-show-kill-ring)
  ("C-x C-b" . helm-mini)
  ("C-x C-f" . helm-find-files)
  :config
  (setf helm-display-function 'helm-display-buffer-in-own-frame)
  :init
  (helm-mode 1))

(use-package helm-projectile
  :defer t
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

(use-package paredit
  :defer t)

(use-package fold-dwim
  :defer t
  :bind
  ("C-c <tab>" . fold-dwim-toggle))

(use-package helpful
  :defer nil
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key]      . helpful-key)
  ([remap describe-command]  . helpful-command))

(use-package restclient
  :defer t
  :requires restclient-jq)

(use-package windmove
  :bind
  ("C-c <up>"    . #'windmove-up)
  ("C-c <right>" . #'windmove-right)
  ("C-c <down>"  . #'windmove-down)
  ("C-c <left>"  . #'windmove-left))

(use-package web-mode
  :ensure t
  :config
  (setf (alist-get 'web-mode lsp--formatting-indent-alist) 'web-mode-code-indent-offset)
  :mode
  (("\\.vue\\'" . web-mode)))

(use-package treemacs
  :defer t
  :config
  (treemacs-resize-icons 16))

(use-package diff-hl
  :hook
  ;; Somehow shortcuts don't work here because of "diff-hl.el faield to define function diff-hl"
  ((prog-mode . diff-hl-mode)
   (vc-dir-mode . diff-hl-mode)))

;; -----------------------------------------------------------------------------
;; ENABLED DISABLED BY DEFAULT COMMANDS
;; -----------------------------------------------------------------------------

(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; -----------------------------------------------------------------------------
;; CUSTOM KEYBINDINGS
;; -----------------------------------------------------------------------------

;; Enter for newline-and-indent in programming modes
(add-hook 'prog-mode-hook #'(lambda ()
  (local-set-key (kbd "RET") 'newline-and-indent)
  ;; (hs-minor-mode t)
  ))

(keymap-global-set "M-o" 'my:other-window-mru)

;; -----------------------------------------------------------------------------
;; TO SORT OUT
;; -----------------------------------------------------------------------------

;; (use-package dap-mode
;;   :defer t
;;   :config
;;   (setq dap-python-debugger 'debugpy)
;;   :init
;;   (require 'dap-cpptools)
;;   (dap-register-debug-template
;;    "Rust::GDB Run Configuration"
;;    (list :type "gdb"
;;          :request "launch"
;;          :name "GDB::Run"
;;          :gdbpath "rust-gdb"
;;          :target nil
;;          :cwd nil))
;;   (require 'dap-python)
;;   )

;; Next form is copypaste and should be cleaned
;; (use-package ccls
;;   :defer t
;;   :after projectile
;; ;  :ensure-system-package ccls
;;   ;; :custom
;;   ;; (ccls-args nil)
;;   ;; (ccls-executable (executable-find "/home/alex/tmp/ccls/Release/ccls"))
;;   )

;; -----------------------------------------------------------------------------
;; LOCAL INITIALIZATION
;; -----------------------------------------------------------------------------

;; (load-file (expand-file-name "icons-in-terminal.el" user-emacs-directory))
;; (load-file (expand-file-name "icons-in-terminal-local.el" user-emacs-directory))

(if (file-exists-p (expand-file-name "local-init.el" user-emacs-directory))
    (load-file (expand-file-name "local-init.el" user-emacs-directory))
  (message "No local-init.el found"))

(require 'color)

(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

;;; init.el ends here
