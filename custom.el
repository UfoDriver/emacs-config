;;; package --- The file of custom variables
;;; Commentary:
;;; Custom varialbles need a place to live and here it is.
;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(column-number-mode t)
 '(company-format-margin-function 'company-detect-icons-margin)
 '(company-minimum-prefix-length 1)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-idle-delay 0)
 '(css-indent-offset 2)
 '(custom-enabled-themes '(tango-dark))
 '(custom-file "~/.emacs.d/custom.el")
 '(custom-safe-themes
   '("3448e3f5d01b39ce75962328a5310438e4a19e76e4b691c21c8e04ca318a5f62" default))
 '(dap-auto-configure-mode t)
 '(delete-selection-mode t)
 '(ecb-options-version "2.40")
 '(electric-pair-mode t)
 '(fill-column 80)
 '(flycheck-gcc-language-standard "c++11")
 '(gc-cons-threshold 1000000)
 '(gdb-many-windows t)
 '(geiser-active-implementations '(guile) t)
 '(geiser-default-implementation 'guile)
 '(geiser-guile-binary "/usr/bin/guile3.0")
 '(global-company-mode t)
 '(global-whitespace-mode t)
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-prefix-key "\3-cg")
 '(helm-gtags-pulse-at-cursor t)
 '(helm-gtags-suggested-key-mapping t)
 '(helm-gtags-use-input-at-cursor t)
 '(helm-visible-mark-prefix "")
 '(hs-set-up-overlay 'my:display-fold-marker)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(jedi:complete-on-dot t)
 '(js-indent-level 2)
 '(js-switch-indent-offset 2)
 '(js2-basic-offset 2)
 '(lsp-enable-snippet nil)
 '(lsp-pylsp-server-command '("pylsp" "-v"))
 '(magit-push-always-verify nil)
 '(markdown-command "markdown2 -x tables -x smarty-pants -x strike")
 '(markdown-command-needs-filename t)
 '(menu-bar-mode nil)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount '(1 ((control)) ((shift) . 5)))
 '(org-support-shift-select 'always)
 '(org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "BLOCKED" "DONE")))
 '(package-archives
   '(("melpa" . "http://melpa.org/packages/")
     ("original" . "http://tromey.com/elpa/")
     ("gnu" . "https://elpa.gnu.org/packages/")
     ("org" . "https://orgmode.org/elpa/")))
 '(package-enable-at-startup nil)
 '(package-selected-packages
   '(treesit-auto company-quickhelp restclient-jq go-mode pdf-tools docker-compose-mode sass-mode csharp-mode dotnet fold-dwim paredit rust-mode magit lsp-treemacs all-the-icons flycheck company-restclient restclient restclient-helm helm-slime slime slime-company inspector erlang web-mode systemd company helm-posframe python-pytest spice-mode lua-mode geiser-guile helpful helm-make helm-dash dap-mode org-babel-eval-in-repl meson-mode elisp-format helm-lsp which-key diminish typescript-mode vue-mode svg yasnippet-snippets docker-tramp hy-mode nasm-mode flycheck-mypy queue cider fsm jabber jabber-otr helm-unicode iedit use-package lsp-ui ccls scad-mode geiser ediprolog ctags liso-theme json-navigator company-ansible flycheck-pycheckers json-mode elisp-slime-nav notify drag-stuff helm-gtags rpm-spec-mode company-qml qml-mode graphviz-dot-mode stickyfunc-enhance dockerfile-mode cython-mode feature-mode helm-emmet helm-package yaml-mode xmlgen scss-mode request-deferred python-pep8 pymacs pyflakes pycomplete multi-web-mode marmalade markdown-mode+ jsx-mode json-rpc jinja2-mode helm-projectile helm-css-scss helm-ag fuzzy fabric emmet-mode))
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
 '(slime-company-completion 'fuzzy)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(typescript-indent-level 2)
 '(uniquify-buffer-name-style 'post-forward nil (uniquify))
 '(uniquify-separator ":")
 '(use-package-always-defer t)
 '(use-package-always-ensure t)
 '(use-package-verbose t)
 '(warning-suppress-log-types '((comp)))
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-markup-indent-offset 2)
 '(whitespace-display-mappings '((space-mark 32) (newline-mark 13) (tab-mark 9)))
 '(whitespace-line-column 100)
 '(whitespace-style
   '(face trailing tabs spaces lines-tail newline empty indentation space-after-tab space-before-tab space-mark tab-mark newline-mark)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#2e3436" :foreground "#eeeeec" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 140 :width normal :foundry "UKWN" :family "Iosevka Term"))))
 '(company-tooltip ((t (:background "#292a2b" :inherit default))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-scrollbar-thumb ((t (:background "#39c6414f43d2"))))
 '(company-tooltip-scrollbar-track ((t (:background "#458d4e9f51a5"))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face :background "navy"))))
 '(fixed-pitch ((t nil)))
 '(font-lock-comment-face ((t (:foreground "#73d216" :slant oblique))))
 '(font-lock-keyword-face ((t (:foreground "#b4fa70" :weight bold))))
 '(magit-diff-add ((t (:inherit nil :foreground "green"))))
 '(magit-diff-del ((t (:inherit diff-removed :foreground "red"))))
 '(magit-item-highlight ((t (:inherit highlight-))))
 '(mmm-default-submode-face ((t nil)))
 '(mode-line ((t (:background "#555753" :foreground "#eeeeec" :box nil))))
 '(mode-line-inactive ((t (:background "#2e3436" :foreground "#eeeeec" :box nil))))
 '(whitespace-empty ((t (:background "DarkGoldenrod4" :foreground "firebrick"))))
 '(whitespace-indentation ((t (:background "gray15" :foreground "firebrick"))))
 '(whitespace-space-after-tab ((t (:background "gray15" :foreground "firebrick"))))
 '(whitespace-space-before-tab ((t (:background "gray15" :foreground "firebrick"))))
 '(whitespace-tab ((t (:background "gray18")))))

;;; custom.el ends here
