;;; package: --- Collection of local useful functions
;;; Commentary:
;;; Just an assorted useful stuff
;;; Code:

(require 'cl-lib)

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
  (when (fboundp 'projectile-project-name)
    (set-frame-name
     (if (string= (projectile-project-name) "-")
         (progn (message "Projectile project not found")
                default-directory)
       (capitalize (projectile-project-name))))))

(defun my:notes ()
  "Open index org file."
  (interactive)
  (find-file "~/Documents/Org-files/index.org"))

; Based on https://www.reddit.com/r/emacs/comments/7rho4f/now_you_can_use_helm_with_frames_instead_of/
;; (defun my:helm-display-frame-center (buffer &optional resume)
;;   "Display `helm-buffer' in a separate frame which centered in parent frame."
;;   (if (not (display-graphic-p))
;;       ;; Fallback to default when frames are not usable.
;;       (helm-default-display-buffer buffer)
;;     (setq helm--buffer-in-new-frame-p t)
;;     (let* ((parent (selected-frame))
;;            (frame-pos (frame-position parent))>
;;            (parent-left (car frame-pos))
;;            (parent-top (cdr frame-pos))
;;            (width (/ (frame-width parent) 2))
;;            (height (- (frame-height parent) 10))
;;            tab-bar-mode
;;            (default-frame-alist
;;              (if resume
;;                  (buffer-local-value 'helm--last-frame-parameters
;;                                      (get-buffer buffer))
;;                `((parent . ,parent)
;;                  (width . ,width)
;;                  (height . ,height)
;;                  (left-fringe . 0)
;;                  (right-fringe . 0)
;;                  (tool-bar-lines . 0)
;;                  (line-spacing . 0)
;;                  (desktop-dont-save . t)
;;                  (no-special-glyphs . t)
;;                  (inhibit-double-buffering . t)
;;                  (tool-bar-lines . 0)
;;                  (left . ,(+ parent-left (/ (* (frame-char-width parent) (frame-width parent)) 4)))
;;                  (top . ,(+ parent-top (/ (* (frame-char-width parent) (frame-height parent)) 5)))
;;                  (title . "Helm")
;;                  (vertical-scroll-bars . nil)
;;                  (menu-bar-lines . 0)
;;                  (fullscreen . nil)
;;                  (visible . ,(null helm-display-buffer-reuse-frame))
;;                  (skip-taskbar . t)
;;                  (undecorated . t)
;;                  (internal-border-width . 1))))
;;            display-buffer-alist)
;;       (set-face-background 'internal-border (face-foreground 'default))
;;       (helm-display-buffer-popup-frame buffer default-frame-alist))

;;     (helm-log-run-hook 'helm-window-configuration-hook)))

(cl-defun my:mark-test-with-tag (filename functionname &key (tag "wip"))
  "Mark test (FUNCTIONNAME) in FILENAME with pytest TAG."
  (find-file filename)
  (goto-char (point-min))
  (search-forward functionname)
  (move-beginning-of-line nil)
  (insert (format "@pytest.mark.%s\n" tag))
  (save-buffer))

(defun my:parse-pytest-output-and-mark-tests (pytest-output)
  "Parse copypaste string of PYTEST-OUTPUT and mark found tests with wip mark."
  (let ((pairs (mapcar
                (lambda (line)
                  (let ((chunks (split-string line "[][: ]" t)))
                    (cons (cadr chunks) (caddr chunks))))
                (split-string pytest-output "\n"))))
  (dolist (pair (delete-dups pairs))
    (my:mark-test-with-tag (car pair) (cdr pair))))
)

;;; custom-functions.el ends here
