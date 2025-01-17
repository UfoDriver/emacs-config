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

(defun my:notes (prefix)
  "Open index org file.  With PREFIX open attic."
  (interactive "P")
  (if prefix
      (find-file "~/Documents/Org-files/attic.org.gpg")
    (find-file "~/Documents/Org-files/index.org")))


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

(defun my:display-fold-marker (ov)
  "Format text replacement for folding marker for overlay OV."
  (when (eq 'code (overlay-get ov 'hs))
    (overlay-put ov 'display
                 (propertize
                  (format "(···%d···)"
                          (count-lines (overlay-start ov)
                                       (overlay-end ov)))
                  'face 'font-lock-type-face))))

;; https://karthinks.com/software/emacs-window-management-almanac/
(defun my:other-window-mru ()
  "Select the most recently used window on this frame."
  (interactive)
  (when-let ((mru-window
              (get-mru-window
               nil nil 'not-this-one-dummy)))
    (select-window mru-window)))

;;; custom-functions.el ends here
