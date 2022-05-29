;;; init-local.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; yasnippet
(require-package 'yasnippet)
(add-hook 'after-init-hook 'yas-global-mode)


;; xref
(global-set-key (kbd "C-,") 'xref-find-references)


;;; minibuffer
(global-set-key (kbd "M-s /") 'consult-line)
(global-set-key (kbd "M-s i") 'consult-imenu)
(global-set-key (kbd "M-?") 'consult-grep)


;;; org
(with-eval-after-load 'org
  (setq-default org-default-notes-file (concat org-directory "/inbox.org"))
  (setq-default org-agenda-files (list org-default-notes-file
                                       (concat org-directory "/gtd.org"))))


;;; alter
(with-eval-after-load 'alert
  (setq-default alert-default-style 'libnotify))



;;; eglot

;; don't use an event's buffer at all.
(setq-default eglot-events-buffer-size 0)



;;; javascript/typescript
(add-hook 'js-mode-hook
          (lambda ()
            (setq-local flymake-diagnostic-functions
                        (list (flymake-flycheck-diagnostic-function-for 'javascript-eslint)))
            (setq-local js-indent-level 2)
            (setq-local tab-width 2)))

(add-hook 'typescript-mode-hook (lambda ()
                                  (setq-local typescript-indent-level 2)
                                  (setq-local tab-width 2)))

(add-hook 'js-mode-hook 'eglot-ensure)

(add-hook 'typescript-mode-hook 'eglot-ensure)


(defun archer/eslint-fix-current-file ()
  (interactive)
  (when (fboundp 'projectile-mode)
    (projectile-with-default-dir (projectile-acquire-root)
      (save-excursion
        (let ((command (concat "npx eslint --fix " (buffer-file-name))))
          (message command)
          (shell-command command))
        (revert-buffer t t)))))


(provide 'init-local)
;;; init-local.el ends here
