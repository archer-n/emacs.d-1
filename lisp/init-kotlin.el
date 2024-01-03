;;; init-kotlin.el --- Kotlin support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'kotlin-mode)

(use-package flymake-ktlint
  :ensure t
  :init
  (flymake-ktlint-setup)
  :config
  (setq ktlint-flymake-args '())) ;; "--code-style=android_studio" not working

;;; Formatting
(with-eval-after-load "kotlin-mode"
  (define-key kotlin-mode-map (kbd "C-c C-o")
              (lambda ()
                (interactive "")
                (shell-command (concat "ktlint" " -F " buffer-file-name)))))

(provide 'init-kotlin)
;;; init-kotlin.el ends here
