;;; init-eglot.el --- LSP support via eglot          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; For Emacs >= 27
(setq read-process-output-max (* 1024 1024))

(setq eglot-extend-to-xref t)

(when (require 'eglot)
  (maybe-require-package 'consult-eglot))

(add-hook 'eglot-managed-mode-hook (lambda ()
                                     (local-set-key (kbd "M-RET") #'eglot-code-actions)))



(provide 'init-eglot)
;;; init-eglot.el ends here
