;;; init-eglot.el --- LSP support via eglot          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(when (maybe-require-package 'eglot)
  (maybe-require-package 'consult-eglot))

(with-eval-after-load 'eglot
  (setq eglot-extend-to-xref t
        eglot-events-buffer-size 0
        eglot-events-buffer-config (list :size 0
                                         :format 'full)
        ))

(add-hook 'eglot-managed-mode-hook (lambda ()
                                     (local-set-key (kbd "M-RET") #'eglot-code-actions)))


(global-set-key (kbd "C-,") #'xref-find-references)


(provide 'init-eglot)
;;; init-eglot.el ends here
