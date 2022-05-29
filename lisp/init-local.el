;;; init-local.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'yasnippet)
(yas-global-mode 1)


(global-set-key (kbd "C-,") 'xref-find-references)

;; don't use an event's buffer at all.
(setq-default eglot-events-buffer-size 0)

(add-hook 'js-mode-hook 'eglot-ensure)
(add-hook 'typescript-mode-hook 'eglot-ensure)

(provide 'init-local)
;;; init-local.el ends here
