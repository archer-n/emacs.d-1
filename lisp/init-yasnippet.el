;;; init-yasnippet.el --- Support for code templates -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require-package 'yasnippet)

(add-hook 'after-init-hook (lambda ()
                             (yas-global-mode 1)))

(require-package 'yasnippet-snippets)

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
