;;; init-c.el --- Eglot support for C -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (fboundp 'eglot)
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c-ts-mode-hook 'eglot-ensure))

(provide 'init-c)
;;; init-c.el ends here
