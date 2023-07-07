;;; init-react.el --- Support for React -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'treesit)

(if (and (treesit-available-p) (treesit-language-available-p 'tsx))

    ;; Prioritize the use of treesit
    (progn
      (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
      (add-hook 'tsx-ts-mode-hook 'eglot-ensure))

  ;; Downgrade to web-mode
  (when (maybe-require-package 'web-mode)

    (define-derived-mode react-tsx-mode web-mode "TSX"
      "A major mode derived from web-mode, for editing .tsx files with LSP support.")

    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . react-tsx-mode))

    (add-hook 'react-tsx-mode-hook 'eglot-ensure)

    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs '(react-tsx-mode . ("typescript-language-server" "--stdio"))))))

(provide 'init-react)
;;; init-react.el ends here
