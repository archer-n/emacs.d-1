;;; init-lsp-bridge.el ---  Fastest LSP client for Emacs  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'posframe)
(require-package 'markdown-mode)
(require-package 'yasnippet)

(require 'lsp-bridge)



(with-eval-after-load 'lsp-bridge
  (define-key lsp-bridge-mode-map (kbd "M-.") 'lsp-bridge-find-def)
  (define-key lsp-bridge-mode-map (kbd "C-x 4 .") 'lsp-bridge-find-def-other-window)
  (define-key lsp-bridge-mode-map (kbd "M-,") 'lsp-bridge-return-from-def)
  (define-key lsp-bridge-mode-map (kbd "C-.") 'lsp-bridge-find-impl)
  (define-key lsp-bridge-mode-map (kbd "C-x 4 ,") 'lsp-bridge-find-impl-other-window)
  (define-key lsp-bridge-mode-map (kbd "C-,") 'lsp-bridge-find-references)
  (define-key lsp-bridge-mode-map (kbd "C-c h") 'lsp-bridge-lookup-documentation))

(provide 'init-lsp-bridge)
;;; init-lsp-bridge.el ends here
