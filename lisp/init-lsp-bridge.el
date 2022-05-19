;;; init-lsp-bridge.el --- Fastest LSP client for Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'all-the-icons)
(require-package 'posframe)

(require 'cape)
(require 'lsp-bridge)
(require 'lsp-bridge-orderless) ;; make lsp-bridge support fuzzy match, optional
(require 'lsp-bridge-icon) ;; show icon for completion items, optional
(require 'lsp-bridge-jdtls)

;; 打开日志，开发者才需要
;; (setq lsp-bridge-enable-log t)

;; 通过Cape融合不同的补全后端，比如lsp-bridge、 tabnine、 file、 dabbrev.
(defun lsp-bridge-mix-multi-backends ()
  (setq-local completion-category-defaults nil)
  (setq-local completion-at-point-functions
              (list
               (cape-capf-buster
                (cape-super-capf
                 #'lsp-bridge-capf
                 #'cape-file
                 #'cape-dabbrev
                 )
                'equal)
               )))

(add-hook 'lsp-bridge-mode-hook 'lsp-bridge-mix-multi-backends)

(with-eval-after-load 'lsp-bridge
  (define-key lsp-bridge-mode-map (kbd "M-.") 'lsp-bridge-find-def)
  (define-key lsp-bridge-mode-map (kbd "C-x 4 .") 'lsp-bridge-find-def-other-window)
  (define-key lsp-bridge-mode-map (kbd "M-,") 'lsp-bridge-return-from-def)
  (define-key lsp-bridge-mode-map (kbd "C-.") 'lsp-bridge-find-impl)
  (define-key lsp-bridge-mode-map (kbd "C-x 4 ,") 'lsp-bridge-find-impl-other-window)
  (define-key lsp-bridge-mode-map (kbd "C-,") 'lsp-bridge-find-references)
  (define-key lsp-bridge-mode-map (kbd "C-c h") 'lsp-bridge-lookup-documentation))

(global-lsp-bridge-mode)

(provide 'init-lsp-bridge)
;;; init-lsp-bridge.el ends here
