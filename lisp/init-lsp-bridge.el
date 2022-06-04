;;; init-lsp-bridge.el --- Fastest LSP client for Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'yasnippet)
(require 'lsp-bridge)
(require 'lsp-bridge-jdtls)       ;; provide Java third-party library jump and -data directory support, optional

(setq lsp-bridge-enable-auto-import t)
(setq lsp-bridge-jdtls-jvm-args `(,(concat "-javaagent:" (expand-file-name "~/.m2/repository/org/projectlombok/lombok/1.18.22/lombok-1.18.22.jar"))))

;; 打开日志，开发者才需要
(setq lsp-bridge-enable-log nil)

(with-eval-after-load 'lsp-bridge
  (define-key lsp-bridge-mode-map (kbd "M-.") 'lsp-bridge-find-def)
  (define-key lsp-bridge-mode-map (kbd "C-x 4 .") 'lsp-bridge-find-def-other-window)
  (define-key lsp-bridge-mode-map (kbd "M-,") 'lsp-bridge-return-from-def)
  (define-key lsp-bridge-mode-map (kbd "C-.") 'lsp-bridge-find-impl)
  (define-key lsp-bridge-mode-map (kbd "C-x 4 ,") 'lsp-bridge-find-impl-other-window)
  (define-key lsp-bridge-mode-map (kbd "C-,") 'lsp-bridge-find-references)
  (define-key lsp-bridge-mode-map (kbd "C-c h") 'lsp-bridge-lookup-documentation))

(global-lsp-bridge-mode)

(yas-global-mode 1)
(global-lsp-bridge-mode)


(provide 'init-lsp-bridge)
;;; init-lsp-bridge.el ends here
