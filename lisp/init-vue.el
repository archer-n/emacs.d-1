;;; init-vue.el --- Vue editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'web-mode)
  (define-derived-mode vue-mode web-mode "Vue")

  (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))

  (add-hook 'vue-mode-hook (lambda ()
                             (setq-local tab-width 2)
                             (setq-local web-mode-code-indent-offset 2)
                             (eglot-ensure)))

  ;; Volar
  (with-eval-after-load 'eglot
    (defun vue-eglot-init-options ()
      (let ((tsdk-path (expand-file-name
                        "lib"
                        (string-trim-right (shell-command-to-string "npm list --global --parseable typescript | head -n1")))))
        `(:typescript (:tsdk ,tsdk-path
                             :languageFeatures (:completion
                                                (:defaultTagNameCase "both"
                                                                     :defaultAttrNameCase "kebabCase"
                                                                     :getDocumentNameCasesRequest nil
                                                                     :getDocumentSelectionRequest nil)
                                                :diagnostics
                                                (:getDocumentVersionRequest nil))
                             :documentFeatures (:documentFormatting
                                                (:defaultPrintWidth 100
                                                                    :getDocumentPrintWidthRequest nil)
                                                :documentSymbol t
                                                :documentColor t)))))
    (add-to-list 'eglot-server-programs
                 `(vue-mode . ("vue-language-server" "--stdio" :initializationOptions ,(vue-eglot-init-options))))))


(provide 'init-vue)
;;; init-vue.el ends here
