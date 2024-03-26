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

  ;; Volar - This configuration is not very easy to use and needs to be adjusted.
  ;;
  ;; @see Typescript plugin for the language server. - https://github.com/vuejs/language-tools/tree/master/packages/typescript-plugin
  ;;
  ;; typescript@5.4.3
  ;; typescript-language-server@3.3.1
  ;; @vue/typescript-plugin@2.0.7
  ;; @vue/language-server@2.0.7
  ;;
  (add-to-list 'eglot-server-programs
               `(vue-mode . ("typescript-language-server" "--stdio" :initializationOptions
                             (:plugins [(:name "@vue/typescript-plugin" :location ,(string-trim-right (shell-command-to-string "npm list --global --parseable @vue/typescript-plugin | head -n1")) :languages ["vue"])])))))


(provide 'init-vue)
;;; init-vue.el ends here
