;;; init-local.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; auto save
(add-hook 'after-init-hook 'auto-save-visited-mode)


;; yasnippet
(require-package 'yasnippet)
(require-package 'yasnippet-snippets)
(add-hook 'after-init-hook 'yas-global-mode)


;; xref
(global-set-key (kbd "C-,") 'xref-find-references)


;;; minibuffer
(global-set-key (kbd "M-s /") 'consult-line)
(global-set-key (kbd "M-s i") 'consult-imenu)
(global-set-key (kbd "M-?") 'consult-grep)


;;; org
(defun archer/org-babel-execute-src-block (&optional _arg info _params)
  "Load language if needed"
  (let* ((lang (nth 0 info))
         (sym (if (member (downcase lang) '("c" "cpp" "c++")) 'C (intern lang)))
         (backup-languages org-babel-load-languages))
    ;; - (LANG . nil) 明确禁止的语言，不加载。
    ;; - (LANG . t) 已加载过的语言，不重复载。
    (unless (assoc sym backup-languages)
      (condition-case err
          (progn
            (org-babel-do-load-languages 'org-babel-load-languages (list (cons sym t)))
            (setq-default org-babel-load-languages (append (list (cons sym t)) backup-languages)))
        (file-missing
         (setq-default org-babel-load-languages backup-languages)
         err)))))

(advice-add 'org-babel-execute-src-block :before #'archer/org-babel-execute-src-block)

;; Export pdf to support Chinese
(with-eval-after-load 'org
  (setq org-latex-pdf-process '("xelatex -interaction nonstopmode %f"
                                "xelatex -interaction nonstopmode %f"))
  (setq org-latex-default-packages-alist
        (remove '("AUTO" "inputenc" t) org-latex-default-packages-alist)))

(with-eval-after-load 'org
  (setq-default org-default-notes-file (concat org-directory "/inbox.org"))
  (setq-default org-agenda-files (list org-default-notes-file
                                       (concat org-directory "/gtd.org"))))


;;; alter
(with-eval-after-load 'alert
  (setq-default alert-default-style 'libnotify))



;;; eglot

;; don't use an event's buffer at all.
(setq-default eglot-events-buffer-size 0)



;;; javascript/typescript
(add-hook 'js-mode-hook
          (lambda ()
            (setq-local js-indent-level 2)
            (setq-local tab-width 2)
            (eglot-ensure)))


;; Enable eslint
(add-hook 'eglot-managed-mode-hook
          (lambda ()
            (if (derived-mode-p 'js-mode)
                (setq-local flymake-diagnostic-functions
                            (list (flymake-flycheck-diagnostic-function-for 'javascript-eslint))))))

(add-hook 'typescript-mode-hook (lambda ()
                                  (setq-local typescript-indent-level 2)
                                  (setq-local tab-width 2)
                                  (eglot-ensure)))

(defun archer/eslint-fix-current-file ()
  (interactive)
  (when (fboundp 'projectile-mode)
    (projectile-with-default-dir (projectile-acquire-root)
      (save-excursion
        (let ((command (concat "npx eslint --fix " (buffer-file-name))))
          (message command)
          (shell-command command))
        (revert-buffer t t)))))


;;; web
(require-package 'web-mode)
(require 'web-mode)

(setq web-mode-markup-indent-offset 2)

;; html
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; wechat miniprogram
(define-derived-mode wxml-mode web-mode "WXML")
(add-to-list 'auto-mode-alist '("\\.wxss\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.wxml\\'" . wxml-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(wxml-mode . ("wxml-langserver" "--stdio"))))


(add-hook 'wxml-mode-hook 'eglot-ensure)
(add-hook 'css-mode-hook 'eglot-ensure)

;; vue
(define-derived-mode vue-mode web-mode "Vue")
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
(add-hook 'vue-mode-hook 'eglot-ensure)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(vue-mode . (eglot-volar "vue-language-server" "--stdio")))
  (defclass eglot-volar (eglot-lsp-server) ()
    :documentation "volar")
  (cl-defmethod eglot-initialization-options ((server eglot-volar))
    "Passes through required cquery initialization options"
    `(
      :typescript (:serverPath ,(expand-file-name "~/.nvm/versions/node/v16.14.2/lib/node_modules/typescript/lib/tsserverlibrary.js"))
      :languageFeatures (
                         :references t
                         :implementation t
                         :definition t
                         :typeDefinition t
                         :rename t
                         :renameFileRefactoring t
                         :signatureHelp t
                         :codeAction t
                         :workspaceSymbol t
                         :completion (
                                      :defaultTagNameCase ""
                                      :defaultAttrNameCase ""
                                      :getDocumentNameCasesRequest :json-false
                                      :getDocumentSelectionRequest :json-false)
                         :schemaRequestService (:getDocumentContentRequest :json-false))
      :documentFeatures (
                         :selectionRange t,
                         :foldingRange :json-false,
                         :linkedEditingRange t,
                         :documentSymbol t,
                         :documentColor t,
                         :documentFormatting (
                                              :defaultPrintWidth 100
                                              :getDocumentPrintWidthRequest :json-false)
                         :defaultPrintWidth 100
                         :getDocumentPrintWidthRequest :json-false))))




(provide 'init-local)
;;; init-local.el ends here
