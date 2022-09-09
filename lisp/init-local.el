;;; init-local.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; auto save
(require 'auto-save)
(setq auto-save-silent t)   ; quietly save
(setq auto-save-delete-trailing-whitespace t)  ; automatically delete spaces at the end of the line when saving
(auto-save-enable)

;; When using auto-save, if the auto-clear blank mode is enabled,
;; it will interfere with the current input. Turn it off here
(remove-hook 'after-init-hook 'global-whitespace-cleanup-mode)

;; Shut down automatically saved when editing a remote file
(setq auto-save-disable-predicates
      '((lambda ()
          (file-remote-p (buffer-file-name)))))


;; yasnippet
(require-package 'yasnippet)
(require-package 'yasnippet-snippets)
(add-hook 'after-init-hook 'yas-global-mode)


;; xref
(global-set-key (kbd "C-,") 'xref-find-references)


;;; minibuffer
(require 'consult)
(global-set-key (kbd "M-s /") 'consult-line)
(global-set-key (kbd "M-s i") 'consult-imenu)
(global-set-key (kbd "M-s r") 'consult-recent-file)
(global-set-key (kbd "M-?") 'consult-ripgrep)



;; separedit
(require-package 'separedit)
(define-key prog-mode-map        (kbd "C-c '") #'separedit)


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
                                       (concat org-directory "/gtd.org")
                                       (concat org-directory "/notes.org"))))


;;; alter
(with-eval-after-load 'alert
  (setq-default alert-default-style 'libnotify))



;;; eglot


;; don't use an event's buffer at all.
(setq-default eglot-events-buffer-size 0)


;;; python
(add-hook 'python-mode-hook 'eglot-ensure)


;;; javascript/typescript

(add-hook 'js-mode-hook
          (lambda ()
            (setq-local js-switch-indent-offset 2)
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

(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

;; wechat miniprogram
(define-derived-mode wxml-mode web-mode "WXML")
(add-to-list 'auto-mode-alist '("\\.wxss\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.wxml\\'" . wxml-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(wxml-mode . ("wxml-langserver" "--stdio"))))

(add-hook 'web-mode-hook 'eglot-ensure)
(add-hook 'wxml-mode-hook 'eglot-ensure)
(add-hook 'css-mode-hook 'eglot-ensure)


;;; miniprogram-mode
(defvar miniprogram-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-l q") #'miniprogram-quick-layout)
    (define-key map (kbd "C-c C-l c") #'miniprogram-quick-layout-code)
    (define-key map (kbd "C-c C-l l") #'miniprogram-quick-layout-style)
    (define-key map (kbd "C-c C-l j") #'miniprogram-quick-layout-config)
    map)
  "keymap while miniprogram-mode is active")

(define-minor-mode miniprogram-mode
  "Provides some auxiliary functions for WeChat minprogram.
Add the code to enable miniprogram-mode in the .dir-locales.el file in the
root directory of the miniprogram project.
For example: ((nil . ((miniprogram-mode . t))))"

  :lighter " mini"
  :keymap miniprogram-mode-map)

(defun miniprogram-find-file (ext-name)
  (when (and (stringp ext-name)
             (file-exists-p (concat (file-name-base (buffer-file-name)) ext-name)))
    (find-file (concat (file-name-base (buffer-file-name)) ext-name))))

(defun miniprogram-quick-layout ()
  (interactive)
  (delete-other-windows)
  (let* ((left-top (selected-window))
         (right-top (split-window-horizontally))
         (left-bottom (split-window-vertically))
         (_ (select-window right-top))
         (right-bottom (split-window-vertically)))
    (select-window left-top)
    (miniprogram-find-file ".wxml")
    (select-window right-top)
    (miniprogram-find-file ".js")
    (select-window left-bottom)
    (miniprogram-find-file ".wxss")
    (select-window right-bottom)
    (miniprogram-find-file ".json")))

;; A quick layout
(defun miniprogram-layout-left-right (left-file-type right-file-type)
  (delete-other-windows)
  (let* ((left (selected-window))
         (right (split-window-horizontally)))
    (select-window left)
    (miniprogram-find-file left-file-type)
    (select-window right)
    (miniprogram-find-file right-file-type)
    (select-window right)))

(defun miniprogram-quick-layout-config ()
  (interactive)
  (miniprogram-layout-left-right ".wxml" ".json"))

(defun miniprogram-quick-layout-code ()
  (interactive)
  (miniprogram-layout-left-right ".wxml" ".js"))

(defun miniprogram-quick-layout-style ()
  (interactive)
  (miniprogram-layout-left-right ".wxml" ".wxss"))


;; vue

(define-derived-mode vue-mode web-mode "Vue")
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
(add-hook 'vue-mode-hook (lambda ()
                           (setq-local tab-width 2)
                           (eglot-ensure)))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(vue-mode . (eglot-volar "vue-language-server" "--stdio")))
  (defclass eglot-volar (eglot-lsp-server) ()
    :documentation "volar")
  (cl-defmethod eglot-initialization-options ((server eglot-volar))
    "Passes through required cquery initialization options"
    `(
      :typescript (:serverPath ,(expand-file-name "~/.nvm/versions/node/v18.7.0/lib/node_modules/typescript/lib/tsserverlibrary.js"))
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


;;; dart
(require-package 'dart-mode)
(add-hook 'dart-mode-hook 'eglot-ensure)


;;; kotlin
(require-package 'kotlin-mode)


;;; plantuml
(require-package 'plantuml-mode)
(setq plantuml-jar-path (expand-file-name "~/.cache/plantuml/plantuml.jar"))
(setq org-plantuml-jar-path (expand-file-name "~/.cache/plantuml/plantuml.jar"))
(setq plantuml-default-exec-mode 'jar)
(setq plantuml-output-type "png")
(when (not (file-exists-p plantuml-jar-path))
  (require 'plantuml-mode)
  (plantuml-download-jar))

(maybe-require-package 'flycheck-plantuml)
(flycheck-plantuml-setup)

(defun company-plantuml (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-plantuml))
    (prefix (let* ((symbol (company-grab-symbol))
                   (max-match-result (try-completion symbol plantuml-kwdList)))
              (if (length> max-match-result 0)
                  symbol)))
    (candidates (all-completions arg plantuml-kwdList))))

(add-hook 'plantuml-mode-hook
          (lambda ()
            (setq-local company-backends '(company-plantuml
                                           (company-dabbrev-code company-keywords)
                                           company-dabbrev))))



;;

(require-package 'citre)
(require 'citre)
(require 'citre-config)
(global-set-key (kbd "C-x c j") 'citre-jump)
(global-set-key (kbd "C-x c J") 'citre-jump-back)
(global-set-key (kbd "C-x c p") 'citre-ace-peek)
(global-set-key (kbd "C-x c u") 'citre-update-this-tags-file)

(provide 'init-local)
;;; init-local.el ends here
