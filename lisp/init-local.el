;;; init-local.el --- Changing themes and adding your own customization -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; theme
(setq-default custom-enabled-themes nil)


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


;;; beancount
(require 'beancount)
(add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode))

;; Experimental: Bind a key to reformat the entire file using bean-format.
(defun beancount-format-file ()
  (interactive)
  (let ((line-no (line-number-at-pos)))
    (call-process-region (point-min) (point-max) "bean-format" t (current-buffer))
    (goto-line line-no)
    (recenter)
    ))


(add-hook 'beancount-mode-hook #'outline-minor-mode)
(define-key beancount-mode-map [(control c)(control f)] 'beancount-format-file)

;; Make sure we don't accidentally pick up ;;; as headers. Use org section headers only.
(setq beancount-outline-regexp "\\(\\*+\\)")

;; Automatically enable outline-mode.
(add-hook 'beancount-mode-hook #'outline-minor-mode)

;; Add movement between sections.
(define-key beancount-mode-map [(control c)(control n)] #'outline-next-visible-heading)
(define-key beancount-mode-map [(control c)(control p)] #'outline-previous-visible-heading)
(define-key beancount-mode-map [(control c)(control u)] #'outline-up-heading)

;; Register support for a 'beancount-account thing for (thing-at-point).
(put 'beancount-account 'bounds-of-thing-at-point
     (lambda ()
       (let ((thing (thing-at-point-looking-at
                     beancount-account-regexp 500)))
         (if thing
             (let ((beginning (match-beginning 0))
                   (end (match-end 0)))
               (cons beginning end))))))


;;; org
(with-eval-after-load 'org
  (setq-default org-default-notes-file (concat org-directory "/inbox.org"))
  (setq-default org-agenda-files (list org-default-notes-file
                                       (concat org-directory "/gtd.org"))))


;;; alter
(with-eval-after-load 'alert
  (setq-default alert-default-style 'libnotify))


;; auto-save
(require 'auto-save)
(auto-save-enable)

(setq auto-save-silent t)   ; quietly save
(setq auto-save-delete-trailing-whitespace t)  ; automatically delete spaces at the end of the line when saving

;; When using auto-save, if the auto-clear blank mode is enabled,
;; it will interfere with the current input. Turn it off here
(remove-hook 'after-init-hook 'global-whitespace-cleanup-mode)


;; Shut down automatically saved when editing a remote file
(setq auto-save-disable-predicates
      '((lambda ()
          (file-remote-p (buffer-file-name)))))


;;; yasnippet
(require-package 'yasnippet)
(require-package 'yasnippet-snippets)
(add-hook 'after-init-hook 'yas-global-mode)


;;; javascript
(add-hook 'js-mode-hook
          (lambda ()
            (setq-local js-indent-level 2)
            (setq-local tab-width 2)))

;; Enable eslint
(add-hook 'eglot-managed-mode-hook
          (lambda ()
            (if (derived-mode-p 'js-mode)
                (setq-local flymake-diagnostic-functions
                            (list (flymake-flycheck-diagnostic-function-for 'javascript-eslint))))))

(defun archer/eslint-fix-current-file ()
  (interactive)
  (when (fboundp 'projectile-mode)
    (projectile-with-default-dir (projectile-acquire-root)
      (save-excursion
        (let ((command (concat "npx eslint --fix " (buffer-file-name))))
          (message command)
          (shell-command command))
        (revert-buffer t t)))))


;;; typescript
(add-hook 'typescript-mode-hook (lambda ()
                                  (setq-local typescript-indent-level 2)
                                  (setq-local tab-width 2)))


;;; web
(require-package 'web-mode)
(define-derived-mode archer/web-mode web-mode "Web")
(add-to-list 'auto-mode-alist '("\\.html?\\'" . archer/web-mode))

(setq web-mode-markup-indent-offset 2)


;;; wechat mini program
(define-derived-mode wxml-mode web-mode "WXML")
(add-to-list 'auto-mode-alist '("\\.wxml\\'" . wxml-mode))
(add-to-list 'auto-mode-alist '("\\.wxss\\'" . css-mode))

;; miniprogram-mode
(defvar miniprogram-mode-map
  (let ((map (make-sparse-keymap)))
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

;;; java decompiler
;; https://github.com/xiongtx/jdecomp
(require-package 'jdecomp)
(setq jdecomp-decompiler-type 'cfr)
(setq jdecomp-decompiler-paths `((cfr . ,(expand-file-name "plugins/cfr-0.152.jar" user-emacs-directory))))
(jdecomp-mode 1)


;;; maven
(require-package 'mvn)


;;; eglot
(when (require 'eglot nil t)
  ;; c/c++
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)

  ;; wxml
  (when (fboundp 'wxml-mode)
    (add-to-list 'eglot-server-programs '(wxml-mode . ("wxml-langserver" "--stdio")))
    (add-hook 'wxml-mode-hook 'eglot-ensure))

  ;; html
  ;; FIXME: Temporarily cannot complete js in html
  (when (fboundp 'archer/web-mode)
    (add-to-list 'eglot-server-programs '(archer/web-mode . ("vscode-html-language-server" "--stdio")))
    (add-hook 'web-mode-hook 'eglot-ensure))

  ;; css
  (add-hook 'css-mode-hook 'eglot-ensure)

  ;; js/ts
  (add-hook 'js-mode-hook 'eglot-ensure)
  (add-hook 'typescript-mode-hook 'eglot-ensure)

  ;; java
  ;; Realize the source jump
  (defun eglot-java-handle-uri (fn url)
    (if (and (stringp url)
             (numberp (string-match-p "^jdt://" url)))
        (eglot-java--resolve-uri url)
      (funcall fn url)))

  (defun eglot-java--ensure-dir (path)
    "Ensure that directory PATH exists."
    (unless (file-directory-p path)
      (make-directory path t)))

  (defun eglot-java--get-metadata-location (file-location)
    "Given a FILE-LOCATION return the file containing the metadata for the file."
    (format "%s.%s.metadata"
            (file-name-directory file-location)
            (file-name-base file-location)))

  (defun eglot-java--get-filename (url)
    "Get the name of the buffer calculating it based on URL."
    (or (save-match-data
          (when (string-match "jdt://contents/\\(.*?\\)/\\(.*\\)\.class\\?" url)
            (format "%s.java"
                    (replace-regexp-in-string "/" "." (match-string 2 url) t t))))
        (-when-let ((_ file-name _ jar)
                    (s-match
                     "jdt://.*?/\\(.*?\\)\\?=\\(.*?\\)/.*/\\(.*\\)"
                     (url-unhex-string url)))
          (format "%s(%s)" file-name
                  (->> jar
                       (s-replace "/" "")
                       (s-replace "\\" ""))))
        (save-match-data
          (when (string-match "chelib://\\(.*\\)" url)
            (let ((matched (match-string 1 url)))
              (replace-regexp-in-string (regexp-quote ".jar") "jar" matched t t))))
        (error "Unable to match %s" url)))

  (defun eglot-java--resolve-uri (uri)
    "Load a file corresponding to URI executing request to the jdt server."
    (let* ((buffer-name (eglot-java--get-filename uri))
           (file-location (concat (expand-file-name (project-root (eglot--current-project))) "workspace/jdt.ls-java-project/src/" buffer-name)))
      (unless (file-readable-p file-location)
        (eglot-java--ensure-dir (file-name-directory file-location))
        (let ((content (jsonrpc-request
                        (eglot--current-server-or-lose)
                        :java/classFileContents
                        (list :uri uri))))
          (with-temp-file file-location
            (insert content))
          (with-temp-file (eglot-java--get-metadata-location file-location)
            (insert uri))
          ))
      file-location))
  (advice-add 'eglot--uri-to-path :around #'eglot-java-handle-uri)

  (defun eglot-java-workspace-dir ()
    (let ((workspace (expand-file-name (md5 (project-root (eglot--current-project)))
                                       (expand-file-name "~/.cache/eglot-eclipse-jdt-cache"))))
      (unless (file-directory-p workspace)
        (make-directory workspace t))

      workspace))

  (add-to-list 'eglot-server-programs '(java-mode . eglot--eclipse-jdt-contact))

  (defun eglot--eclipse-jdt-contact (interactive)
    "Return cons (CLASS . ARGS) for connecting to Eclipse JDT.
If INTERACTIVE, prompt user for details."
    (let ((lombok-jar-path (expand-file-name "~/.m2/repository/org/projectlombok/lombok/1.18.22/lombok-1.18.22.jar")))
      (cons 'eglot-eclipse-jdt
            (list
             "jdtls"
             (concat "--jvm-arg=-javaagent:" lombok-jar-path)
             "-configuration" (expand-file-name "~/.cache/jdtls")
             "-data" (eglot-java-workspace-dir)
             ))))

  ;; Define said class and its methods
  (defclass eglot-eclipse-jdt (eglot-lsp-server) ()
    :documentation "Eclipse's Java Development Tools Language Server.")

  (cl-defmethod eglot-initialization-options ((server eglot-eclipse-jdt))
    "Passes through required JDT initialization options."
    `(
      :settings (:java (:completion (:importOrder ["java" "javax" "com" "org"])))
      :extendedClientCapabilities (:classFileContentsSupport t)))

  (add-hook 'java-mode-hook (lambda ()
                              (setq-local c-basic-offset 2) ;; The indentation configuration
                              (setq-local tab-width 2) ;; The indentation configuration
                              (eglot-ensure)))

  ;; python
  (add-hook 'python-mode-hook 'eglot-ensure))


(define-derived-mode volar-api-mode web-mode "volar-api")
(add-to-list 'auto-mode-alist '("\\.vue?\\'" . volar-api-mode))
(add-hook 'volar-api-mode-hook 'eglot-ensure)


(add-to-list 'eglot-server-programs '(volar-api-mode . (eglot-volar-api "vue-language-server" "--stdio")))

(defclass eglot-volar-api (eglot-lsp-server) ()
  :documentation "volar-api")

(cl-defmethod eglot-initialization-options ((server eglot-volar-api))
  "Passes through required cquery initialization options"
  `(
    :typescript (:serverPath ,(expand-file-name "~/.nvm/versions/node/v16.9.0/lib/node_modules/typescript/lib/tsserverlibrary.js"))
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
                                    :defaultTagNameCase "both"
                                    :defaultAttrNameCase "kebabCase"
                                    :getDocumentNameCasesRequest :json-false
                                    :getDocumentSelectionRequest :json-false
                                    )
                       :schemaRequestService (:getDocumentContentRequest :json-false)
                       )
    :documentFeatures (
                       :selectionRange t
                       :foldingRange t
                       :linkedEditingRange t
                       :documentSymbol t
                       :documentColor t
                       )))



;;; english
(add-to-list 'load-path (expand-file-name "site-lisp/popweb/extension/dict" user-emacs-directory))

;; Chinese-English translation popup
(require 'popweb-dict-bing) ; Translation using Bing
(global-set-key (kbd "M-s l") 'popweb-dict-bing-pointer)

;; This is english helper extension that base on Emacs company-mode.
(require 'company-english-helper)

;; Translate plug-in
(require 'insert-translated-name)
(setq insert-translated-name-translate-engine "youdao")
(global-set-key (kbd "M-s n") 'insert-translated-name-insert)


;;; plantuml
(require-package 'plantuml-mode)
(setq plantuml-jar-path (expand-file-name "~/.cache/plantuml/plantuml.jar"))
(setq plantuml-default-exec-mode 'jar)
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


(provide 'init-local)
;;; init-local.el ends here
