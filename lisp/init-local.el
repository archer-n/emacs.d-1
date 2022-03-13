;;; init-local.el --- Changing themes and adding your own customization -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; theme
(light)


;;; consult
(global-set-key (kbd "M-s /") 'consult-line)
(global-set-key (kbd "M-s i") 'consult-imenu)


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


;;; yasnippet
(require-package 'yasnippet)
;; (require-package 'yasnippet-snippets)
(add-hook 'after-init-hook 'yas-global-mode)


;;; web
(require-package 'web-mode)
(define-derived-mode archer/web-mode web-mode "Web")
(add-to-list 'auto-mode-alist '("\\.html?\\'" . archer/web-mode))
(setq web-mode-markup-indent-offset 2)


;;; wechat mini program
(define-derived-mode wxml-mode web-mode "WXML")
(add-to-list 'auto-mode-alist '("\\.wxml\\'" . wxml-mode))


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
  (defun eglot-java-workspace-dir ()
    (let ((workspace (expand-file-name (md5 (project-root (eglot--current-project)))
                                       (expand-file-name "~/.cache/eglot-eclipse-jdt-cache"))))
      (unless (file-directory-p workspace)
        (make-directory workspace t))

      workspace))

  ;; The location of the data workspace needs to be specified, it cannot be placed in the default location in the project
  (let ((lombok-jar-path (expand-file-name "~/.m2/repository/org/projectlombok/lombok/1.18.22/lombok-1.18.22.jar")))
    (setcdr (assq 'java-mode eglot-server-programs)
            (lambda (_)
              (cons"jdtls"
                   (list
                    (concat "--jvm-arg=-javaagent:" lombok-jar-path)
                    "-data" (eglot-java-workspace-dir))))))

  (add-hook 'java-mode-hook (lambda ()
                              (setq-local c-basic-offset 2) ;; The indentation configuration
                              (setq-local tab-width 2) ;; The indentation configuration
                              (eglot-ensure)))

  ;; python
  (add-hook 'python-mode-hook 'eglot-ensure))



(provide 'init-local)
;;; init-local.el ends here
