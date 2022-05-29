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


;;; yasnippet
(require-package 'yasnippet)
(require-package 'yasnippet-snippets)
(add-hook 'after-init-hook 'yas-global-mode)


;;; javascript
(add-hook 'js-mode-hook
          (lambda ()
            (setq-local flymake-diagnostic-functions
                        (list (flymake-flycheck-diagnostic-function-for 'javascript-eslint)))
            (setq-local js-indent-level 2)
            (setq-local tab-width 2)))

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
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.wxml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.wxss\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
(setq web-mode-markup-indent-offset 2)


;;; wechat mini program
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
(setq org-plantuml-jar-path (expand-file-name "~/.cache/plantuml/plantuml.jar"))
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
