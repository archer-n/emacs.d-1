;;; init-local.el --- Changing themes and adding your own customization -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; theme
(light)


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


;;; eglot
(require-package 'eglot)
(require 'eglot)

;; js/ts
(add-hook 'js-mode-hook 'eglot-ensure)
(add-hook 'typescript-mode-hook 'eglot-ensure)


;; java
(defun my-eglot-eclipse-jdt-contact (interactive)
  "Contact with the jdt server input INTERACTIVE."
  (let ((cp (getenv "CLASSPATH"))
        (jdtls-launcher-version "1.6.400.v20210924-0641"))
    (setenv "CLASSPATH" (concat cp ":" (expand-file-name "~/.cache/jdt-language-server/plugins/org.eclipse.equinox.launcher_") jdtls-launcher-version ".jar"))
    (unwind-protect (let ((command (eglot--eclipse-jdt-contact nil))
                          (lombok-jar-path (expand-file-name "~/.m2/repository/org/projectlombok/lombok/1.18.22/lombok-1.18.22.jar")))
                      (cons (car command)
                            (flatten-list (list
                                           (cadr command)
                                           (concat "-javaagent:" lombok-jar-path)
                                           (cddr command)
                                           "--add-modules=ALL-SYSTEM"
                                           "--add-opens java.base/java.util=ALL-UNNAMED"
                                           "--add-opens java.base/java.lang=ALL-UNNAMED"))))
      (setenv "CLASSPATH" cp))))


(setcdr   (assq 'java-mode eglot-server-programs) #'my-eglot-eclipse-jdt-contact)

(add-hook 'java-mode-hook (lambda ()
                            (setq-local c-basic-offset 2) ;; The indentation configuration
                            (setq-local tab-width 2) ;; The indentation configuration
                            (eglot-ensure)))



;;; wechat mini program
(require 'wxml-mode)

(provide 'init-local)
;;; init-local.el ends here
