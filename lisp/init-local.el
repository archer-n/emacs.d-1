;;; init-local.el --- Changing themes and adding your own customization -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; theme
(light)


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


(provide 'init-local)
;;; init-local.el ends here
