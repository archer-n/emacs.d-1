;;; init-local.el --- Personal configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; GTD
(with-eval-after-load 'org
  (setq-default org-default-notes-file (concat org-directory "/inbox.org"))
  (setq-default org-agenda-files (list org-default-notes-file
                                       (concat org-directory "/gtd.org"))))


;;; svg
;;(add-hook 'after-init-hook 'auto-image-file-mode)


;; 设置字体大小
;; (set-face-attribute 'default nil :height 110)


;; imenu
(with-eval-after-load 'imenu
  (setq imenu-max-item-length "Unlimited"))


;; alter
(with-eval-after-load 'alert
  (setq-default alert-default-style 'libnotify))

;; org-pomodoro 的通知默认不消失，需要手动点击
(with-eval-after-load 'org-pomodoro
  (defun org-pomodoro-notify (title message)
    "Send a notification with TITLE and MESSAGE using `alert'."
    (alert message :title title :category 'org-pomodoro :persistent t)))


;;consult
(when (maybe-require-package 'consult)
  (global-set-key (kbd "M-s /") 'consult-line)
  (global-set-key (kbd "M-s i") 'consult-imenu)
  (global-set-key (kbd "M-s r") 'consult-recent-file))


;; python
(add-hook 'python-mode-hook #'eglot-ensure)


;; yasnippet
(require-package 'yasnippet)
(add-hook 'after-init-hook (lambda ()
                             (yas-global-mode 1)))
(require-package 'yasnippet-snippets)


;; andorid

;; support for jar
(with-eval-after-load 'jarchive
  (diminish 'jarchive-mode))
(when (maybe-require-package 'jarchive)
  (add-hook 'after-init-hook 'jarchive-setup))

;;; java decompiler
;; https://github.com/xiongtx/jdecomp
(when (maybe-require-package 'jdecomp)
  (setq jdecomp-decompiler-type 'fernflower)
  (setq jdecomp-decompiler-paths `((cfr . ,(expand-file-name "plugins/cfr-0.152.jar" user-emacs-directory))
                                   (fernflower . ,(expand-file-name "plugins/java-decompiler.jar" user-emacs-directory))))
  (jdecomp-mode 1))

;; search  file in gralde cache
(defun wf/find-file-in-gradle-cache ()
  (interactive)
  (consult-find (expand-file-name "~/.gradle/caches/modules-2/files-2.1/")))

(defun wf/find-android-jar ()
  (interactive)
  (find-file "~/Android/Sdk/platforms/android-33/android.jar"))

(defun wf/open-file-in-android-studio ()
  (interactive)
  (shell-command (concat "/opt/android-studio/bin/studio.sh" " " (format "--line %d %s" (line-number-at-pos) buffer-file-name))))



;;; moonshot - kimi
(use-package gptel
  :ensure t
  :config
  (setq gptel-model "moonshot-v1-8k")
  (setq gptel-default-mode 'org-mode)
  (setq gptel-backend
        (gptel-make-openai "Moonshot"
          :key 'gptel-api-key
          :models '("moonshot-v1-8k"
                    "moonshot-v1-32k"
                    "moonshot-v1-128k")
          :host "api.moonshot.cn")))

(provide 'init-local)

;;; init-local.el ends here
