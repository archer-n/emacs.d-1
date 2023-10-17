;;; init-local.el --- Personal configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; GTD
(with-eval-after-load 'org
  (setq-default org-default-notes-file (concat org-directory "/inbox.org"))
  (setq-default org-agenda-files (list org-default-notes-file
                                       (concat org-directory "/gtd.org"))))


;;; svg
(add-hook 'after-init-hook 'auto-image-file-mode)


;; 设置字体大小
(set-face-attribute 'default nil :height 110)


;; alter
(with-eval-after-load 'alert
  (setq-default alert-default-style 'libnotify))

(provide 'init-local)

;;; init-local.el ends here
