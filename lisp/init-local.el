;;; init-local.el --- Personal configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; GTD
(with-eval-after-load 'org
  (setq-default org-default-notes-file (concat org-directory "/inbox.org"))
  (setq-default org-agenda-files (list org-default-notes-file
                                       (concat org-directory "/gtd.org")
                                       (concat org-directory "/notes.org"))))


;;; svg
(add-hook 'after-init-hook 'auto-image-file-mode)


;; 设置字体大小
(set-face-attribute 'default nil :height 110)


(provide 'init-local)

;;; init-local.el ends here
