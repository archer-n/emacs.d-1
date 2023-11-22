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


;; sdcv
(require-package 'posframe)
(require 'sdcv)
(setq sdcv-say-word-p t)               ;say word after translation

(setq sdcv-dictionary-data-dir (expand-file-name "~/.stardict/dic")) ;setup directory of stardict dictionary

(setq sdcv-dictionary-simple-list    ;setup dictionary list for simple search
      '("简明英汉字典"
        "简明汉英字典"
        "计算机词汇"
        "牛津现代英汉双解词典"
        "懒虫简明英汉词典"
        "懒虫简明汉英词典"
        "KDic11万英汉词典"

        ))

(setq sdcv-dictionary-complete-list     ;setup dictionary list for complete search
      '("简明英汉字典"
        "简明汉英字典"
        "计算机词汇"
        "牛津现代英汉双解词典"
        "懒虫简明英汉词典"
        "懒虫简明汉英词典"
        "KDic11万英汉词典"
        ))

(provide 'init-local)

;;; init-local.el ends here
