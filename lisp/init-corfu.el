;;; init-corfu.el --- Completion Overlay Region FUnction  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require-package 'corfu)
(require-package 'cape)
(require 'corfu-history)
(require 'corfu-info)
(require 'corfu-quick)

(setq corfu-auto-prefix 0)
(setq corfu-auto t)

;; 默认用这三个补全后端
(add-to-list 'completion-at-point-functions #'cape-symbol)
(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)

;; 全局开启补全
(global-corfu-mode)
(corfu-history-mode t)

(provide 'init-corfu)
;;; init-corfu.el ends here
