;;; init-corfu.el --- Completion with corfu -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'corfu)
(require-package 'cape)
(require 'corfu-history)
(require 'corfu-info)
(require 'corfu-quick)

(global-corfu-mode)
(setq tab-always-indent 'complete)
(setq corfu-auto t)

(require-package 'dabbrev)
(global-set-key (kbd "M-/") 'dabbrev-completion)
(with-eval-after-load 'dabbrev
  (setq dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(provide 'init-corfu)
;;; init-corfu.el ends here
