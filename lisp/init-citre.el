;;; init-citre.el ---  Ctags IDE on the True Editor  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require-package 'citre)
(require 'citre)
(require 'citre-config)
(setq-default citre-auto-enable-citre-mode-modes '(java-mode kotlin-mode nxml-mode))
(global-set-key (kbd "C-x c j") 'citre-jump)
(global-set-key (kbd "C-x c J") 'citre-jump-back)
(global-set-key (kbd "C-x c p") 'citre-ace-peek)
(global-set-key (kbd "C-x c u") 'citre-update-this-tags-file)

;; citre-global
;; https://github.com/universal-ctags/citre/blob/master/docs/user-manual/citre-global.md
(setq xref-prompt-for-identifier nil)

(add-hook 'citre-mode-hook (lambda () (interactive "")
                             (setq-local company-backends '((company-keywords company-capf company-dabbrev-code :with company-yasnippet)))))


(provide 'init-citre)
;;; init-citre.el ends here
