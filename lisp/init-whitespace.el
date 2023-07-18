;;; init-whitespace.el --- Special handling for whitespace -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default show-trailing-whitespace nil)


;;; Whitespace

(defun sanityinc/show-trailing-whitespace ()
  "Enable display of trailing whitespace in this buffer."
  (setq-local show-trailing-whitespace t))

(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook 'sanityinc/show-trailing-whitespace))

;; When using auto-save, if the auto-clear blank mode is enabled,
;; it will interfere with the current input. Turn it off here
(when (not (featurep 'auto-save))
  (require-package 'whitespace-cleanup-mode)
  (add-hook 'after-init-hook 'global-whitespace-cleanup-mode)
  (with-eval-after-load 'whitespace-cleanup-mode
    (diminish 'whitespace-cleanup-mode)))


(global-set-key [remap just-one-space] 'cycle-spacing)


(provide 'init-whitespace)
;;; init-whitespace.el ends here
