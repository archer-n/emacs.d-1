;;; init-company.el --- Interactive completion in buffers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; WAITING: haskell-mode sets tags-table-list globally, breaks tags-completion-at-point-function
;; TODO Default sort order should place [a-z] before punctuation

(setq tab-always-indent 'complete)
(when (maybe-require-package 'orderless)
  (with-eval-after-load 'vertico
    (require 'orderless)
    (setq completion-styles '(orderless basic)))

  ;; We follow a suggestion by company maintainer u/hvis:
  ;; https://www.reddit.com/r/emacs/comments/nichkl/comment/gz1jr3s/
  (defun company-completion-styles (capf-fn &rest args)
    (let ((completion-styles '(basic partial-completion)))
      (apply capf-fn args)))

  (advice-add 'company-capf :around #'company-completion-styles))


(setq completion-category-defaults nil
      completion-category-overrides nil)
(setq completion-cycle-threshold 4)

(when (maybe-require-package 'company)
  (add-hook 'after-init-hook 'global-company-mode))

(with-eval-after-load 'company
  (diminish 'company-mode)
  (define-key company-mode-map [remap completion-at-point] 'company-complete)
  (define-key company-mode-map [remap indent-for-tab-command] 'company-indent-or-complete-common)
  (setq company-dabbrev-minimum-length 2
        company-dabbrev-other-buffers t
        company-dabbrev-downcase nil
        company-show-quick-access 'right
        company-format-margin-function #'company-text-icons-margin))

(provide 'init-company)
;;; init-company.el ends here
