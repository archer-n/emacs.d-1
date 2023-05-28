;;; init-corfu.el --- Interactive completion in buffers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; WAITING: haskell-mode sets tags-table-list globally, breaks tags-completion-at-point-function
;; TODO Default sort order should place [a-z] before punctuation

(setq tab-always-indent 'complete)

(when (maybe-require-package 'orderless)
  (with-eval-after-load 'vertico
    (require 'orderless)
    (setq completion-styles '(orderless basic))))

(setq completion-category-defaults nil
      completion-category-overrides nil)
(setq completion-cycle-threshold 4)

(when (maybe-require-package 'corfu)
  (setq-default corfu-auto t)
  (with-eval-after-load 'eshell
    (add-hook 'eshell-mode-hook (lambda () (setq-local corfu-auto nil))))
  (setq-default corfu-quit-no-match 'separator)
  (add-hook 'after-init-hook 'global-corfu-mode)

  (when (featurep 'corfu-popupinfo)
    (with-eval-after-load 'corfu
      (corfu-popupinfo-mode)))

  ;; TODO: https://github.com/jdtsmith/kind-icon
  )


(require-package 'cape)

;; Merge the dabbrev, dict and keyword capfs, display candidates together.
(with-eval-after-load 'citre
  (add-hook 'citre-mode-hook (lambda ()
                               (setq-local completion-at-point-functions
                                           (list (cape-super-capf
                                                  #'cape-dabbrev
                                                  #'cape-keyword
                                                  #'citre-completion-at-point
                                                  ))))))


(provide 'init-corfu)
;;; init-corfu.el ends here
