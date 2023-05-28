;;; init-company.el --- Interactive completion in buffers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'company)

(add-hook 'after-init-hook 'global-company-mode)

(when (maybe-require-package 'orderless)
  (with-eval-after-load 'vertico
    (require 'orderless)
    (setq completion-styles '(orderless basic))))

;; We follow a suggestion by company maintainer u/hvis:
;; https://www.reddit.com/r/emacs/comments/nichkl/comment/gz1jr3s/
(defun company-completion-styles (capf-fn &rest args)
  (let ((completion-styles '(basic partial-completion)))
    (apply capf-fn args)))
(advice-add 'company-capf :around #'company-completion-styles)



(defmacro citre-backend-to-company-backend (backend)
  "Create a company backend from Citre completion backend BACKEND.
The result is a company backend called
`company-citre-<backend>' (like `company-citre-tags') and can be
used in `company-backends'."
  (let ((backend-name (intern (concat "company-citre-" (symbol-name backend))))
        (docstring (concat "`company-mode' backend from the `"
                           (symbol-name backend)
                           "' Citre backend.\n"
                           "`citre-mode' needs to be enabled to use this.")))
    `(defun ,backend-name (command &optional arg &rest ignored)
       ,docstring
       (pcase command
         ('interactive (company-begin-backend ',backend-name))
         ('prefix (and (bound-and-true-p citre-mode)
                       (citre-backend-usable-p ',backend)
                       ;; We shouldn't use this as it's defined for getting
                       ;; definitions/references.  But the Citre completion
                       ;; backend design is not fully compliant with company's
                       ;; design so there's no simple "right" solution, and this
                       ;; works for tags/global backends.
                       (or (citre-get-symbol-at-point-for-backend ',backend)
                           'stop)))
         ('meta (citre-get-property 'signature arg))
         ('annotation (citre-get-property 'annotation arg))
         ('candidates (let ((citre-completion-backends '(,backend)))
                        (all-completions arg (nth 2 (citre-completion-at-point)))))))))

(citre-backend-to-company-backend tags)

(citre-backend-to-company-backend global)

(setq company-backends '((company-capf
                          company-citre-tags
                          company-citre-global
                          :with company-yasnippet
                          :separate)))

(provide 'init-company)
;;; init-company.el ends here
