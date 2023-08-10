;;; init-citre.el ---  Ctags IDE on the True Editor  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(require-package 'citre)
(require 'citre)
(require 'citre-config)
;;(setq-default citre-enable-imenu-integration nil)
(setq-default citre-auto-enable-citre-mode-modes '(java-mode kotlin-mode))
;;(setq-default citre-enable-xref-integration nil)
(global-set-key (kbd "C-x c j") 'citre-jump)
(global-set-key (kbd "C-x c J") 'citre-jump-back)
(global-set-key (kbd "C-x c p") 'citre-ace-peek)
(global-set-key (kbd "C-x c u") 'citre-update-this-tags-file)
(define-advice xref--create-fetcher (:around (-fn &rest -args) fallback)
  (let ((fetcher (apply -fn -args))
        (citre-fetcher
         (let ((xref-backend-functions '(citre-xref-backend t)))
           (apply -fn -args))))
    (lambda ()
      (or (with-demoted-errors "%s, fallback to citre"
            (funcall fetcher))
          (funcall citre-fetcher)))))

;; citre-global
;; https://github.com/universal-ctags/citre/blob/master/docs/user-manual/citre-global.md
(setq xref-prompt-for-identifier nil)

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
(add-hook 'citre-mode-hook (lambda ()
                             (setq company-backends '((company-capf
                                                       company-citre-tags
                                                       company-citre-global
                                                       :with company-yasnippet
                                                       :separate)))))


(provide 'init-citre)
;;; init-citre.el ends here
