;;; init-plantuml.el --- Plantuml editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'plantuml-mode)

  (setq plantuml-jar-path (expand-file-name "~/.cache/plantuml/plantuml.jar")
        org-plantuml-jar-path (expand-file-name "~/.cache/plantuml/plantuml.jar")
        plantuml-default-exec-mode 'jar
        plantuml-output-type "svg")

  (when (not (file-exists-p plantuml-jar-path))
    (require 'plantuml-mode)
    (plantuml-download-jar))

  (when (maybe-require-package 'flycheck-plantuml)
    (flycheck-plantuml-setup))

  (defun company-plantuml (command &optional arg &rest ignored)
    (interactive (list 'interactive))
    (cl-case command
      (interactive (company-begin-backend 'company-plantuml))
      (prefix (let* ((symbol (company-grab-symbol))
                     (max-match-result (try-completion symbol plantuml-kwdList)))
                (if (length> max-match-result 0)
                    symbol)))
      (candidates (all-completions arg plantuml-kwdList))))

  (add-hook 'plantuml-mode-hook
            (lambda ()
              (setq-local company-backends '(company-plantuml
                                             (company-dabbrev-code company-keywords)
                                             company-dabbrev)))))


(provide 'init-plantuml)
;;; init-plantuml.el ends here
