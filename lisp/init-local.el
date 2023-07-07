;;; init-local.el --- Personal configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; GTD
(with-eval-after-load 'org
  (setq-default org-default-notes-file (concat org-directory "/inbox.org"))
  (setq-default org-agenda-files (list org-default-notes-file
                                       (concat org-directory "/gtd.org")
                                       (concat org-directory "/notes.org"))))

(provide 'init-local)

;;; init-local.el ends here
