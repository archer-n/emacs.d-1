;;; init-auto-save.el --- Automatically save files without temporary files to protect your finger. ;)  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'auto-save)
(setq auto-save-silent t)   ; quietly save
(setq auto-save-delete-trailing-whitespace t)  ; automatically delete spaces at the end of the line when saving
(auto-save-enable)

;; Shut down automatically saved when editing a remote file
(setq auto-save-disable-predicates
      '((lambda ()
          (file-remote-p (buffer-file-name)))))


(provide 'init-auto-save)
;;; init-auto-save.el ends here
