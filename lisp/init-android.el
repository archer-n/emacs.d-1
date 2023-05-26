;;; init-android.el --- android helper -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar android-studio-cli "/opt/android-studio/bin/studio.sh")

(defun open-file-in-android-studio ()
  (interactive)
  (shell-command (concat android-studio-cli " " (format "--line %d %s" (line-number-at-pos) buffer-file-name))))



(provide 'init-android)
;;; init-android.el ends here
