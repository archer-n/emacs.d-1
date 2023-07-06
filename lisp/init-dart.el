;;; init-dart.el --- Dart editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'dart-mode)

;;; Formatting
(when (maybe-require-package 'reformatter)
  (reformatter-define dart-format
    :program "dart"
    :args '("format")))

(with-eval-after-load "dart-mode"
  (define-key dart-mode-map (kbd "C-c C-o") 'dart-format-buffer))


;;; LSP
(defun project-try-dart (dir)
  (let ((project (or (locate-dominating-file dir "pubspec.yaml")
                     (locate-dominating-file dir "BUILD"))))
    (if project
        (cons 'dart project)
      (cons 'transient dir))))
(add-hook 'project-find-functions #'project-try-dart)
(cl-defmethod project-roots ((project (head dart)))
  (list (cdr project)))

(add-hook 'dart-mode-hook 'eglot-ensure)


(provide 'init-dart)
;;; init-dart.el ends here
