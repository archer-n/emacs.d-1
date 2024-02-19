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
(add-hook 'dart-mode-hook 'eglot-ensure)


(provide 'init-dart)
;;; init-dart.el ends here
