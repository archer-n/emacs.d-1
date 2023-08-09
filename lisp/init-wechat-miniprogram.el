;;; init-wechat-miniprogram.el --- Editing Wechat mini program -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'web-mode)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (define-derived-mode wxml-mode web-mode "WXML")
  (add-to-list 'auto-mode-alist '("\\.wxml\\'" . wxml-mode))

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(wxml-mode . ("wxml-langserver" "--stdio")))))


(add-to-list 'auto-mode-alist '("\\.wxss\\'" . css-mode))

(add-hook 'wxml-mode-hook 'eglot-ensure)
(add-hook 'css-mode-hook 'eglot-ensure)


(defvar miniprogram-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-l q") #'miniprogram-quick-layout)
    (define-key map (kbd "C-c C-l c") #'miniprogram-quick-layout-code)
    (define-key map (kbd "C-c C-l l") #'miniprogram-quick-layout-style)
    (define-key map (kbd "C-c C-l j") #'miniprogram-quick-layout-config)
    map)
  "keymap while miniprogram-mode is active")

(define-minor-mode miniprogram-mode
  "Provides some auxiliary functions for WeChat minprogram.
Add the code to enable miniprogram-mode in the .dir-locales.el file in the
root directory of the miniprogram project.
For example: ((nil . ((miniprogram-mode . t))))"

  :lighter " mini"
  :keymap miniprogram-mode-map)

(defun miniprogram-find-file (ext-name)
  (when (and (stringp ext-name)
             (file-exists-p (concat (file-name-base (buffer-file-name)) ext-name)))
    (find-file (concat (file-name-base (buffer-file-name)) ext-name))))

(defun miniprogram-quick-layout ()
  (interactive)
  (delete-other-windows)
  (let* ((left-top (selected-window))
         (right-top (split-window-horizontally))
         (left-bottom (split-window-vertically))
         (_ (select-window right-top))
         (right-bottom (split-window-vertically)))
    (select-window left-top)
    (miniprogram-find-file ".wxml")
    (select-window right-top)
    (miniprogram-find-file ".js")
    (select-window left-bottom)
    (miniprogram-find-file ".wxss")
    (select-window right-bottom)
    (miniprogram-find-file ".json")))

;; A quick layout
(defun miniprogram-layout-left-right (left-file-type right-file-type)
  (delete-other-windows)
  (let* ((left (selected-window))
         (right (split-window-horizontally)))
    (select-window left)
    (miniprogram-find-file left-file-type)
    (select-window right)
    (miniprogram-find-file right-file-type)
    (select-window right)))

(defun miniprogram-quick-layout-config ()
  (interactive)
  (miniprogram-layout-left-right ".wxml" ".json"))

(defun miniprogram-quick-layout-code ()
  (interactive)
  (miniprogram-layout-left-right ".wxml" ".js"))

(defun miniprogram-quick-layout-style ()
  (interactive)
  (miniprogram-layout-left-right ".wxml" ".wxss"))

(provide 'init-wechat-miniprogram)
;;; init-wechat-miniprogram.el ends here
