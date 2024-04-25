;;; init-kotlin.el --- Kotlin support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'kotlin-mode)

(use-package flymake-ktlint
  :ensure t
  :init
  (flymake-ktlint-setup)
  ;; fix
  (defun flymake-ktlint--lint-done (report-fn
                                    source-buffer
                                    output-buffer)
    "Process ktlint result and call REPORT-FN.

SOURCE-BUFFER is the buffer to apply flymake to.
OUTPUT-BUFFER is the result of running ktlint on SOURCE-BUFFER."
    (with-current-buffer
        source-buffer
      (save-excursion
        (save-restriction
          (widen)
          (funcall
           report-fn
           (with-current-buffer output-buffer
             (mapcar (lambda (line)
                       ;; ex: /Users/user/kotlin/File.kt:32:30: Unnecessary space(s)
                       ;; ex: 17:58:17.869 [main] WARN com.pinterest.ktlint.cli.internal.KtlintCommandLine - Lint has found errors than can be autocorrected using 'ktlint --format
                       (let* ((split (split-string line ":" t))
                              (_ (nth 0 split)) ; filename
                              (line (string-to-number (nth 1 split)))
                              (column (string-to-number (nth 2 split)))
                              (message (string-trim (nth 3 split)))
                              (point (flymake-ktlint--find-point source-buffer line column)))
                         (flymake-make-diagnostic
                          source-buffer
                          (1- point)
                          point
                          :warning
                          message)))
                     (seq-take-while (lambda (line) (string-prefix-p "/" line)) (split-string (buffer-string) "\n" t)))))))))
  :config
  (setq ktlint-flymake-args '())) ;; "--code-style=android_studio" not working


;;; Formatting
(with-eval-after-load "kotlin-mode"
  (define-key kotlin-mode-map (kbd "C-c C-o")
              (lambda ()
                (interactive "")
                (shell-command (concat "ktlint" " -F " buffer-file-name)))))

(provide 'init-kotlin)
;;; init-kotlin.el ends here
