;;; init-java.el --- LSP support for java -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'eglot)


;;; Eglot launch JDTLS
(defun jdtls-command-contact (&optional interactive)
  (let* ((jvm-args `(,(concat "-javaagent:" (expand-file-name "~/.m2/repository/org/projectlombok/lombok/1.18.24/lombok-1.18.24.jar"))))
         (jvm-args (mapcar (lambda (arg) (concat "--jvm-arg=" arg)) jvm-args))
         ;; tell jdtls the data directory and jvm args
         (contact (append '("jdtls") jvm-args)))
    contact))

(push '((java-mode java-ts-mode) . jdtls-command-contact) eglot-server-programs)


;;; Eglot initialize JDTLS (User-specific configuration)
(defun jdtls-initialization-options ()
  `(:extendedClientCapabilities (:classFileContentsSupport t)))

(cl-defmethod eglot-initialization-options (server &context (major-mode java-mode))
  (jdtls-initialization-options))

(cl-defmethod eglot-initialization-options (server &context (major-mode java-ts-mode))
  (jdtls-initialization-options))


;;; Eglot handle jdt:// scheme uri
(defvar eglot-path-uri-cache (make-hash-table :test #'equal)
  "File path to uri cache.")

(cl-defgeneric +eglot/ext-uri-to-path (uri)
  "Support extension uri."
  nil)

(define-advice eglot-uri-to-path (:around (orig-fn uri) advice)
  "Support non standard LSP uri scheme."
  (when (keywordp uri) (setq uri (substring (symbol-name uri) 1)))
  (or (+eglot/ext-uri-to-path uri)
      (funcall orig-fn uri)))

(define-advice eglot-path-to-uri (:around (orig-fn &rest args) advice)
  "Support non standard LSP uri scheme."
  (or (gethash (car args) eglot-path-uri-cache)
      (apply orig-fn args)))

(defun +eglot/jdtls-uri-to-path (uri)
  "Support Eclipse jdtls `jdt://' uri scheme."
  (when-let* ((jdt-scheme-p (string-prefix-p "jdt://" uri))
              (filename (when (string-match "^jdt://contents/\\(.*?\\)/\\(.*\\)\.class\\?" uri)
                          (format "%s.java" (replace-regexp-in-string "/" "." (match-string 2 uri) t t))))
              (source-dir (expand-file-name (md5 (project-root (eglot--project (eglot-current-server)))) "/tmp/eglot-jdtls"))
              (source-file (expand-file-name (file-name-concat source-dir filename))))
    (unless (file-directory-p source-dir)
      (make-directory source-dir t))
    (unless (file-readable-p source-file)
      (let ((content (jsonrpc-request (eglot--current-server-or-lose)
                                      :java/classFileContents
                                      (list :uri uri))))
        (with-temp-file source-file (insert content))))
    (puthash source-file uri eglot-path-uri-cache)
    source-file))

(cl-defmethod +eglot/ext-uri-to-path (uri &context (major-mode java-mode))
  (+eglot/jdtls-uri-to-path uri))

(cl-defmethod +eglot/ext-uri-to-path (uri &context (major-mode java-ts-mode))
  (+eglot/jdtls-uri-to-path uri))



(defun java-eglot-ensure ()
  (interactive)
  (setq-local tab-width 4)
  (let ((isAndroid (string-match (rx (or (one-or-more "aosp")
                                         (one-or-more "android")))
                                 buffer-file-name)))
    (if (not isAndroid)
        (eglot-ensure))))

(add-hook 'java-mode-hook 'java-eglot-ensure)
(add-hook 'java-ts-mode-hook 'java-eglot-ensure)

(provide 'init-java)
;;; init-java.el ends here