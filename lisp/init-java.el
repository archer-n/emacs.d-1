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

(define-advice eglot-path-to-uri (:around (orig-fn path) advice)
  "Support non standard LSP uri scheme."
  (or (gethash path eglot-path-uri-cache)
      (funcall orig-fn path)))

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

;; Android project java and kotlin code disable Eglot config:
;;
;; .dir-locals.el
;;
;; ((dart-mode . ((fill-column . 120)
;;                (eglot-workspace-configuration
;;                 . (:dart (:lineLength 120))))))


;; support for jar
(when (maybe-require-package 'jarchive)
  (add-hook 'after-init-hook 'jarchive-setup))


;;; java decompiler
;; https://github.com/xiongtx/jdecomp
(when (maybe-require-package 'jdecomp)
  (setq jdecomp-decompiler-type 'cfr)
  (setq jdecomp-decompiler-paths `((cfr . ,(expand-file-name "plugins/cfr-0.152.jar" user-emacs-directory))))
  (jdecomp-mode 1))



;; search  file in gralde cache
(defun wf/find-file-in-gradle-cache ()
  (interactive)
  (consult-find (expand-file-name "~/.gradle/caches/modules-2/files-2.1/")))

(defun wf/find-file-in-aosp ()
  (interactive)
  (consult-find (expand-file-name "~/workspace/aosp/")))

(defun wf/find-file-in-aosp-framework ()
  (interactive)
  (consult-find (expand-file-name "~/workspace/aosp/frameworks/")))

(defun wf/find-android-jar ()
  (interactive)
  (find-file "~/Android/Sdk/platforms/android-33/android.jar"))

(defvar wf/android-studio-cli "/opt/android-studio/bin/studio.sh")

(defun wf/open-file-in-android-studio ()
  (interactive)
  (shell-command (concat wf/android-studio-cli " " (format "--line %d %s" (line-number-at-pos) buffer-file-name))))

(provide 'init-java)
;;; init-java.el ends here
