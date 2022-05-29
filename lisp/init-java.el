;;; init-java.el --- simple java configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Setup specific to the Eclipse JDT setup in case one can't use the simpler 'jdtls' script
(with-eval-after-load 'eglot
  ;; Tell Eglot to use a specific class to handle java-mode files
  (add-to-list 'eglot-server-programs '(java-mode . eglot--eclipse-jdt-contact))

  (defun eglot--eclipse-jdt-contact (interactive)
    "Return cons (CLASS . ARGS) for connecting to Eclipse JDT.
    If INTERACTIVE, prompt user for details."
    (let ((workspace (eglot-java--get-data-dir))
          (configuration (expand-file-name "~/.config/jdtls"))
          (lombok (expand-file-name "~/.m2/repository/org/projectlombok/lombok/1.18.22/lombok-1.18.22.jar")))

      (unless (file-directory-p workspace)
        (make-directory workspace t))

      (cons 'eglot-eclipse-jdt
            (list (executable-find "jdtls")
                  (concat "--jvm-arg=-javaagent:" lombok)
                  "-configuration" configuration
                  "-data" workspace))))

  ;; Define said class and its methods
  (defclass eglot-eclipse-jdt (eglot-lsp-server) ()
    :documentation "Eclipse's Java Development Tools Language Server.")

  (cl-defmethod eglot-initialization-options ((server eglot-eclipse-jdt))
    "Passes through required JDT initialization options."
    `(:extendedClientCapabilities (:classFileContentsSupport t)))

  ;;; eclipse-jdt breaks the spec which in turn breaks code actions
  ;;; This behaviour can't be disabled and needs to be worked around
  (cl-defmethod eglot-execute-command
    ((_server eglot-eclipse-jdt) (_cmd (eql java.apply.workspaceEdit)) arguments)
    "Eclipse JDT breaks spec and replies with edits as arguments."
    (mapc #'eglot--apply-workspace-edit arguments))

  (defun eglot-java--get-data-dir ()
    "Get data directory"
    (let ((project (if (eglot-current-server)
                       (eglot--project (eglot-current-server))
                     (eglot--current-project))))
      (expand-file-name
       (md5 (project-root project))
       (expand-file-name "~/.cache/eglot-eclipse-jdt-cache"))))

  ;; classFileContentsSupport impl
  (defun eglot-java-handle-uri (fn url)
    (if (and (stringp url)
             (numberp (string-match-p "^jdt://" url)))
        (eglot-java--resolve-uri url)
      (funcall fn url)))

  (defun eglot-java--get-filename (url)
    "Get the name of the buffer calculating it based on URL."
    (or (save-match-data
          (when (string-match "jdt://contents/\\(.*?\\)/\\(.*\\)\.class\\?" url)
            (format "%s.java"
                    (replace-regexp-in-string "/" "." (match-string 2 url) t t))))
        (-when-let ((_ file-name _ jar)
                    (s-match
                     "jdt://.*?/\\(.*?\\)\\?=\\(.*?\\)/.*/\\(.*\\)"
                     (url-unhex-string url)))
          (format "%s(%s)" file-name
                  (->> jar
                       (s-replace "/" "")
                       (s-replace "\\" ""))))
        (save-match-data
          (when (string-match "chelib://\\(.*\\)" url)
            (let ((matched (match-string 1 url)))
              (replace-regexp-in-string (regexp-quote ".jar") "jar" matched t t))))
        (error "Unable to match %s" url)))

  (defun eglot-java--get-metadata-location (file-location)
    "Given a FILE-LOCATION return the file containing the metadata for the file."
    (format "%s.%s.metadata"
            (file-name-directory file-location)
            (file-name-base file-location)))

  (defun eglot-java--ensure-dir (path)
    "Ensure that directory PATH exists."
    (unless (file-directory-p path)
      (make-directory path t)))


  (defun eglot-java--resolve-uri (uri)
    "Load a file corresponding to URI executing request to the jdt server."
    (let* ((buffer-name (eglot-java--get-filename uri))
           (file-location (concat  (eglot-java--get-data-dir) "/jdt.ls-java-project/src/" buffer-name)))
      (unless (file-readable-p file-location)

        (eglot-java--ensure-dir (file-name-directory file-location))

        (let ((content (jsonrpc-request
                        (eglot--current-server-or-lose)
                        :java/classFileContents
                        (list :uri uri))))
          (with-temp-file file-location
            (insert content))
          (with-temp-file (eglot-java--get-metadata-location file-location)
            (insert uri))
          ))
      file-location))

  (advice-add 'eglot--uri-to-path :around #'eglot-java-handle-uri))

(add-hook 'java-mode-hook (lambda ()
                            ;; Decompiled source files are placed in a directory outside the project
                            (setq-local eglot-extend-to-xref t)
                            (eglot-ensure)))


(provide 'init-java)
;;; init-java.el ends here
