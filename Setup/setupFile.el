
;; [[file:~/GitHub/Project-Template-For-OrgMode/README.org::*Default%20%3DsetupFile.el%3D%20file][Default\ =setupFile\.el=\ file:1]]

(if my-project-root
    ;; Configuration
    (progn
      (message "Configuring %s " my-project-root)
      ;;
      ;; Local bibliography
      ;;
      (setq my-bibtex-filename 
            (concat my-project-root "Bibliography/bibliography.bib"))
      (if (file-exists-p my-bibtex-filename)
          ;; If bibliography.bib exists 
          (setq reftex-default-bibliography  `(,my-bibtex-filename)
                
                bibtex-completion-bibliography my-bibtex-filename
                bibtex-completion-library-path (file-name-directory my-bibtex-filename)
                bibtex-completion-notes-path (file-name-directory my-bibtex-filename)
                
                org-ref-default-bibliography  `(,my-bibtex-filename)
                org-ref-pdf-directory (file-name-directory my-bibtex-filename)
                )
        ;; otherwise unbound meaningless my-bibtex-filename
        (makunbound 'my-bibtex-filename)
        )
      ;;
      ;; Agenda files
      ;;
      (setq org-agenda-files
            (mapcar 'abbreviate-file-name
                    (split-string
                     (shell-command-to-string (format "find %s -name \"*.org\"" my-project-root))
                     "\n")))
      ;;
      ;; My my-workInProgress-filename and its associated captures
      ;;
      (setq my-workInProgress-filename (concat my-project-root "workInProgress.org"))

      (when (file-exists-p my-workInProgress-filename)
        (setq org-capture-templates `(
                                      ("t" "Todo" entry (file+headline ,my-workInProgress-filename "Project TODO")
                                       "* TODO %^{Title?} %^G\n:PROPERTIES:\n:Created: %U\n:END:\n%?"
                                       :empty-lines 1  
                                       :create t
                                       )
                                      
                                      ("T" "Todo with file link" entry (file+headline ,my-workInProgress-filename "Project TODO")
                                       "* TODO %^{Title|%f} %^G\n:PROPERTIES:\n:Created: %U\n:END:\n\n[[%l][In file %f]]:\n\n#+BEGIN_EXAMPLE\n%i\n#+END_EXAMPLE\n\n%?"
                                       :empty-lines 1  
                                       :create t
                                       )
                                      
                                      ("j" "Journal" entry (file+olp+datetree ,my-workInProgress-filename "Project journal")
                                       "* %^{Title} %^G\n\n%?"
                                       :empty-lines 1  
                                       :create t
                                       )

                                      ("J" "Journal with file link" entry (file+olp+datetree ,my-workInProgress-filename "Project journal")
                                       "* %^{Title|%f} %^G\n\n[[%l][In file %f]]:\n\n#+BEGIN_EXAMPLE\n%i\n#+END_EXAMPLE\n\n%?"
                                       :empty-lines 1  
                                       :create t
                                       )
                                        ; CAVEAT: emacs must run in server mode for org-protocol
                                      ("L" "Protocol Link" entry (file+headline ,my-workInProgress-filename "W3 Links")
                                       "* [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]] %^G\n:PROPERTIES:\n:Created: %U\n:END:\n\n%?"
                                       :empty-lines 1  
                                       :create t
                                       )
                                      
                                      ("p" "Protocol" entry (file+headline ,my-workInProgress-filename "W3 Links")
                                       "* [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]] %^G\n:PROPERTIES:\n:Created: %U\n:END:\n#+BEGIN_EXAMPLE\n%i\n#+END_EXAMPLE\n\n%?"
                                       :empty-lines 1  
                                       :create t
                                       )
                                      )))
      ;;
      ;; How to publish
      ;;
      (setq my-publish-dir my-project-root)
      
      (setq org-publish-project-alist
            `(
              ("DefaultProject_Org"
               :base-directory ,my-project-root
               :base-extension "org"
               :recursive t
               :publishing-directory ,my-publish-dir
               :publishing-function org-html-publish-to-html
               :htmlize-source t
               :org-html-head-include-default-style nil
               :exclude "Setup*"
               :makeindex t
               :auto-sitemap t
               )

              ;; ("PPack_Tangle"
              ;;  :base-directory ,my-project-root
              ;;  :base-extension "org"
              ;;  :recursive t
              ;;  :publishing-directory ,my-publish-dir
              ;;  :publishing-function org-babel-tangle-publish
              ;;  :exclude ".*bazel-.*"
              ;;  )

              ("DefaultProject_Data"
               :base-directory ,my-project-root
               :base-extension "nb\\|css\\|?pp\\|png"
               :exclude "bazel-*"
               :recursive t
               :publishing-directory ,my-publish-dir
               :publishing-function org-publish-attachment
               :exclude ".*bazel-.*"
               )

              ;; Main
              ("DefaultProject"
               :components (
                            "DefaultProject_Org" 
                            ;;"DefaultProject_Tangle"
                            "DefaultProject_Data")
               )
              )
            )
      ) ; progn
  ;; else
  (error "Project root undefined")
  )

;; Default\ =setupFile\.el=\ file:1 ends here
