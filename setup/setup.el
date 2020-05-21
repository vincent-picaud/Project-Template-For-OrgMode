(if my-project-root
    ;; Configuration
    (progn
      (message (format "Configuring %s" my-project-root))
      (setq org-agenda-files
            (split-string
             (shell-command-to-string (format "cd %s; find -name '*.org' ! -name 'index.org'  ! -name 'agenda.org'  ! -name '.#*' ! -path './setup/*'" my-project-root))
             ))
      (setq my-publish-dir (concat my-project-root "doc"))
      (setq my-project-name "MyProject")
      
      (defun my-org-publish-sitemap (title list)
        "Create my own index.org instead of the default one"
        (concat	"#+INCLUDE: \"setup/index_preamble.org\"\n"
      		"#+OPTIONS: toc:nil\n\n"
      		"#+TITLE: "
      		title
      		"\n"
      		"* My Sitemap\n\n"
      		(org-list-to-org list)
      		"\n\n"))
      
      (setq org-publish-project-alist
            `(
      	(,(concat my-project-name "_Org")
      	 :base-directory ,my-project-root
      	 :base-extension "org"
      	 :recursive t
      	 :publishing-directory ,my-publish-dir
      	 :publishing-function org-html-publish-to-html
      	 :sitemap-function my-org-publish-sitemap
      	 :htmlize-source t
      					;	 :org-html-head-include-default-style nil
      	 :exclude "setup/*"
      	 ;; Generates theindex.org + inc files
      	 :makeindex t
      	 ;; Creates index.org, calls my-org-publish-sitemap to fill it
      	 :auto-sitemap t
      	 :sitemap-filename "index.org"
      	 )
      
      	;; (,(concat my-project-name "_Tangle")
      	;;  :base-directory ,my-project-root
      	;;  :base-extension "org"
      	;;  :recursive t
      	;;  :publishing-directory ,my-publish-dir
      	;;  :publishing-function org-babel-tangle-publish
      	;;  :exclude ".*bazel-.*"
      	;;  )
      
      	;; (,(concat my-project-name "_Data")
      	;;  :base-directory ,my-project-root
      	;;  :base-extension "nb\\|?pp\\|png"
      	;;  :recursive t
      	;;  :publishing-directory ,my-publish-dir
      	;;  :publishing-function org-publish-attachment
      	;;  :exclude ".*bazel-.*"
      	;;  )
      
      	;; Main
      	(,my-project-name
      	 :components (,(concat my-project-name "_Org")
      	              ;; ,(concat my-project-name "_Tangle")
      		      ;; ,(concat my-project-name "_Data")
      		      )
      	 )
      	)
            )
      )
  )
