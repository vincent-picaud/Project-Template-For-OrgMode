(if my-project-root
    ;; Configuration
    (progn
      (message (format "Configuring %s" my-project-root))
      (setq my-publish-dir (concat my-project-root "doc"))
      
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
      	("my-project-org-files",
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
      
      	("my-project-data-files",
      	 :base-directory ,my-project-root
      	 :base-extension "nb\\|?pp\\|png"
      	 :recursive t
      	 :publishing-directory ,my-publish-dir
      	 :publishing-function org-publish-attachment
      	 :exclude ".*bazel-.*"
      	 )
      
      	;; Main
      	("my-project",
      	 :components ("my-project-org-files" "my-project-data-files")
      	 )
      	)
            )
      (setq org-agenda-files
            (split-string
             (shell-command-to-string (format "cd %s; find -name '*.org' ! -name 'index.org'  ! -name 'agenda.org'  ! -name '.#*' ! -path './setup/*'" my-project-root))
             ))
      )
  )
