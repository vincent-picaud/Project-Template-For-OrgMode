(if my-project-root
    ;; Configuration
    (progn
      ;; Configuration message
      (message (format "Configuring %s" my-project-root))
      ;; use =minted= for listings
      (setq org-latex-listings 'minted)
      ;; Load =library-of-babel.org=
      (org-babel-lob-ingest (concat my-project-root "setup/library-of-babel.org"))
      ;; =org-publish-project-alist= definition
      (setq my-publish-dir (concat my-project-root "docs"))
      
      (defun my-org-publish-sitemap (title list)
        "Create my own index.org instead of the default one"
        (concat
         "#+CALL: Setup()\n"
         "#+INCLUDE: \"setup/index_preamble.org\"\n"
         "#+OPTIONS: toc:nil\n\n"
         "* My Sitemap\n\n"
         (org-list-to-org list)
         "\n\n"))
      
      (setq org-publish-project-alist
            `(
      	("My_Project_Name-org-files",
      	 :base-directory ,my-project-root
      	 :base-extension "org"
      	 :recursive t
      	 :publishing-directory ,my-publish-dir
      	 :publishing-function org-html-publish-to-html
      	 :sitemap-function my-org-publish-sitemap
      	 :htmlize-source t
      	 :sitemap-sort-files anti-chronologically
      	 :exclude "setup/*\\|docs/*"
      	 ;; Generates theindex.org + inc files
      	 :makeindex t
      	 ;; Creates index.org, calls my-org-publish-sitemap to fill it
      	 :auto-sitemap t
      	 :sitemap-filename "index.org"
      	 )
      
      	("My_Project_Name-data-files",
      	 :base-directory ,my-project-root
      	 :base-extension "nb\\|?pp\\|png"
      	 :recursive t
      	 :publishing-directory ,my-publish-dir
      	 :publishing-function org-publish-attachment
      	 :exclude ".*bazel-.*"
      	 )
      
      	;; Main
      	("My_Project_Name",
      	 :components ("My_Project_Name-org-files" "My_Project_Name-data-files")
      	 )
      	)
            )
      
      (setq org-agenda-files
            (split-string
             (shell-command-to-string (format "cd %s; find -name '*.org' ! -name 'index.org'  ! -name 'agenda.org'  ! -name '.#*' ! -path './setup/*'" my-project-root))
             ))
      (setq my-www-links-filename (concat my-project-root "www_links.org"))
      (setq my-journal-filename (concat my-project-root "journal.org"))
      (setq my-todo-filename (concat my-project-root "todo.org"))
      
      (setq-default org-display-custom-times t)
      (setq org-time-stamp-custom-formats '("<%a %b %e %Y>" . "<%a %b %e %Y %H:%M>"))
      
      (setq org-capture-templates
            `(
      	("A"
      	 "Agenda/Meeting" entry (file+headline "~/GitLab/PVBibliography/agenda.org" "Agenda")
      	 "* %^{Title?} %^G\n:PROPERTIES:\n:Created: %U\n:END:\n\n%?"
      	 :empty-lines 1  
      	 :create t
      	 )
      
      	("K" "Log Time" entry (file+datetree "~/GitLab/PVBibliography/activity.org" "Activity")
      	 "* %U - %^{Activity}  :TIME:"
      	 )
      	
      
      	;;----------------
      	
      	("t"
      	 "Todo" entry (file+olp+datetree ,my-todo-filename)
      	 "* TODO %^{Title?} [/] %^G\n:PROPERTIES:\n:Created: %U\n:END:\n\n - [ ] %?"
      	 :empty-lines 1  
      	 :create t
      	 )
      	
      	("T"
      	 "Todo with file link" entry (file+olp+datetree ,my-todo-filename)
      	 "* TODO %^{Title|%f} [/] %^G\n:PROPERTIES:\n:Created: %U\n:END:\n\nBack link: %a\n\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n - [ ] %?"
      	 :empty-lines 1  
      	 :create t
      	 )
      	
      	("j" "Journal" entry (file+olp+datetree ,my-journal-filename)
      	 "* %^{Title} %^G\n\n%?"
      	 :empty-lines 1  
      	 :create t
      	 )
      	("J" 
      	 "Journal with file link" entry (file+olp+datetree ,my-journal-filename)
      	 "* %^{Title|%f} %^G\n\nBack link: %a\n\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n%?"
      	 :empty-lines 1  
      	 :create t
      	 )
      	;;
      	;; See: https://github.com/sprig/org-capture-extension for further details
      	;;
      	("L" 
      	 "Protocol Link" entry (file ,my-www-links-filename)
      	 "* [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]] \
      		%^G\n:PROPERTIES:\n:Created: %U\n:END:\n\n%?"
      	 :empty-lines 1  
      	 :create t
      	 )
      	
      	("p" 
      	 "Protocol" entry (file ,my-www-links-filename)
      	 "* [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]] \
      		 %^G\n:PROPERTIES:\n:Created: %U\n:END:\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n%?"
      	 :empty-lines 1  
      	 :create t
      	 )
      	))
      (setq my-bibtex-filename 
            (concat my-project-root "bibliography/bibliography.bib"))
      (if (file-exists-p my-bibtex-filename)
          ;; If bibliography.bib exists 
          (setq reftex-default-bibliography  `(,my-bibtex-filename)
      	  bibtex-completion-notes-extension "-notes.org"
      	  bibtex-completion-notes-template-multiple-files "#+CALL: Setup()\n#+TITLE: ${author-or-editor} (${year}): ${title}\n\n* Personal Notes\n  :PROPERTIES:\n  :NOTER_DOCUMENT: ~/AnnotatedPDF/${=key=}.pdf\n  :END:\n\n[[file:~/AnnotatedPDF/${=key=}.pdf][${title}]]\n"
      	  bibtex-completion-bibliography my-bibtex-filename
      	  bibtex-completion-library-path (file-name-directory my-bibtex-filename)
      	  bibtex-completion-notes-path (file-name-directory my-bibtex-filename)
      	  
      	  org-ref-default-bibliography  `(,my-bibtex-filename)
      	  org-ref-pdf-directory (file-name-directory my-bibtex-filename)
      	  )
        ;; otherwise unbound meaningless my-bibtex-filename
        (makunbound 'my-bibtex-filename)
        )
      ;; defines how to generate the pdf file using lualatex + biber
      (setq org-latex-pdf-process
            '("lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"
      	"biber %b"
      	"lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"
      	"lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
      )
  )
