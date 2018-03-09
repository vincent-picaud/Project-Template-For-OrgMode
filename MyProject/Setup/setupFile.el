(if my-project-root
    ;; Configuration
    (progn
      (message "Configuring %s " my-project-root)
      ;;
      ;; PDF Export config 
      ;;
      (setq org-image-actual-width (/ (display-pixel-width) 4))

      ;; uses the minted package instead of the listings one
      (setq org-latex-listings 'minted)
      
      ;; defines how to generate the pdf file using lualatex + biber
      (setq org-latex-pdf-process
      '("lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"
      "biber %b"
      "lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"
      "lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
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
	(setq org-capture-templates
	      `(
		("t"
		 "Todo" entry (file+headline ,my-workInProgress-filename "Project TODO")
		 "* TODO %^{Title?} [/] %^G\n:PROPERTIES:\n:Created: %U\n:END:\n\n - [ ] %?"
		 :empty-lines 1  
		 :create t
		)
		
		("T"
		 "Todo with file link" entry (file+headline ,my-workInProgress-filename "Project TODO")
		 "* TODO %^{Title|%f} [/] %^G\n:PROPERTIES:\n:Created: \
		 %U\n:END:\n\n[[%l][In file %f]]:\n\n#+BEGIN_EXAMPLE\n%i\n#+END_EXAMPLE\n\n - [ ] %?"
		 :empty-lines 1  
		 :create t
		)
		
		("j" "Journal" entry (file+olp+datetree ,my-workInProgress-filename "Project Journal")
		 "* %^{Title} %^G\n\n%?"
		 :empty-lines 1  
		 :create t
		)

		("J" 
		"Journal with file link" entry (file+olp+datetree ,my-workInProgress-filename "Project journal")
		 "* %^{Title|%f} %^G\n\n[[%l][In file %f]]:\n\n#+BEGIN_EXAMPLE\n%i\n#+END_EXAMPLE\n\n%?"
		 :empty-lines 1  
		 :create t
		)
		;; CAVEAT: emacs must run in server mode for org-protocol
		;; One must use this function:
		;;
		;; (defun transform-square-brackets-to-round-ones(string-to-transform)
		;;   "Transforms [ into ( and ] into ), other chars left unchanged."
		;;   (concat 
		;;   (mapcar #'(lambda (c) (if (equal c ?[) ?\( (if (equal c ?]) ?\) c))) string-to-transform))
		;;   )
		;;
		;; See: https://github.com/sprig/org-capture-extension#set-up-handlers-in-emacs
		;; for further details
		;;
		("L" 
		"Protocol Link" entry (file+headline ,my-workInProgress-filename "W3 Links")
		"* [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]] \
		%^G\n:PROPERTIES:\n:Created: %U\n:END:\n\n%?"
		:empty-lines 1  
		:create t
		)
		
		("p" 
		"Protocol" entry (file+headline ,my-workInProgress-filename "W3 Links")
		 "* [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]] \
		 %^G\n:PROPERTIES:\n:Created: %U\n:END:\n#+BEGIN_EXAMPLE\n%i\n#+END_EXAMPLE\n\n%?"
		 :empty-lines 1  
		 :create t
		)
		)))
      ;;
      ;; How to publish
      ;;
      ;; Note: you can publish somewhere else, for instance:
      ;;
      ;; (setq my-publish-dir "~/Temp/Publish")
      ;;
      ;; by default we publish in-place 
      ;; (advantage: C-c C-e h h directly update the published page)
      (setq my-publish-dir my-project-root)

      (setq my-project-name "MyProject")

      (setq org-publish-project-alist
	    `(
	      (,(concat my-project-name "_Org")
	       :base-directory ,my-project-root
	       :base-extension "org"
	       :recursive t
	       :publishing-directory ,my-publish-dir
	       :publishing-function org-html-publish-to-html
	       :htmlize-source t
	       :org-html-head-include-default-style nil
	       :exclude "Setup*\\|sitemap.org"
	       :makeindex t
	       :auto-sitemap t
	       :sitemap-title ,my-project-name 
	      )

	      ;; ("PPack_Tangle"
	      ;;  :base-directory ,my-project-root
	      ;;  :base-extension "org"
	      ;;  :recursive t
	      ;;  :publishing-directory ,my-publish-dir
	      ;;  :publishing-function org-babel-tangle-publish
	      ;;  :exclude ".*bazel-.*"
	      ;;  )

	      (,(concat my-project-name "_Data")
	       :base-directory ,my-project-root
	       :base-extension "nb\\|?pp\\|png"
	       :recursive t
	       :publishing-directory ,my-publish-dir
	       :publishing-function org-publish-attachment
	       :exclude ".*bazel-.*"
	      )

	      ;; Main
	      (,my-project-name
	       :components (,(concat my-project-name "_Org")
			    ;;"DefaultProject_Tangle"
			    ,(concat my-project-name "_Data"))
	      )
	      )
	)
      ) ; progn
  ;; else
  (error "Project root undefined")
    )
