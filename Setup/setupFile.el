;; [[file:~/GitHub/Project-Template-For-OrgMode/doc/InternalDoc.org::#+BEGIN_SRC%20emacs-lisp%20:tangle%20yes%20:tangle%20"../Setup/setupFile.el"%20(if%20my-project-root%20;;%20Configuration%20(progn%20(message%20"Configuring%20%25s%20"%20my-project-root)%20;;%20;;%20Local%20bibliography%20;;%20(setq%20my-bibtex-filename%20(concat%20my-project-root%20"Bibliography/bibliography.bib"))%20(if%20(file-exists-p%20my-bibtex-filename)%20;;%20If%20bibliography.bib%20exists%20(setq%20reftex-default-bibliography%20`(,my-bibtex-filename)%20bibtex-completion-bibliography%20my-bibtex-filename%20bibtex-completion-library-path%20(file-name-directory%20my-bibtex-filename)%20bibtex-completion-notes-path%20(file-name-directory%20my-bibtex-filename)%20org-ref-default-bibliography%20`(,my-bibtex-filename)%20org-ref-pdf-directory%20(file-name-directory%20my-bibtex-filename)%20)%20;;%20otherwise%20unbound%20meaningless%20my-bibtex-filename%20(makunbound%20'my-bibtex-filename)%20)%20;;%20;;%20Agenda%20files%20;;%20(setq%20org-agenda-files%20(mapcar%20'abbreviate-file-name%20(split-string%20(shell-command-to-string%20(format%20"find%20%25s%20-name%20\"*.org\""%20my-project-root))%20"\n")))%20;;%20;;%20My%20my-workInProgress-filename%20and%20its%20associated%20captures%20;;%20(setq%20my-workInProgress-filename%20(concat%20my-project-root%20"workInProgress.org"))%20(when%20(file-exists-p%20my-workInProgress-filename)%20(setq%20org-capture-templates%20`(%20("t"%20"Todo"%20entry%20(file+headline%20,my-workInProgress-filename%20"Project%20TODO")%20"*%20TODO%20%25^{Title?}%20%5B/%5D%20%25^G\n:PROPERTIES:\n:Created:%20%25U\n:END:\n\n%20-%20%5B%20%5D%20%25?"%20:empty-lines%201%20:create%20t%20)%20("T"%20"Todo%20with%20file%20link"%20entry%20(file+headline%20,my-workInProgress-filename%20"Project%20TODO")%20"*%20TODO%20%25^{Title|%25f}%20%5B/%5D%20%25^G\n:PROPERTIES:\n:Created:%20\%20%25U\n:END:\n\n%5B%5B%25l%5D%5BIn%20file%20%25f%5D%5D:\n\n#+BEGIN_EXAMPLE\n%25i\n#+END_EXAMPLE\n\n%20-%20%5B%20%5D%20%25?"%20:empty-lines%201%20:create%20t%20)%20("j"%20"Journal"%20entry%20(file+olp+datetree%20,my-workInProgress-filename%20"Project%20Journal")%20"*%20%25^{Title}%20%25^G\n\n%25?"%20:empty-lines%201%20:create%20t%20)%20("J"%20"Journal%20with%20file%20link"%20entry%20(file+olp+datetree%20,my-workInProgress-filename%20"Project%20journal")%20"*%20%25^{Title|%25f}%20%25^G\n\n%5B%5B%25l%5D%5BIn%20file%20%25f%5D%5D:\n\n#+BEGIN_EXAMPLE\n%25i\n#+END_EXAMPLE\n\n%25?"%20:empty-lines%201%20:create%20t%20)%20;;%20CAVEAT:%20emacs%20must%20run%20in%20server%20mode%20for%20org-protocol%20;;%20One%20must%20use%20this%20function:%20;;%20;;%20(defun%20transform-square-brackets-to-round-ones(string-to-transform)%20;;%20"Transforms%20%5B%20into%20(%20and%20%5D%20into%20),%20other%20chars%20left%20unchanged."%20;;%20(concat%20;;%20(mapcar%20#'(lambda%20(c)%20(if%20(equal%20c%20?%5B)%20?\(%20(if%20(equal%20c%20?%5D)%20?\)%20c)))%20string-to-transform))%20;;%20)%20;;%20;;%20See:%20https://github.com/sprig/org-capture-extension#set-up-handlers-in-emacs%20;;%20for%20further%20details%20;;%20("L"%20"Protocol%20Link"%20entry%20(file+headline%20,my-workInProgress-filename%20"W3%20Links")%20"*%20%5B%5B%25:link%5D%5B%25(transform-square-brackets-to-round-ones%20\"%25:description\")%5D%5D%20\%20%25^G\n:PROPERTIES:\n:Created:%20%25U\n:END:\n\n%25?"%20:empty-lines%201%20:create%20t%20)%20("p"%20"Protocol"%20entry%20(file+headline%20,my-workInProgress-filename%20"W3%20Links")%20"*%20%5B%5B%25:link%5D%5B%25(transform-square-brackets-to-round-ones%20\"%25:description\")%5D%5D%20\%20%25^G\n:PROPERTIES:\n:Created:%20%25U\n:END:\n#+BEGIN_EXAMPLE\n%25i\n#+END_EXAMPLE\n\n%25?"%20:empty-lines%201%20:create%20t%20)%20)))%20;;%20;;%20How%20to%20publish%20;;%20;;%20Note:%20you%20can%20publish%20somewhere%20else,%20for%20instance:%20;;%20;;%20(setq%20my-publish-dir%20"~/Temp/Publish")%20;;%20;;%20by%20default%20we%20publish%20in-place%20;;%20(advantage:%20C-c%20C-e%20h%20h%20directly%20update%20the%20published%20page)%20(setq%20my-publish-dir%20my-project-root)%20(setq%20my-project-name%20"My_Project_Name")%20(setq%20org-publish-project-alist%20`(%20(,(concat%20my-project-name%20"_Org")%20:base-directory%20,my-project-root%20:base-extension%20"org"%20:recursive%20t%20:publishing-directory%20,my-publish-dir%20:publishing-function%20org-html-publish-to-html%20:htmlize-source%20t%20:org-html-head-include-default-style%20nil%20:exclude%20"Setup*\\|sitemap.org"%20:makeindex%20t%20:auto-sitemap%20t%20:sitemap-title%20,my-project-name%20)%20;;%20("PPack_Tangle"%20;;%20:base-directory%20,my-project-root%20;;%20:base-extension%20"org"%20;;%20:recursive%20t%20;;%20:publishing-directory%20,my-publish-dir%20;;%20:publishing-function%20org-babel-tangle-publish%20;;%20:exclude%20".*bazel-.*"%20;;%20)][Default =setupFile.el= file:1]]
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

      (setq my-project-name "My_Project_Name")

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
;; Default =setupFile.el= file:1 ends here
