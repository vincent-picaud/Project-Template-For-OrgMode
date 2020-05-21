(if my-project-root
    ;; Configuration
    (progn
      (setq org-agenda-files
            (mapcar 'abbreviate-file-name
      	      (split-string
      	       (shell-command-to-string (format "find %s -name \"*.org\" ! -name \"index.org\"  ! -name \"agenda.org\"  ! -path \"./setup/*\" ! -path \"./docs/*\"" my-project-root))
      	       "\n")))
      )
  )
