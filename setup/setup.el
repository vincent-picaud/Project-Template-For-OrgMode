(if my-project-root
    ;; Configuration
    (progn
      (message (format "Configuring %s" my-project-root))
      (setq org-agenda-files
            (split-string
             (shell-command-to-string (format "cd %s; find -name '*.org' ! -name 'index.org'  ! -name 'agenda.org'  ! -name '.#*' ! -path './setup/*'" my-project-root))
             ))
      )
  )
