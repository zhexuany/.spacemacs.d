(setq zhexuany-dired-packages
      '(
        (dired-mode :location built-in)
        ))
(defun zhexuany-dired/init-dired-mode ()
  (use-package dired-mode
    :init
    (progn
      (require 'dired-x)
      (require 'dired-aux)
      (setq dired-listing-switches "-alh")
      (setq dired-guess-shell-alist-user
            '(("\\.pdf\\'" "open")
              ("\\.docx\\'" "open")
              ("\\.\\(?:djvu\\|eps\\)\\'" "open")
              ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" "open")
              ("\\.\\(?:xcf\\)\\'" "open")
              ("\\.csv\\'" "open")
              ("\\.tex\\'" "open")
              ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|ogv\\)\\(?:\\.part\\)?\\'"
               "open")
              ("\\.\\(?:mp3\\|flac\\)\\'" "open")
              ("\\.html?\\'" "open")
              ("\\.md\\'" "open")))

      ;; always delete and copy recursively
      (setq dired-recursive-deletes 'always)
      (setq dired-recursive-copies 'always)

      (defvar dired-filelist-cmd
        '(("vlc" "-L")))

      (defun dired-get-size ()
        (interactive)
        (let ((files (dired-get-marked-files)))
          (with-temp-buffer
            (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
            (message
             "Size of all marked files: %s"
             (progn
               (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*total$")
               (match-string 1))))))

      (defun dired-start-process (cmd &optional file-list)
        (interactive
         (let ((files (dired-get-marked-files
                       t current-prefix-arg)))
           (list
            (dired-read-shell-command "& on %s: "
                                      current-prefix-arg files)
            files)))
        (let (list-switch)
          (start-process
           cmd nil shell-file-name
           shell-command-switch
           (format
            "nohup 1>/dev/null 2>/dev/null %s \"%s\""
            (if (and (> (length file-list) 1)
                     (setq list-switch
                           (cadr (assoc cmd dired-filelist-cmd))))
                (format "%s %s" cmd list-switch)
              cmd)
            (mapconcat #'expand-file-name file-list "\" \"")))))

      (defun dired-open-term ()
        "Open an `ansi-term' that corresponds to current directory."
        (interactive)
        (let* ((current-dir (dired-current-directory))
               (buffer (if (get-buffer "*zshell*")
                           (switch-to-buffer "*zshell*")
                         (ansi-term "/bin/zsh" "zshell")))
               (proc (get-buffer-process buffer)))
          (term-send-string
           proc
           (if (file-remote-p current-dir)
               (let ((v (tramp-dissect-file-name current-dir t)))
                 (format "ssh %s@%s\n"
                         (aref v 1) (aref v 2)))
             (format "cd '%s'\n" current-dir)))))

      (defun dired-copy-file-here (file)
        (interactive "fCopy file: ")
        (copy-file file default-directory))

      ;;dired find alternate file in other buffer
      (defun my-dired-find-file ()
        "Open buffer in another window"
        (interactive)
        (let ((filename (dired-get-filename nil t)))
          (if (car (file-attributes filename))
              (dired-find-alternate-file)
            (dired-find-file-other-window))))

      ;; do command on all marked file in dired mode
      (defun zhexuany-dired/dired-do-command (command)
        "Run COMMAND on marked files. Any files not already open will be opened.
After this command has been run, any buffers it's modified will remain
open and unsaved."
        (interactive "CRun on marked files M-x ")
        (save-window-excursion
          (mapc (lambda (filename)
                  (find-file filename)
                  (call-interactively command))
                (dired-get-marked-files))))

      (defun zhexuany-dired/dired-up-directory()
        "goto up directory and resue buffer"
        (interactive)
        (find-alternate-file ".."))

      (evilified-state-evilify-map dired-mode-map
        :mode dired-mode
        :bindings
        (kbd "C-k") 'zhexuany-dired/dired-up-directory
        "<RET>" 'dired-find-alternate-file
        "E" 'dired-toggle-read-only
        "C" 'dired-do-copy
        "<mouse-2>" 'my-dired-find-file
        "`" 'dired-open-term
        "z" 'dired-get-size
        "c" 'dired-copy-file-here)
      )
    :defer t
    )
  )
