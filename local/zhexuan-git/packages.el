(setq zhexuan-git-packages
      '(
        magit
        git-messenger
        ))
(defun zhexuan-git/post-init-magit ()
  (use-package magit
    :defer t
    :config
    (progn
      (add-to-list 'magit-no-confirm 'stage-all-changes)
      (define-key magit-log-mode-map (kbd "W") 'magit-copy-as-kill)
      (define-key magit-status-mode-map (kbd "s-1") 'magit-jump-to-unstaged)
      (define-key magit-status-mode-map (kbd "s-2") 'magit-jump-to-untracked)
      (define-key magit-status-mode-map (kbd "s-3") 'magit-jump-to-staged)
      (define-key magit-status-mode-map (kbd "s-4") 'magit-jump-to-stashes)

      ;; (add-hook 'magit-section-set-visibility-hook '(lambda (section) (let ((section-type (magit-section-type section)))
      ;;                                                              (if (or (eq 'untracked section-type)
      ;;                                                                      (eq 'stashes section-type))
      ;;                                                                  'hide))))
      )

    :init
    (progn
      ;; Githu PR settings
      ;; "http://endlessparentheses.com/create-github-prs-from-emacs-with-magit.html"
      (setq magit-repository-directories '("~/cocos2d-x/"))
      (setq magit-push-always-verify nil)

      (defun endless/visit-pull-request-url ()
        "Visit the current branch's PR on Github."
        (interactive)
        (browse-url
         (format "https://github.com/%s/pull/new/%s"
                 (replace-regexp-in-string
                  "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
                  (magit-get "remote"
                             (magit-get-remote)
                             "url"))
                 (cdr (magit-get-remote-branch)))))


      (eval-after-load 'magit
        '(define-key magit-mode-map (kbd "s-g")
           #'endless/visit-pull-request-url))


      (defadvice magit-blame-mode (after magit-blame-change-to-emacs-state activate compile)
        "when entering magit blame mode, change evil normal state to emacs state"
        (if (evil-normal-state-p)
            (evil-emacs-state)
          (evil-normal-state)))

      (ad-activate 'magit-blame-mode)

      (defadvice git-timemachine-mode (after git-timemachine-change-to-emacs-state activate compile)
        "when entering git-timemachine mode, change evil normal state to emacs state"
        (if (evil-normal-state-p)
            (evil-emacs-state)
          (evil-normal-state)))

      (ad-activate 'git-timemachine-mode)

      (setq magit-process-popup-time 10))))

(defun zhexuan-git/post-init-git-messenger ()
  (use-package git-messenger
    :defer t
    :config
    (progn
      (defun my-vc-visit-file-revision (file rev)
        "Visit revision REV of FILE in another window.
With prefix argument, uses the current window instead.
If the current file is named `F', the revision is named `F.~REV~'.
If `F.~REV~' already exists, use it instead of checking it out again."
        ;; based on `vc-revision-other-window'.
        (interactive
         (let ((file (expand-file-name
                      (read-file-name
                       (if (buffer-file-name)
                           (format "File (%s): " (file-name-nondirectory
                                                  (buffer-file-name)))
                         "File: ")))))
           (require 'vc)
           (unless (vc-backend file)
             (error "File %s is not under version control" file))
           (list file (vc-read-revision
                       "Revision to visit (default is working revision): "
                       (list file)))))
        (require 'vc)
        (unless (vc-backend file)
          (error "File %s is not under version control" file))
        (let ((revision (if (string-equal rev "")
                            (vc-working-revision file)
                          rev))
              (visit (if current-prefix-arg
                         'switch-to-buffer
                       'switch-to-buffer-other-window)))
          (funcall visit (vc-find-revision file revision))))

      (define-key git-messenger-map (kbd "f") 'my-vc-visit-file-revision))))
