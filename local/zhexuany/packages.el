;;; packages.el --- zhexuany Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2016 zhexuany 
;;
;; Author: zhexuany
;; URL: https://github.com/zhexuany/
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq zhexuany-packages
      '(
        ;; package names go here
        ;; lispy
        ;; company
        ;; discover-my-major
        ;; cmake-font-lock
        ;; cmake-mode
        ;; company-c-headers
        ;; impatient-mode
        ;; helm-github-stars
        ;; elfeed
        swiper
        ;; magit
        ;; ox-reveal
        ;; org-mac-link
        ;; org
        ;; git-messenger
        ;; helm-flyspell
        ;; helm
        ;; ace-window
        ;; helm-ls-git
        ;; mwe-log-commands
        ;; flycheck-package
        ;; deft
        hungry-delete
        ;; multiple-cursors
        ;; persp-mode
        ;; ;; hydra
        ;; emacs-eclim
        ))

;; List of packages to exclude.
(setq zhexuany-excluded-packages '())

;; make java mode defaultly open eclimd
(defun zhexuany/post-init-emacs-eclim ()
  (use-package eclimd
    :defer t
    :diminish eclim-mode
    :init (add-hook 'java-mode-hook 'eclim-mode)
    )
  )

(defun zhexuany/post-init-hungry-delete ()
  (add-hook 'prog-mode-hook 'hungry-delete-mode)
  )

;; snippets get from https://github.com/AndreaCrotti/yasnippet-snippets
(defun zhexuany/post-init-yasnippet ()
  (progn
    (setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))
    (mapc #'(lambda (hook) (remove-hook hook 'spacemacs/load-yasnippet))
          '(prog-mode-hook
            org-mode-hook
            markdown-mode-hook))
    (defun zhexuany/load-yasnippet ()
      (unless yas-global-mode
        (progn
          (yas-global-mode 1)
          (setq my-snippet-dir (expand-file-name "~/.emacs.d/private/snippets"))
          (setq yas-snippet-dirs  my-snippet-dir)
          (yas-load-directory my-snippet-dir)
          (setq yas-wrap-around-region t)))
      (yas-minor-mode 1))

    (spacemacs/add-to-hooks 'zhexuany/load-yasnippet '(prog-mode-hook
                                                            markdown-mode-hook
                                                            org-mode-hook))
    ))

(defun zhexuany/init-hydra ()
  (use-package hydra
    :init
    (progn
      (when (configuration-layer/package-usedp 'org)
        ;; major mode hydra is really cool, don't need to switch mode anymore
        ;; C-c [a-z] and s-[a-z] is very quick to pressed even in emacs-state and F1-F9 is also the same
        ;; If the command will change the buffer, they should be put in these groups.
        ;; otherwise, use which-key + spacems + user defined key mappings in evil normal mode
        (defhydra hydra-org (:color blue :hint nil)
          "
              ^Org Mode^
--------------------------------------------
          _t_ags   _p_riority
          "
          ("p" org-priority)
          ("t" org-set-tags))
        (require 'org)
        (define-key org-mode-map (kbd "<f2>") 'hydra-org/body)
        )
      (defhydra hydra-yasnippet (:color blue :hint nil)
        "
              ^YASnippets^
--------------------------------------------
  Modes:    Load/Visit:    Actions:

 _g_lobal  _d_irectory    _i_nsert
 _m_inor   _f_ile         _t_ryout
 _e_xtra   _l_ist         _n_ew
         _a_ll
"
        ("d" yas-load-directory)
        ("e" yas-activate-extra-mode)
        ("i" yas-insert-snippet)
        ("f" yas-visit-snippet-file :color blue)
        ("n" yas-new-snippet)
        ("t" yas-tryout-snippet)
        ("l" yas-describe-tables)
        ("g" yas/global-mode)
        ("m" yas/minor-mode)
        ("a" yas-reload-all))

      (bind-key* "<f3>" 'hydra-yasnippet/body)

      (defhydra hydra-apropos (:color blue)
        "Apropos"
        ("a" apropos "apropos")
        ("c" apropos-command "cmd")
        ("d" apropos-documentation "doc")
        ("e" apropos-value "val")
        ("l" apropos-library "lib")
        ("o" apropos-user-option "option")
        ("u" apropos-user-option "option")
        ("v" apropos-variable "var")
        ("i" info-apropos "info")
        ("t" tags-apropos "tags")
        ("z" hydra-customize-apropos/body "customize"))

      (defhydra hydra-customize-apropos (:color blue)
        "Apropos (customize)"
        ("a" customize-apropos "apropos")
        ("f" customize-apropos-faces "faces")
        ("g" customize-apropos-groups "groups")
        ("o" customize-apropos-options "options"))

      (bind-key*  "<f4>" 'hydra-apropos/body)
      )))

(defun zhexuany/post-init-multiple-cursors ()
  (use-package multiple-cursors
    :init
    (progn
      (bind-key* "C-s-f" 'mc/edit-lines)
      (bind-key* "C-s-." 'mc/mark-next-like-this)
      (bind-key* "C-s-," 'mc/mark-previous-like-this)
      (bind-key* "C-c C-s-." 'mc/mark-all-like-this))))



(defun zhexuany/post-init-cc-mode ()
  ;; company backend should be grouped
  (setq company-backends-c-mode-common '((company-c-headers
                                          company-dabbrev-code
                                          company-keywords
                                          company-etags
                                          company-gtags :with company-yasnippet)
                                         company-files company-dabbrev )))

(defun zhexuany/post-init-company-c-headers ()
  (use-package company-c-headers
    :defer t
    :init (progn
            (setq company-c-headers-path-system
                  (quote
                   ("/usr/include/" "/usr/local/include/" "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1")
                 )))))

(defun zhexuany/init-discover-my-major ()
  (use-package discover-my-major
    :defer t
    :init
    (progn
      (evil-leader/set-key (kbd "mhm") 'discover-my-major)
      (evilify makey-key-mode makey-key-mode-get-key-map)
      )))

(defun zhexuany/init-lispy ()
  "Initialize lispy"
  (use-package lispy
    :defer t
    :diminish (lispy-mode)
    :config
    (progn
      (define-key lispy-mode-map (kbd "s-1") 'lispy-describe-inline)
      (define-key lispy-mode-map (kbd "s-2") 'lispy-arglist-inline))
    :init
    (progn
      ;; (define-key evil-insert-state-map (kbd "C-y") 'lispy-yank)
      ;; (define-key evil-insert-state-map (kbd "C-d") 'lispy-delete)
      (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'spacemacs-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'clojure-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'scheme-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'racket-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'cider-repl-mode-hook (lambda () (lispy-mode 1))))))


(defun zhexuany/post-init-company ()
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.4)
  (global-set-key (kbd "C-.") 'company-complete)
  (when (configuration-layer/package-usedp 'company)
  ))


(defun zhexuany/init-cmake-font-lock ()
  (use-package cmake-font-lock
    :defer t))


(defun zhexuany/post-init-cmake-mode ()
  (use-package cmake-mode
    :defer
    :config
    (progn
      (defun cmake-rename-buffer ()
        "Renames a CMakeLists.txt buffer to cmake-<directory name>."
        (interactive)
        (when (and (buffer-file-name)
                   (string-match "CMakeLists.txt" (buffer-name)))
          (setq parent-dir (file-name-nondirectory
                            (directory-file-name
                             (file-name-directory (buffer-file-name)))))
          (setq new-buffer-name (concat "cmake-" parent-dir))
          (rename-buffer new-buffer-name t)))

      (add-hook 'cmake-mode-hook (function cmake-rename-buffer)))))


(defun zhexuany/post-init-flycheck ()
  (use-package flycheck
    :defer t
    :config (progn
              (flycheck-package-setup)
              (setq flycheck-display-errors-function 'flycheck-display-error-messages)
              (setq flycheck-display-errors-delay 0.2))))


;; configs for writing
(defun zhexuany/post-init-markdown-mode ()
  (use-package markdown-mode
    :defer t
    :config
    (progn
      (when (configuration-layer/package-usedp 'company)
        ;; (spacemacs|add-company-hook markdown-mode)
        )
      (defun zhexuany/markdown-to-html ()
        (interactive)
        (start-process "grip" "*gfm-to-html*" "grip" (buffer-file-name))
        (browse-url (format "http://localhost:5000/%s.%s" (file-name-base) (file-name-extension (buffer-file-name)))))

      (evil-leader/set-key-for-mode 'gfm-mode-map
        "mp" 'zhexuany/markdown-to-html)
      (evil-leader/set-key-for-mode 'markdown-mode
        "mp" 'zhexuany/markdown-to-html)
      )))


(defun zhexuany/init-impatient-mode ()
  "Initialize impatient mode"
  (use-package impatient-mode
    :init
    (progn

      (defun zhexuany-mode-hook ()
        "my web mode hook for HTML REPL"
        (interactive)
        (impatient-mode)
        (httpd-start))
      (add-hook 'web-mode-hook 'zhexuany-mode-hook))))


(defun zhexuany/init-helm-github-stars ()
  (use-package helm-github-stars
    :defer t
    :config
    (progn
      (setq helm-github-stars-username "andyque")
      (setq helm-github-stars-cache-file "~/.emacs.d/.cache/hgs-cache"))))




(defun zhexuany/init-mwe-log-commands ()
  (use-package mwe-log-commands
    :init
    (progn
      (evil-leader/set-key
        "oll" 'mwe:log-keyboard-commands
        "olf" 'mwe:open-command-log-buffer))))

(defun zhexuany/init-swiper ()
  "Initialize my package"
  (use-package swiper
    :init
    (progn
      (setq ivy-display-style 'fancy)

      ;; http://oremacs.com/2015/04/16/ivy-mode/
      ;; (ivy-mode -1)
      ;; (setq magit-completing-read-function 'ivy-completing-read)

      ;; http://oremacs.com/2015/04/19/git-grep-ivy/
      (defun counsel-git-grep-function (string &optional _pred &rest _u)
        "Grep in the current git repository for STRING."
        (split-string
         (shell-command-to-string
          (format
           "git --no-pager grep --full-name -n --no-color -i -e \"%s\""
           string))
         "\n"
         t))

      (defun counsel-git-grep ()
        "Grep for a string in the current git repository."
        (interactive)
        (let ((default-directory (locate-dominating-file
                                  default-directory ".git"))
              (val (ivy-read "pattern: " 'counsel-git-grep-function))
              lst)
          (when val
            (setq lst (split-string val ":"))
            (find-file (car lst))
            (goto-char (point-min))
            (forward-line (1- (string-to-number (cadr lst)))))))
      (use-package ivy
        :defer t
        :config
        (progn
          (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
          (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)))

      (define-key global-map (kbd "C-s") 'swiper)
      (setq ivy-use-virtual-buffers t)
      (global-set-key (kbd "C-c C-r") 'ivy-resume)
      (global-set-key (kbd "C-c j") 'counsel-git-grep))))


(defun zhexuany/post-init-magit ()
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

(defun zhexuany/post-init-git-messenger ()
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

(defun zhexuany/post-init-helm-flyspell ()
  (use-package helm-flyspell
    :commands helm-flyspell-correct
    :init
    ;; "http://emacs.stackexchange.com/questions/14909/how-to-use-flyspell-to-efficiently-correct-previous-word/14912#14912"
    (defun zhexuany/flyspell-goto-previous-error (arg)
      "Go to arg previous spelling error."
      (interactive "p")
      (while (not (= 0 arg))
        (let ((pos (point))
              (min (point-min)))
          (if (and (eq (current-buffer) flyspell-old-buffer-error)
                   (eq pos flyspell-old-pos-error))
              (progn
                (if (= flyspell-old-pos-error min)
                    ;; goto beginning of buffer
                    (progn
                      (message "Restarting from end of buffer")
                      (goto-char (point-max)))
                  (backward-word 1))
                (setq pos (point))))
          ;; seek the next error
          (while (and (> pos min)
                      (let ((ovs (overlays-at pos))
                            (r '()))
                        (while (and (not r) (consp ovs))
                          (if (flyspell-overlay-p (car ovs))
                              (setq r t)
                            (setq ovs (cdr ovs))))
                        (not r)))
            (backward-word 1)
            (setq pos (point)))
          ;; save the current location for next invocation
          (setq arg (1- arg))
          (setq flyspell-old-pos-error pos)
          (setq flyspell-old-buffer-error (current-buffer))
          (goto-char pos)
          (call-interactively 'helm-flyspell-correct)
          (if (= pos min)
              (progn
                (message "No more miss-spelled word!")
                (setq arg 0))))))

    (bind-key* "C-," 'zhexuany/flyspell-goto-previous-error)
    (global-set-key (kbd "C-c s") 'helm-flyspell-correct)))

(defun zhexuany/post-init-helm ()
  (use-package helm
    :init
    (progn
      ;; See https://github.com/bbatsov/prelude/pull/670 for a detailed
      ;; discussion of these options.
      (setq helm-split-window-in-side-p t
            helm-move-to-line-cycle-in-source t
            helm-ff-search-library-in-sexp t
            helm-ff-file-name-history-use-recentf t)

      (setq helm-completing-read-handlers-alist
            '((describe-function . ido)
              (describe-variable . ido)
              (debug-on-entry . helm-completing-read-symbols)
              (find-function . helm-completing-read-symbols)
              (find-tag . helm-completing-read-with-cands-in-buffer)
              (ffap-alternate-file . nil)
              (tmm-menubar . nil)
              (dired-do-copy . nil)
              (dired-do-rename . nil)
              (dired-create-directory . nil)
              (find-file . ido)
              (copy-file-and-rename-buffer . nil)
              (rename-file-and-buffer . nil)
              (w3m-goto-url . nil)
              (ido-find-file . nil)
              (ido-edit-input . nil)
              (mml-attach-file . ido)
              (read-file-name . nil)
              (yas/compile-directory . ido)
              (execute-extended-command . ido)
              (minibuffer-completion-help . nil)
              (minibuffer-complete . nil)
              (c-set-offset . nil)
              (wg-load . ido)
              (rgrep . nil)
              (read-directory-name . ido))))))



(defun zhexuany/post-init-ace-window ()
  (use-package ace-window
    :defer t
    :init
    (global-set-key (kbd "C-x C-o") #'ace-window)))


(defun zhexuany/init-helm-ls-git ()
  (use-package helm-ls-git
    :defer t
    :init
    :config
    (setq helm-ls-git-show-abs-or-relative 'relative)))


(defun zhexuany/init-ox-reveal ()
  (use-package ox-reveal
    :defer t
    :init
    (progn
      (setq org-reveal-root "file:///Users/zhexuany/.emacs.d/reveal-js"))))

(defun zhexuany/init-worf ()
  (use-package worf
    :defer t
    :init
    (add-hook 'org-mode-hook 'worf-mode)))

(defun zhexuany/init-org-download ()
  (use-package org-download
    :defer t
    :init
    (org-download-enable)))

;;In order to export pdf to support Chinese, I should install Latex at here: https://www.tug.org/mactex/
;; http://freizl.github.io/posts/2012-04-06-export-orgmode-file-in-Chinese.html
;;http://stackoverflow.com/questions/21005885/export-org-mode-code-block-and-result-with-different-styles


(defun zhexuany/post-init-deft ()
  (setq deft-use-filter-string-for-filename t)
  (evil-leader/set-key-for-mode 'deft-mode "mq" 'quit-window)
  (setq deft-extension "org")
  (setq deft-directory "~/Dropbox/ORG"))



(defun zhexuany/post-init-prodigy ()
  (prodigy-define-tag
    :name 'jekyll
    :env '(("LANG" "en_US.UTF-8")
           ("LC_ALL" "en_US.UTF-8")))
  ;; define service
  (prodigy-define-service
    :name "Preview Cocos2D-HTML5"
    :command "python"
    :args '("-m" "SimpleHTTPServer" "6001")
    ;; :cwd "~/cocos2d-x/web"
    :tags '(work)
    :kill-signal 'sigkill
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "Hexo Server"
    :command "hexo"
    :args '("server")
    :cwd "~/4gamers.cn"
    :tags '(hexo server)
    :kill-signal 'sigkill
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "Hexo Deploy"
    :command "hexo"
    :args '("deploy" "--generate")
    :cwd "~/4gamers.cn"
    :tags '(hexo deploy)
    :kill-signal 'sigkill
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "Org wiki preview"
    :command "python"
    :args '("-m" "SimpleHTTPServer" "8088")
    :cwd "~/Dropbox/ORG/public_html"
    :tags '(org-mode)
    :init (lambda () (browse-url "http://localhost:8088"))
    :kill-signal 'sigkill
    :kill-process-buffer-on-stop t))




(defun zhexuany/post-init-popwin ()
  (push "*zhexuany/run-current-file output*" popwin:special-display-config))



;; Thanks for zilongshanren. For org mode, almost code was copied from him.
;;In order to export pdf to support Chinese, I should install Latex at here: https://www.tug.org/mactex/
;; http://freizl.github.io/posts/2012-04-06-export-orgmode-file-in-Chinese.html
;;http://stackoverflow.com/questions/21005885/export-org-mode-code-block-and-result-with-different-styles
(defun zhexuany/post-init-org ()
  (progn
    (spacemacs|disable-company org-mode)
    ;; (when (configuration-layer/package-usedp 'company)
    ;; (spacemacs|add-company-hook org-mode))

    (require 'org-compat)
    (require 'org)
    ;; (add-to-list 'org-modules "org-habit")
    (add-to-list 'org-modules 'org-habit)
    (require 'org-habit)

    ;; define the refile targets
    (setq org-agenda-files (quote ("~/Dropbox/ORG" )))
    (setq org-refile-use-outline-path 'file)
    (setq org-outline-path-complete-in-steps nil)
    (setq org-refile-targets
          '((nil :maxlevel . 4)
            (org-agenda-files :maxlevel . 4)))
    ;; config stuck project
    (setq org-stuck-projects
          '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:"))

    (setq org-agenda-inhibit-startup t)       ;; ~50x speedup
    (setq org-agenda-use-tag-inheritance nil) ;; 3-4x speedup
    (setq org-agenda-window-setup 'current-window)
    (setq org-log-done t)

    (add-to-list 'auto-mode-alist '("\\.org\\’" . org-mode))

    (setq org-mobile-directory "~/Dropbox/ORG")


    (setq org-todo-keywords
          (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
                  (sequence "WAITING(w@/!)" "SOMEDAY(S)"  "|" "CANCELLED(c@/!)" "MEETING(m)" "PHONE(p)"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Org clock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; Change task state to STARTED when clocking in
    (setq org-clock-in-switch-to-state "STARTED")
    ;; Save clock data and notes in the LOGBOOK drawer
    (setq org-clock-into-drawer t)
    ;; Removes clocked tasks with 0:00 duration
    (setq org-clock-out-remove-zero-time-clocks t) ;; Show the clocked-in task - if any - in the header line


    (setq org-default-notes-file "~/Dropbox/ORG/gtd.org")

    ;; the %i would copy the selected text into the template
    ;;http://www.howardism.org/Technical/Emacs/journaling-org.html
    ;;add multi-file journal
    (setq org-capture-templates
          '(("t" "Todo" entry (file+headline "~/Dropbox/ORG/gtd.org" "Daily Tasks")
             "* TODO %?\n  %i\n"
             :empty-lines 1)
            ("n" "notes" entry (file+headline "~/Dropbox/ORG/notes.org" "Quick notes")
             "* TODO %?\n  %i\n %U"
             :empty-lines 1)
            ("b" "Blog Ideas" entry (file+headline "~/Dropbox/ORG/notes.org" "Blog Ideas")
             "* TODO %?\n  %i\n %U"
             :empty-lines 1)
            ("w" "work" entry (file+headline "~/Dropbox/ORG/gtd.org" "Cocos2D-X")
             "* TODO %?\n  %i\n %U"
             :empty-lines 1)
            ("j" "Journal Entry"
             entry (file+datetree "~/Dropbox/ORG/journal.org")
             "* %?"
             :empty-lines 1)))

    (setq org-tags-match-list-sublevels nil)
    (setq org-agenda-custom-commands
          '(
            ("w" . "任务安排")
            ("wa" "重要且紧急的任务" tags-todo "+PRIORITY=\"A\"")
            ("wb" "重要且不紧急的任务" tags-todo "+PRIORITY=\"B\"")
            ("b" "Blog" tags-todo "BLOG")
            ("p" . "项目安排")
            ("pw" tags-todo "PROJECT+WORK+CATEGORY=\"cocos2d-x\"")
            ("pl" tags-todo "PROJECT+DREAM+CATEGORY=\"zhexuany\"")
            ("W" "Weekly Review"
             ((stuck "") ;; review stuck projects as designated by org-stuck-projects
              (tags-todo "PROJECT") ;; review all projects (assuming you use todo keywords to designate projects)
              ))))

    (defun org-summary-todo (n-done n-not-done)
      "Switch entry to DONE when all subentries are done, to TODO otherwise."
      (let (org-log-done org-log-states) ; turn off logging
        (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
    
    (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
    ;; used by org-clock-sum-today-by-tags
    (defun filter-by-tags ()
      (let ((head-tags (org-get-tags-at)))
        (member current-tag head-tags)))

    (defun org-clock-sum-today-by-tags (timerange &optional tstart tend noinsert)
      (interactive "P")
      (let* ((timerange-numeric-value (prefix-numeric-value timerange))
             (files (org-add-archive-files (org-agenda-files)))
             (include-tags '("WORK" "EMACS" "DREAM" "WRITING" "MEETING"
                             "LIFE" "PROJECT" "OTHER"))
             (tags-time-alist (mapcar (lambda (tag) `(,tag . 0)) include-tags))
             (output-string "")
             (tstart (or tstart
                         (and timerange (equal timerange-numeric-value 4) (- (org-time-today) 86400))
                         (and timerange (equal timerange-numeric-value 16) (org-read-date nil nil nil "Start Date/Time:"))
                         (org-time-today)))
             (tend (or tend
                       (and timerange (equal timerange-numeric-value 16) (org-read-date nil nil nil "End Date/Time:"))
                       (+ tstart 86400)))
             h m file item prompt donesomething)
        (while (setq file (pop files))
          (setq org-agenda-buffer (if (file-exists-p file)
                                      (org-get-agenda-file-buffer file)
                                    (error "No such file %s" file)))
          (with-current-buffer org-agenda-buffer
            (dolist (current-tag include-tags)
              (org-clock-sum tstart tend 'filter-by-tags)
              (setcdr (assoc current-tag tags-time-alist)
                      (+ org-clock-file-total-minutes (cdr (assoc current-tag tags-time-alist)))))))
        (while (setq item (pop tags-time-alist))
          (unless (equal (cdr item) 0)
            (setq donesomething t)
            (setq h (/ (cdr item) 60)
                  m (- (cdr item) (* 60 h)))
            (setq output-string (concat output-string (format "[-%s-] %.2d:%.2d\n" (car item) h m)))))
        (unless donesomething
          (setq output-string (concat output-string "[-Nothing-] Done nothing!!!\n")))
        (unless noinsert
          (insert output-string))
        output-string))


    ;; http://wenshanren.org/?p=327
    ;; change it to helm
    (defun zhexuany/org-insert-src-block (src-code-type)
      "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
      (interactive
       (let ((src-code-types
              '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
                "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
                "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
                "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
                "scheme" "sqlite")))
         (list (ido-completing-read "Source code type: " src-code-types))))
      (progn
        (newline-and-indent)
        (insert (format "#+BEGIN_SRC %s\n" src-code-type))
        (newline-and-indent)
        (insert "#+END_SRC\n")
        (previous-line 2)
        (org-edit-src-code)))

    (add-hook 'org-mode-hook '(lambda ()
                                ;; keybinding for editing source code blocks
                                ;; keybinding for inserting code blocks
                                (local-set-key (kbd "C-c i s")
                                               'zhexuany/org-insert-src-block)
                                ))
    (require 'ox-publish)
    (add-to-list 'org-latex-classes '("ctexart" "\\documentclass[11pt]{ctexart}
                                        [NO-DEFAULT-PACKAGES]
                                        \\usepackage[utf8]{inputenc}
                                        \\usepackage[T1]{fontenc}
                                        \\usepackage{fixltx2e}
                                        \\usepackage{graphicx}
                                        \\usepackage{longtable}
                                        \\usepackage{float}
                                        \\usepackage{wrapfig}
                                        \\usepackage{rotating}
                                        \\usepackage[normalem]{ulem}
                                        \\usepackage{amsmath}
                                        \\usepackage{textcomp}
                                        \\usepackage{marvosym}
                                        \\usepackage{wasysym}
                                        \\usepackage{amssymb}
                                        \\usepackage{booktabs}
                                        \\usepackage[colorlinks,linkcolor=black,anchorcolor=black,citecolor=black]{hyperref}
                                        \\tolerance=1000
                                        \\usepackage{listings}
                                        \\usepackage{xcolor}
                                        \\lstset{
                                        %行号
                                        numbers=left,
                                        %背景框
                                        framexleftmargin=10mm,
                                        frame=none,
                                        %背景色
                                        %backgroundcolor=\\color[rgb]{1,1,0.76},
                                        backgroundcolor=\\color[RGB]{245,245,244},
                                        %样式
                                        keywordstyle=\\bf\\color{blue},
                                        identifierstyle=\\bf,
                                        numberstyle=\\color[RGB]{0,192,192},
                                        commentstyle=\\it\\color[RGB]{0,96,96},
                                        stringstyle=\\rmfamily\\slshape\\color[RGB]{128,0,0},
                                        %显示空格
                                        showstringspaces=false
                                        }
                                        "
                                      ("\\section{%s}" . "\\section*{%s}")
                                      ("\\subsection{%s}" . "\\subsection*{%s}")
                                      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                      ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                      ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    ))








