
(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     scala
     shell-scripts
     ;; java
     neotree
     ;; osx
     ;; php
     ;; javascript
     rust
     ;; nginx
     ;; html
     yaml
     ;; helm
     ivy
     auto-completion
     emacs-lisp
     (git :variables
          git-magit-status-fullscreen t
          magit-push-always-verify nil
          magit-save-repository-buffers 'dontask
          magit-revert-buffers 'silent
          magit-refs-show-commit-count 'all
          magit-revision-show-gravatars nil)
     github
     colors
     markdown
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     syntax-checking 
     (spell-checking :variables spell-checking-enable-by-default nil)
     ibuffer
     version-control
     (c-c++ :variables
            c-c++-default-mode-for-headers 'c++-mode)
     ;; common-lisp
     (vinegar :variables vinegar-reuse-dired-buffer t)
     (spacemacs-layouts :variables layouts-enable-autosave t
                        layouts-autosave-delay 300)
     evil-commentary
     python
     (go :variables go-use-gometalinter t)
     )
   dotspacemacs-additional-packages '(
                                      editorconfig
                                      )
   dotspacemacs-excluded-packages '(
                                    org-plus-contrib
                                    evil-tuto
                                    r
                                    lorem-ipsum
                                    )
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https nil
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update t
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '(recents projects)
   dotspacemacs-startup-recent-list-size 5
   dotspacemacs-scratch
   -mode 'vim-powerline
   dotspacemacs-themes '(
                         monokai
                         solarized-light
                         spacemacs-dark
                         spacemacs-light
                         solarized-dark
                         leuven
                         zenburn)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ nil
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers 'relative
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
   ;;(setq configuration-layer--elpa-archives
    ;;     '(("melpa-cn" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
     ;;      ("org-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
      ;;     ("gnu-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")))
    (setq exec-path-from-shell-arguments '("-l"))
    (defun copy-to-clipboard ()
      "Copies selection to x-clipboard."
      (interactive)
      (if (display-graphic-p)
          (progn
            (message "Yanked region to x-clipboard!")
            (call-interactively 'clipboard-kill-ring-save)
            )
        (if (region-active-p)
            (progn
              (shell-command-on-region (region-beginning) (region-end) "pbcopy")
              (message "Yanked region to clipboard!")
              (deactivate-mark))
          (message "No region active; can't yank to clipboard!")))
      )

    (defun paste-from-clipboard ()
      "Pastes from x-clipboard."
      (interactive)
      (if (display-graphic-p)
          (progn
            (clipboard-yank)
            (message "graphics active")
            )
        (insert (shell-command-to-string "pbpaste"))
        )
      )
    (evil-leader/set-key "o y" 'copy-to-clipboard)
    (evil-leader/set-key "o p" 'paste-from-clipboard)
  )

(defun dotspacemacs/user-config ()
  (exec-path-from-shell-copy-env "GOPATH")
   ;; configure editior config
  (use-package editorconfig
    :ensure t
    :config
    (editorconfig-mode 1))
  ;; configuration magithub 
  ;; (use-package magithub
  ;;   :after magit
  ;;   :config (magithub-feature-autoinject t))
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (winum stickyfunc-enhance noflet insert-shebang fuzzy seq fish-mode ghub let-alist ensime sbt-mode scala-mode editorconfig company-shell company-emacs-eclim eclim rust-mode powerline request spinner window-purpose imenu-list parent-mode flx iedit anzu evil goto-chg undo-tree diminish projectile pkg-info epl bind-map bind-key packed async f dash s avy popup package-build smartparens highlight helm helm-core hydra org markdown-mode haml-mode gitignore-mode fringe-helper git-gutter git-gutter+ gh marshal logito pcache ht flyspell-correct flycheck magit magit-popup git-commit with-editor web-completion-data pos-tip go-mode company yasnippet anaconda-mode pythonic auto-complete protobuf-mode wolfram-mode thrift stan-mode scad-mode qml-mode matlab-mode julia-mode arduino-mode wgrep smex slime-company slime ivy-purpose ivy-hydra flyspell-correct-ivy counsel-projectile counsel swiper ivy common-lisp-snippets monokai-theme yapfify yaml-mode xterm-color ws-butler window-numbering which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toml-mode toc-org tagedit spacemacs-theme spaceline smeargle slim-mode shell-pop scss-mode sass-mode restart-emacs rainbow-mode rainbow-identifiers rainbow-delimiters racer quelpa pyvenv pytest pyenv-mode py-isort pug-mode popwin pip-requirements persp-mode pcre2el paradox orgit org-plus-contrib org-bullets open-junk-file neotree multi-term move-text mmm-mode markdown-toc magit-gitflow magit-gh-pulls macrostep lorem-ipsum livid-mode live-py-mode linum-relative link-hint less-css-mode json-mode js2-refactor js-doc info+ indent-guide ido-vertical-mode ibuffer-projectile hy-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-pydoc helm-purpose helm-projectile helm-mode-manager helm-make helm-gtags helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag google-translate golden-ratio go-guru go-eldoc github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist gh-md ggtags flyspell-correct-helm flycheck-rust flycheck-pos-tip flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-commentary evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav dumb-jump disaster diff-hl define-word cython-mode company-web company-tern company-statistics company-quickhelp company-go company-c-headers company-anaconda column-enforce-mode color-identifiers-mode coffee-mode cmake-mode clean-aindent-mode clang-format cargo auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (flycheck-gometalinter winum stickyfunc-enhance noflet insert-shebang fuzzy seq fish-mode ghub let-alist ensime sbt-mode scala-mode editorconfig company-shell company-emacs-eclim eclim rust-mode powerline request spinner window-purpose imenu-list parent-mode flx iedit anzu evil goto-chg undo-tree diminish projectile pkg-info epl bind-map bind-key packed async f dash s avy popup package-build smartparens highlight helm helm-core hydra org markdown-mode haml-mode gitignore-mode fringe-helper git-gutter git-gutter+ gh marshal logito pcache ht flyspell-correct flycheck magit magit-popup git-commit with-editor web-completion-data pos-tip go-mode company yasnippet anaconda-mode pythonic auto-complete protobuf-mode wolfram-mode thrift stan-mode scad-mode qml-mode matlab-mode julia-mode arduino-mode wgrep smex slime-company slime ivy-purpose ivy-hydra flyspell-correct-ivy counsel-projectile counsel swiper ivy common-lisp-snippets monokai-theme yapfify yaml-mode xterm-color ws-butler window-numbering which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toml-mode toc-org tagedit spacemacs-theme spaceline smeargle slim-mode shell-pop scss-mode sass-mode restart-emacs rainbow-mode rainbow-identifiers rainbow-delimiters racer quelpa pyvenv pytest pyenv-mode py-isort pug-mode popwin pip-requirements persp-mode pcre2el paradox orgit org-plus-contrib org-bullets open-junk-file neotree multi-term move-text mmm-mode markdown-toc magit-gitflow magit-gh-pulls macrostep lorem-ipsum livid-mode live-py-mode linum-relative link-hint less-css-mode json-mode js2-refactor js-doc info+ indent-guide ido-vertical-mode ibuffer-projectile hy-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-pydoc helm-purpose helm-projectile helm-mode-manager helm-make helm-gtags helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag google-translate golden-ratio go-guru go-eldoc github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist gh-md ggtags flyspell-correct-helm flycheck-rust flycheck-pos-tip flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-commentary evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav dumb-jump disaster diff-hl define-word cython-mode company-web company-tern company-statistics company-quickhelp company-go company-c-headers company-anaconda column-enforce-mode color-identifiers-mode coffee-mode cmake-mode clean-aindent-mode clang-format cargo auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
