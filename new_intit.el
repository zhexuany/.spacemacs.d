(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     (
      auto-completion
      :variables
      auto-completion-tab-key-behavior 'cycle
      auto-completion-return-key-behavior 'complete
      auto-completion-enable-help-tooltip t
      auto-completion-enable-snippets-in-popup t
      auto-completion-private-snippets-directory "~/.spacemacs.d/snippets/"
      :disabled-for org erc)
      emacs-lisp
      git
      github
      markdown
      (colors :variables
             colors-enable-nyan-cat-progress-bar t)
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     spell-checking
     syntax-checking
     (ibuffer :variables ibuffer-group-buffers-by nil)
     eyebrowse
     version-control
     c-c++
     java
     vim
     evil-commentary
     latex
     python
     ;; Peosonal layeres
;     zhexuan-c-style
;     zhexuany
     )
   ))
(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-editing-style 'hybrid
   ;; dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-lists '(recents projects)
   dotspacemacs-startup-recent-list-size 5
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light
                         solarized-light
                         solarized-dark
                         leuven
                         monokai
                         zenburn)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Menlo"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-command-key ":"
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-default-layout-name "Home"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 10
   dotspacemacs-use-ido nil
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-enable-paste-micro-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar nil
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers t
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'changed
   ))

(defun dotspacemacs/user-init ()
  (setq anaconda-mode-server-script "/usr/local/lib/python2.7/site-packages/anaconda_mode.py")
  )


(defun dotspacemacs/user-config ()
  (setq eclim-eclipse-dirs "~/Applications/Eclipse.app/Contents/Eclipse"
        eclim-executable "~/Applications/Eclipse.app/Contents/Eclipse/eclim"
        )

  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
