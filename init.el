(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation nil
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     spacemacs-helm
     spacemacs-ivy
     (
      auto-completion
      :variables
      auto-completion-tab-key-behavior 'complete
      auto-completion-return-key-behavior 'nil
      auto-completion-enable-help-tooltip t
      auto-completion-enable-snippets-in-popup t
      auto-completion-private-snippets-directory "~/.spacemacs.d/snippets/"
      :disabled-for org erc)
     emacs-lisp
     git
     github
     (colors :variables
             colors-enable-nyan-cat-progress-bar t)
     markdown
     org
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     (syntax-checking :variables syntax-checking-enable-by-default nil)
     (spell-checking :variables spell-checking-enable-by-default nil)
     (ibuffer :variables ibuffer-group-buffers-by nil)
     version-control
     ycmd
     (c-c++ :variables
            c-c++-default-mode-for-headers 'c++-mode)
     common-lisp
     java
     (vinegar :variables vinegar-reuse-dired-buffer t)
     eyebrowse
     (clojure :variables clojure-enable-fancify-symbols t)
     evil-commentary
     python
     scheme
     ;; Peosonal layeres
     zhexuany
     zhexuany-yasnippet
     ;; using spacemacs-ivy mode now, no need to enbale personal layer: zhexuany-swiper
     ;; zhexuany-swipper
     zhexuany-whitespace
     ;; ;; zhexuany-c-c++
     ;; ;; zhexuany-js
     ;; zhexuany-dired
     zhexuany-git
     zhexuany-helm
     zhexuany-org
     )
   dotspacemacs-additional-packages
   '(
     helm-flycheck
     help-fns+
     )
   dotspacemacs-excluded-packages '(
                                    git-gutter
                                    magit-gh-pulls
                                    magit-gitflow
                                    evil-mc
                                    emmet-mode
                                    srefactor
                                    org-download
                                    org-timer
                                    org-tree-slide
                                    git-gutter
                                    ;; disable it for lispy-mode
                                    ;;https://github.com/abo-abo/lispy/issues/137
                                    evil-escape
                                    ;;remove from spacemacs distribution
                                    neotree
                                    leuven-theme
                                    gh-md
                                    evil-lisp-state
                                    spray
                                    evil-tutor
                                    define-word
                                    doc-view
                                    lorem-ipsum
                                    solarized-theme
                                    ;; remove mode for python layer
                                    nose
                                    pony-mode
                                    hy-mode)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-editing-style 'hybrid
   ;; dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-lists '(recents projects)
   dotspacemacs-startup-recent-list-size 5
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(spacemacs-light
                         spacemacs-dark
                         solarized-light
                         solarized-dark
                         leuven
                         monokai
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
   dotspacemacs-command-key ":"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ t
   Dotspacemacs-emacs-command-key "SPC"
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
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
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

  (global-company-mode t)
  (setq-default powerline-default-separator 'arrow)

  ;; Utility functions
  (defun bb/define-key (keymap &rest bindings)
    (declare (indent 1))
    (while bindings
      (define-key keymap (pop bindings) (pop bindings))))
  (bb/define-key evil-normal-state-map
    "+" 'spacemacs/evil-numbers-increase
    "_" 'spacemacs/evil-numbers-decrease
    "\\" 'evil-repeat-find-char-reverse
    "[s" (lambda (n) (interactive "p") (dotimes (c n nil) (insert " ")))
    "]s" (lambda (n) (interactive "p")
           (forward-char) (dotimes (c n nil) (insert " ")) (backward-char (1+ n))))

  (bb/define-key company-active-map
    (kbd "C-w") 'evil-delete-backward-word)

  (with-eval-after-load 'helm
    (define-key helm-map (kbd "C-w") 'evil-delete-backward-word))

  (with-eval-after-load 'helm-swoop
    (define-key helm-swoop-map (kbd "C-w") 'evil-delete-backward-word))

  (add-hook 'text-mode-hook 'auto-fill-mode)
  (add-hook 'org-mode-hook 'auto-fill-mode)

  (remove-hook 'emacs-lisp-mode-hook 'auto-compile-mode)
  (define-key helm-find-files-map (kbd "s-c") 'helm-ff-run-copy-file)

  ;; http://emacsredux.com/blog/2014/04/05/which-function-mode/
  ;; when editing js file, this feature is very useful
  (setq-default header-line-format
                '((which-func-mode ("" which-func-format " "))))
  (setq mode-line-misc-info
        ;; We remove Which Function Mode from the mode line, because it's mostly
        ;; invisible here anyway.
        (assq-delete-all 'which-func-mode mode-line-misc-info))

  (add-hook 'prog-mode-hook
            (lambda ()
              (when (> (buffer-size) 100000)
                (turn-off-show-smartparens-mode))))

  ;; improve the performance of opening large file
  (add-hook 'org-mode-hook (lambda () (spacemacs/toggle-line-numbers-off)) 'append)
  (defun spacemacs/check-large-file ()
    (when (> (buffer-size) 100000)
      (progn (fundamental-mode)
             (hl-line-mode -1))))

  (add-hook 'find-file-hook 'spacemacs/check-large-file)
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right
 )
