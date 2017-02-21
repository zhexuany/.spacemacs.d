(setq zhexuany-swiper-packages
      '(
        swiper
        counsel
        ))

(defun zhexuany-swiper/init-counsel ()
  (use-package counsel
    :init
    (progn
      (global-set-key (kbd "C-h v") 'counsel-describe-variable)
      (global-set-key (kbd "C-h f") 'counsel-describe-function)
      (evil-leader/set-key "hdv" 'counsel-describe-variable)
      (evil-leader/set-key "hdf" 'counsel-describe-function)
      (bind-key* "M-x" 'counsel-M-x)
      (evil-leader/set-key dotspacemacs-command-key 'counsel-M-x)
      )))

(defun zhexuany-swiper/init-swiper ()
  "Initialize my package"
  (use-package swiper
    :init
    (progn
      (setq ivy-use-virtual-buffers t)
      (setq ivy-display-style 'fancy)
      (use-package recentf
        :config
        (setq recentf-exclude
              '("COMMIT_MSG" "COMMIT_EDITMSG" "github.*txt$"
                ".*png$"))
        (setq recentf-max-saved-items 60))
      (evilified-state-evilify ivy-occur-mode ivy-occur-mode-map)
      (use-package ivy
        :defer t
        :config
        (progn
          (spacemacs|hide-lighter ivy-mode)
          (define-key ivy-minibuffer-map (kbd "C-c o") 'ivy-occur)
          (define-key ivy-minibuffer-map (kbd "s-o") 'ivy-dispatching-done)
          (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
          (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)))

      (define-key global-map (kbd "C-s") 'swiper)
      (ivy-mode t)
      (evil-leader/set-key (kbd "bb") 'ivy-switch-buffer)
      (global-set-key (kbd "C-c C-r") 'ivy-resume)
      (global-set-key (kbd "C-c j") 'counsel-git-grep))))
