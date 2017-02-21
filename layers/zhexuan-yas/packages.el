(setq zhexuan-yas-packages
      '(
        company
        yasnippet
        ))

(defun zhexuan-yas/post-init-company ()
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.4)
  (global-set-key (kbd "C-/") 'company-complete)
  (when (configuration-layer/package-usedp 'company)
    ))

;; snippets get from https://github.com/AndreaCrotti/yasnippet-snippets
(defun zhexuan-yas/post-init-yasnippet ()
  (progn
    (setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))
    (mapc #'(lambda (hook) (remove-hook hook 'spacemacs/load-yasnippet))
          '(prog-mode-hook
            org-mode-hook
            markdown-mode-hook))
    (defun zhexuan-yas/load-yasnippet ()
      (unless yas-global-mode
        (progn
          (yas-global-mode 1)
          (setq my-snippet-dir (expand-file-name "~/.spacemacs.d/snippets"))
          (setq yas-snippet-dirs  my-snippet-dir)
          (yas-load-directory my-snippet-dir)
          (setq yas-wrap-around-region t)))
      (yas-minor-mode 1))

    (spacemacs/add-to-hooks 'zhexuan-yas/load-yasnippet '(prog-mode-hook
                                                       markdown-mode-hook
                                                       org-mode-hook))
    ))
