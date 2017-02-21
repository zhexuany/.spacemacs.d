(setq zhexuany-yasnippet-packages
      '(
        yasnippet
        ))

(defun zhexuany-yasnippet/post-init-yasnippet ()
  (progn
    (setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))
    (mapc #'(lambda (hook) (remove-hook hook 'spacemacs/load-yasnippet)) '(prog-mode-hook
                                                                           org-mode-hook
                                                                           markdown-mode-hook))

(defun zhexuany-yasnippet/load-yasnippet ()
      (unless yas-global-mode
        (progn
          (yas-global-mode 1)
          (setq my-snippet-dir (expand-file-name "~/.spacemacs.d/snippets"))
          (setq yas-snippet-dirs  my-snippet-dir)
          (yas-load-directory my-snippet-dir)
          (setq yas-wrap-around-region t)))
      (yas-minor-mode 1))

    (spacemacs/add-to-hooks 'zhexuany-yasnippet/load-yasnippet '(prog-mode-hook
                                                       markdown-mode-hook
                                                       c-c++-hook
                                                       org-mode-hook))
    ))
