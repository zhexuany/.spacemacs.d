(setq zhexuany-c-c++-packages
      '(
        (doxymacs :location local)
        cmake-font-lock
        google-c-style
        cmake-mode
        flycheck
        flycheck-package
        ))

(defun zhexuany-c-c++/init-doxymacs ()
  "Initialize doxymacs"
  (use-package doxymacs
    :init
    (add-hook 'c-mode-common-hook 'doxymacs-mode)
    :config
    (progn
      (defun my-doxymacs-font-lock-hook ()
        (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
            (doxymacs-font-lock)))
      (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
      (spacemacs|hide-lighter doxymacs-mode))))

(defun zhexuany-c-c++/init-cmake-font-lock ()
  (use-package cmake-font-lock
    :defer t))

(defun zhexuany-c-c++/init-google-c-style ()
  (use-package google-c-style
    :init (add-hook 'c-mode-common-hook 'google-set-c-style)))

(defun zhexuany-c-c++/post-init-cmake-mode ()
  (use-package cmake-mode
    :defer
    :init
    (progn
      (spacemacs/declare-prefix-for-mode 'cmake-mode
                                         "mh" "docs"))
    (evil-leader/set-key-for-mode 'cmake-mode
      "hd" 'cmake-help)
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

(defun zhexuany/init-flycheck-package ()
  (use-package flycheck-package))

(defun zhexuany-c-c++/post-init-flycheck ()
  (use-package flycheck
    :defer t
    :config (progn
              (flycheck-package-setup)
              ;; (setq flycheck-display-errors-function 'flycheck-display-error-messages)
              (setq flycheck-display-errors-delay 0.2)
              ;; (remove-hook 'c-mode-hook 'flycheck-mode)
              ;; (remove-hook 'c++-mode-hook 'flycheck-mode)
              ;; (evilify flycheck-error-list-mode flycheck-error-list-mode-map)
              )))
