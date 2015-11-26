(setq zhexuan-c-styles-packages
      '(
        xcompany
        cmake-font-lock
        cmake-mode
        company-c-headers
        ))
(defun zhexuan-c-styles/post-init-cc-mode ()
  ;; company backend should be grouped
  (setq company-backends-c-mode-common '((company-c-headers
                                          company-dabbrev-code
                                          company-keywords
                                          company-etags
                                          company-gtags :with company-yasnippet)
                                         company-files company-dabbrev )))

(defun zhexuan-c-styles/post-init-company-c-headers ()
  (use-package company-c-headers
    :defer t
    :init (progn
            (setq company-c-headers-path-system
                  (quote
                   ("/usr/include/" "/usr/local/include/"
                    "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1")
                   )))))
(defun zhexuan-c-styles/post-init-multiple-cursors ()
  (use-package multiple-cursors
    :init
    (progn
      (bind-key* "C-s-f" 'mc/edit-lines)
      (bind-key* "C-s-." 'mc/mark-next-like-this)
      (bind-key* "C-s-," 'mc/mark-previous-like-this)
      (bind-key* "C-c C-s-." 'mc/mark-all-like-this))))



(defun zhexuan-c-styles/init-cmake-font-lock ()
  (use-package cmake-font-lock
    :defer t))


(defun zhexuan-c-styles/post-init-cmake-mode ()
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
