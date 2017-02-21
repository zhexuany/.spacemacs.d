(setq zhexuany-helm-packages
      '(
        helm-flyspell
        helm
        helm-ls-git
        helm-gtags
        helm-ag
        ))
(defun zhexuany-helm/post-init-helm-ag ()
  (setq helm-ag-use-agignore t)
  ;; This settings use .agignore file to ignore items, and it don't respect to .hgignore, .gitignore
  ;; when there are some git repositories are in .gitignore file, this options is very useful.
  ;;And the .agignore file while be searched at PROJECT_ROOT/.agignore and ~/.agignore
  ;; Thanks to 'man ag' and 'customize-group<RET> helm-ag' for finding the solution... Always RTFM.
  (setq helm-ag-command-option " -U" )
  )

(defun zhexuany-helm/post-init-helm-gtags ()
  (use-package helm-gtags
    :diminish helm-gtags-mode
    :defer t
    :config
    (progn
      (evil-make-overriding-map helm-gtags-mode-map 'normal)
      (add-hook 'helm-gtags-mode-hook #'evil-normalize-keymaps)

      )))

(defun zhexuany-helm/post-init-helm-flyspell ()
  (use-package helm-flyspell
    :commands helm-flyspell-correct
    :init
    ;; "http://emacs.stackexchange.com/questions/14909/how-to-use-flyspell-to-efficiently-correct-previous-word/14912#14912"
    (defun zhexuany-helm/flyspell-goto-previous-error (arg)
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

    (bind-key* "C-;" 'zhexuany-helm/flyspell-goto-previous-error)
    (global-set-key (kbd "C-c s") 'helm-flyspell-correct)))

(defun zhexuany-helm/post-init-helm ()
  (use-package helm
    :init
    (progn
      (global-set-key (kbd "C-s-y") 'helm-show-kill-ring)
      ;; See https://github.com/bbatsov/prelude/pull/670 for a detailed
      ;; discussion of these options.
      (setq helm-split-window-in-side-p t
            helm-move-to-line-cycle-in-source t
            helm-ff-search-library-in-sexp t
            helm-ff-file-name-history-use-recentf t
            helm-buffer-max-length 45)

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


(defun zhexuany-helm/init-helm-ls-git ()
  (use-package helm-ls-git
    :init
    (progn
      ;;beautify-helm buffer when long file name is present
      (setq helm-ls-git-show-abs-or-relative 'relative))))
