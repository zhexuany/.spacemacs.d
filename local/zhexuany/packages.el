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
        discover-my-major
        impatient-mode
        swiper
        ;; ox-reveal
;;        helm-flyspell
        helm
        ace-window
        ;; helm-ls-git
        ;; mwe-log-commands
        flycheck-package
        ;; deft
        hungry-delete
        multiple-cursors
        ;; persp-mode
        ))


(defun zhexuany/post-init-hungry-delete ()
  (add-hook 'prog-mode-hook 'hungry-delete-mode)
  )

(defun zhexuany/init-discover-my-major ()
  (use-package discover-my-major
    :defer t
    :init
    (progn
      (evil-leader/set-key (kbd "mhm") 'discover-my-major)
      (evilify makey-key-mode makey-key-mode-get-key-map)
      )))


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


(defun zhexuany/post-init-deft ()
  (setq deft-use-filter-string-for-filename t)
  (evil-leader/set-key-for-mode 'deft-mode "mq" 'quit-window)
  (setq deft-extension "org")
  (setq deft-directory "~/Dropbox/ORG"))

(defun zhexuany/post-init-popwin ()
  (push "*zhexuany/run-current-file output*" popwin:special-display-config))
