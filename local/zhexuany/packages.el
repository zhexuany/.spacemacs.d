(setq zhexuany-packages
      '(
        lispy
        company
        markdown-mode
        impatient-mode
        keyfreq
        visual-regexp
        visual-regexp-steroids
        persp-mode
        hungry-delete
        ;; flyspell
        find-file-in-project
        wrap-region
        ;; tagedit
        ;; js-comint
        ctags-update
        evil-vimish-fold
        beacon
        evil-visual-mark-mode
        (occur-mode :location built-in)
        erc
        ))

(defun zhexuany/post-init-erc ()
  (defun my-erc-hook (match-type nick message)
    "Shows a growl notification, when user's nick was mentioned. If the buffer is currently not visible, makes it sticky."
    (unless (posix-string-match "^\\** *Users on #" message)
      (zhexuany/growl-notification
       (concat "ERC: : " (buffer-name (current-buffer)))
       message
       t
       )))
  (add-hook 'erc-text-matched-hook 'my-erc-hook)
  (spaceline-toggle-erc-track-off)
  )

(defun zhexuany/init-occur-mode ()
  (defun occur-dwim ()
    "Call `occur' with a sane default."
    (interactive)
    (push (if (region-active-p)
              (buffer-substring-no-properties
               (region-beginning)
               (region-end))
            (let ((sym (thing-at-point 'symbol)))
              (when (stringp sym)
                (regexp-quote sym))))
          regexp-history)
    (call-interactively 'occur))
  (bind-key* "M-s o" 'occur-dwim)
  (evilified-state-evilify occur-mode occur-mode-map
    "RET" 'occur-mode-goto-occurrence))

(defun zhexuany/init-evil-visual-mark-mode ()
  (use-package evil-visual-mark-mode
    :init
    (progn
      (spacemacs|add-toggle evil-visual-mark-mode
        :status evil-visual-mark-mode
        :on (evil-visual-mark-mode)
        :off (evil-visual-mark-mode -1)
        :documentation "Show evil marks"
        :evil-leader "otm")
      (evil-visual-mark-mode))))

(defun zhexuany/init-beacon ()
  (use-package beacon
    :init
    (progn
      (spacemacs|add-toggle beacon
        :status beacon-mode
        :on (beacon-mode)
        :off (beacon-mode -1)
        :documentation "Enable point highlighting after scrolling"
        :evil-leader "otb")
      (spacemacs/toggle-beacon-on))
    :config (spacemacs|hide-lighter beacon-mode)))


(defun zhexuany/init-evil-vimish-fold ()
  (use-package evil-vimish-fold
    :init
    (vimish-fold-global-mode 1)))

(defun zhexuany/init-ctags-update ()
  (use-package ctags-update
    :init
    (progn
      ;; (add-hook 'js2-mode-hook 'turn-on-ctags-auto-update-mode)
      (define-key evil-normal-state-map (kbd "gf")
        (lambda () (interactive) (find-tag (find-tag-default-as-regexp))))

      (define-key evil-normal-state-map (kbd "gb") 'pop-tag-mark)

      (define-key evil-normal-state-map (kbd "gn")
        (lambda () (interactive) (find-tag last-tag t)))
      )
    :defer t
    :config
    (spacemacs|hide-lighter ctags-auto-update-mode)))

(defun zhexuany/init-wrap-region ()
  (use-package wrap-region
    :init
    (progn
      (wrap-region-global-mode t)
      (wrap-region-add-wrappers
       '(("$" "$")
         ("{-" "-}" "#")
         ("/" "/" nil ruby-mode)
         ("/* " " */" "#" (java-mode javascript-mode css-mode js2-mode))
         ("`" "`" nil (markdown-mode ruby-mode))))
      (add-to-list 'wrap-region-except-modes 'dired-mode)
      (add-to-list 'wrap-region-except-modes 'web-mode)
      )
    :defer t
    :config
    (spacemacs|hide-lighter wrap-region-mode)))

(defun zhexuany/init-find-file-in-project ()
  (use-package find-file-in-project
    :defer t
    :init))

(defun zhexuany/post-init-hungry-delete ()
  ;; (add-hook 'prog-mode-hook 'hungry-delete-mode)
  (global-hungry-delete-mode t)
  )

(defun zhexuany/init-visual-regexp-steroids ()
  (use-package visual-regexp-steroids
    :init))

(defun zhexuany/init-visual-regexp ()
  (use-package visual-regexp
    :init))



(defun zhexuany/init-lispy ()
  "Initialize lispy"
  (use-package lispy
    :defer t
    :diminish (lispy-mode)
    :init
    (progn
      (add-hook 'lispy-mode-hook 'spacemacs/toggle-aggressive-indent-on)
      (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'spacemacs-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'clojure-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'scheme-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'cider-repl-mode-hook (lambda () (lispy-mode 1))))))


(defun zhexuany/post-init-company ()
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.08)
  (when (configuration-layer/package-usedp 'company)
    (spacemacs|add-company-hook lua-mode)
    (spacemacs|add-company-hook nxml-mode)))

;; configs for writing
(defun zhexuany/post-init-markdown-mode ()
  (use-package markdown-mode
    :defer t
    :config
    (progn
      (add-to-list 'auto-mode-alist '("\\.mdown\\'" . markdown-mode))
      (when (configuration-layer/package-usedp 'company)
        (spacemacs|add-company-hook markdown-mode))
      (defun zhexuany/markdown-to-html ()
        (interactive)
        (start-process "grip" "*gfm-to-html*" "grip" (buffer-file-name))
        (browse-url (format "http://localhost:5000/%s.%s" (file-name-base) (file-name-extension (buffer-file-name)))))

      (evil-leader/set-key-for-mode 'gfm-mode-map
        "p" 'zhexuany/markdown-to-html)
      (evil-leader/set-key-for-mode 'markdown-mode
        "p" 'zhexuany/markdown-to-html))))

(defun zhexuany/init-keyfreq ()
  (use-package keyfreq
    :init
    (progn
      (keyfreq-mode t)
      (keyfreq-autosave-mode 1))))
