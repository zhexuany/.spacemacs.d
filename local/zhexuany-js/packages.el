(setq zhexuany-js-packages
      '(
        js-doc-insert-tag
        js2-mode
        css-mode
        web-mode
        projectile
        impatient-mode
        (nodejs-repl-eval :location local)
        json-mode
        nodejs-repl
        ))

(defun zhexuany-js/init-nodejs-repl ()
  (use-package nodejs-repl
    :init
    :defer t))

;; For each extension, define a function zhexuany/init-<extension-name>
;; https://atlanis.net/blog/posts/nodejs-repl-eval.html
(defun zhexuany-js/init-nodejs-repl-eval ()
  (use-package nodejs-repl-eval
    :init))

(defun zhexuany-js/post-init-json-mode ()
  (add-to-list 'auto-mode-alist '("\\.tern-project\\'" . json-mode)))

(defun zhexuany/init-impatient-mode ()
  "Initialize impatient mode"
  (use-package impatient-mode
    :init
    (progn

      (defun zhexuany-js-mode-hook ()
        "my web mode hook for HTML REPL"
        (interactive)
        (impatient-mode)
        (spacemacs|hide-lighter impatient-mode)
        (httpd-start))

      (add-hook 'web-mode-hook 'zhexuany-js-mode-hook)
      (evil-leader/set-key-for-mode 'web-mode
        "p" 'imp-visit-buffer)
      )))

(defun zhexuany-js/post-init-tagedit ()
  (add-hook 'web-mode-hook (lambda () (tagedit-mode 1))))

(defun zhexuany-js/post-init-projectile ()
  (use-package projectile
    :defer t
    :config
    (progn
      (setq projectile-completion-system 'ivy)
      (add-to-list 'projectile-other-file-alist '("html" "js")) ;; switch from html -> js
      (add-to-list 'projectile-other-file-alist '("js" "html")) ;; switch from js -> html
      )))


(defun zhexuany-js/post-init-web-mode ()
  (setq company-backends-web-mode '((company-dabbrev-code
                                     company-keywords
                                     company-etags)
                                    company-files company-dabbrev)))

(defun zhexuany-js/post-init-css-mode ()
  (progn
    (dolist (hook '(css-mode-hook sass-mode-hook less-mode-hook))
      (add-hook hook 'rainbow-mode))

    (defun css-imenu-make-index ()
      (save-excursion
        (imenu--generic-function '((nil "^ *\\([^ ]+\\) *{ *$" 1)))))

    (add-hook 'css-mode-hook
              (lambda ()
                (setq imenu-create-index-function 'css-imenu-make-index)))))

(defun zhexuany-js/post-init-js-doc ()
  (use-package js-doc
    :defer t
    :config
    (setq js-doc-mail-address "guanghui8827@gmail.com"
          js-doc-author (format "Guanghui Qu <%s>" js-doc-mail-address)
          js-doc-url "http://www.zhexuany-js.com"
          js-doc-license "MIT")
    ))

(defun zhexuany-js/post-init-js2-mode ()
  (progn
    ;; (remove-hook 'js2-mode-hook 'flycheck-mode)
    (defun conditional-disable-modes ()
      (when (> (buffer-size) 50000)
        (flycheck-mode -1)))

    (add-hook 'js2-mode-hook 'which-function-mode)
    (add-hook 'js2-mode-hook 'conditional-disable-modes)
    (add-hook 'js2-mode-hook '(lambda ()
                                (local-set-key "\C-x\C-e" 'js-send-last-sexp)
                                (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
                                (local-set-key "\C-cb" 'js-send-buffer)
                                (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
                                (local-set-key "\C-cl" 'js-load-file-and-go)
                                ))

    (spacemacs/declare-prefix-for-mode 'js2-mode "ms" "repl")
    (evil-leader/set-key-for-mode 'js2-mode
      ;; "sr" 'js-send-region
      ;; "sR" 'js-send-region-and-go
      ;; "sb" 'js-send-buffer
      ;; "sB" 'js-send-buffer-and-go
      ;; "sd" 'js-send-last-sexp
      ;; "sD" 'js-send-last-sexp-and-go
      "gd" 'helm-etags-select)


    (use-package js2-mode
      :defer t
      :config
      (progn
        ;; these mode related variables must be in eval-after-load
        ;; https://github.com/magnars/.emacs.d/blob/master/settings/setup-js2-mode.el
        (setq-default js2-allow-rhino-new-expr-initializer nil)
        (setq-default js2-auto-indent-p nil)
        (setq-default js2-enter-indents-newline nil)
        (setq-default js2-global-externs '("module" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON"))
        (setq-default js2-idle-timer-delay 0.1)
        (setq-default js2-mirror-mode nil)
        (setq-default js2-strict-inconsistent-return-warning nil)
        (setq-default js2-include-rhino-externs nil)
        (setq-default js2-include-gears-externs nil)
        (setq-default js2-concat-multiline-strings 'eol)
        (setq-default js2-rebind-eol-bol-keys nil)
        (setq-default js2-auto-indent-p t)

        (setq-default js2-bounce-indent nil)
        (setq-default js-indent-level 4)
        (setq-default js2-basic-offset 4)
        (setq-default js2-indent-switch-body t)
        ;; Let flycheck handle parse errors
        (setq-default js2-show-parse-errors nil)
        (setq-default js2-strict-missing-semi-warning nil)
        (setq-default js2-highlight-external-variables t)
        (setq-default js2-strict-trailing-comma-warning nil)

        (add-hook 'js2-mode-hook
                  #'(lambda ()
                      (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc)
                      (define-key js2-mode-map "@" 'js-doc-insert-tag)))

        (defun js2-toggle-indent ()
          (interactive)
          (setq js-indent-level (if (= js-indent-level 2) 4 2))
          (setq js2-indent-level (if (= js-indent-level 2) 4 2))
          (setq js2-basic-offset (if (= js-indent-level 2) 4 2))
          (message "js-indent-level, js2-indent-level, and js2-basic-offset set to %d"
                   js2-basic-offset))

        (evil-leader/set-key-for-mode 'js2-mode
          "oj" 'js2-toggle-indent)
        (spacemacs/declare-prefix-for-mode 'js2-mode "mo" "toggle")

        (autoload 'flycheck-get-checker-for-buffer "flycheck")
        (defun sanityinc/disable-js2-checks-if-flycheck-active ()
          (unless (flycheck-get-checker-for-buffer)
            (set (make-local-variable 'js2-mode-show-parse-errors) t)
            (set (make-local-variable 'js2-mode-show-strict-warnings) t)))
        (add-hook 'js2-mode-hook 'sanityinc/disable-js2-checks-if-flycheck-active)
        (eval-after-load 'tern-mode
          '(spacemacs|hide-lighter tern-mode))
        ))

    (evilified-state-evilify js2-error-buffer-mode js2-error-buffer-mode-map)


    (defun js2-imenu-make-index ()
      (interactive)
      (save-excursion
        ;; (setq imenu-generic-expression '((nil "describe\\(\"\\(.+\\)\"" 1)))
        (imenu--generic-function '(("describe" "\\s-*describe\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                                   ("it" "\\s-*it\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                                   ("before" "\\s-*before\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                                   ("after" "\\s-*after\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                                   ("Controller" "[. \t]controller([ \t]*['\"]\\([^'\"]+\\)" 1)
                                   ("Controller" "[. \t]controllerAs:[ \t]*['\"]\\([^'\"]+\\)" 1)
                                   ("Filter" "[. \t]filter([ \t]*['\"]\\([^'\"]+\\)" 1)
                                   ("State" "[. \t]state([ \t]*['\"]\\([^'\"]+\\)" 1)
                                   ("Factory" "[. \t]factory([ \t]*['\"]\\([^'\"]+\\)" 1)
                                   ("Service" "[. \t]service([ \t]*['\"]\\([^'\"]+\\)" 1)
                                   ("Module" "[. \t]module([ \t]*['\"]\\([a-zA-Z0-9_\.]+\\)" 1)
                                   ("ngRoute" "[. \t]when(\\(['\"][a-zA-Z0-9_\/]+['\"]\\)" 1)
                                   ("Directive" "[. \t]directive([ \t]*['\"]\\([^'\"]+\\)" 1)
                                   ("Event" "[. \t]\$on([ \t]*['\"]\\([^'\"]+\\)" 1)
                                   ("Config" "[. \t]config([ \t]*function *( *\\([^\)]+\\)" 1)
                                   ("Config" "[. \t]config([ \t]*\\[ *['\"]\\([^'\"]+\\)" 1)
                                   ("OnChange" "[ \t]*\$(['\"]\\([^'\"]*\\)['\"]).*\.change *( *function" 1)
                                   ("OnClick" "[ \t]*\$([ \t]*['\"]\\([^'\"]*\\)['\"]).*\.click *( *function" 1)
                                   ("Watch" "[. \t]\$watch( *['\"]\\([^'\"]+\\)" 1)
                                   ("Function" "function[ \t]+\\([a-zA-Z0-9_$.]+\\)[ \t]*(" 1)
                                   ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function[ \t]*(" 1)
                                   ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*:[ \t]*function[ \t]*(" 1)
                                   ("Class" "^[ \t]*var[ \t]*\\([0-9a-zA-Z]+\\)[ \t]*=[ \t]*\\([a-zA-Z]*\\).extend" 1)
                                   ("Class" "^[ \t]*cc\.\\(.+\\)[ \t]*=[ \t]*cc\.\\(.+\\)\.extend" 1)
                                   ("Task" "[. \t]task([ \t]*['\"]\\([^'\"]+\\)" 1)))))

    (add-hook 'js2-mode-hook
              (lambda ()
                (setq imenu-create-index-function 'js2-imenu-make-index)))
    ))
