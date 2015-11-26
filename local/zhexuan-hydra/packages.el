(setq zhexuan-hydra-packages
      '(hydra) )


(defun zhexuan-hydra/init-hydra ()
  (use-package hydra
    :init
    (progn
      (when (configuration-layer/package-usedp 'org)
        ;; major mode hydra is really cool, don't need to switch mode anymore
        ;; C-c [a-z] and s-[a-z] is very quick to pressed even in emacs-state and F1-F9 is also the same
        ;; If the command will change the buffer, they should be put in these groups.
        ;; otherwise, use which-key + spacems + user defined key mappings in evil normal mode
        (defhydra hydra-org (:color blue :hint nil)
          "
              ^Org Mode^
--------------------------------------------
          _t_ags   _p_riority
          "
          ("p" org-priority)
          ("t" org-set-tags))
        (require 'org)
        (define-key org-mode-map (kbd "<f2>") 'hydra-org/body)
        )
      (defhydra hydra-yasnippet (:color blue :hint nil)
        "
              ^YASnippets^
--------------------------------------------
  Modes:    Load/Visit:    Actions:

 _g_lobal  _d_irectory    _i_nsert
 _m_inor   _f_ile         _t_ryout
 _e_xtra   _l_ist         _n_ew
         _a_ll
"
        ("d" yas-load-directory)
        ("e" yas-activate-extra-mode)
        ("i" yas-insert-snippet)
        ("f" yas-visit-snippet-file :color blue)
        ("n" yas-new-snippet)
        ("t" yas-tryout-snippet)
        ("l" yas-describe-tables)
        ("g" yas/global-mode)
        ("m" yas/minor-mode)
        ("a" yas-reload-all))

      (bind-key* "<f3>" 'hydra-yasnippet/body)

      (defhydra hydra-apropos (:color blue)
        "Apropos"
        ("a" apropos "apropos")
        ("c" apropos-command "cmd")
        ("d" apropos-documentation "doc")
        ("e" apropos-value "val")
        ("l" apropos-library "lib")
        ("o" apropos-user-option "option")
        ("u" apropos-user-option "option")
        ("v" apropos-variable "var")
        ("i" info-apropos "info")
        ("t" tags-apropos "tags")
        ("z" hydra-customize-apropos/body "customize"))

      (defhydra hydra-customize-apropos (:color blue)
        "Apropos (customize)"
        ("a" customize-apropos "apropos")
        ("f" customize-apropos-faces "faces")
        ("g" customize-apropos-groups "groups")
        ("o" customize-apropos-options "options"))

      (bind-key*  "<f4>" 'hydra-apropos/body)
      )))
