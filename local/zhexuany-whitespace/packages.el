(setq zhexuany-whitespace-packages
      '(
        (whitespace :location built-in)
        ))

(defun zhexuany-whitespace/post-init-whitespace ()
  (use-package whitespace
    :init
    (progn
      ;; ;; http://emacsredux.com/blog/2013/05/31/highlight-lines-that-exceed-a-certain-length-limit/
      (setq whitespace-line-column fill-column) ;; limit line length
      ;;https://www.reddit.com/r/emacs/comments/2keh6u/show_tabs_and_trailing_whitespaces_only/
      (setq whitespace-display-mappings
            ;; all numbers are Unicode codepoint in decimal. try (insert-char 182 ) to see it
            '(
              (space-mark 32 [183] [46]) ; 32 SPACE, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
              (newline-mark 10 [182 10]) ; 10 LINE FEED
              (tab-mark 9 [187 9] [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
              ))
      (setq whitespace-style '(face tabs trailing tab-mark ))
      ;; (setq whitespace-style '(face lines-tail))
      ;; show tab;  use untabify to convert tab to whitespace
      ;; (setq spacemacs-show-trailing-whitespace nil)

      (setq-default tab-width 4)
      ;; set-buffer-file-coding-system -> utf8 to convert dos to utf8
      (setq inhibit-eol-conversion t)
      (add-hook 'prog-mode-hook 'whitespace-mode)
      ;; (global-whitespace-mode +1)

      (with-eval-after-load 'whitespace
        (progn
          (set-face-attribute 'whitespace-tab nil
                              :background "#Adff2f"
                              :foreground "#00a8a8"
                              :weight 'bold)
          (set-face-attribute 'whitespace-trailing nil
                              :background "#e4eeff"
                              :foreground "#183bc8"
                              :weight 'normal)))
      ))
  (diminish 'whitespace-mode))
