(setq zhexuan-org-packages
      '(org
        org-mac-link
        ))

;; Thanks for zilongshanren. For org mode, almost code was copied from him.
;;In order to export pdf to support Chinese, I should install Latex at here: https://www.tug.org/mactex/
;; http://freizl.github.io/posts/2012-04-06-export-orgmode-file-in-Chinese.html
;;http://stackoverflow.com/questions/21005885/export-org-mode-code-block-and-result-with-different-styles
(defun zhexuan-org/post-init-org ()
  (progn
    (spacemacs|disable-company org-mode)
    ;; (when (configuration-layer/package-usedp 'company)
    ;; (spacemacs|add-company-hook org-mode))

    (require 'org-compat)
    (require 'org)
    ;; (add-to-list 'org-modules "org-habit")
    (add-to-list 'org-modules 'org-habit)
    (require 'org-habit)

    ;; define the refile targets
    (setq org-agenda-files (quote ("~/Dropbox/ORG" )))
    (setq org-refile-use-outline-path 'file)
    (setq org-outline-path-complete-in-steps nil)
    (setq org-refile-targets
          '((nil :maxlevel . 4)
            (org-agenda-files :maxlevel . 4)))
    ;; config stuck project
    (setq org-stuck-projects
          '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:"))

    (setq org-agenda-inhibit-startup t)       ;; ~50x speedup
    (setq org-agenda-use-tag-inheritance nil) ;; 3-4x speedup
    (setq org-agenda-window-setup 'current-window)
    (setq org-log-done t)

    (add-to-list 'auto-mode-alist '("\\.org\\’" . org-mode))

    (setq org-mobile-directory "~/Dropbox/ORG")


    (setq org-todo-keywords
          (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
                  (sequence "WAITING(w@/!)" "SOMEDAY(S)"  "|" "CANCELLED(c@/!)" "MEETING(m)" "PHONE(p)"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Org clock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; Change task state to STARTED when clocking in
    (setq org-clock-in-switch-to-state "STARTED")
    ;; Save clock data and notes in the LOGBOOK drawer
    (setq org-clock-into-drawer t)
    ;; Removes clocked tasks with 0:00 duration
    (setq org-clock-out-remove-zero-time-clocks t) ;; Show the clocked-in task - if any - in the header line


    (setq org-default-notes-file "~/Dropbox/ORG/gtd.org")

    ;; the %i would copy the selected text into the template
    ;;http://www.howardism.org/Technical/Emacs/journaling-org.html
    ;;add multi-file journal
    (setq org-capture-templates
          '(("t" "Todo" entry (file+headline "~/Dropbox/ORG/gtd.org" "Daily Tasks")
             "* TODO %?\n  %i\n"
             :empty-lines 1)
            ("n" "notes" entry (file+headline "~/Dropbox/ORG/notes.org" "Quick notes")
             "* TODO %?\n  %i\n %U"
             :empty-lines 1)
            ("b" "Blog Ideas" entry (file+headline "~/Dropbox/ORG/notes.org" "Blog Ideas")
             "* TODO %?\n  %i\n %U"
             :empty-lines 1)
            ("w" "work" entry (file+headline "~/Dropbox/ORG/gtd.org" "Cocos2D-X")
             "* TODO %?\n  %i\n %U"
             :empty-lines 1)
            ("j" "Journal Entry"
             entry (file+datetree "~/Dropbox/ORG/journal.org")
             "* %?"
             :empty-lines 1)))

    (setq org-tags-match-list-sublevels nil)
    (setq org-agenda-custom-commands
          '(
            ("w" . "任务安排")
            ("wa" "重要且紧急的任务" tags-todo "+PRIORITY=\"A\"")
            ("wb" "重要且不紧急的任务" tags-todo "+PRIORITY=\"B\"")
            ("b" "Blog" tags-todo "BLOG")
            ("p" . "项目安排")
            ("pw" tags-todo "PROJECT+WORK+CATEGORY=\"cocos2d-x\"")
            ("pl" tags-todo "PROJECT+DREAM+CATEGORY=\"zhexuan-org\"")
            ("W" "Weekly Review"
             ((stuck "") ;; review stuck projects as designated by org-stuck-projects
              (tags-todo "PROJECT") ;; review all projects (assuming you use todo keywords to designate projects)
              ))))

    (defun org-summary-todo (n-done n-not-done)
      "Switch entry to DONE when all subentries are done, to TODO otherwise."
      (let (org-log-done org-log-states) ; turn off logging
        (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

    (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
    ;; used by org-clock-sum-today-by-tags
    (defun filter-by-tags ()
      (let ((head-tags (org-get-tags-at)))
        (member current-tag head-tags)))

    (defun org-clock-sum-today-by-tags (timerange &optional tstart tend noinsert)
      (interactive "P")
      (let* ((timerange-numeric-value (prefix-numeric-value timerange))
             (files (org-add-archive-files (org-agenda-files)))
             (include-tags '("WORK" "EMACS" "DREAM" "WRITING" "MEETING"
                             "LIFE" "PROJECT" "OTHER"))
             (tags-time-alist (mapcar (lambda (tag) `(,tag . 0)) include-tags))
             (output-string "")
             (tstart (or tstart
                         (and timerange (equal timerange-numeric-value 4) (- (org-time-today) 86400))
                         (and timerange (equal timerange-numeric-value 16) (org-read-date nil nil nil "Start Date/Time:"))
                         (org-time-today)))
             (tend (or tend
                       (and timerange (equal timerange-numeric-value 16) (org-read-date nil nil nil "End Date/Time:"))
                       (+ tstart 86400)))
             h m file item prompt donesomething)
        (while (setq file (pop files))
          (setq org-agenda-buffer (if (file-exists-p file)
                                      (org-get-agenda-file-buffer file)
                                    (error "No such file %s" file)))
          (with-current-buffer org-agenda-buffer
            (dolist (current-tag include-tags)
              (org-clock-sum tstart tend 'filter-by-tags)
              (setcdr (assoc current-tag tags-time-alist)
                      (+ org-clock-file-total-minutes (cdr (assoc current-tag tags-time-alist)))))))
        (while (setq item (pop tags-time-alist))
          (unless (equal (cdr item) 0)
            (setq donesomething t)
            (setq h (/ (cdr item) 60)
                  m (- (cdr item) (* 60 h)))
            (setq output-string (concat output-string (format "[-%s-] %.2d:%.2d\n" (car item) h m)))))
        (unless donesomething
          (setq output-string (concat output-string "[-Nothing-] Done nothing!!!\n")))
        (unless noinsert
          (insert output-string))
        output-string))


    ;; http://wenshanren.org/?p=327
    ;; change it to helm
    (defun zhexuan-org/org-insert-src-block (src-code-type)
      "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
      (interactive
       (let ((src-code-types
              '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
                "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
                "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
                "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
                "scheme" "sqlite")))
         (list (ido-completing-read "Source code type: " src-code-types))))
      (progn
        (newline-and-indent)
        (insert (format "#+BEGIN_SRC %s\n" src-code-type))
        (newline-and-indent)
        (insert "#+END_SRC\n")
        (previous-line 2)
        (org-edit-src-code)))

    (add-hook 'org-mode-hook '(lambda ()
                                ;; keybinding for editing source code blocks
                                ;; keybinding for inserting code blocks
                                (local-set-key (kbd "C-c i s")
                                               'zhexuan-org/org-insert-src-block)
                                ))
    (require 'ox-publish)
    (add-to-list 'org-latex-classes '("ctexart" "\\documentclass[11pt]{ctexart}
                                        [NO-DEFAULT-PACKAGES]
                                        \\usepackage[utf8]{inputenc}
                                        \\usepackage[T1]{fontenc}
                                        \\usepackage{fixltx2e}
                                        \\usepackage{graphicx}
                                        \\usepackage{longtable}
                                        \\usepackage{float}
                                        \\usepackage{wrapfig}
                                        \\usepackage{rotating}
                                        \\usepackage[normalem]{ulem}
                                        \\usepackage{amsmath}
                                        \\usepackage{textcomp}
                                        \\usepackage{marvosym}
                                        \\usepackage{wasysym}
                                        \\usepackage{amssymb}
                                        \\usepackage{booktabs}
                                        \\usepackage[colorlinks,linkcolor=black,anchorcolor=black,citecolor=black]{hyperref}
                                        \\tolerance=1000
                                        \\usepackage{listings}
                                        \\usepackage{xcolor}
                                        \\lstset{
                                        %行号
                                        numbers=left,
                                        %背景框
                                        framexleftmargin=10mm,
                                        frame=none,
                                        %背景色
                                        %backgroundcolor=\\color[rgb]{1,1,0.76},
                                        backgroundcolor=\\color[RGB]{245,245,244},
                                        %样式
                                        keywordstyle=\\bf\\color{blue},
                                        identifierstyle=\\bf,
                                        numberstyle=\\color[RGB]{0,192,192},
                                        commentstyle=\\it\\color[RGB]{0,96,96},
                                        stringstyle=\\rmfamily\\slshape\\color[RGB]{128,0,0},
                                        %显示空格
                                        showstringspaces=false
                                        }
                                        "
                                      ("\\section{%s}" . "\\section*{%s}")
                                      ("\\subsection{%s}" . "\\subsection*{%s}")
                                      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                      ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                      ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    ))
