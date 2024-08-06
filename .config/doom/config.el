;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "Ubuntu Mono" :size 14)
      doom-variable-pitch-font (font-spec :family "Ubuntu Mono" :size 15))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-solarized-dark)

(setq display-line-numbers-type 'relative)

(setq delete-by-moving-to-trash t)
(setq trash-directory "~/.trash")

(setq org-directory "~/wiki")
(setq org-roam-directory "~/wiki/vault")
(setq org-roam-dailies-directory "daily")

(add-to-list 'auto-mode-alist '("\\.service\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.timer\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.target\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.mount\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.automount\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.slice\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.socket\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.path\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.netdev\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.network\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.link\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("/etc/ansible/hosts\\'" . yaml-mode))

(after! hl-todo
  (use-package! hl-todo
    :custom
    (hl-todo-keyword-faces
     '(("TODO"   . "goldenrod")
       ("LEGERE"  . "goldenrod")
       ("HITCH"  . "tomato") ;; Overlooked, can be remediated quickly
       ("DEBUG"  . "dark orchid") ;; In process of debugging
       ("BUG"    . "red")
       ("KNOWN-CAUSE" . "royal blue")
       ("STUB"   . "dark red")))))

(after! evil
  (use-package! evil
    :preface
    (defun my/evil-delete-char (orig-fn beg end &optional type _ &rest args)
      "Make x to not write to clipboard."
      (apply orig-fn beg end type ?_ args))
    :custom
    (evil-want-minibuffer t)
    :config
    (advice-add 'evil-delete-char :around 'my/evil-delete-char)))

(after! org
  (use-package! org
    :preface
    (defun my/marginalia-mark-category (seq category)
      "Add category to a completing-read for marginalia use"
      (lambda (str pred flag)
        (pcase flag
          ('metadata
           `(metadata (category . ,category)))
          (_
           (all-completions str seq pred)))))

    (defun my/org-roam-dailies-save-to-file (title)
      "Save to an org roam file"
      (let* ((filename (file-truename (concat org-roam-directory org-roam-dailies-directory "/" title ".org"))))
        (unless (file-exists-p filename)
          (save-current-buffer
            (set-buffer (org-capture-target-buffer filename))
            (insert ":PROPERTIES:\n:ID:        \n:END:\n#+title: " title)
            (goto-char 25)
            (org-id-get-create)
            (write-file filename)
            (org-roam-db-update-file filename)))))

    (defun my/propertize-org-task (level org-todo-keyword raw-value)
      "Give face value to an org todo task"
      (concat (propertize (make-string level ?*) 'face (nth (- level 1) org-level-faces))
              " "
              (propertize org-todo-keyword 'face (cdr (assoc org-todo-keyword org-todo-keyword-faces)))
              " "
              raw-value))
    :config
    (add-to-list 'org-file-apps '("\\.pdf\\'" . "zathura %s"))
    (advice-add #'org-archive-subtree-default :before
                (lambda () (my/org-roam-dailies-save-to-file (format-time-string "%Y-%m-%d"))))
    :custom
    (org-use-fast-todo-selection 'auto)
    (org-startup-folded t)
    (org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(i@/!)" "WAITING(w@/!)" "|" "DONE(d@/!)" "CANCELLED(c@/!)")
                         (sequence "LEGERE(l)" "|" "DONE(d@/!)")
                         (sequence "BUG(b@/!)" "HITCH(h@/!)" "|" "FIXED(f@/!)" "KNOWN-CAUSE(k@/!)" "STUB(s@/!)")))
    (org-todo-keyword-faces
     '(("TODO" . (:foreground "goldenrod" :weight bold))
       ("LEGERE" . (:foreground "goldenrod" :weight bold))
       ("IN-PROGRESS" . (:foreground "royal blue" :weight bold))
       ("WAITING" . (:foreground "tomato" :weight bold))
       ("DONE" . (:foreground "spring green" :weight bold))
       ("CANCELLED" . (:foreground "slate gray" :weight bold))
       ("HITCH" . (:foreground "tomato" :weight bold)) ;; Overlooked, can be remediated quickly
       ("BUG" . (:foreground "red" :weight bold))
       ("FIXED" . (:foreground "spring green" :weight bold))
       ("KNOWN-CAUSE" . (:foreground "royal blue" :weight bold))
       ("STUB" . (:foreground "dark red" :weight bold))))
    (org-log-done 'time)
    (org-log-into-drawer t)
    (org-hide-emphasis-markers t)
    (org-archive-location (file-truename (concat org-directory
                                                 "/vault/daily/"
                                                 (format-time-string "%Y-%m-%d")
                                                 ".org::")))
    (org-archive-subtree-save-file-p t)
    (org-agenda-files '("deanima.org" "inbox.org"))
    (org-agenda-custom-commands
     `(("r" "Readings" todo "LEGERE"
        ((org-agenda-files '(,(expand-file-name "reading_list.org" org-directory)))
         (org-agenda-overriding-header "Readings")
         (org-agenda-prefix-format "\t")))))
    (org-capture-templates
     '(("t" "Task entry")
       ("tt" "Task" entry
        (function (lambda ()
                    (let* ((choice (completing-read "Which org-agenda file? "
                                                    (my/marginalia-mark-category (mapcar #'(lambda (file) (expand-file-name file org-directory)) org-agenda-files)
                                                                                 'file)
                                                    nil nil)))
                      (set-buffer (org-capture-target-buffer choice))
                      (goto-char (point-min)))))
        "* %^{Task|TODO|BUG} %?\n:PROPERTIES:\n:CAPTURED: %U\n:END:"
        :prepend t)
       ("ts" "Subtask" entry
        (function (lambda ()
                    (let* ((ql-queries (org-ql-select (org-agenda-files)
                                           '(and (todo)
                                                 (level 1))
                                           :action 'element-with-markers
                                           :sort 'todo))
                           (queries (mapcar #'(lambda (query)
                                                 (let* ((level (org-element-property :level query))
                                                        (org-todo-keyword (org-element-property :todo-keyword query))
                                                        (raw-value (org-element-property :raw-value query))
                                                        (org-marker (org-element-property :org-marker query)))
                                                   (list
                                                    (my/propertize-org-task level org-todo-keyword raw-value)
                                                    org-marker))) ql-queries))
                           (choice (completing-read "Which task to subtask? " (my/marginalia-mark-category queries 'face) nil nil))
                           (query-marker (cadr (assoc choice queries))))
                      (set-buffer (marker-buffer query-marker))
                      (goto-char (marker-position query-marker)))))
        "* %^{Task|TODO|BUG} %?\n:PROPERTIES:\n:CAPTURED: %U\n:END:"
        :prepend t)
       ("r" "Readings" entry (file "reading_list.org")
        "* LEGERE %^{Title}\n:PROPERTIES:\n:CAPTURED: %U\n:PAGES: %^{Number of pages}\n:LINK: %^{LINK}\n:END:"
        :empty-lines-after 1
        :prepend t))))

  (use-package! org-roam
    :init
    (setq org-roam-v2-ack t)
    :custom
    (org-roam-dailies-capture-templates
     `(("d" "default" entry "\n* %<%I:%M %p> %?"
        :empty-lines-before 1
        :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n")))))

  (use-package evil-org
    :after org
    :hook (org-mode . (lambda () evil-org-mode))
    :config
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys))

  (use-package! ox-extra
    :config
    (ox-extras-activate '(latex-header-blocks ignore-headlines)))

  (use-package! ox-latex
    :config
    ;; code here will run after the package is loaded
    (setq org-latex-pdf-process
          '("pdflatex -interaction nonstopmode -output-directory %o %f"
            "bibtex %b"
            "pdflatex -interaction nonstopmode -output-directory %o %f"
            "pdflatex -interaction nonstopmode -output-directory %o %f"))
    (setq org-latex-with-hyperref nil) ;; stop org adding hypersetup{author..} to latex export
    (setq org-latex-logfiles-extensions
          (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl" "xmpi" "run.xml" "bcf" "acn" "acr" "alg" "glg" "gls" "ist")))
    (unless (boundp 'org-latex-classes)
      (setq org-latex-classes nil))))

(after! company
  (dolist (key '("<return>" "RET"))
    (define-key company-active-map (kbd key)
                `(menu-item nil company-complete
                  :filter ,(lambda (cmd)
                             (when (company-explicit-action-p)
                               cmd)))))
  (map! :map company-active-map "S-RET" #'company-complete-selection)
  (map! :map company-active-map "S-<return>" #'company-complete-selection)
  (define-key company-active-map (kbd "SPC") nil))

(use-package! openwith
  :config
  (add-to-list 'openwith-associations '("\\.pdf\\'" "firejail zathura" (file)))
  (add-to-list 'openwith-associations '("\\.djvu\\'" "firejail zathura" (file)))
  (add-to-list 'openwith-associations '("\\.html\\'" "firefox" (file)))
  (add-to-list 'openwith-associations '("\\.jpg\\'" "nsxiv" (file)))
  (add-to-list 'openwith-associations '("\\.jpeg\\'" "nsxiv" (file)))
  (add-to-list 'openwith-associations '("\\.png\\'" "nsxiv" (file)))
  (openwith-mode t))

;; (use-package! lsp-bridge
;;   :bind* (:map acm-mode-map
;;                ("<tab>" . acm-select-next)
;;                ("<backtab>" . acm-select-prev)
;;                ("C-RET" . acm-complete)
;;                ("C-<return>" . acm-complete)
;;                ("RET" . nil)
;;                ("<return>" . nil)
;;                ("M-h" . nil)
;;                ("C-m" . nil))
;;   :init
;;   (setq lsp-bridge-default-mode-hooks
;;         '(emacs-lisp-mode-hook lisp-interaction-mode-hook rust-mode-hook rust-ts-mode-hook rustic-mode-hook toml-ts-mode-hook go-mode-hook go-ts-mode yaml-ts-mode-hook python-mode-hook python-ts-mode-hook))
;;   (global-lsp-bridge-mode))
;;; config.el ends here
