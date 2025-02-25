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

(setq org-directory "~/org")
(setq org-roam-directory "~/org/vault")
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
    :config
    (setq org-publish-project-alist
        '(
          ("org-vincnt054.github.io"
           ;; path to org files.
           :base-directory "~/workspace/vincnt054.github.io/org"
           :base-extension "org"
           ;; path to jekyll posts
           :publishing-directory "~/workspace/vincnt054.github.io/jekyll"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4
           :html-extension "html"
           :body-only t)

          ("asset-vincnt054.github.io" ;; my blog project (just a name)
           :base-directory "~/workspace/vincnt054.github.io/org"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php"
           :publishing-directory "~/workspace/vincnt054.github.io/jekyll"
           :recursive t
           :publishing-function org-publish-attachment)

          ("vincnt054.github.io" :components ("org-vincnt054.github.io" "asset-vincnt054.github.io"))
          )))

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
    (evil-org-agenda-set-keys)))

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
