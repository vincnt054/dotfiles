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
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-solarized-dark)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

(setq delete-by-moving-to-trash t)
(setq trash-directory "~/.trash")

(setq org-directory "~/wiki")
(setq org-roam-directory "~/wiki/vault")

(after! org
  (use-package! org
    :custom
    (org-use-fast-todo-selection 'expert)
    (org-todo-keywords '((sequence "TODO(t)" "IN PROGRESS(i@/!)" "WAITING(w@/!)" "|" "DONE(d@/!)" "CANCELLED(c@/!)")
                         (sequence "RIAGE(r@/!)" "BUG(b@/!)" "|" "FIXED(f@/!)" "KNOWN-CAUSE(k@/!)")))
    (org-todo-keyword-faces
     '(("TODO" . (:foreground "goldenrod" :weight bold))
       ("IN PROGRESS" . (:foreground "royal blue" :weight bold))
       ("WAITING" . (:foreground "tomato" :weight bold))
       ("DONE" . (:foreground "spring green" :weight bold))
       ("CANCELLED" . (:foreground "slate gray" :weight bold))
       ("RIAGE" . (:foreground "steel blue" :background "gainsboro" :weight bold))
       ("BUG" . (:foreground "peru" :background "dark red" :weight bold))
       ("FIXED" . (:foreground "spring green" :weight bold))
       ("KNOWN-CAUSE" . (:foreground "salmon" :weight bold))))
    (org-log-done 'time)
    (org-log-into-drawer t)
    (org-hide-emphasis-markers t)
    (org-archive-location (file-truename (concat org-directory
                                                 "/vault/daily/"
                                                 (format-time-string "%Y-%m-%d")
                                                 ".org::")))
    (org-agenda-files '("deanima.org" "inbox.org"))
    (org-capture-templates
     '(("t" "Task" entry
        (function (lambda ()
                    (interactive)
                    (let ((fpath (concat org-directory "/"
                                         (read-answer "File: "
                                                      '(("deanima" ?d "for my own soul")
                                                        ("inbox" ?i "for the unknown")))
                                         ".org")))
                      (set-buffer (org-capture-target-buffer fpath)))))
        "* TODO %?\n:PROPERTIES:\n:CAPTURED: %U\n:END:"
        :empty-lines-before 1))))
  (use-package! org-roam
    :init
    (setq org-roam-v2-ack t)
    (setq org-roam-dailies-directory "daily")
    :custom
    (org-roam-dailies-capture-templates
     `(("d" "default" entry "\n* %<%I:%M %p> %?"
        :empty-lines-before 1
        :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))))

(after! company
  (dolist (key '("<return>" "RET"))
    (define-key company-active-map (kbd key)
                `(menu-item nil company-complete
                  :filter ,(lambda (cmd)
                             (when (company-explicit-action-p)
                               cmd)))))
  (map! :map company-active-map "C-RET" #'company-complete-selection)
  (map! :map company-active-map "C-<return>" #'company-complete-selection)
  (define-key company-active-map (kbd "SPC") nil))

(after! dired-x
  (setq dired-guess-shell-alist-user
        '(("\\.pdf\\'" "zathura")
          ("\\.djvu\\'" "zathura")
          ("\\.jpeg\\'" "nsxiv")
          ("\\.jpg\\'" "nsxiv")
          ("\\.png\\'" "nsxiv")
          ("\\.gif\\'" "nsxiv")
          ("\\.tex\\'" "pdflatex")
          ("\\.html?\\'" "firefox"))))

(after! simple
  (set-popup-rule! "^\\*Async Shell Command"
                     :side 'bottom
                     :slot 1
                     :vslot 1
                     :ttl nil
                     :quit 'other
                     :select nil
                     :modeline nil))

(after! sage-shell-mode
  (use-package! sage-shell-mode
    :init
    (sage-shell:define-alias)
    (custom-set-variables
     '(sage-shell:use-prompt-toolkit nil)
     '(sage-shell:use-simple-prompt t)
     '(sage-shell:set-ipython-version-on-startup nil)
     '(sage-shell:check-ipython-version-on-startup nil))
    :config
    (set-popup-rule! "*Sage*"
      :side 'bottom
      :size 0.3
      :slot 1
      :vslot 2
      :ttl nil
      :quit nil
      :select nil
      :modeline t)))

(map! "C-x C-g" #'find-file-other-window
      "<f5>" #'consult-ripgrep
      "<f6>" #'consult-git-grep
      "<f7>" #'consult-compile-error
      "<f8>" #'consult-flymake
      "<f9>" #'magit
      "<f10>" #'vterm
      "<f11>" #'recompile
      "<f12>" #'compile)

;;; config.el ends here
