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

;; Don't open a new Async Shell Command window
(add-to-list 'display-buffer-alist '("*Async Shell Command*" . (display-buffer-no-window . nil)))

(after! org
  (use-package! org
    :custom
    (org-use-fast-todo-selection 'expert)
    (org-todo-keywords '((sequence "TODO(t)" "IN PROGRESS(i@/!)" "WAITING(w@/!)" "|" "DONE(d@/!)" "CANCELLED(c@/!)")
                         (sequence "BUG(b)" "KNOWN-CAUSE(k)" "|" "FIXED(f@/!)" "LOOPHOLE(l@/!)")))
    (org-todo-keyword-faces
     '(("TODO" . (:foreground "goldenrod" :weight bold))
       ("IN PROGRESS" . (:foreground "royal blue" :weight bold))
       ("WAITING" . (:foreground "tomato" :weight bold))
       ("DONE" . (:foreground "spring green" :weight bold))
       ("CANCELLED" . (:foreground "slate gray" :weight bold))
       ("BUG" . (:foreground "peru" :background "dark red" :weight bold))
       ("KNOWN-CAUSE" . (:foreground "steel blue" :background "gainsboro" :weight bold))
       ("FIXED" . (:foreground "spring green" :weight bold))
       ("LOOPHOLE" . (:foreground "salmon" :weight bold))))
    (org-log-done 'time)
    (org-log-into-drawer t)
    (org-hide-emphasis-markers t)
    (org-archive-location (file-truename (concat org-directory
                                                 "/vault/daily/"
                                                 (format-time-string "%Y-%m-%d")
                                                 ".org::")))
    (org-agenda-files '("deanima.org" "proletarii.org" "domus.org" "inbox.org" "hexis.org"))
    (org-capture-templates
     '(("t" "Task" entry
        (function (lambda ()
                    (interactive)
                    (let ((fpath (concat org-directory "/"
                                         (read-answer "File: "
                                                      '(("deanima" ?d "for my own soul")
                                                        ("proletarii" ?p "for my line of work")
                                                        ("domus" ?f "for my girlfriend and family")
                                                        ("inbox" ?i "for the unknown")))
                                         ".org")))
                      (set-buffer (org-capture-target-buffer fpath)))))
        "* %?\n:PROPERTIES:\n:CAPTURED: %U\n:END:"
        :empty-lines-before 1))))
  (use-package! org-roam
    :init
    (setq org-roam-v2-ack t)
    (setq org-roam-dailies-directory "daily")
    :custom
    (org-roam-dailies-capture-templates
     `(("d" "default" entry "\n* %<%I:%M %p> %?"
        :empty-lines-before 1
        :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n")))))
  )

(after! dired-x
  (setq dired-guess-shell-alist-user
        '(("\\.pdf\\'" "zathura")
          ("\\.djvu\\'" "zathura")
          ("\\.jpeg\\'" "nsxiv")
          ("\\.jpg\\'" "nsxiv")
          ("\\.png\\'" "nsxiv")
          ("\\.gif\\'" "nsxiv")
          ("\\.tex\\'" "pdflatex")
          ("\\.html?\\'" "firefox")))
  )

(map! "<f9>" #'vterm
      "<f10>" #'magit
      "<f11>" #'recompile
      "<f12>" #'compile)

;;; config.el ends here
