;;; config.el --- Configuration
;;; Commentary:

;;; Code:

;; Garbage Collection
(defun my-minibuffer-setup-hook ()
  "GC will never occur."
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  "GC will kick off."
  (setq gc-cons-threshold gc-cons-threshold-original))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; Async compilations focus suppress
(setq warning-suppress-types '((comp)))

;; Ad-redef warnings off
(setq ad-redefinition-action 'accept)

;; Speed up cursor operations
(setq auto-window-vscroll nil)

;; Default custom file
(setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file nil t)

;; Load custom themes in `~/.emacs.d/themes'
(when (file-exists-p (expand-file-name "themes/" user-emacs-directory))
  (setq custom-sage-themes t)
  (add-to-list 'custom-theme-load-path (expand-file-name "themes/" user-emacs-directory)))

;; No confirmation opening when symlinked file
(setq vc-follow-symlinks t)

;; Disable backup files
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq vc-make-backup-files nil)

;; UTF-8
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Pager
(setenv "PAGER" "cat")

;; Ring-bell disabled
(setq ring-bell-function 'ignore)

;; Scrolling
(setq scroll-margin 0)
(setq scroll-conservatively 100000)
(setq scroll-preserve-screen-position 1)
(setq fast-but-imprecise-scrolling t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; Copy and paste
(setq select-enable-clipboard t)
(setq save-interprogram-paste-before-kill t)

;; Paren-mode
(setq show-paren-delay 0
	  show-paren-style 'parenthesis)
(show-paren-mode 1)

;; Bracket pair-matching
(setq electric-pair-pairs '((?\{ . ?\})
							(?\( . ?\))
							(?\[ . ?\])
							(?\" . ?\")))
(electric-pair-mode t)

;; Indentation
(setq-default tab-width 4)
(setq-default standard-indent 4)
(setq-default indent-tabs-mode t)
(setq-default electric-indent-inhibit nil)
(setq backward-delete-char-untabify-method 'nil)
(electric-indent-mode t) ;; Auto indentation

;; C tabs, braces, subword-mode
(setq c-default-style "linux")
(setq c-basic-offset tab-width)

(global-subword-mode 1)
(add-hook 'c-mode-common-hook
		  (lambda () (subword-mode 1)))

;; Fill-column
(setq-default fill-column 80)

;; Linum
(setq linum-format "%4d ")

;; Show trailing whitespace
(add-hook 'prog-mode-hook
		  (lambda ()
			(setq show-trailing-whitespace t)))

;; Unique buffer and Window title
(setq-default frame-title-format '("%b"))
(setq-default uniquify-buffer-name-style 'forward)

;; Internal Border Width
(add-to-list 'default-frame-alist '(internal-border-width . 0))

;; Fill space by WM
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)

;; Disable default startup screen
(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;; Delete selection
(delete-selection-mode 1)

;; Fringe
(fringe-mode nil) ;; Default fringe-mode.
(setq-default fringes-outside-margins nil)
(setq-default indicate-buffer-boundaries nil)
(setq-default indicate-empty-lines nil)
(setq-default overflow-newline-into-fringe t)

;; Column-number-mode
(column-number-mode 1)

;; Global-highlight-line-mode
(global-hl-line-mode nil)

;; Line-numbers-mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)

;; Enable visual-line-mode for text buffers & org mode
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

;; Aliases
(defalias 'yes-or-no-p 'y-or-n-p)

;;; Use-package

;; org-roam
(use-package org-roam
  :straight t
  :init
  (setq org-roam-v2-ack t)
  (org-roam-directory "~/wiki")
  :custom
  (setq org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%H:%M> %?"
	  :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   :map org-roam-dailies-map
   ("Y" . org-roam-dailies-capture-yesterday)
   ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies)
  (org-roam-setup))

;; org-mode
(use-package org
  :straight (:type built-in)
  :init
  (with-eval-after-load 'org
    (setq org-directory "~/wiki"))
  :config
  (setq org-todo-keywords '((sequence "TODO" "STARTED" "DOING" "WAITING" "|" "DONE" "CANCELLED")))
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-archive-location (concat "daily/"
									 (format-time-string "%Y-%m-%d")
									 ".org::"))
  (setq org-agenda-files '("agenda.org" "inbox.org"))
  :bind
  ("C-c a" . org-agenda))

;; dired
(use-package dired
  :straight (:type built-in)
  :commands (dired dired-jump open-in-external-app)
  :bind
  ("C-x C-j" . dired-jump)
  :custom ((dired-listing-switches "-agho --group-directories-first")))

;; Async
(use-package async
  :straight t
  :demand t
  :init
  (dired-async-mode 1)
  :config
  (async-bytecomp-package-mode 1)
  (add-to-list 'display-buffer-alist '("*Async Shell Command*" display-buffer-no-window (nil))))

;; GCMH
(use-package gcmh
  :straight t
  :ensure t
  :init
  (setq gcmh-idle-delay 15
		gcmh-high-cons-threshold (* 16 1024 1024))
  :config
  (gcmh-mode))

;; Diminish
(use-package diminish
  :straight t
  :init
  (diminish 'auto-revert-mode)
  (diminish 'flycheck-mode)
  (diminish 'abbrev-mode)
  (diminish 'subword-mode)
  (diminish 'visual-line-mode)
  (diminish 'outline-mode)
  (diminish 'gcmh-mode)
  (diminish 'eldoc-mode)
  :config
  (with-eval-after-load "undo-tree" (diminish 'undo-tree-mode))
  (eval-after-load "evil-collection-unimpaired-mode" '(diminish 'evil-collection-unimpaired-mode))
  (eval-after-load "c-mode" '(diminish 'c-mode))
  (eval-after-load "c++-mode" '(diminish 'c++-mode))
  (eval-after-load "which-key" '(diminish 'which-key-mode))
  (eval-after-load "outline" '(diminish 'outline-minor-mode))
  (eval-after-load "dired" '(diminish 'dired-async-mode))
  (eval-after-load "dired" '(diminish 'dired-hide-dotfiles-mode))
  (eval-after-load "dired" '(diminish 'all-the-icons-dired-mode))
  (eval-after-load "magit" '(diminish 'auto-fill-mode))
  (eval-after-load "magit" '(diminish 'with-editor-mode))
  (eval-after-load "auto-revert-mode" '(diminish 'auto-revert-mode)))

;; Emacs adjustment to completion
(use-package emacs
  :init
  (defun crm-indicator (args)
	(cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  (setq minibuffer-prompt-properties
		'(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq read-extended-command-predicate
		#'command-completion-default-include-p)
  (setq enable-recursive-minibuffers t)
  (setq completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq resize-mini-windows t)
  :config
  (load-theme 'solarized-dark t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

;; Rainbow-delimiters
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

;; so-long
(use-package so-long
  :defer t
  :straight t
  :bind
  (:map so-long-mode-map
		("C-s" . isearch-forward)
		("C-r" . isearch-backward))
  :config (global-so-long-mode 1))

;; undo-tree
(use-package undo-tree
  :straight t
  :ensure t
  :config
  (global-undo-tree-mode)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

;; evil-collection
(use-package evil-collection
  :straight t
  :ensure t
  :after evil
  :diminish evil-collection-unimpaired-mode
  :init
  (evil-collection-init))

;; evil-mode
(use-package evil
  :straight t
  :defer nil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :config
  ;; enable evil-mode
  (evil-mode 1)
  ;; more granular undo with evil
  (setq evil-want-fine-undo t)
  ;; set evil state on a per mode basis
  ;; insert
  (evil-set-initial-state 'vterm-mode 'insert)
  ;; normal
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  ;; emacs
  (evil-set-initial-state 'term-mode 'emacs)
  (evil-set-initial-state 'eshell-mode 'emacs)
  (evil-set-initial-state 'inferior-scheme-mode 'emacs)
  ;; <tab> cycles org-mode visiblity
  (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)
  (evil-set-undo-system 'undo-tree)
  ;; :q kills buffer
  (evil-ex-define-cmd "q" 'delete-window))

;; flycheck
(use-package flycheck
  :straight t
  :init (global-flycheck-mode)
  :hook (prog-mode . flycheck-mode))

;; magit
(use-package magit
  :straight t
  :ensure t)

;; which-key
(use-package which-key
  :straight t
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-dely 0.3))

;; switch-window
(use-package switch-window
  :straight t
  :config
  (setq switch-window-input-style 'minibuffer)
  (setq switch-window-increase 4)
  (setq switch-window-threshold 2)
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-qwerty-shortcuts
		'("a" "s" "d" "f" "j" "k" "l"))
  :bind
  ("C-x o" . switch-window))

;; Corfu
(use-package corfu
  :straight t
  :demand t
  :bind (:map corfu-map
			  ("<escape>". corfu-quit)
			  ("<return>" . corfu-insert)
			  ("C-h" . corfu-show-documentation)
			  ("M-l" . 'corfu-show-location)
			  ("RET" . nil)
			  ("TAB" . corfu-next)
			  ([tab] . corfu-next)
			  ("S-TAB" . corfu-previous)
			  ([backtab] . corfu-previous))
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 3)
  (corfu-auto-delay 0)
  (corfu-echo-documentation 0)
  (corfu-preview-current nil)
  (corfu-quit-no-match 'separator)
  (corfu-separator ?\s)
  :init (global-corfu-mode)
  :config
  (defun contrib/corfu-enable-always-in-minibuffer ()
	(unless (bound-and-true-p vertico--input)
	  (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'contrib/corfu-enable-always-in-minibuffer 1))

;; cape
(use-package cape
  :straight t
  :bind (("C-c p p" . completion-at-point)
		 ("C-c p d" . cape-dabbrev)
		 ("C-c p f" . cape-file)
		 ("C-c p s" . cape-symbol))
  :config
  (setq cape-dabbrev-min-length 3)
  (dolist (backend '( cape-symbol cape-keyword cape-file cape-dabbrev))
	(add-to-list 'completion-at-point-functions backend)))

;; vertico
(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :ensure t
  :bind (:map vertico-map
			  ("C-j" . vertico-next)
			  ("C-k" . vertico-previous)
			  ("C-f" . vertico-exit)
			  :map minibuffer-local-map
			  ("M-h" . backward-kill-word))
  :custom
  (vertico-cycle t)
  (vertico-resize t)
  :init
  (vertico-mode)
  :config
  (vertico-mouse-mode))

(use-package vertico-directory
  :straight nil
  :load-path "straight/repos/vertico/extensions"
  :after vertico
  :ensure nil
  :bind (:map vertico-map
			  ("RET" . vertico-directory-enter)
			  ("DEL" . vertico-directory-delete-char)
			  ("M-DEL" . vertico-directory-delete-word)))

;; marginalia
(use-package marginalia
  :straight t
  :after vertico
  :init
  (marginalia-mode))

;; Vterm
(use-package vterm
  :straight t
  :ensure t
  :config
  (define-key vterm-mode-map (kbd "<f1>") nil)
  (define-key vterm-mode-map (kbd "<f2>") nil)
  (define-key vterm-mode-map (kbd "<f3>") nil)
  (define-key vterm-mode-map (kbd "<f4>") nil)
  (define-key vterm-mode-map (kbd "<f5>") nil)
  (define-key vterm-mode-map (kbd "<f6>") nil)
  (define-key vterm-mode-map (kbd "<f7>") nil)
  (define-key vterm-mode-map (kbd "<f8>") nil)
  (define-key vterm-mode-map (kbd "<f9>") nil)
  (define-key vterm-mode-map (kbd "<f10>") nil)
  (define-key vterm-mode-map (kbd "<f11>") nil)
  (define-key vterm-mode-map (kbd "<f12>") nil)
  :custom
  (vterm-shell "bash" "Set to bash instead of the default $SHELL so that vterm from TRAMP uses bash.")
  :hook
  (vterm-mode . goto-address-mode))

(use-package vterm-toggle
  :straight t
  :config
  ;; show vterm buffer in side window
  (add-to-list 'display-buffer-alist
               '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side . bottom)
                 (dedicated . t)
                 ;; (reusable-frames . visible)
                 (window-height . 0.3)))
  :bind
  ("C-c t" . vterm-toggle)
  ("C-c i" . (lambda ()
			   (interactive)
			   (vterm-toggle-cd)
			   (vterm-toggle-insert-cd))))

(use-package go-mode
  :straight t
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode)))

(use-package yaml-mode
  :straight t
  :config
  (add-hook 'yaml-mode-hook
			'(lambda ()
			   (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(server-start)
(provide 'config)
;;; config.el ends here
