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

;; Global Modes
(global-subword-mode 1)

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
(fringe-mode nil)

;; Default fringe-mode.
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

;; require
(require 'compile)

;;; Use-package

;; Solarized Theme
(use-package solarized-theme
  :straight t)

;; Emacs adjustment to completion
(use-package emacs
  :straight (:type built-in)
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
  :custom
  (trash-directory "~/.trash")
  (delete-by-moving-to-trash t)
  (compile-command "")
  :config
  (load-theme 'solarized-dark t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (global-set-key (kbd "<f1>") 'compile)
  (global-set-key (kbd "<f2>") 'recompile)
  :bind
  (("C-c @ n" . next-error)
   ("C-c @ p" . previous-error)
   ("C-c @ N" . compilation-next-error)
   ("C-c @ P" . compilation-previous-error)
   ("C-c @ {" . compilation-previous-file)
   ("C-c @ }" . compilation-next-file)
   ("C-c @ b" . next-error-select-buffer)))

;; org-mode
(use-package org
  :straight (:type built-in)
  :init
  (setq org-directory (file-truename "~/wiki"))
  (when (file-readable-p
		 (concat user-emacs-directory "external/org-agenda-ext.el"))
	(load-file
	 (concat user-emacs-directory "external/org-agenda-ext.el")))
  :custom
  (org-use-fast-todo-selection 'expert)
  (org-todo-keywords '((sequence "TODO(t)" "DOING(d@/!)" "WAITING(w@/!)" "|" "HALT(h@/!)" "CANCELLED(c@/!)")
					   (sequence "REPORTED(r)" "BUG(b@/!)" "KNOWN-CAUSE(k@/!)" "|" "FIXED(f@/!)")))
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-archive-location (file-truename (concat org-directory
											   "/vault/daily/"
											   (format-time-string "%Y-%m-%d")
											   ".org::")))
  (org-agenda-custom-commands
   `(("A" "Frontrunner" ,org-custom-daily-agenda)))
  (org-agenda-files '("deanima.org" "proletarii.org" "domus.org" "inbox.org" "hexis.org"))
  (org-capture-templates
   '(("t" "Task" entry
	  (function
	   (lambda ()
		 (interactive)
		 (let ((fpath (concat org-directory "/"
							  (read-answer "File: "
										   '(("deanima" ?d "for my own soul")
											 ("proletarii" ?p "for my line of work")
											 ("domus" ?f "for my girlfriend and family")
											 ("inbox" ?i "for the unknown")))
							  ".org")))
		   (set-buffer (org-capture-target-buffer fpath))))
	   )
	  "* TODO %?\n:PROPERTIES:\n:CAPTURED: %U\n:END:"
	  :empty-lines-before 1)
	 ("r" "Report" entry
	  (function
	   (lambda ()
		 (interactive)
		 (let ((fpath (concat org-directory "/"
							  (read-answer "File: "
										   '(("deanima" ?d "for my own soul")
											 ("proletarii" ?p "for my line of work")
											 ("domus" ?f "for my girlfriend and family")
											 ("inbox" ?i "for the unknown")))
							  ".org")))
		   (set-buffer (org-capture-target-buffer fpath))))
	   )
	  "* REPORTED %?\n:PROPERTIES:\n:CAPTURED: %U\n:END:"
	  :empty-lines-before 1)
	 ("b" "Bug" entry
	  (function
	   (lambda ()
				  (interactive)
				  (let ((fpath (concat org-directory "/"
									   (read-answer "File: "
													'(("deanima" ?d "for my own soul")
													  ("proletarii" ?p "for my line of work")
													  ("domus" ?f "for my girlfriend and family")
													  ("inbox" ?i "for the unknown")))
									   ".org")))
					(set-buffer (org-capture-target-buffer fpath))))
	   )
	  "* BUG %?\n:PROPERTIES:\n:CAPTURED: %U\n:END:"
	  :empty-lines-before 1)))
  :bind
  ("C-c a" . org-custom-agenda)
  ("C-c c" . org-capture))

;; org-roam
(use-package org-roam
  :straight t
  :after org
  :init
  (setq org-roam-v2-ack t)
  (setq org-roam-directory (file-truename "~/wiki/vault"))
  (setq org-roam-dailies-directory "daily")
  :custom
  (org-roam-dailies-capture-templates
   `(("d" "default" entry "\n* %<%I:%M %p> %?"
	  :empty-lines-before 1
	  :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  :config
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode)
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   :map org-roam-dailies-map
   ("Y" . org-roam-dailies-capture-yesterday)
   ("T" . org-roam-dailies-capture-tomorrow)))

;; org-bullets
(use-package org-bullets
  :straight t
  :after org
  :hook
  (org-mode . org-bullets-mode))

;; dired
(use-package dired
  :straight (:type built-in)
  :custom
  (dired-listing-switches "-AXgho --group-directories-first")
  (dired-omit-files "^\\\...+$")
  :hook
  (dired-mode . dired-omit-mode)
  :bind
  (:map dired-mode-map
		("C-c ." . dired-omit-mode)
		("C-c o" . xah-open-in-external-app)))

;; deagrep
(use-package deadgrep
  :straight t
  :bind
  (("<f5>" . deadgrep)))

;; async
(use-package async
  :straight t
  :init
  (dired-async-mode 1)
  :config
  (async-bytecomp-package-mode 1)
  (add-to-list 'display-buffer-alist '("*Async Shell Command*" display-buffer-no-window (nil))))

;; GCMH
(use-package gcmh
  :straight t
  :init
  (setq gcmh-idle-delay 15
		gcmh-high-cons-threshold (* 16 1024 1024))
  :config
  (gcmh-mode))

;; Rainbow-delimiters
(use-package rainbow-delimiters
  :straight t
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; so-long
(use-package so-long
  :defer t
  :straight t
  :config
  (global-so-long-mode 1)
  :bind
  (:map so-long-mode-map
		("C-s" . isearch-forward)
		("C-r" . isearch-backward)))

;; undo-tree
(use-package undo-tree
  :straight t
  :custom
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  :config
  (global-undo-tree-mode))

;; evil-mode
(use-package evil
  :straight t
  :init
  (setq evil-want-keybinding nil)
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
  ;; <tab> cycles org-mode visiblity
  (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)
  (evil-set-undo-system 'undo-tree))

;; evil-collection
(use-package evil-collection
  :after evil
  :straight t
  :config
  (evil-collection-init))

;; flycheck
(use-package flycheck
  :straight t
  :init
  (global-flycheck-mode))

;; magit
(use-package magit
  :straight t)

;; which-key
(use-package which-key
  :straight t
  :init
  (which-key-mode)
  :config
  (setq which-key-popup-type 'minibuffer)
  (setq which-key-idle-dely 0.5))

;; switch-window
(use-package switch-window
  :straight t
  :custom
  (switch-window-input-style 'minibuffer)
  (switch-window-increase 4)
  (switch-window-threshold 2)
  (switch-window-shortcut-style 'qwerty)
  (switch-window-qwerty-shortcuts
   '("a" "s" "d" "f" "j" "k" "l"))
  :bind
  ("C-x o" . switch-window))

;; Minions
(use-package minions
  :straight t
  :init
  (minions-mode 1))

;; Corfu
(use-package corfu
  :straight t
  :demand t
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 3)
  (corfu-auto-delay 0)
  (corfu-echo-documentation 0)
  (corfu-preview-current nil)
  (corfu-quit-no-match 'separator)
  (corfu-separator ?\s)
  :config
  (defun contrib/corfu-enable-always-in-minibuffer ()
	(unless (bound-and-true-p vertico--input)
	  (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'contrib/corfu-enable-always-in-minibuffer 1)
  :bind
  (:map corfu-map
		("<escape>". corfu-quit)
		("<return>" . corfu-insert)
		("C-h" . corfu-show-documentation)
		("M-l" . 'corfu-show-location)
		("RET" . nil)
		("TAB" . corfu-next)
		([tab] . corfu-next)
		("S-TAB" . corfu-previous)
		([backtab] . corfu-previous)))

;; cape
(use-package cape
  :straight t
  :custom
  (cape-dabbrev-min-length 3)
  :config
  (dolist (backend '( cape-symbol cape-keyword cape-file cape-dabbrev))
	(add-to-list 'completion-at-point-functions backend))
  :bind
  (("C-c p p" . completion-at-point)
   ("C-c p d" . cape-dabbrev)
   ("C-c p f" . cape-file)
   ("C-c p s" . cape-symbol)))

;; vertico
(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-resize t)
  :config
  (vertico-mouse-mode)
  :bind
  (:map vertico-map
		("C-j" . vertico-next)
		("C-k" . vertico-previous)
		("C-f" . vertico-exit)
		:map minibuffer-local-map
		("M-h" . backward-kill-word)))

;; vertico-directory
(use-package vertico-directory
  :straight nil
  :load-path "straight/repos/vertico/extensions"
  :after vertico
  :ensure nil
  :bind
  (:map vertico-map
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
  :custom
  (vterm-shell "bash" "Set to bash instead of the default $SHELL so that vterm from TRAMP uses bash.")
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
  :hook
  (vterm-mode . goto-address-mode))

;; vterm-toggle
(use-package vterm-toggle
  :straight t
  :custom
  (vterm-toggle-cd-auto-create-buffer nil)
  (vterm-toggle-fullscreen-p nil)
  :config
  (add-to-list 'display-buffer-alist
			   '((lambda (buffer-or-name _)
				   (let ((buffer (get-buffer buffer-or-name)))
					 (with-current-buffer buffer
					   (or (equal major-mode 'vterm-mode)
						   (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
				 (display-buffer-reuse-window display-buffer-at-bottom)
				 (reusable-frames . visible)
				 (window-height . 0.3)))
  :bind
  ("C-c t" . vterm-toggle)
  ("C-c i" . vterm-toggle-cd-buffer))

;; cc-mode
(use-package cc-mode
  :hook
  (c-mode . (lambda ()
			  (if (file-exists-p "Makefile")
				  (set (make-local-variable 'compile-command) "make -k")
				(set (make-local-variable 'compile-command)
					 ;; emulate make's .c.o implicit pattern rule, but with
					 ;; different defaults for the CC, CPPFLAGS, and CFLAGS
					 ;; variables:
					 ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
					 (let ((file (file-name-nondirectory buffer-file-name)))
					   (format "%s -c -o %s.o %s %s %s"
							   (or (getenv "CC") "gcc")
							   (file-name-sans-extension file)
							   (or (getenv "CPPFLAGS") "-DDEBUG=9")
							   (or (getenv "CFLAGS") "-ansi -pedantic -Wall -g")
							   file))))))
  :bind
  (:map c-mode-map
		("<f1>" . compile)))

;; go-mode
(use-package go-mode
  :straight t
  :mode "\\.go\\'"
  :hook
  (go-mode .
		   (lambda ()
			 (if (file-exists-p "Makefile")
				 (set (make-local-variable 'compile-command) "make -k")
			   (set (make-local-variable 'compile-command)
					;; /usr/bin/go test $@
					"/usr/bin/go test ./...")))))

;; docker-mode
(use-package dockerfile-mode
  :straight t)

;; jinja2 -mode
(use-package jinja2-mode
  :straight t)

;; yml-mode
(use-package yaml-mode
  :straight t
  :hook
  (yaml-mode . (lambda ()
				(define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(provide 'config)
;;; config.el ends here
