;;; init.el --- Init file                            -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:

(setq debug-on-error t)
(setq load-prefer-newer t)

(require 'cl-lib)

(require 'package)
(setq package-archives
      `(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")))
(setq package-enable-at-startup nil)
(package-initialize)

(require 'use-package)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(use-package exec-path-from-shell
  :if (eq window-system 'ns)
  :config
  (exec-path-from-shell-initialize))

(cond
 ((eq system-type 'darwin)
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs)))

;; mode on
(line-number-mode 1)
(column-number-mode 1)
(transient-mark-mode 1)
(delete-selection-mode 1)
(auto-compression-mode 1)
(auto-image-file-mode 1)
(temp-buffer-resize-mode 1)
(electric-indent-mode 1)
(electric-pair-mode 1)
(global-auto-revert-mode 1)

;; mode off
(unless (eq window-system 'ns)
  (menu-bar-mode -1))
(blink-cursor-mode -1)
(when window-system
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1))

;; misc
(require 'generic-x)
(setq system-time-locale "C")
(setq visible-bell t)
(setq ring-bell-function #'ignore)
(setq inhibit-startup-screen t)
(setq inhibit-default-init t)
(setq require-final-newline t)
(setq read-file-name-completion-ignore-case nil)
(setq gc-cons-threshold 8000000)
(setq mouse-drag-copy-region t)
(setq line-spacing 1)

(setq custom-file (locate-user-emacs-file "custom.el"))

;; Split window horizontally
(setq split-height-threshold nil)

(when window-system
  (setq-default indicate-empty-lines t))

(setq-default indent-tabs-mode nil)
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    ;; Emacs Lisp Coding Conventions
	    ;; Indent the file using the default indentation parameters.
	    (setq indent-tabs-mode t)))

(cond
 ((eq window-system 'mac)
  ;; C-j を IME に渡さない
  (setq mac-pass-control-to-system nil))
 ((eq window-system 'ns)
  (defvar ns-alternate-modifier)
  (defvar ns-command-modifier)
  (setq ns-alternate-modifier 'meta)
  (setq ns-command-modifier 'meta)))

;; Avoid "Symbolic link to VC-controlled source file; follow link? (yes or no)"
(setq vc-follow-symlinks t)

;; Single backup directory
(setq backup-directory-alist
      `(("." . ,(locate-user-emacs-file "backup"))))

;; bindings
(keyboard-translate ?\C-h ?\C-?)
(keyboard-translate ?\C-? ?\C-h)
(bind-key "C-c h" 'help-command)
(bind-key "C-c C-c" 'comment-region)
(bind-key "C-x C-b" 'ibuffer)

(use-package dired
  :defer t
  :config
  (setq dired-listing-switches "-AFlo")
  (setq dired-ls-F-marks-symlinks t)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-dwim-target t)

  (use-package dired-x
    :init
    (setq dired-omit-size-limit nil)
    (setq dired-omit-verbose nil)
    (setq dired-omit-files
	  "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^__pycache__$")
    :config
    (add-hook 'dired-mode-hook 'dired-omit-mode)))

(use-package outline
  :defer t
  :config
  (setq outline-minor-mode-prefix (kbd "C-c C-o")))

(use-package paren
  :init
  (show-paren-mode 1)
  :config
  (setq show-paren-delay 0)
  (setq show-paren-style 'expression))

;; savehist
(setq history-length t)
(savehist-mode 1)

;; uniquify
(setq uniquify-buffer-name-style 'forward)

;; browse-url
(bind-key "C-c C-z ." 'browse-url-at-point)
(bind-key "C-c C-z b" 'browse-url-of-buffer)
(bind-key "C-c C-z r" 'browse-url-of-region)
(bind-key "C-c C-z u" 'browse-url)
(bind-key "C-c C-z v" 'browse-url-of-file)
(use-package dired
  :defer t
  :config
  (bind-key "C-c C-z f" 'browse-url-of-dired-file dired-mode-map))

;; grep
(setq grep-program (or (and (executable-find "xzgrep") "xzgrep")
		       (and (executable-find "bzgrep") "bzgrep")
		       "grep"))

;; スクリプトの保存時に自動的に chmod +x を行う
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(use-package ediff
  :defer t
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

;; fonts
(setq use-default-font-for-symbols nil)
(cond
 ((memq window-system '(ns mac))
  (set-face-attribute 'default nil :family "Menlo" :height 120)
  (let ((font-spec (font-spec :family "Hiragino Kaku Gothic ProN" :size 14)))
    (dolist (charset (get-language-info "Japanese" 'charset))
      (set-fontset-font nil charset font-spec)))
  ;; (set-fontset-font nil 'latin "Menlo")
  (set-fontset-font nil 'greek "Menlo")))

;; frame size
(add-to-list 'default-frame-alist
	     `(width . ,(* (+ 80 1 2) 2))) ; 80 + 1 + fringes
(set-frame-parameter nil 'fullscreen 'fullheight)

;; font-lock
(global-font-lock-mode 1)

;; emacs-lisp
(require 'ert)
(use-package ert-async
  :config
  (remove-hook 'emacs-lisp-mode-hook 'ert--activate-font-lock-keywords)
  (add-hook 'emacs-lisp-mode-hook 'ert-async-activate-font-lock-keywords))
(add-to-list 'auto-mode-alist '("/Cask\\'" . emacs-lisp-mode))
(add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)

(use-package flyspell
  :if (executable-find "aspell")
  :defer t
  :diminish flyspell-mode
  :init
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'git-commit-mode-hook 'flyspell-mode))

;; vc
(remove-hook 'find-file-hook 'vc-find-file-hook)
(remove-hook 'kill-buffer-hook 'vc-kill-buffer-hook)

(use-package view
  :defer t
  :init
  (setq view-read-only t)
  :config
  (bind-keys :map view-mode-map
	     ("j" . next-line)
	     ("k" . previous-line)
	     ("h" . backward-char)
	     ("l" . forward-char)))

(require 'server)
(unless (server-running-p)
  (server-start))

(use-package eww
  :defer t
  :config
  (defadvice eww-render (before my/fix-fragment activate)
    (let* ((status (ad-get-arg 0))
	   (url (ad-get-arg 1))
	   (redirect (plist-get status :redirect)))
      (when redirect
	(let ((old-target (url-target (url-generic-parse-url url)))
	      (new-target (url-target (url-generic-parse-url redirect))))
	  (when (and old-target (not new-target))
	    (plist-put status :redirect (concat redirect "#" old-target))
	    (ad-set-arg 0 status)))))))

(use-package nxml-mode
  :defer t
  :config
  (defun my-nxml-mode-hook ()
    (setq tab-width 2)
    (setq nxml-slash-auto-complete-flag t))
  (add-hook 'nxml-mode-hook 'my-nxml-mode-hook))

(use-package org-capture
  :defer t
  :init
  (global-set-key (kbd "C-c c") #'org-capture)
  :config
  (setq org-capture-templates
	'(("n" "Note" entry
	   (file (expand-file-name (format-time-string
				    "notes/%Y/%m/%Y-%m-%d.org")
				   org-directory))
	   "* %?\n %U\n  %i"
	   :unnarrowed t))))

(use-package python
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.wsgi\\'" . python-mode))
  :config
  (setq python-shell-interpreter "ipython")
  (add-hook 'python-mode-hook
	    (lambda ()
	      (setq tab-width 4)))

  (add-hook 'find-file-hook
	    (lambda ()
	      (if (and buffer-file-name
		       (string-match "/site-packages/" buffer-file-name))
		  (read-only-mode 1))))

  (use-package jedi-core
    :config
    (setq jedi:complete-on-dot t))

  (use-package company-jedi
    :config
    (add-to-list 'company-backends 'company-jedi))
  )

(use-package ruby-mode
  :defer t
  :config
  (defun my-ruby-mode-hook ()
    (setq tab-width 2)
    (setq ruby-deep-indent-paren-style nil))
  (add-hook 'ruby-mode-hook 'my-ruby-mode-hook)

  (use-package ruby-end
    :diminish ruby-end-mode
    :config
    (setq ruby-end-insert-newline nil))
  )

(use-package sql
  :defer
  :config
  (defun my-sql-mode-hook ()
    (setq tab-width 2)
    (setq sql-indent-offset 2)
    (setq sql-indent-maybe-tab t)
    (setq sql-electric-stuff 'semicolon)
    (use-package sql-indent))
  (add-hook 'sql-mode-hook 'my-sql-mode-hook))

(use-package whitespace
  :diminish global-whitespace-mode
  :config
  (setq whitespace-style '(face tabs spaces trailing tab-mark space-mark))
  (setq whitespace-space-regexp "\\(\u3000+\\)")
  (setq whitespace-display-mappings
	'((space-mark ?\u3000 [?\u25a1])
	  (space-mark ?\xA0   [?¤]      [?_])))
  (setq whitespace-global-modes '(not go-mode mew-summary-mode))
  (global-whitespace-mode 1))

(use-package solarized-light-theme
  :init
  (setq solarized-use-variable-pitch nil)
  (setq solarized-height-minus-1 1.0)
  (setq solarized-height-plus-1 1.0)
  (setq solarized-height-plus-2 1.0)
  (setq solarized-height-plus-3 1.0)
  (setq solarized-height-plus-4 1.0)
  :config
  (load-theme 'solarized-light t)

  (custom-theme-set-faces
   'solarized-light
   '(elscreen-tab-current-screen-face ((t :inherit mode-line :weight bold)))
   '(elscreen-tab-other-screen-face ((t :inherit mode-line-inactive)))
   ;; '(helm-ff-dotted-directory ((t :inherit helm-ff-directory)))
   ;; '(helm-source-header ((t :inherit mode-line)))
   )

  (set-face-attribute 'header-line nil :box nil)
  (set-face-attribute 'mode-line nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :box nil)

  (use-package whitespace
    :defer t
    :config
    (set-face-attribute 'whitespace-space nil
			:italic nil)
    (set-face-attribute 'whitespace-tab nil
			:foreground "#eee8d5" ;base2
			:strike-through t
			:inverse-video nil))

  (use-package paren
    :defer t
    :config
    (set-face-attribute 'show-paren-match nil
			:weight 'unspecified)))

(use-package quiet-save
  :init
  (setq auto-save-default nil)
  (setq quiet-save-exclude '("/\\.emacs\\.d/elpa/"
			     (lambda () (overlays-in (point-min) (point-max)))))
  (setq quiet-save-keep '("\\.md\\'" quiet-save-vc-root))
  :config
  (quiet-save-mode))

(use-package aggressive-indent
  :defer t
  :diminish aggressive-indent-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode))

(use-package company
  :diminish company-mode
  :defer t
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package dabbrev-highlight)
(use-package "dabbrev-ja")

(use-package direx
  :defer t
  :init
  (bind-key "C-x C-d" 'direx:jump-to-directory-other-window))

(use-package elscreen
  :defer t
  :init
  (unless (featurep 'elscreen-start)
    (autoload 'elscreen-start "elscreen" nil t))
  (setq elscreen-tab-display-control nil)
  (setq elscreen-tab-display-kill-screen nil)
  (setq elscreen-display-tab nil)
  (setq elscreen-display-screen-number nil)
  :config
  (push (cons "^navi2ch-" "Navi2ch") elscreen-mode-to-nickname-alist)
  (elscreen-rebuild-mode-to-nickname-alist)
  (elscreen-start))

(use-package flycheck
  :defer t
  :init
  (add-hook 'prog-mode-hook 'flycheck-mode)
  :config
  (bind-key "M-n" 'flycheck-next-error flycheck-mode-map)
  (bind-key "M-p" 'flycheck-previous-error flycheck-mode-map)
  (setq flycheck-display-errors-delay 0.3)
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc
					     javascript-jshint
					     ruby-rubocop))

  ;; http://emacs.stackexchange.com/questions/21205
  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
		  (or (buffer-file-name) default-directory)
		  "node_modules"))
	   (eslint (and root
			(expand-file-name "node_modules/.bin/eslint" root))))
      (when (and eslint (file-executable-p eslint))
	(setq-local flycheck-javascript-eslint-executable eslint))))

  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules))

(use-package go-mode
  :defer t
  :init
  (setq gofmt-command "goimports")
  :config
  (defun my-go-mode-hook ()
    (setq tab-width 4))
  (add-hook 'go-mode-hook 'my-go-mode-hook)

  (use-package company-go
    :config
    (add-to-list 'company-backends 'company-go))
  )

(use-package haskell-mode
  :defer t
  :config
  (add-hook 'haskell-mode-hook #'haskell-indent-mode)
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode))

(use-package helm-config
  :diminish helm-mode
  :init
  (bind-key "C-x b" 'helm-mini)

  ;; emacs-helm.sh
  (bind-key [remap find-file] 'helm-find-files)
  (bind-key [remap occur] 'helm-occur)
  (bind-key [remap list-buffers] 'helm-buffers-list)
  (bind-key [remap completion-at-point] 'helm-lisp-completion-at-point
	    lisp-interaction-mode-map)
  (bind-key [remap completion-at-point] 'helm-lisp-completion-at-point
	    emacs-lisp-mode-map)

  (helm-mode 1)

  (use-package popwin
    :config
    (add-hook 'helm-after-initialize-hook
              (lambda ()
                (popwin:display-buffer helm-buffer t)
                (popwin-mode -1)))
    (add-hook 'helm-cleanup-hook #'popwin-mode))

  :config
  (use-package helm
    :defer t
    :init
    (bind-key "M-x" 'helm-M-x)
    (bind-key "C-M-n" 'helm-next-source helm-map)
    (bind-key "C-M-p" 'helm-previous-source helm-map))

  (use-package helm-files
    :defer t
    :config
    (setq helm-ff-newfile-prompt-p nil)
    (setq helm-ff-skip-boring-files t)
    (add-to-list 'helm-boring-file-regexp-list "\\.egg-info$")
    (add-to-list 'helm-boring-file-regexp-list "__pycache__"))

  (use-package helm-man
    :defer t
    :config
    (setq helm-man-format-switches "%s"))

  (use-package helm-descbinds
    :config
    (helm-descbinds-mode 1))

  (use-package helm-gtags
    :diminish helm-gtags-mode
    :defer t
    :config
    (add-hook 'prog-mode-hook
	      (lambda ()
		(unless (memq major-mode '(js-mode js2-mode))
		  (helm-gtags-mode))))
    (bind-keys :map helm-gtags-mode-map
	       ("M-." . helm-gtags-find-tag)
	       ("M-*" . helm-gtags-pop-stack)
	       ("M-," . helm-gtags-find-rtag))
    (setq helm-gtags-ignore-case t))

  (use-package helm-dash
    :defer t
    :init
    (bind-key "d" 'helm-dash helm-command-map)
    (bind-key "D" 'helm-dash-at-point helm-command-map)
    :config
    (setq helm-dash-common-docsets (helm-dash-installed-docsets))
    (setq helm-dash-browser-func
	  (lambda (url &optional _new-window)
	    (interactive (browse-url-interactive-arg "URL: "))
	    (start-process (concat "osascript " url) nil
			   "osascript" "-e"
			   (format
			    "open location \"%s\""
			    (replace-regexp-in-string "\"" "%22" url))))))

  (autoload 'haskell-ident-at-point "haskell-mode")

  (use-package helm-hoogle
    :defer t
    :init
    (bind-key "H" 'helm-hoogle helm-command-map)
    :config
    (defun helm-hoogle ()
      (interactive)
      (helm :sources 'helm-c-source-hoogle
	    :input (haskell-ident-at-point)
	    :prompt "Hoogle: "
	    :buffer "*Hoogle search*"))
    ;; (when (fboundp 'eww-browse-url)
    ;;   (push '("Lookup Entry (eww)" . eww-browse-url)
    ;; 	    (cdr (assq 'action helm-c-source-hoogle))))
    )

  (use-package helm-ghq
    :init
    (defun my/helm-ghq-list ()
      (interactive)
      (helm :sources 'helm-source-ghq))
    (bind-key "g" 'my/helm-ghq-list helm-command-map))

  (use-package helm-ag
    :init
    (bind-key "a" 'helm-ag helm-command-map))

  )

(use-package howm
  :defer t
  :init
  (autoload 'howm-menu "howm" "Hitori Otegaru Wiki Modoki" t)
  (bind-key "\C-c , ," 'howm-menu)
  (setq howm-view-title-header "#")
  :config
  (setq howm-menu-lang 'ja)
  (setq howm-file-name-format "%Y/%m/%Y-%m-%d.md")
  (setq howm-template (concat howm-view-title-header
			      " %title%cursor\n%date\n\n")))

(define-derived-mode json-mode js-mode "JSON")

(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.jshintrc\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.eslintrc\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.bowerrc\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.babelrc\\'" . json-mode))

(add-hook 'json-mode-hook
	  (lambda () (setq js-indent-level 2)))

(use-package js2-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
  :config
  (setq js2-include-browser-externs nil)
  ;; JSHint shows this errors
  (setq js2-mode-show-parse-errors nil)
  ;; (default): for `js2-strict-trailing-comma-warning' and
  ;; `js2-strict-inconsistent-return-warning'
  (setq js2-mode-show-strict-warnings t)
  ;; Show this warning without JSHint's es3 option
  (setq js2-strict-trailing-comma-warning nil)
  ;; JSHint shows this warning
  (setq js2-strict-missing-semi-warning nil)
  ;; (default): JSHint's lastsemic option can suppress this warning
  (setq js2-missing-semi-one-line-override nil)
  ;; (default): JSHint cannot show this warning
  (setq js2-strict-inconsistent-return-warning nil)
  ;; JSHint shows these warnings
  (setq js2-strict-cond-assign-warning nil)
  (setq js2-strict-var-redeclaration-warning nil)
  (setq js2-strict-var-hides-function-arg-warning nil)
  (setq js2-highlight-external-variables nil)
  (setq js2-include-jslint-globals nil)

  (use-package js2-refactor
    :config
    (add-hook 'js2-mode-hook #'js2-refactor-mode)
    (js2r-add-keybindings-with-prefix "C-c C-m"))

  (use-package tern
    :if (executable-find "tern")
    :init
    (add-to-list 'company-backends 'company-tern)
    :config
    (add-hook 'js2-mode-hook #'tern-mode)
    (use-package terndoc
      :config
      (add-hook 'tern-mode-hook
		(lambda ()
		  (define-key tern-mode-keymap "@" #'terndoc-insert-tag))))))

(if (and (eq system-type 'darwin)
	 (featurep 'htmlize))
    (use-package mac-print-mode))

(autoload 'mac-print-buffer "mac-print-mode" nil t)

(use-package magit
  :defer t
  :config
  (setq magit-diff-refine-hunk t)
  (setq magit-commit-extend-override-date t))

(use-package markdown-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
  :config
  (setq markdown-gfm-use-electric-backquote nil)
  (add-hook 'markdown-mode-hook #'outline-minor-mode)
  (add-hook 'markdown-mode-hook
	    (lambda ()
	      (visual-line-mode -1))))

(use-package mew
  :defer t
  :init
  (if (boundp 'mail-user-agent)
      (setq mail-user-agent 'mew-user-agent))
  (if (fboundp 'define-mail-user-agent)
      (define-mail-user-agent
	'mew-user-agent
	'mew-user-agent-compose
	'mew-draft-send-message
	'mew-draft-kill
	'mew-send-hook)))

(use-package migemo
  :if (executable-find "cmigemo")
  :config
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "-e"))
  (setq migemo-dictionary
	(expand-file-name "../share/migemo/utf-8/migemo-dict"
			  (file-name-directory
			   (executable-find migemo-command))))
  (setq migemo-coding-system 'utf-8-unix)
  ;; cmigemo で必須の設定
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  ;; キャッシュの設定
  (setq migemo-use-pattern-alist t)
  (setq migemo-use-frequent-pattern-alist t)
  (setq migemo-pattern-alist-length 1024)
  (migemo-init))

(use-package navbar
  :init
  (unless (featurep 'navbar)
    (autoload 'navbar-mode "navbar" nil t))
  (unless (featurep 'navbarx-elscreen)
    (autoload 'navbarx-elscreen "navbarx-elscreen"))
  (unless (featurep 'navbarx-mew)
    (autoload 'navbarx-mew "navbarx-mew"))
  (unless (featurep 'navbarx-time)
    (autoload 'navbarx-time "navbarx-time"))
  (unless (featurep 'navbarx-version)
    (autoload 'navbarx-version "navbarx-version"))
  (unless (featurep 'navbarx-ddskk)
    (autoload 'navbarx-ddskk "navbarx-ddskk"))
  (unless (featurep 'navbarx-eyebrowse)
    (autoload 'navbarx-eyebrowse "navbarx-eyebrowse"))

  (setq navbar-item-list
        (list
         (and (locate-library "elscreen") 'navbarx-elscreen)
         'navbarx-glue
         (and (locate-library "mew") 'navbarx-mew)
         'navbarx-time))

  (setq display-time-day-and-date t)
  (setq display-time-24hr-format t)
  :config
  (set-face-background 'navbar-item "#859900")
  (display-time-mode)
  (navbar-mode))

(use-package popwin
  :config
  (bind-key "C-c C-p" popwin:keymap)
  (add-to-list 'popwin:special-display-config '("\\`\\*[Hh]elm.*\\*\\'" :regexp t))
  (add-to-list 'popwin:special-display-config 'git-commit-mode)
  (add-to-list 'popwin:special-display-config "COMMIT_EDITMSG")
  (add-to-list 'popwin:special-display-config
	       '(direx:direx-mode :position left :width 30 :dedicated t))
  (add-to-list 'popwin:special-display-config 'flycheck-error-list-mode)
  (add-to-list 'popwin:special-display-config "*HS-Error*")
  (add-to-list 'popwin:special-display-config "*Hoogle search*")
  (popwin-mode 1))

(use-package recentf
  :config
  (setq recentf-max-saved-items nil)
  (setq recentf-exclude `(,(expand-file-name recentf-save-file)
			  "\\.howm-keys$" "COMMIT_EDITMSG"))
  (setq recentf-auto-cleanup 10)
  (run-with-idle-timer 60 t 'recentf-save-list)

  (use-package shut-up
    :config
    (defadvice recentf-save-list (around my/shut-up activate)
      (shut-up
       ad-do-it))
    (defadvice recentf-cleanup (around my/shut-up activate)
      (shut-up
       ad-do-it)))

  (use-package recentf-ext))

(use-package savekill)

(use-package skk
  :defer t
  :init
  (setq default-input-method "japanese-skk")
  (setq skk-user-directory (locate-user-emacs-file "skk/"))
  :config
  (setq skk-byte-compile-init-file t)
  (setq skk-isearch-start-mode 'latin)

  ;; Enable tab completion in markdown-mode
  (defadvice skk-setup-abbrev-mode-map-options (before completion activate)
    (if (eq skk-try-completion-char 9)
	(bind-key [(tab)] 'skk-comp-wrapper skk-abbrev-mode-map))))

(use-package slime
  :defer t
  :init
  (setq inferior-lisp-program "ccl64"))

;; (auto-install-from-url "http://github.com/imakado/emacs-smartchr/raw/master/smartchr.el")
(use-package smartchr)

(use-package web-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jinja2\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

;; Local Variables:
;; mode: emacs-lisp
;; coding: utf-8-unix
;; End:
;;; init.el ends here
