;;; init.el --- Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2021 Tommaso Rossi

;; Author: Tommaso Rossi <tommaso.rossi1@protonmail.com

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; GNU Emacs configuration.

;;; Code:

;;;; Measure init time

(defconst emacs-start-time (current-time))

(add-hook 'after-init-hook
	  `(lambda ()
	     (let ((elapsed
		    (float-time
		     (time-subtract (current-time) emacs-start-time))))
	       (message "Initialized in %.2fs with %d garbage collections" elapsed gcs-done))) t)

;; disable really ugly stuff
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq inhibit-startup-echo-area-message "tomrss")

;; silent native compilation warning
(setq native-comp-async-report-warnings-errors 'silent)

;; start fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; add lisp folder to load path
(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/lisp/"))

;;;; packaging

;;;;; Configure `straight.el' as package manager

;; bootstrap `straight.el'
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; configure straight lockfile (it can be committed)
(setq straight-profiles '((nil . "~/.emacs.d/lockfile.el")))

;;;;; Keep folders clean

;; disable pesky lockfiles
(setq create-lockfiles nil)

;; change some backup default
(setq backup-by-copying t)
(setq delete-old-versions t)
(setq kept-new-versions 4)
(setq kept-old-versions 2)
(setq version-control t)

;; custom file to temp file (practically disable `customize')
(setq custom-file
      (if (boundp 'server-socket-dir)
	  (expand-file-name "custom.el" server-socket-dir)
	(expand-file-name (format "emacs-custom-%s.el" (user-uid))
			  temporary-file-directory)))

;;;; Key bindings

(defvar customized-keys-minor-mode-map (make-sparse-keymap)
  "Keymap for the customized-keys-minor-mode.")

(define-minor-mode customized-keys-minor-mode
  "A minor mode so that custom key settings override major modes."
  :init-value t
  :lighter " custom-keys")

(customized-keys-minor-mode +1)

(defun +define-key (key def)
  "Define customized KEY with definition DEF."
  (define-key customized-keys-minor-mode-map key def))

;;;; Completion framework

;;;;; Getting help and docs

;; improve self documentation
(straight-use-package 'helpful)
(+define-key [remap describe-command] #'helpful-command)
(+define-key [remap describe-function] #'helpful-callable)
(+define-key [remap describe-key] #'helpful-key)
(+define-key [remap describe-symbol] #'helpful-symbol)
(+define-key [remap describe-variable] #'helpful-variable)
(+define-key (kbd "C-h o") #'helpful-symbol)
(+define-key (kbd "C-h p") #'helpful-at-point)

;;;;; Minibuffer completions

;; completion UI
(straight-use-package 'vertico)
(vertico-mode 1)
(define-key vertico-map (kbd "C-j") #'vertico-next)
(define-key vertico-map (kbd "C-k") #'vertico-previous)

;; builtin `savehist-mode' allows to persist orderless selections
(setq history-length 20)
(savehist-mode +1)

(straight-use-package 'orderless)
(setq orderless-matching-styles
      '(orderless-literal orderless-initialism orderless-regexp))
(setq orderless-component-separator "[ +]+")
(setq completion-styles '(orderless))

;; get help and docs in minibuffer
(straight-use-package 'marginalia)
(marginalia-mode 1)

;; add useful functions with consult
(straight-use-package 'consult)
(with-eval-after-load 'consult
  (setq consult-narrow-key "<"))
(+define-key (kbd "C-x b")   #'consult-buffer)
(+define-key (kbd "C-x 4 b") #'consult-buffer-other-window)
(+define-key (kbd "C-x 5 b") #'consult-buffer-other-frame)
(+define-key (kbd "M-y")     #'consult-yank-pop)
(+define-key (kbd "C-x f")   #'consult-recent-file)
(+define-key (kbd "M-g e")   #'consult-compile-error)
(+define-key (kbd "M-g g")   #'consult-goto-line)
(+define-key (kbd "M-g M-g") #'consult-goto-line)
(+define-key (kbd "M-g o")   #'consult-outline)
(+define-key (kbd "M-g i")   #'consult-imenu)
(+define-key (kbd "M-g I")   #'consult-imenu-multi)
(+define-key (kbd "M-s f")   #'consult-find)
(+define-key (kbd "M-s L")   #'consult-locate)
(+define-key (kbd "M-s g")   #'consult-grep)
(+define-key (kbd "M-s G")   #'consult-git-grep)
(+define-key (kbd "M-s r")   #'consult-ripgrep)
(+define-key (kbd "C-s")     #'consult-line)
(+define-key (kbd "M-s m")   #'consult-multi-occur)
(+define-key (kbd "M-s k")   #'consult-keep-lines)
(+define-key (kbd "M-s u")   #'consult-focus-lines)
(define-key minibuffer-local-map (kbd "C-r") #'consult-history)

;; enable acting on minibuffer candidates (and much more)
(straight-use-package 'embark)
(setq prefix-help-command #'embark-prefix-help-command)
(+define-key (kbd "C-.") #'embark-act)
(with-eval-after-load 'embark
  (define-key embark-symbol-map (kbd "h") #'helpful-symbol))

(straight-use-package 'embark-consult)
(add-hook 'embark-collect-mode-hook #'embark-consult-preview-minor-mode)

;;;;;  Autocomplete in-buffer

(straight-use-package 'corfu)
(setq corfu-auto t)
(setq corfu-auto-delay 0.1)
(setq corfu-cycle t)
(setq corfu-quit-at-boundary t)
(setq corfu-preselect-first t)
(setq corfu-echo-documentation 0)
(corfu-global-mode 1)
(define-key corfu-map (kbd "C-j") #'corfu-next)
(define-key corfu-map (kbd "C-k") #'corfu-previous)

;; TODO whats this, why is it here
(setq tab-always-indent 'complete)

;;;; User interface

;; recognize system
(defconst IS-GNU     (eq system-type 'gnu/linux))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

;; disable the really annoying bell
(setq ring-bell-function 'ignore)

;;;;; Fonts

(when (symbol-value 'window-system)
  (when (x-list-fonts "JetBrains Mono")
    (set-face-attribute 'default     nil :font "JetBrains Mono" :height 110 :weight 'regular)
    (set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :height 110 :weight 'regular))
  (when (x-list-fonts "Cantarell")
    (set-face-attribute 'variable-pitch nil :font "Cantarell" :height 130 :weight 'regular)))

(straight-use-package 'all-the-icons)
(when (symbol-value 'window-system)
  (unless (x-list-fonts "all-the-icons")
    (if IS-WINDOWS
	(warn "Run M-x all-the-icons-install-fonts to download the fonts, then install them manually")
      (all-the-icons-install-fonts t)))
  (require 'all-the-icons nil nil))

;;;;; Theme

(if (symbol-value 'window-system)
    (progn
      (straight-use-package 'doom-themes)
      (load-theme 'doom-nord t)
      (set-face-attribute 'font-lock-doc-face nil :foreground "#EBCB8B")
      (set-face-attribute 'shadow nil :foreground "#EBCB8B"))
  (load-theme 'misterioso))

;;;;; Modeline

;; minions is a package that hides minor modes and provides a menu for managing them
(straight-use-package 'minions)
(minions-mode 1)

(when (symbol-value 'window-system)
  (straight-use-package 'doom-modeline)
  (doom-modeline-mode 1))

;; some modes do not need modeline
(straight-use-package 'hide-mode-line)
(add-hook 'help-mode-hook #'hide-mode-line-mode)
(add-hook 'helpful-mode-hook #'hide-mode-line-mode)
(add-hook 'vterm-mode-hook #'hide-mode-line-mode)
(add-hook 'treemacs-mode-hook #'hide-mode-line-mode)

;;;;; Line and column numbers

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))

(column-number-mode)

;;;;; Smooth scrolling

(setq scroll-margin 1)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position 1)

;; highlight current line
(straight-use-package 'hl-line)
(setq hl-line-sticky-flag nil)
(add-hook 'dired-mode-hook #'hl-line-mode)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'special-mode-hook #'hl-line-mode)

(setq use-dialog-box nil)

;;;; Projects and file management

;;;;; File managing with `dired'

;; base `dired' tweaks
(with-eval-after-load 'dired
  (setq dired-auto-revert-buffer t)
  (setq dired-dwim-target t)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'top)
  (setq dired-create-destination-dirs 'ask)
  (setq dired-listing-switches "-agho --group-directories-first")
  (when IS-MAC
    (setq dired-use-ls-dired nil))
  (setq delete-by-moving-to-trash t))
(+define-key (kbd "C-x j") #'dired-jump)

;; use icons in dired
(when (symbol-value 'window-system)
  (straight-use-package 'all-the-icons-dired)
  (with-eval-after-load 'dired
    (add-hook 'dired-mode-hook #'all-the-icons-dired-mode)))

;; minor mode for hiding dotfiles
(straight-use-package 'dired-hide-dotfiles)
;; TODO map key

;; colorize dired
(straight-use-package 'diredfl)
(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook #'diredfl-mode))

(straight-use-package 'treemacs)
(with-eval-after-load 'treemacs
  (treemacs-resize-icons 16)
  (add-hook 'treemacs-mode-hook #'hide-mode-line-mode))

;;;;; Process management

(with-eval-after-load 'proced
  (setq proced-auto-update-interval 5)
  (proced-toggle-auto-update 1))

;;;; Editing features

;; navigable undo/redo tree
(straight-use-package 'undo-tree)
(global-undo-tree-mode 1)

;;;;; Vim emulation

;; base evil configuration
(straight-use-package 'evil)
(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(setq evil-want-C-u-scroll t)
(setq evil-want-C-i-jump nil)
(setq evil-respect-visual-line-mode t)
(setq evil-undo-system 'undo-tree)
(evil-mode 1)

;; automatically configure evil for some common modes
(straight-use-package 'evil-collection)
(with-eval-after-load 'evil
  (evil-collection-init))

;; integrate with treemacs
(straight-use-package 'treemacs-evil)
(with-eval-after-load 'evil
  (with-eval-after-load 'treemacs
    (require 'treemacs-evil nil nil)))

(add-hook 'with-editor-mode-hook 'evil-insert-state)

(fset 'yes-or-no-p 'y-or-n-p)

(recentf-mode +1)

(save-place-mode +1)

;; auto revert non file buffers
(setq global-auto-revert-non-file-buffers t)

;; auto revert file buffers (when file changes)
(global-auto-revert-mode 1)

(setq save-interprogram-paste-before-kill t)

;;;;; Text selection and navigation commands

;; increases the selected region by semantic units
(straight-use-package 'expand-region)
(+define-key (kbd "C-ò") #'er/expand-region)

;;;; Configure Org mode

(straight-use-package 'visual-fill-column)

(with-eval-after-load 'org
  (add-hook 'org-mode-hook 'visual-line-mode)
  (setq visual-fill-column-width 100)
  (setq visual-fill-column-center-text t)
  (add-hook 'org-mode-hook 'visual-fill-column-mode)

  (require 'org-tempo)
  (org-babel-do-load-languages 'org-babel-load-languages
			       '((emacs-lisp . t)
				 (python . t)
				 (shell . t)))
  (add-to-list 'org-structure-template-alist
	       '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist
	       '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist
	       '("py" . "src python"))
  (add-to-list 'org-structure-template-alist
	       '("yml" . "src yaml"))
  (add-to-list 'org-structure-template-alist
	       '("json" . "src json"))
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-edit-src-content-indentation 2)
  (setq org-hide-block-startup nil)
  (setq org-src-preserve-indentation nil)
  (setq org-hide-leading-stars t)
  (setq org-adapt-indentation nil)
  (setq org-startup-folded 'content)
  (setq org-cycle-separator-lines 2)
  (setq org-return-follows-link t)
  (setq org-startup-truncated nil)
  (setq org-startup-with-inline-images t))

;;;;; Auto tangle on save

(defun +org-auto-tangle ()
  "Set hook for auto tangling org files on save."
  (let ((org-confirm-babel-evaluate nil))
    (org-babel-tangle)))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'+org-auto-tangle 0 t)))

(straight-use-package 'org-roam)
(setq org-roam-v2-ack t)
(with-eval-after-load 'org-roam
  (setq org-roam-directory "~/RoamNotes")
  (org-roam-setup))
;; TODO keys: (bind-keys :package org-roam
;; 	     ("C-c n l" . org-roam-buffer-toggle)
;; 	     ("C-c n f" . org-roam-node-find)
;; 	     ("C-c n i" . org-roam-node-insert)))

;;;; Windows and buffer management

(winner-mode +1)
(+define-key (kbd "C-c w") 'winner-undo)
(+define-key (kbd "C-c W") 'winner-redo)

;; define window placement rules
(straight-use-package 'shackle)
(shackle-mode 1)
(setq shackle-rules
      '((compilation-mode :noselect t)
	(help-mode :popup t :select t :align bottom :size 0.33)
	(helpful-mode :popup t :select t :align bottom :size 0.33)
	("\\*Warnings\\*" :regexp t :noselect t))
      shackle-default-rule
      '(:noselect t))

;; define and manage popup buffers
(straight-use-package 'popper)
(setq popper-reference-buffers
      '("\\*Messages\\*" "Output\\*$" help-mode helpful-mode compilation-mode))
(setq popper-mode-line
      '(:eval
	(propertize " P " 'face 'mode-line-emphasis)))
(setq popper-display-control nil)
(popper-mode 1)
(+define-key (kbd "C-è") #'popper-toggle-latest)
(+define-key (kbd "M-è") #'popper-cycle)
(+define-key (kbd "C-M-è") #'popper-toggle-type)

;; window commands 
(+define-key (kbd "M-o") #'other-window)

(+define-key (kbd "M-h") #'windmove-left)
(+define-key (kbd "M-j") #'windmove-down)
(+define-key (kbd "M-k") #'windmove-up)
(+define-key (kbd "M-l") #'windmove-right)

(+define-key (kbd "C-M-h") #'shrink-window-horizontally)
(+define-key (kbd "C-M-j") #'shrink-window)
(+define-key (kbd "C-M-k") #'enlarge-window)
;; (+define-key (kbd "C-M-l") 'recenter-other-window)
(+define-key (kbd "C-M-l") 'enlarge-window-horizontally)

(+define-key (kbd "C-M-S-h") #'windmove-swap-states-left)
(+define-key (kbd "C-M-S-j") #'windmove-swap-states-down)
(+define-key (kbd "C-M-S-k") #'windmove-swap-states-up)
(+define-key (kbd "C-M-S-l") #'windmove-swap-states-right)

;;;;; buffer helpers

;; use `ibuffer' instead of buffer list
(+define-key (kbd "C-x C-b") 'ibuffer)

(defun +kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun +kill-mode-buffers (mode)
  "Kill all buffers with major mode MODE."
  (interactive
   (list
    (intern
     (completing-read
      "Mode: "
      (delete-dups
       (mapcar
	(lambda (buffer)
	  (buffer-local-value 'major-mode buffer))
	(buffer-list)))))))
  (mapc (lambda (buffer)
	  (when (eq mode (buffer-local-value 'major-mode buffer))
	    (kill-buffer buffer)))
	(buffer-list))
  (message "Killed buffers with major mode %s" mode))

(defun +kill-dired-buffers ()
  "Kill all Dired buffers."
  (interactive)
  (+kill-mode-buffers 'dired-mode))

(defun +kill-help-buffers ()
  "Kill all help buffers."
  (interactive)
  (+kill-mode-buffers 'help-mode)
  (+kill-mode-buffers 'helpful-mode))

;;;; Terminals

;;;;; term

(straight-use-package 'eterm-256color)
(with-eval-after-load 'term
  (add-hook 'term-mode-hook #'eterm-256color-mode))

;;;;; vterm

(straight-use-package 'vterm)
(with-eval-after-load 'vterm
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (setq vterm-max-scrollback 10000)
  (setq vterm-kill-buffer-on-exit t))

;; TODO try vterm-toggle: https://github.com/jixiuf/vterm-toggle

;;;;; eshell

;;;;;; eshell prompt

(with-eval-after-load 'em-prompt
  (defun +prompt-path ()
    "Path (pwd) that will be displayed in prompt."
    (let* ((pwd (eshell/pwd)))
      (if (equal pwd "~")
	  pwd
	;; (abbreviate-file-name (shrink-path-file-pwd)))))
	(abbreviate-file-name pwd))))

  (defun +eshell-prompt ()
    "The eshell prompt."
    (let ((current-branch (when (fboundp 'magit-get-current-branch)
			    (magit-get-current-branch))))
      (concat
       (if (bobp) "" "\n")
       (propertize user-login-name 'face `(:foreground "#62aeed"))
       (propertize " • " 'face `(:foreground "white"))
       (propertize (+prompt-path) 'face `(:foreground "#82cfd3"))
       (when current-branch
	 (concat
	  (propertize " • " 'face `(:foreground "white"))
	  (propertize (concat " " current-branch) 'face `(:foreground "#c475f0"))))
       (propertize " • " 'face `(:foreground "white"))
       (propertize (format-time-string "%H:%M:%S") 'face `(:foreground "#5a5b7f"))
       (let ((user-prompt
	      (if (= (user-uid) 0) "\n#" "\nλ")))
	 (propertize user-prompt 'face (if (zerop eshell-last-command-status) 'success 'error)))
       " ")))

  (setq eshell-prompt-function #'+eshell-prompt
	eshell-prompt-regexp "^.*λ "
	eshell-highlight-prompt t))

;;;;;; eshell banner

(with-eval-after-load 'em-banner
  (setq eshell-banner-message
	'(format "%s %s\n\n"
		 (propertize (format " %s " (string-trim (buffer-name)))
			     'face 'mode-line-highlight)
		 (propertize (current-time-string)
			     'face 'font-lock-keyword-face))))

;;;;;; eshell keybindings and aliases

(with-eval-after-load 'em-alias
  (evil-collection-eshell-setup)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-R") #'consult-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-l") #'eshell/clear)
  (evil-normalize-keymaps)

  (dolist
      (alias
       '(("q"     "exit")
	 ("f"     "find-file $1")
	 ("ff"    "find-file $1")
	 ("d"     "dired $1")
	 ("pd"    "proced $1")
	 ("rg"    "rg --color=always $*")
	 ("l"     "ls -lh $*")
	 ("ll"    "ls -lah $*")
	 ("git"   "git --no-pager $*")
	 ("gg"    "magit-status")
	 ("clear" "clear-scrollback")
	 ("u"     "eshell-up $1")))	; see section below for `eshell-up' command and package
    (add-to-list 'eshell-command-aliases-list alias))
  (eshell-write-aliases-list))

;;;;;; eshell colors

(straight-use-package 'xterm-color)

(with-eval-after-load 'eshell 		; don't know if there is a specific module
  (delq 'eshell-handle-ansi-color eshell-output-filter-functions)

  (push 'xterm-color-filter eshell-preoutput-filter-functions)
  (add-hook 'eshell-before-prompt-hook
	    (lambda ()
	      (setq xterm-color-preserve-properties t)))

  (setq eshell-term-name "xterm-256color")

  ;; We want to use xterm-256color when running interactive commands
  ;; in eshell but not during other times when we might be launching
  ;; a shell command to gather its output (from daviwil conf)
  (add-hook 'eshell-pre-command-hook
	    (lambda () (setenv "TERM" "xterm-256color")))
  (add-hook 'eshell-post-command-hook
	    (lambda () (setenv "TERM" "dumb"))))

;;;;;; eshell history

(with-eval-after-load 'em-hist
  (add-hook 'eshell-pre-command-hook #'eshell-save-some-history)
  (setq eshell-history-size 10000
	eshell-history-ignoredups t
	eshell-input-filter #'eshell-input-filter-initial-space
	;; don't record command in history if prefixed with whitespace
	eshell-input-filter #'eshell-input-filter-initial-space)
  (eshell-hist-initialize))

;;;;;; eshell visual commands

(with-eval-after-load 'em-term
  (dolist (cmd '("htop" "vim" "nvim"))
    (add-to-list 'eshell-visual-commands cmd)))

;;;;;; eshell defaults and generic conf

(with-eval-after-load 'eshell
  (setenv "PAGER" "cat")
  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions #'eshell-truncate-buffer)
  ;; use TRAMP
  (add-to-list 'eshell-modules-list 'eshell-tramp)
  ;; Enable autopairing in eshell
  (add-hook 'eshell-mode-hook #'smartparens-mode)

  (setq password-cache t
	password-cache-expiry 3600)

  (setq eshell-buffer-maximum-lines 10000
	eshell-scroll-to-bottom-on-input 'all
	eshell-scroll-to-bottom-on-output 'all
	eshell-kill-processes-on-exit t
	eshell-glob-case-insensitive t
	eshell-error-if-no-glob t)

  (defun eshell-up-closest-parent-dir (file)
    "Find the closest parent directory of a file.
Argument FILE the file to find the closest parent directory for."
    (file-name-directory
     (directory-file-name
      (expand-file-name file))))

  (defun eshell-up-find-parent-dir (path &optional match)
    "Find the parent directory based on the user's input.
Argument PATH the source directory to search from.
Argument MATCH a string that identifies the parent directory to search for."
    (let ((closest-parent (eshell-up-closest-parent-dir path)))
      (if match
          (let ((case-fold-search nil))
            (locate-dominating-file closest-parent
                                    (lambda (parent)
                                      (let ((dir (file-name-nondirectory
                                                  (expand-file-name
                                                   (directory-file-name parent)))))
					(if (string-match match dir)
                                            dir
                                          nil)))))
	closest-parent)))

  (defun eshell-up (&optional match)
    "Go to a specific parent directory in eshell.
Argument MATCH a string that identifies the parent directory to go
to."
    (interactive)
    (let* ((path default-directory)
           (parent-dir (eshell-up-find-parent-dir path match)))
      (when parent-dir
        (eshell/cd parent-dir)))))

;;;;;; eshell help, highlighting, suggestions, completions

(straight-use-package 'esh-help)
(with-eval-after-load 'eshell
  (setup-esh-help-eldoc))

(straight-use-package 'eshell-syntax-highlighting)
(add-hook 'eshell-mode-hook #'eshell-syntax-highlighting-mode)

(straight-use-package 'esh-autosuggest)
(with-eval-after-load 'esh-autosuggest
  (setq esh-autosuggest-delay 0.5))
(add-hook 'eshell-mode-hook #'esh-autosuggest-mode)

;; TODO: remove this, drop eshell-toggle and write simple popup with shackle and popper
(defun +eshell-toggle-init-eshell (dir)
  "Init eshell in DIR for `eshell-toggle'."
  (let* ((buffer-name (format "*eshell-popup:%s*"
			      (file-name-nondirectory
			       (directory-file-name default-directory))))
	 (eshell-buffer (get-buffer-create buffer-name)))
    (with-current-buffer (switch-to-buffer eshell-buffer)
      (if (eq major-mode 'eshell-mode)
	  (run-hooks 'eshell-mode-hook)
	(eshell-mode))
      (hide-mode-line-mode 1))
    (pop-to-buffer eshell-buffer)))

(straight-use-package 'eshell-toggle)
(with-eval-after-load 'eshell-toggle
  (setq eshell-toggle-size-fraction 3)
  (setq eshell-toggle-use-git-root t)
  (setq eshell-toggle-run-command nil)
  (setq eshell-toggle-init-function #'+eshell-toggle-init-eshell))
(+define-key (kbd "C-M-'") #'eshell-toggle)

;;;; Development

;;;;; Version control

(straight-use-package 'magit)
(unless (fboundp 'magit-get-current-branch)
  (autoload #'magit-get-current-branch "magit" nil t))
(with-eval-after-load 'magit
  (setq magit-no-message
	'("Turning on magit-auto-revert-mode...")))

;; integrate with treemacs
(straight-use-package 'treemacs-magit)
(with-eval-after-load 'magit
  (with-eval-after-load 'treemacs
    (require 'treemacs-magit nil nil)))

;; highlight uncommitted changes (git gutters)
(straight-use-package 'diff-hl)
(autoload #'diff-hl-magit-post-refresh "diff-hl" nil t)
(add-hook 'dired-mode-hook #'diff-hl-dired-mode-unless-remote)
(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
(add-hook 'prog-mode-hook #'diff-hl-mode)
(add-hook 'prog-mode-hook #'diff-hl-margin-mode)

;;;;; Configure parentheses

;; highlight mathing parentesis
(show-paren-mode 1)

;; auto close parentheses in prog mode
(straight-use-package 'smartparens)
(with-eval-after-load 'smartparens
  (require 'smartparens-config))
(add-hook 'prog-mode-hook #'smartparens-mode)
(add-hook 'inferior-emacs-lisp-mode-hook #'smartparens-mode)

;; highlight matching delimiters with rainbow colors
(straight-use-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'inferior-emacs-lisp-mode-hook #'rainbow-delimiters-mode)

;;;;; Syntax checking

;; (progn
;;   (add-hook 'prog-mode-hook #'flycheck-mode))

;; (progn
;;   (straight-use-package 'flycheck-inline)
;;   (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))

;; (progn
;;   (straight-use-package 'consult-flycheck)
;;   (eval-after-load 'flycheck
;;     '(eval-after-load 'consult
;;        '(progn
;; 	  (define-key flycheck-command-map (kbd "!") #'consult-flycheck)))))

;;;;; Enhance compilation buffer

(with-eval-after-load 'compilation-mode
  (require 'ansi-color))

;; colorize compilation buffer
(defun +colorize-compilation-buffer ()
  "Support ANSI colors in compilation buffer."
  (when (eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))

(add-hook 'compilation-filter-hook #'+colorize-compilation-buffer)

;; follow output with scroll in compilation buffer
(setq compilation-scroll-output t)

;;;;; Indentation

;; show indent guides in some modes
(straight-use-package 'highlight-indent-guides)
(with-eval-after-load 'highlight-indent-guides
  (setq highlight-indent-guides-method 'character))
(add-hook 'python-mode-hook #'highlight-indent-guides-mode)
(add-hook 'yaml-mode-hook #'highlight-indent-guides-mode)

;; aggressively indent as you type
(straight-use-package 'aggressive-indent)
(with-eval-after-load 'aggressive-indent
  (setq aggressive-indent-comments-too t)
  (add-to-list 'aggressive-indent-protected-commands 'comment-dwim))
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'scheme-mode-hook #'aggressive-indent-mode)

;;;;; lsp and dap mode

(straight-use-package 'lsp-mode)
(with-eval-after-load 'lsp-mode
  (setq lsp-keymap-prefix "M-RET")
  (setq lsp-enable-which-key-integration t)
  (setq lsp-headerline-breadcrumb-segments
	'(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(straight-use-package 'dap-mode)
(with-eval-after-load 'lsp-mode
  (with-eval-after-load 'dap-mode
    (dap-auto-configure-mode)))

;;;;; Languages

;;;;;; Java

;; TODO parse lombok version from pom
(defvar lombok-jar-path
  (expand-file-name
   "~/.m2/repository/org/projectlombok/lombok/1.18.10/lombok-1.18.10.jar"))

(straight-use-package 'lsp-java)
(with-eval-after-load 'lsp-java
  (add-hook 'java-mode-hook 'lsp)
  (when
      (file-exists-p lombok-jar-path)
    (setq lsp-java-vmargs
	  `("-XX:+UseStringDeduplication" ,(concat "-javaagent:" lombok-jar-path)
	    ,(concat "-Xbootclasspath/a:" lombok-jar-path)
	    "--add-modules=ALL-SYSTEM"))))
(add-hook 'java-mode-hook #'lsp-java)

;;;;;; Groovy

(straight-use-package 'groovy-mode)
(add-hook 'groovy-mode-hook #'lsp-deferred)
(add-to-list 'auto-mode-alist
	     '("\\.groovy\\'" . groovy-mode))

;;;;;; Clojure

(straight-use-package 'clojure-mode)
(add-hook 'clojure-mode-hook #'lsp-deferred)
(add-to-list 'auto-mode-alist
	     '("\\.clj\\'" . clojure-mode))

(straight-use-package 'cider)

;;;;;; Node.js

(straight-use-package 'typescript-mode)
(with-eval-after-load 'typescript-mode
  (require 'dap-node)
  (dap-node-setup)
  (setq typescript-indent-level 2))
(add-hook 'typescript-mode-hook #'lsp-deferred)
(add-to-list 'auto-mode-alist
	     '("\\.ts\\'" . typescript-mode))

(straight-use-package 'npm-mode)
(with-eval-after-load 'typescript-mode
  (require 'npm-mode nil nil))

(straight-use-package 'nvm)

;; TODO: this is global but should be buffer local
(defun +nvm-use (version)
  "Interactive wrapper of `nvm-use'.  Choose node VERSION amongst installed versions."
  (interactive
   (list
    (completing-read "Version: " (mapcar 'car (nvm--installed-versions)))))
  (nvm-use version)
  (message "Using node version %s" version))

(require 's)
(require 'f)

;; TODO: continue prompt of node version in case of missing .nvmrc
;; TODO: this probably sucks
(defun +nvm-project ()
  "Set up project node interpreter with nvm.
If .nvmrc is present in project root, it will be used.  Otherwise node
version will be prompted."
  (interactive)
  (when (file-exists-p (expand-file-name "package.json" (project-root (project-current))))
    (let ((nvmrc (expand-file-name ".nvmrc" (project-root (project-current)))))
      (if (file-exists-p nvmrc)
	  (let ((version (s-trim (f-read nvmrc))))
	    (nvm-use version)
	    (message "Using node version %s" version))
	(call-interactively '+nvm-use)))))

;;;;;; Python

(straight-use-package 'elpy)
(advice-add 'python-mode :before 'elpy-enable)
(setq python-shell-interpreter "python3.8" python-shell-interpreter-args "-i")
(with-eval-after-load 'elpy
  (setq elpy-get-info-from-shell t)
  (setq elpy-shell-echo-input nil)
  (setq elpy-syntax-check-command "pyflakes")
  (elpy-shell-set-local-shell
   (elpy-project-root))
  (when
      (load "flycheck" t t)
    (message "loading flycheck for elpy")
    (setq elpy-modules
	  (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode)))

;;;;;; Rust

(straight-use-package 'rust-mode)
(with-eval-after-load 'rust-mode
  (straight-use-package 'flycheck-rust)
  (require 'flycheck-rust nil nil)
  (straight-use-package 'cargo)
  (add-hook 'rust-mode-hook #'cargo-minor-mode)
  (straight-use-package 'racer)
  (with-eval-after-load 'racer
    (progn
      (add-hook 'racer-mode-hook #'company-mode)
      (add-hook 'racer-mode-hook #'eldoc-mode)))
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'rust-mode-hook 'flycheck-mode)
  (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)
  (add-hook 'before-save-hook
	    (lambda nil
	      (when
		  (eq major-mode 'rust-mode)
		(rust-format-buffer))))
  (define-key rust-mode-map ("C-c C-t") #'racer-describe))
(add-to-list 'auto-mode-alist
	     '("\\.rs\\'" . rust-mode))

;;;;;; C#

(straight-use-package 'csharp-mode)
(add-hook 'csharp-mode-hook 'lsp)
(add-to-list 'auto-mode-alist
	     '("\\.cs\\'" . csharp-mode))

;;;;;; LaTeX

;; add a preview pane of the current edited LaTeX buffer.
(straight-use-package 'latex-preview-pane)
(add-hook 'latex-mode-hook #'latex-preview-pane-mode)

;;;;;; configuration and markup

(straight-use-package 'dockerfile-mode)
(add-to-list 'auto-mode-alist
	     '("Dockerfile" . dockerfile-mode))

(straight-use-package 'markdown-mode)
(unless
    (fboundp 'gfm-mode)
  (autoload #'gfm-mode "markdown-mode" nil t))
(setq mardown-command "multimarkdown")
(add-to-list 'auto-mode-alist
	     '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist
	     '("\\.mkd\\'" . markdown-mode))
(add-to-list 'auto-mode-alist
	     '("\\.markdown\\'" . markdown-mode))

(straight-use-package 'yaml-mode)
(add-to-list 'auto-mode-alist
	     '("\\.ya?ml\\'" . yaml-mode))

(straight-use-package 'toml-mode)
(add-to-list 'auto-mode-alist
	     '("\\.toml\\'" . toml-mode))

(straight-use-package 'csv-mode)
(add-to-list 'auto-mode-alist
	     '("\\.csv\\'" . csv-mode))

(add-to-list 'auto-mode-alist
	     '("\\.tf\\'" . terraform-mode))

;;; init.el ends here
