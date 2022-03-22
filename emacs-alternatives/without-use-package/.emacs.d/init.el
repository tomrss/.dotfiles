;;; init.el --- Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2021 Tommaso Rossi

;; Author: Tommaso Rossi <tommaso.rossi1@protonmail.com

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; GNU Emacs configuration.

;;; Code:

(require 'seq)

;;;; Early configuration

;;; Early UI tweaks

;; disable unwanted ui components
(menu-bar-mode -1)
(when (symbol-value 'window-system)
  (toggle-scroll-bar -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1))
;; mute the bell
(setq ring-bell-function 'ignore)
;; remove graphical dialog box and keep it keyboard driven
(setq use-dialog-box nil)
;; silent native compilation warning
(setq native-comp-async-report-warnings-errors 'silent)

;;; Manage backups

;; disable lockfiles
(setq create-lockfiles nil)

;; put backups in one folder
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

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

;;;; Packaging

;;; Configure `straight.el' as package manager

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

;;;; Key bindings

;; define one centralized minor mode holding custom keys
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

;;; Getting help and docs

;; improve self documentation
(straight-use-package 'helpful)
(+define-key [remap describe-command] #'helpful-command)
(+define-key [remap describe-function] #'helpful-callable)
(+define-key [remap describe-key] #'helpful-key)
(+define-key [remap describe-symbol] #'helpful-symbol)
(+define-key [remap describe-variable] #'helpful-variable)
(+define-key (kbd "C-h o") #'helpful-symbol)
(+define-key (kbd "C-h p") #'helpful-at-point)

;; hint keybindings
(straight-use-package 'which-key)
(which-key-mode +1)
(with-eval-after-load 'which-key-mode
  (setq which-key-idle-delay 0.5))

;; help and docs in minibuffer
(straight-use-package 'marginalia)
(marginalia-mode +1)

;;; Completion styles and functions

;; completion style (how completion candidates are narrowed)
(straight-use-package 'orderless)
(setq orderless-matching-styles
      '(orderless-literal orderless-initialism orderless-regexp))
(setq orderless-component-separator "[ +]+")
(setq completion-styles '(orderless))

;; persist selections with builtin savehist mode
(setq history-length 20)
(savehist-mode +1)

;; completing read functions
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

(defun +consult-preview-p ()
  "Helper function to find out if Consult is previewing."
  (when-let (win (active-minibuffer-window))
    (not (eq nil (buffer-local-value
                  'consult--preview-function
                  (window-buffer win))))))

;;; Minibuffer completions

;; completion UI
(straight-use-package 'vertico)
(vertico-mode +1)
(define-key vertico-map (kbd "C-j") #'vertico-next)
(define-key vertico-map (kbd "C-k") #'vertico-previous)

;; enable acting on minibuffer candidates (and much more)
(straight-use-package 'embark)
(setq prefix-help-command #'embark-prefix-help-command)
(+define-key (kbd "C-.") #'embark-act)
(with-eval-after-load 'embark
  (define-key embark-symbol-map (kbd "h") #'helpful-symbol))

(straight-use-package 'embark-consult)
(autoload #'embark-consult-preview-minor-mode "embark-consult")
(add-hook 'embark-collect-mode-hook #'embark-consult-preview-minor-mode)

;;; Completions in region

;; completion UI
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

(setq tab-always-indent 'complete)

;;; Completion at point functions

(straight-use-package 'cape)

(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)

;; TODO following commented code seems promising but is not tested
;; ;; Silence the pcomplete capf, no errors or messages!
;; (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

;; ;; Ensure that pcomplete does not write to the buffer
;; ;; and behaves as a pure `completion-at-point-function'.
;; (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
;; (add-hook 'eshell-mode-hook
;;           (lambda ()
;;             (setq-local corfu-quit-at-boundary t
;;                         corfu-quit-no-match t
;;                         corfu-auto nil)
;;             (corfu-mode)))

;;;; Editing

;; navigable undo/redo tree
;; TODO disabled because at some point it started to put ~undo-tree~ junk files everywhere...
;; (straight-use-package 'undo-tree)
;; (global-undo-tree-mode +1)

;;; Vim emulation

;; base evil configuration
(straight-use-package 'evil)
(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(setq evil-want-C-u-scroll t)
(setq evil-want-C-i-jump nil)
(setq evil-respect-visual-line-mode t)
;; (setq evil-undo-system 'undo-tree)
(evil-mode 1)

;; automatically configure evil for some common modes
(straight-use-package 'evil-collection)
(with-eval-after-load 'evil
  (evil-collection-init))

(add-hook 'with-editor-mode-hook 'evil-insert-state)

;;; Editing defaults

;; ask y or n instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)
;; recent files
(recentf-mode +1)
;; reopen file at same point
(save-place-mode +1)
;; keep all buffers updated if external program change content
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode +1)
;; share system clipboard
(setq save-interprogram-paste-before-kill t)
(setq kill-do-not-save-duplicates t)

;;; Text selection and navigation

;; increases the selected region by semantic units
(straight-use-package 'expand-region)
(+define-key (kbd "C-ò") #'er/expand-region)

;;;; User interface

;; recognize system
(defconst IS-GNU     (eq system-type 'gnu/linux))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

;;; Fonts and icons

(when (symbol-value 'window-system)
  (when (x-list-fonts "JetBrains Mono NL")
    (set-face-attribute 'default     nil :font "JetBrains Mono NL" :height 110 :weight 'normal)
    (set-face-attribute 'fixed-pitch nil :font "JetBrains Mono NL" :height 110 :weight 'normal))
  (when (x-list-fonts "Cantarell")
    (set-face-attribute 'variable-pitch nil :font "Cantarell" :height 130 :weight 'normal)))

(straight-use-package 'all-the-icons)
(when (symbol-value 'window-system)
  (unless (x-list-fonts "all-the-icons")
    (if IS-WINDOWS
		(warn "Run M-x all-the-icons-install-fonts to download the fonts, then install them manually")
      (all-the-icons-install-fonts t)))
  (require 'all-the-icons nil nil))

;;; Theme

(if (symbol-value 'window-system)
    (progn
	  ;; use doom themes
      (straight-use-package 'doom-themes)
      (load-theme 'doom-nord t)
      (set-face-attribute 'font-lock-doc-face nil :foreground "#EBCB8B"))
  ;; TODO don't remember why i did this:
  ;; (set-face-attribute 'shadow nil :foreground "#EBCB8B"))
  ;; use builtin theme
  (load-theme 'misterioso))

;;; Modeline

(when (symbol-value 'window-system)
  ;; TODO doom-modeline is great but it's heavy, write something smaller
  (straight-use-package 'doom-modeline)
  (doom-modeline-mode +1))

;;; Line and column numbers

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))

(column-number-mode +1)

;;; Smooth scrolling

(setq fast-but-imprecise-scrolling t)
(setq scroll-margin 1)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position 1)

;;; Highlight current line

(straight-use-package 'hl-line)
(setq hl-line-sticky-flag nil)
(add-hook 'dired-mode-hook #'hl-line-mode)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'special-mode-hook #'hl-line-mode)

;;; File management

;; configure Dired
(with-eval-after-load 'dired
  ;; dired defualts
  (setq dired-auto-revert-buffer t)
  (setq dired-dwim-target t)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'top)
  (setq dired-create-destination-dirs 'ask)
  (setq dired-listing-switches "-agho --group-directories-first")
  (when IS-MAC
    (setq dired-use-ls-dired nil))
  (setq delete-by-moving-to-trash t)

  ;; hide/show dotfiles in Dired (taken from 'dired-hide-dotfiles' package)
  ;; TODO could be done by hooking a setq on dired-listing-switches ?
  (defun dired-hide-dotfiles--hide ()
	"Hide all dot-files in the current `dired' buffer."
	(let ((inhibit-message t))
      (dired-mark-files-regexp "^\\."))
	(dired-do-kill-lines nil "Hidden %d dotfile%s."))

  (define-minor-mode dired-hide-dotfiles-mode
	"Toggle `dired-hide-dotfiles-mode'"
	:init-value nil
	:lighter " !."
	:group 'dired
	(if dired-hide-dotfiles-mode
		(progn
          (add-hook 'dired-after-readin-hook 'dired-hide-dotfiles--hide)
          (dired-hide-dotfiles--hide))
      (remove-hook 'dired-after-readin-hook 'dired-hide-dotfiles--hide)
      (revert-buffer)))

  (evil-define-key 'normal dired-mode-map (kbd "g h") #'dired-hide-dotfiles-mode))

(+define-key (kbd "C-x j") #'dired-jump)

;; use icons in dired
(when (symbol-value 'window-system)
  (straight-use-package 'all-the-icons-dired)
  (with-eval-after-load 'dired
    (add-hook 'dired-mode-hook #'all-the-icons-dired-mode)))

;; colorize dired
(straight-use-package 'diredfl)
(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook #'diredfl-mode))

;;; Project tree view

;; i wish i could get rid of treemacs, but lsp (dap) requires it...
(straight-use-package 'treemacs)
(with-eval-after-load 'treemacs
  (treemacs-resize-icons 16))

;; integrate with magit
(straight-use-package 'treemacs-magit)
(with-eval-after-load 'magit
  (with-eval-after-load 'treemacs
    (require 'treemacs-magit nil nil)))

;; integrate with evil
(straight-use-package 'treemacs-evil)
(with-eval-after-load 'evil
  (with-eval-after-load 'treemacs
    (require 'treemacs-evil nil nil)))

;;; Process management

(with-eval-after-load 'proced
  (setq proced-auto-update-interval 5)
  (proced-toggle-auto-update 1))

;;; Visual fill mode

(straight-use-package 'visual-fill-column)

(defun +setup-visual-fill (width)
  "Setup visual line and column centered with WIDTH."
  (setq visual-fill-column-width width)
  (setq visual-fill-column-center-text t)
  (visual-line-mode +1)
  (visual-fill-column-mode +1))

;;; Starting screen
(when (symbol-value 'window-system)
  (straight-use-package '(welcome
                          ;; i wrote this package and it's not great
                          ;; TODO at least add a readme in it
                          ;; TODO make it private because it sucks
                          :repo "https://github.com/tomrss/welcome.el"
                          :files ("welcome.el" "asset")))

  (with-eval-after-load 'welcome
    (setq welcome-menu-items
          '(("Recent files"
             :key "f"
             :action consult-recent-file
             :icon "history")
            ("Projects"
             :key "p"
             :action project-switch-project
             :icon "code")
            ("Dired"
             :key "d"
             :action dired
             :icon "file-directory")
            ("Edit configuration"
             :key "c"
             :action (lambda ()
                       (interactive)
                       (find-file user-init-file))
             :icon "gear")
            ("Eshell"
             :key "e"
             :action eshell
             :icon "terminal")
            ("Scratch"
             :key "s"
             :action (lambda ()
                       (interactive)
                       (switch-to-buffer "*scratch*"))
             :icon "file-text")))

    (evil-set-initial-state 'welcome-mode 'emacs)
    (define-key welcome-mode-map (kbd "j") #'next-line)
    (define-key welcome-mode-map (kbd "k") #'previous-line)
    (add-hook 'welcome-mode-hook
              (lambda () (+setup-visual-fill welcome-window-width))))

  (add-hook 'emacs-startup-hook #'welcome-screen))

;;;; Org mode

(with-eval-after-load 'org
  ;; setup visual fill
  (add-hook 'org-mode-hook (lambda () (+setup-visual-fill 100)))
  ;; org babel languages
  (require 'org-tempo)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
	 (python . t)
	 (shell . t)))

  ;; templates for adding code snippets
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("yml" . "src yaml"))
  (add-to-list 'org-structure-template-alist '("json" . "src json"))

  ;; some defaults
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
  (setq org-startup-with-inline-images t)

  ;; auto tangle on save
  (defun +org-auto-tangle ()
	"Set hook for auto tangling org files on save."
	(let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle)))

  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'after-save-hook #'+org-auto-tangle 0 t))))

;;; Org Roam
;; TODO work in progress
;; TODO disabled until i know how to use it
;; (straight-use-package 'org-roam)
;; (setq org-roam-v2-ack t)
;; (with-eval-after-load 'org-roam
;;   (setq org-roam-directory "~/RoamNotes")
;;   (org-roam-setup))
;; ;; TODO keys: (bind-keys :package org-roam
;; ;; 	     ("C-c n l" . org-roam-buffer-toggle)
;; ;; 	     ("C-c n f" . org-roam-node-find)
;; ;; 	     ("C-c n i" . org-roam-node-insert)))

;;;; Windows and buffer management

;;; Stateful window layout

(winner-mode +1)
(+define-key (kbd "C-c w") 'winner-undo)
(+define-key (kbd "C-c W") 'winner-redo)

;;; Window selection and navigation

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

;;; Window placement and popups

;; define window placement rules 
(straight-use-package 'shackle)
(shackle-mode +1)
(setq shackle-rules
      '((compilation-mode :noselect t)
		(help-mode :popup t :select t :align bottom :size 0.33)
		(helpful-mode :popup t :select t :align bottom :size 0.33)
		("\\*.*-e?shell\\*\\'" :regexp t :popup t :select t :align bottom :size 0.33)
		("\\*Warnings\\*" :regexp t :noselect t))
      shackle-default-rule
      '(:noselect t))

;; define and manage popup buffers
(straight-use-package 'popper)
(setq popper-reference-buffers
	  (mapcar #'car
			  (seq-filter (lambda (rule) (plist-get (cdr rule) :popup))
				          shackle-rules)))
(setq popper-mode-line
      '(:eval (propertize " P " 'face 'mode-line-emphasis)))
(setq popper-display-control nil)
(popper-mode +1)
(+define-key (kbd "C-è") #'popper-toggle-latest)
(+define-key (kbd "M-è") #'popper-cycle)
(+define-key (kbd "C-M-è") #'popper-toggle-type)

;;; Buffer helpers

;; use `ibuffer' instead of buffer list
(+define-key (kbd "C-x C-b") 'ibuffer)

;; TODO this is probably dangerous
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

;;; term

;; colorize term
(straight-use-package 'eterm-256color)
(with-eval-after-load 'term
  (add-hook 'term-mode-hook #'eterm-256color-mode))

;;; vterm

(straight-use-package 'vterm)
(with-eval-after-load 'vterm
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (setq vterm-max-scrollback 10000)
  (setq vterm-kill-buffer-on-exit t))

;;; eshell

;; eshell prompt
(with-eval-after-load 'em-prompt
  (defun +prompt-path ()
    "Path (pwd) that will be displayed in prompt."
    (let* ((pwd (eshell/pwd)))
      (if (equal pwd "~")
		  pwd
		(abbreviate-file-name pwd))))

  (defun +eshell-prompt ()
    "The eshell prompt."
    (let ((current-branch (when (fboundp 'magit-get-current-branch)
							(magit-get-current-branch))))
      (concat
       (if (bobp) "" "\n")
       (propertize user-login-name 'face 'font-lock-keyword-face)
       (propertize " • " 'face `(:foreground "white"))
       (propertize (+prompt-path) 'face 'font-lock-function-name-face)
       (when current-branch
		 (concat
		  (propertize " • " 'face `(:foreground "white"))
		  (propertize (concat " " current-branch) 'face 'font-lock-string-face)))
       (propertize " • " 'face `(:foreground "white"))
       (propertize (format-time-string "%H:%M:%S") 'face 'font-lock-comment-face)
       (let ((user-prompt
			  (if (= (user-uid) 0) "\n#" "\nλ")))
		 (propertize user-prompt 'face (if (zerop eshell-last-command-status) 'success 'error)))
       " ")))

  (setq eshell-prompt-function #'+eshell-prompt
		eshell-prompt-regexp "^.*λ "
		eshell-highlight-prompt t))

;; eshell banner
(with-eval-after-load 'em-banner
  (setq eshell-banner-message
		'(format "%s %s\n\n"
				 (propertize (format " %s " (string-trim (buffer-name)))
							 'face 'mode-line-highlight)
				 (propertize (current-time-string)
							 'face 'font-lock-keyword-face))))

;; eshell keys and aliases
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

;; eshell colors
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

;; eshell history
(with-eval-after-load 'em-hist
  (add-hook 'eshell-pre-command-hook #'eshell-save-some-history)
  (setq eshell-history-size 10000
		eshell-history-ignoredups t
		eshell-input-filter #'eshell-input-filter-initial-space
		;; don't record command in history if prefixed with whitespace
		eshell-input-filter #'eshell-input-filter-initial-space)
  (eshell-hist-initialize))

;; eshell visual commands
(with-eval-after-load 'em-term
  (dolist (cmd '("htop" "vim" "nvim"))
    (add-to-list 'eshell-visual-commands cmd)))

;; eshell defaults and generic conf
(with-eval-after-load 'eshell
  (setenv "PAGER" "cat")
  ;; truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions #'eshell-truncate-buffer)
  ;; use TRAMP
  (add-to-list 'eshell-modules-list 'eshell-tramp)
  ;; enable autopairing in eshell
  (add-hook 'eshell-mode-hook #'smartparens-mode)

  (setq password-cache t
		password-cache-expiry 3600)

  (setq eshell-buffer-maximum-lines 10000
		eshell-scroll-to-bottom-on-input 'all
		eshell-scroll-to-bottom-on-output 'all
		eshell-kill-processes-on-exit t
		eshell-glob-case-insensitive t
		eshell-error-if-no-glob t)

  ;; directory navigation
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
            (locate-dominating-file
			 closest-parent
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

;; eshell help and docs
;; TODO not convinced about this one
;; (straight-use-package 'esh-help)
;; (with-eval-after-load 'eshell
;;   (setup-esh-help-eldoc))

;; eshell syntax highlighting
(straight-use-package 'eshell-syntax-highlighting)
(add-hook 'eshell-mode-hook #'eshell-syntax-highlighting-mode)

;; eshell suggestions in fish style
;; TODO this package requires company...
;; TODO find another
;; (straight-use-package 'esh-autosuggest)
;; (with-eval-after-load 'esh-autosuggest
;;   (setq esh-autosuggest-delay 0.5))
;; (add-hook 'eshell-mode-hook #'esh-autosuggest-mode)

;;;; Development

;;; Version control

;; magit package
(straight-use-package 'magit)
(unless (fboundp 'magit-get-current-branch)
  (autoload #'magit-get-current-branch "magit" nil t))
(with-eval-after-load 'magit
  (setq magit-no-message
		'("Turning on magit-auto-revert-mode...")))

;; integrate with project
(require 'project)			; not sure if needed
(define-key project-prefix-map (kbd "G") #'magit-status)
(add-to-list 'project-switch-commands '(magit-status "Magit"))

(+define-key (kbd "C-x g") #'magit-status) ; is the default but it's somehow deleted

;; highlight changes (git gutters)
(straight-use-package 'diff-hl)
(autoload #'diff-hl-magit-post-refresh "diff-hl" nil t)
(add-hook 'dired-mode-hook #'diff-hl-dired-mode-unless-remote)
(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
(add-hook 'prog-mode-hook #'diff-hl-mode)
(add-hook 'prog-mode-hook #'diff-hl-margin-mode)

;;; Configure parentheses

;; highlight mathing parentesis
(show-paren-mode +1)
;; propably these tree lines can be collapsed in one set face attribute
(set-face-background 'show-paren-match nil)
(set-face-foreground 'show-paren-match "red")
(set-face-italic 'show-paren-match t)

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

;;; Syntax checking

(straight-use-package 'flycheck)
(straight-use-package 'flycheck-inline)
(straight-use-package 'consult-flycheck)
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-inline-mode)
  (with-eval-after-load 'consult
	(define-key flycheck-command-map (kbd "!") #'consult-flycheck)))

;; (add-hook 'prog-mode-hook #'flycheck-mode)

;;; Enhance compilation buffer

;; colorize compilation buffer
(with-eval-after-load 'compilation-mode
  (require 'ansi-color))

(defun +colorize-compilation-buffer ()
  "Support ANSI colors in compilation buffer."
  (when (eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))

(add-hook 'compilation-filter-hook #'+colorize-compilation-buffer)

;; follow output with scroll in compilation buffer
(setq compilation-scroll-output t)

;;; Indentation

;; human tab with
(add-hook 'prog-mode-hook (lambda () (setq tab-width 4)))
;; indent with spaces
(setq-default indent-tabs-mode nil)

;; setup for showing indent guides (configure hook in specific mode)
(straight-use-package 'highlight-indent-guides)
(with-eval-after-load 'highlight-indent-guides
  (setq highlight-indent-guides-method 'character))

;; aggressively indent as you type
;; TODO this sometimes interfere with undo
(straight-use-package 'aggressive-indent)
(with-eval-after-load 'aggressive-indent
  (setq aggressive-indent-comments-too t)
  (add-to-list 'aggressive-indent-protected-commands 'comment-dwim))
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'scheme-mode-hook #'aggressive-indent-mode)

;;; LSP mode

;; code snippets
(straight-use-package 'yasnippet)

;; language server protocol
(straight-use-package 'lsp-mode)
(setq lsp-keymap-prefix "M-RET")	; don't know why, if inside after load doesn't work...
(with-eval-after-load 'lsp-mode
  ;; try to fix completions
  ;; TODO corfu do not work really well with lsp
  (defun +corfu-lsp-setup ()
    "Try to setup corfu in lsp mode."
    (setq-local completion-styles '(orderless))
    (setq-local completion-category-defaults nil))
  (add-hook 'lsp-mode-hook #'+corfu-lsp-setup)
  (setq lsp-completion-provider :none)
  ;; enable snippets
  (add-hook 'lsp-mode-hook #'yas-minor-mode)
  ;; show path of current file
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode)
  ;; suggest keys
  (setq lsp-enable-which-key-integration t))

(defun +lsp-really-deferred ()
  "Load LSP deferred excluding consult previews."
  (unless (+consult-preview-p)
    (lsp-deferred)))

;; debugger
(straight-use-package 'dap-mode)
(with-eval-after-load 'lsp-mode
  (with-eval-after-load 'dap-mode
    (define-key dap-mode-map (kbd "<f8>") #'dap-next)
    (define-key dap-mode-map (kbd "<f9>") #'dap-continue)
    (define-key dap-mode-map (kbd "<f7>") #'dap-step-in)
    (define-key dap-mode-map (kbd "S-<f8>") #'dap-step-out)
    (define-key dap-mode-map (kbd "M-<f8>") #'dap-evaluate)
    (dap-auto-configure-mode)))

;; quick and dirty auto download and compile ls from source
(defun +build-lsp-server-from-git (url ls-install-dir build-command)
  "Build LSP language server in LS-INSTALL-DIR from git source at URL with BUILD-COMMAND."
  (unless (file-directory-p (expand-file-name ".git" ls-install-dir))
    (when (y-or-n-p "Language server not found. Do you want to build one from source? ")
      ;; todo use compilation-start
      (with-current-buffer (get-buffer-create "*custom-language-server-build*")
		(display-buffer (current-buffer))
		(let ((git-clone-cmd (format "git clone %s %s" url ls-install-dir)))
		  (call-process-shell-command git-clone-cmd nil t t)
		  ;; TODO handle error
		  (goto-char (point-max))
		  (insert "git clone done.\n")
		  (let ((default-directory ls-install-dir))
			(display-buffer (current-buffer))
			(call-process-shell-command build-command nil t t))
		  (message "Custom LSP server install success (probably). Check *custom-language-server-build* buffer"))))))

;;; Java

;; TODO parse lombok version from pom
(defvar lombok-jar-path
  (expand-file-name
   "~/.m2/repository/org/projectlombok/lombok/1.18.10/lombok-1.18.10.jar")
  "Path of the lombok jar.")

(straight-use-package 'lsp-java)
(with-eval-after-load 'lsp-java
  (when (file-exists-p lombok-jar-path)
    (setq lsp-java-vmargs
		  `("-XX:+UseStringDeduplication" ,(concat "-javaagent:" lombok-jar-path)
			,(concat "-Xbootclasspath/a:" lombok-jar-path)
			"--add-modules=ALL-SYSTEM"))))

(defun +setup-java ()
  "Setup java with LSP."
  (unless (+consult-preview-p)
    (lsp-deferred)
    (require 'lsp-java)
    (require 'dap-java)))

(add-hook 'java-mode-hook #'+setup-java)

;;; Groovy

(straight-use-package 'groovy-mode)
(add-to-list 'auto-mode-alist '("\\.groovy\\'" . groovy-mode))
(with-eval-after-load 'groovy-mode
  ;; TODO: this classpath stuff was added because groovy ls seems
  ;;  to autocomplete only with java but not with groovy.
  ;;  sadly this doesn't work either!
  ;; (setq lsp-groovy-classpath
  ;; 	(vconcat
  ;; 	 (split-string
  ;; 	  (with-temp-buffer
  ;; 	    (call-process-shell-command "find /usr -wholename '*/groovy/lib/*.jar'" nil t nil)
  ;; 	    (buffer-string)))))
  ;; TODO move server installation in lsp-groovy source
  (defvar groovy-ls-install-dir "~/.emacs.d/.cache/lsp/groovy-language-server/"
    "Groovy language server installation folder")
  ;; TODO check if default server exists before installing custom
  (+build-lsp-server-from-git "https://github.com/GroovyLanguageServer/groovy-language-server.git"
							  groovy-ls-install-dir
							  "./gradlew build")
  ;; TODO handle execution status
  (setq lsp-groovy-server-file
		(expand-file-name "build/libs/groovy-language-server-all.jar"
                          groovy-ls-install-dir)))

(add-hook 'groovy-mode-hook #'+lsp-really-deferred)

;;; Clojure

(straight-use-package 'clojure-mode)
(add-hook 'clojure-mode-hook #'+lsp-really-deferred)
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))

(straight-use-package 'cider)

;;; Node.js

(straight-use-package 'typescript-mode)
(with-eval-after-load 'typescript-mode
  (require 'dap-node)
  (dap-node-setup)
  (setq typescript-indent-level 2))
(add-hook 'typescript-mode-hook #'+lsp-really-deferred)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

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
  (when (file-exists-p (expand-file-name "package.json"
                                         (project-root (project-current))))
    (let ((nvmrc (expand-file-name ".nvmrc" (project-root (project-current)))))
      (if (file-exists-p nvmrc)
		  (let ((version (s-trim (f-read nvmrc))))
			(nvm-use version)
			(message "Using node version %s" version))
		(call-interactively '+nvm-use)))))

;;; Python
;; TODO refactor with lsp

(add-hook 'python-mode-hook #'highlight-indent-guides-mode)

;;; Rust

(straight-use-package 'rust-mode)
(with-eval-after-load 'rust-mode
  (straight-use-package 'flycheck-rust)
  (require 'flycheck-rust nil nil)
  (straight-use-package 'cargo)
  (add-hook 'rust-mode-hook #'cargo-minor-mode)
  (straight-use-package 'racer)
  (with-eval-after-load 'racer
    (progn
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
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;;; C#

(straight-use-package 'csharp-mode)
(add-hook 'csharp-mode-hook 'lsp)
(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))

;;; Go

(straight-use-package 'go-mode)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
(add-hook 'go-mode-hook #'+lsp-really-deferred)
(add-hook 'go-mode-hook (lambda () (setq indent-tabs-mode t)))

;;; LaTeX

;; add a preview pane of the current edited LaTeX buffer.
(straight-use-package 'latex-preview-pane)
(add-hook 'latex-mode-hook #'latex-preview-pane-mode)

;;; Configuration and markup

;; dockerfile
(straight-use-package 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile" . dockerfile-mode))

;; markdown
(straight-use-package 'markdown-mode)
(unless
    (fboundp 'gfm-mode)
  (autoload #'gfm-mode "markdown-mode" nil t))
(setq mardown-command "multimarkdown")
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mkd\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

;; yaml
(straight-use-package 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))
(add-hook 'yaml-mode-hook #'highlight-indent-guides-mode)
(add-hook 'yaml-mode-hook 
          (lambda ()
            (set (make-local-variable 'font-lock-variable-name-face)
                 'font-lock-type-face)))

;; toml
(straight-use-package 'toml-mode)
(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-mode))

;; csv
(straight-use-package 'csv-mode)
(add-to-list 'auto-mode-alist '("\\.csv\\'" . csv-mode))

;; terraform
(straight-use-package 'terraform-mode)
(add-to-list 'auto-mode-alist '("\\.tf\\'" . terraform-mode))
(add-hook 'terraform-mode-hook #'+lsp-really-deferred)

;;; init.el ends here
