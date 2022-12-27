;;; mod-ui.el --- User interface module -*- lexical-binding: t -*-

;; Copyright (C) 2022 Tommaso Rossi

;; Author: Tommaso Rossi <tommaso.rossi1@protonmail.com

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Module for enhancing and personalizing user interface, including
;; fonts, icons, modeline, theme, dired and other.

;;; Code:

(eval-and-compile
  (require 'project))

;; recognize system
(defconst IS-GNU     (eq system-type 'gnu/linux))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

;;; Fonts and icons
(when (display-graphic-p)
  (when (x-list-fonts "JetBrains Mono NL")
    (set-face-attribute 'default     nil :font "JetBrains Mono NL" :height 110 :weight 'normal)
    (set-face-attribute 'fixed-pitch nil :font "JetBrains Mono NL" :height 110 :weight 'normal))
  (when (x-list-fonts "Cantarell")
    (set-face-attribute 'variable-pitch nil :font "Cantarell" :height 130 :weight 'normal)))

(+install-package 'all-the-icons)
(when (display-graphic-p)
  (require 'all-the-icons nil nil)
  (unless (x-list-fonts "all-the-icons")
    (if IS-WINDOWS
	    (warn "RuntimeWarning M-x all-the-icons-install-fonts to download the fonts, then install them manually")
      (all-the-icons-install-fonts t))))

;;; Theme

;; use doom themes
;; (+install-package 'doom-themes)
;; (load-theme 'doom-nord t)
;; (set-face-attribute 'font-lock-doc-face nil :foreground "#EBCB8B")
;; (set-face-attribute 'completions-annotations nil :foreground "#EBCB8B")

(setq modus-themes-mode-line '(accented))
(setq modus-themes-bold-constructs t)
(setq modus-themes-italic-constructs t)
(load-theme 'modus-vivendi)

;;; Modeline

(+install-package 'doom-modeline)
(setq doom-modeline-icon (display-graphic-p))
(doom-modeline-mode +1)

;;; Line and column numbers

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))

(column-number-mode +1)

;;; Smooth scrolling

(pixel-scroll-precision-mode +1)
(setq fast-but-imprecise-scrolling t)
(setq scroll-margin 1)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position 1)

;;; Highlight current line

(+install-package 'hl-line)
(with-eval-after-load 'hl-line
  (setq hl-line-sticky-flag nil))
(add-hook 'dired-mode-hook #'hl-line-mode)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'special-mode-hook #'hl-line-mode)

;;; File management

;; configure Dired
(with-eval-after-load 'dired
  ;; dired defaults
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
(+install-package 'all-the-icons-dired)
(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook #'all-the-icons-dired-mode))

;; colorize dired
(+install-package 'diredfl)
(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook #'diredfl-mode))

;;; Process management

(with-eval-after-load 'proced
  (setq proced-auto-update-interval 5)
  (proced-toggle-auto-update 1))

;;; Visual fill mode

(+install-package 'visual-fill-column)

(defun +setup-visual-fill (width)
  "Setup visual line and column centered with WIDTH."
  (setq visual-fill-column-width width)
  (setq visual-fill-column-center-text t)
  (visual-line-mode +1)
  (visual-fill-column-mode +1))

;;; Starting screen

;; i wrote this package and it's not great
;; TODO at least add a readme in it
;; TODO make it private because it sucks
(+install-package '(welcome
                    :type git
                    :host github
                    :repo "tomrss/welcome.el"
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
           :action +edit-emacs-config
           :icon "gear")
          ("Eshell"
           :key "e"
           :action eshell
           :icon "terminal")
          ("Scratch"
           :key "s"
           :action scratch-new
           :icon "file-text")))

  (evil-set-initial-state 'welcome-mode 'emacs)
  (define-key welcome-mode-map (kbd "j") #'next-line)
  (define-key welcome-mode-map (kbd "k") #'previous-line)
  (add-hook 'welcome-mode-hook
            (lambda () (+setup-visual-fill welcome-window-width))))

(add-hook 'emacs-startup-hook #'welcome-screen)

(provide 'mod-ui)
;;; mod-ui.el ends here
