;;; init-ui.el --- UI configuration -*- lexical-binding: t -*-

;; Copyright (C) 2020  Tommaso Rossi

;; Author: Tommaso Rossi <tommaso.rossi1@protonmail.com>
;; URL: https://github.com/tomrss/conf/emacs
;; Version: 0.1.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides UI configuration.  Main features are:
;; - clean UI from menu, scrollbar, etc
;; - theme
;; - doom modeline
;; - line numbers for prog modes and text modes
;; - colorized compilation buffer (useful for Maven builds)

;;; System requirements:

;; JetBrainsMono font: sudo pacman -S ttf-jetbrains-mono
;; `M-x all-the-icons-install-fonts'

;;; Key bindings:

;; none

;;; Code:
(require 'ansi-color)

;; set default face
(set-face-attribute 'default nil :font "JetBrainsMono" :height 110)
;; set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "JetBrainsMono" :height 110)
;; set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 150 :weight 'regular)

;; Other interesting fonts:
;; - DejaVu Sans Mono 13
;; - Inconsolata 14
;; - Hack 13
;; - Fira Code Retina 13
(set-frame-parameter (selected-frame) 'alpha '(93 . 93))
(add-to-list 'default-frame-alist '(alpha . (93 . 93)))

;; (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; startup message with load time
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)

;; load theme
(use-package doom-themes
  :init (load-theme 'doom-nord t))
;; other interesting theme: misterioso

;; configure icons and modeline
;; M-x all-the-icons-install-fonts RET may be needed
(use-package all-the-icons)
;; TODO this is UI
(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode 0)))
(column-number-mode)

;; colorize compilation buffer
(defun colorize-compilation-buffer ()
  "Support ANSI colors in compilation buffer."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(use-package page-break-lines)

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  (setq dashboard-set-init-info t)
  (setq dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)
  (setq dashboard-projects-backend 'projectile)
  (add-to-list 'dashboard-items '(projects . 5))
  (setq dashboard-banner-logo-title "Welcome to GNU Emacs")
  )

;; (setq initial-buffer-choice 'eshell)

(provide 'init-ui)
;;; init-ui.el ends here
