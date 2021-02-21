;;; init-management.el --- Project and files management -*- lexical-binding: t -*-

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

;; This package provides configuration for project, files and buffer management,
;; most notably with dired basic configuration, magit git client,
;; projectile for project-based functions and treemacs for project tree view.

;;; System requirements:

;; git
;; ripgrep.  Is not really a dependency but it allows nice `projectile-ripgrep' command.


;;; Key bindings:

;; C-c p: `projectile-command-map'.  Prefix for projectile commands.

;;; Code:
(setq delete-by-moving-to-trash t)

;; some buffer management interactive functions
(defun tr/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun tr/kill-mode-buffers (mode)
  "Kill all buffers with major mode MODE."
  (interactive
   (list
    (intern
     (completing-read "Mode: "
		      (delete-dups
		       (mapcar (lambda (buffer)
				 (buffer-local-value 'major-mode buffer))
			       (buffer-list)))))))
  (mapc (lambda (buffer)
          (when (eq mode (buffer-local-value 'major-mode buffer))
	    (kill-buffer buffer)))
        (buffer-list))
  (message "Killed buffers with major mode %s" mode))

(defun tr/kill-dired-buffers ()
  "Kill all dired buffers."
  (interactive)
  (tr/kill-mode-buffers 'dired-mode))

(defun tr/kill-help-buffers ()
  "Kill all help buffers."
  (interactive)
  (tr/kill-mode-buffers 'help-mode)
  (tr/kill-mode-buffers 'helpful-mode))

;; enhanced buffer menu
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; basic dired config
(use-package dired
  :ensure nil				; is builtin
  :commands (dired dired-jump)
  :bind (("C-x j" . dired-jump))
  :custom
  (dired-listing-switches "-agho --group-directories-first")
  (dired-dwim-target t))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-hide-dotfiles
  ;; uncomment to start with dotfiles hidden
  ;; :hook (dired-mode . dired-hide-dotfiles-mode)
  :bind (:map dired-mode-map ("H" . dired-hide-dotfiles-mode)))

;; git
(use-package magit
  :commands magit-status)

;; project management
(use-package projectile
  :commands (projectile-switch-project
	     projectile-ripgrep
	     projectile-find-file)
  :diminish projectile-mode
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-switch-project-action #'projectile-dired))

;; enhance projectile with counsel (apparently selectrum does not have
;; projectile integration)
(use-package counsel-projectile
  :after (:all counsel projectile)
  :config (counsel-projectile-mode))

;; project tree view with treemacs
(use-package treemacs
  :defer t
  :config
  (treemacs-resize-icons 16))
  ;; (add-hook 'treemacs-mode-hook 'variable-pitch-mode))

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package treemacs-magit
  :after treemacs magit)

(provide 'init-management)
;;; init-management.el ends here
