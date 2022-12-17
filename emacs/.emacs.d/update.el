;;; update.el --- Emacs package updater -*- lexical-binding: t -*-

;; Copyright (C) 2022 Tommaso Rossi

;; Author: Tommaso Rossi <tommaso.rossi1@protonmail.com

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Update Emacs packages with the `straight' package manager.

;;; Code:


(defun +install (_)
  "Install packages."
  (load-file (locate-user-emacs-file "early-init.el"))
  (load-file (locate-user-emacs-file "init.el"))
  (straight-rebuild-all))

(defun +update (force-rebuild)
  "Update packages.

If FORCE-REBUILD is non-nil, force rebuild of all packages."
  (load-file (locate-user-emacs-file "early-init.el"))
  (load-file (locate-user-emacs-file "init.el"))
  (straight-pull-all)
  (straight-freeze-versions)
  (when force-rebuild
    (straight-rebuild-all)))

(defun +uninstall (prune-state)
  "Uninstall packages and prune state if PRUNE-STATE is non-nil"
  (message "Feature not implemented."))

;;; update.el ends here
