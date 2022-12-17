;;; update.el --- Emacs package updater -*- lexical-binding: t -*-

;; Copyright (C) 2022 Tommaso Rossi

;; Author: Tommaso Rossi <tommaso.rossi1@protonmail.com

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Update Emacs packages with the `straight' package manager.

;;; Code:

(defun manage--load-init-files ()
  "Load init files."
  (load-file "early-init.el")
  (load-file "init.el"))

(defun manage-install ()
  "Install packages defined in init files."
  (if (and (file-exists-p "straight")
           (not (directory-empty-p "straight")))
      (message "Found not empty install folder, nothing to do")
    (message "Installing emacs packages")
    (manage--load-init-files)
    (let ((lockfile (cdr (assq nil straight-profiles))))
      (unless (file-exists-p lockfile)
        (straight-freeze-versions)))))

(defun manage-upgrade ()
  "Upgrade packages defined in init files."
  (manage--load-init-files)
  (straight-pull-all)
  (straight-freeze-versions))

(defun manage-rebuild ()
  "Rebuild packages."
  (manage--load-init-files))

;;; manage.el ends here
