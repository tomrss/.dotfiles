;;; mod-packaging.el --- Packaging module -*- lexical-binding: t -*-

;; Copyright (C) 2022 Tommaso Rossi

;; Author: Tommaso Rossi <tommaso.rossi1@protonmail.com

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Module for configuring packaging.

;;; Code:

;;; Configure `straight.el' as package manager

;; bootstrap `straight.el'
(defvar bootstrap-version)
(let ((bootstrap-file
       (+locate-emacs-cache-file "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; configure straight lockfile (it can be committed)
(setq straight-profiles
      `((nil . ,(expand-file-name "lockfile.el" user-emacs-directory))))

;; define a macro to decouble package installation from straight

(defmacro +install-package (package)
  "Install PACKAGE."
  `(straight-use-package ,package))

(provide 'mod-packaging)
;;; mod-packaging.el ends here
