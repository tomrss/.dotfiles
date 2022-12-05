;;; update.el --- Emacs package updater -*- lexical-binding: t -*-

;; Copyright (C) 2022 Tommaso Rossi

;; Author: Tommaso Rossi <tommaso.rossi1@protonmail.com

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Update Emacs packages with the `straight' package manager.

;;; Code:
(require 'straight)

(straight-pull-all)
(straight-rebuild-all)
(straight-freeze-versions)

;;; update.el ends here
