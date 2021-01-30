;;; init-typescript.el --- TypeScript configuration -*- lexical-binding: t -*-

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

;; This package provides a configuration for TypeScript programming
;; with LSP mode integration.
;; This configuration requires a TypeScript language server, like
;; https://github.com/theia-ide/typescript-language-server.
;; Install theia LS with npm install -g typescript-language-server.

;;; System requirements:

;; TypeScript: npm install -g typescript
;; A TypeScript language server, e.g.: npm install -g typescript-language-server

;;; Key bindings:

;; none

;;; Code:
(require 's)
(require 'f)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (require 'dap-node)
  (dap-node-setup)
  (setq typescript-indent-level 2))

;; TODO: this is global but should be buffer local
(defun tr/nvm-use (version)
  "Interactive wrapper of `nvm-use'.  Choose node VERSION amongst installed versions."
  (interactive
   (list
     (completing-read "Version: " (mapcar 'car (nvm--installed-versions)))))
  (nvm-use version)
  (message "Using node version %s" version))

;; TODO: continue prompt of node version in case of missing .nvmrc
;; TODO: using dir-locals could be cleaner?
(defun tr/nvm-projectile ()
  "Set up projectile project node interpreter with nvm.
If .nvmrc is present in project root, it will be used.  Otherwise node
version will be prompted."
  (interactive)
  (when (file-exists-p (expand-file-name "package.json" (projectile-project-root)))
    (let ((nvmrc (expand-file-name ".nvmrc" (projectile-project-root))))
      (if (file-exists-p nvmrc)
	  (let ((version (s-trim (f-read nvmrc))))
	    (nvm-use version)
	    (message "Using node version %s" version))
	(call-interactively 'tr/nvm-use)))))

(use-package nvm
  :config
  (add-hook 'projectile-find-file-hook 'tr/nvm-projectile))

(provide 'init-typescript)
;;; init-typescript.el ends here
