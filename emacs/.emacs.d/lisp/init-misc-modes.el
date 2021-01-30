;;; init-misc-modes.el --- Summary -*- lexical-binding: t -*-

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

;; This package provides from various simple and lightweight modes
;; that require little or no configuration.

;;; Key bindings:
;; none

;;; Code:
(use-package dockerfile-mode
  :defer t
  :mode "Dockerfile")

(use-package markdown-mode
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode ("\\.md\\'"
         "\\.mkd\\'"
         "\\.markdown\\'")
  :init
  (setq mardown-command "multimarkdown"))

(use-package yaml-mode
  :defer t
  :mode ("\\.ya?ml\\'"))

(use-package toml-mode
  :defer t)

(use-package groovy-mode
  :defer t
  ;; :hook (groovy-mode . lsp-deferred)	; language server does not work
  :mode "\\.groovy\\'")

(use-package kubel
  :defer t)

(use-package csharp-mode
  :mode (("\\.cs\\'" . csharp-mode))
  :config
  (add-hook 'csharp-mode-hook 'lsp))

(provide 'init-misc-modes)
;;; init-misc-modes.el ends here
