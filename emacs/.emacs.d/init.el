;;; init.el --- Emacs 27+ configuration -*- lexical-binding: t -*-

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

;; This package provides a configuration for Emacs 27+.
;; The configuration includes optimized searching, autocomplete and
;; autocheck of code, as well as modes for some common programming
;; languages.

;; Sources of inspiration:
;; https://github.com/purcell/emacs.d
;; https://github.com/munen/emacs.d
;; https://github.com/daviwil/emacs-from-scratch

;;; Code:

;; Add to load path lisp dir and subdirs
(add-to-list 'load-path "~/.emacs.d/lisp")
(let ((default-directory  "~/.emacs.d/lisp/"))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path)))
           (append
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Bootstrap package management configuration
(require 'init-packaging)

;; Boostrap backup configuration
(require 'init-backups)

;; Bootstrap UI configuration and general behaviours
(require 'init-ui)
(require 'init-behaviour)
(require 'init-management)

;; Bootstrap terminal modes configuration
(require 'init-terminals)

;; Bootstrap common utilities configuration
(require 'init-org)
(require 'init-http)
(require 'init-messaging)

;; Boostrap programming modes configuration
(require 'init-lsp)
(require 'init-rust)
(require 'init-java)
;; (require 'init-javascript)
(require 'init-typescript)
(require 'init-python)
(require 'init-misc-modes)

;; start emacs with eshell loaded
;; (add-hook 'emacs-startup-hook 'eshell)

;; restore garbage collection
(setq gc-cons-threshold (* 2 1000 1000))

(provide 'init)
;;; init.el ends here
