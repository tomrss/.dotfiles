;;; init-python.el --- Python support -*- lexical-binding: t -*-

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

;; This package provides configuration for the Python mode
;; through the elpy package.
;; Flycheck is configured for syntax checking with pyflakes.
;; You should check if jedi server is installed for autocompletions
;; by running `M-x elpy-config'.

;;; System requirements:

;; python 3
;; pyflakes: pip install pyflakes
;; virtualenv may be needed by elpy

;;; Key bindings:

;; none

;;; Code:
(defun tr/setup-elpy ()
  "Setup Elpy package."
  (elpy-shell-set-local-shell (elpy-project-root)))

(use-package elpy
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :custom
  (elpy-get-info-from-shell t)
  (elpy-shell-echo-input nil)
  (elpy-syntax-check-command "pyflakes")
  :config
  (tr/setup-elpy)
  (when (load "flycheck" t t)
    (message "loading flycheck for elpy")
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode)))

(provide 'init-python)
;;; init-python.el ends here
