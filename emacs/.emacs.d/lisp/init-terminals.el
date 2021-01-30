;;; init-terminals.el --- Terminal modes configuration -*- lexical-binding: t -*-

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

;; This package provides a configuration for terminal modes.
;; vterm requires depenendencies: cmake, libtool.
;; See more at https://github.com/akermu/emacs-libvterm/#requirements

;;; System requirements:

;; none

;;; Key bindings:

;; C-c C-e (global): `eshell'.  Open eshell
;; C-R (eshell): `counsel-esh-history'.  Isearch eshell history with counsel.

;;; Code:

;; term
(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

;; vterm
(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  (setq vterm-max-scrollback 10000))

;; eshell
;; TODO cache sudo password in some way.
;; TODO running "which sudo" returns /usr/bin/sudo instead of the elisp sudo implementation, check it out

(defun tr/time-subtract-seconds (b a)
  "Subtract seconds B from A."
  (float-time (time-subtract b a)))

(defun tr/startup-echo-area-message ()
  "Display startup message in echo area with loading time information."
  (message "Welcome to GNU Emacs. Init completed in %.3f s."
	   (tr/time-subtract-seconds after-init-time before-init-time)))

(defun tr/setup-eshell ()
  "Setup eshell."
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)
  (setq eshell-history-size 10000
	eshell-buffer-maximum-lines 10000
	eshell-hist-ignoredups t
	;; eshell-scroll-to-bottom-on-output t
	eshell-scroll-to-bottom-on-input t)
  (define-key eshell-mode-map (kbd "C-S-r") 'counsel-esh-history)
;;  (fish-completion-mode)  too slow
  (tr/startup-echo-area-message))	; this message is printed here because emacs is started with eshell

(use-package eshell
  :ensure nil				; is builtin
  :hook
  (eshell-first-time-mode . tr/setup-eshell))

;; too slow:
;; (use-package fish-completion
;;   :after eshell)

;; TODO verify this package
(use-package eshell-syntax-highlighting
  :after eshell
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode)
  :config
  (setq esh-autosuggest-delay 0))

(global-set-key (kbd "C-c C-e") 'eshell)

(provide 'init-terminals)
;;; init-terminals.el ends here
