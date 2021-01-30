;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

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

;; Early initialization.

;;; Code:

;; disable garbage collection to speedup startup.
;; It MUST be restored at the end of initialization.
(setq gc-cons-threshold most-positive-fixnum)

;; Suppress automatic startup behaviour to mantain startup consistent
(setq package-enable-at-startup nil)

;; resizing the frame can be a terribly expensive part of changing the font.
(setq frame-inhibit-implied-resize t)
;; start full screen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; clean UI from useless stuff
(setq inhibit-default-init t)
(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; careful with this one: it sets emacs directory to other than ~/.emacs.d
;; for not polluting emacs conf git repo
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))
;;; early-init.el ends here
