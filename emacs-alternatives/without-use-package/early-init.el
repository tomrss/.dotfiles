;;; early-init.el --- Emacs early init -*- lexical-binding: t -*-

;; Copyright (C) 2021 Tommaso Rossi

;; Author: Tommaso Rossi <tommaso.rossi1@protonmail.com

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Emacs early init file, that gets evaluated before init.el and even before the GUI is loaded.
;; The focus is on optimizing startup time and perform basic UI cleanups.

;;; Code:

;;;; Optimize garbage collections

(defconst tr/gc-cons-standard-threshold-mb 1024
  "Number of MB of consing between garbage collection during normal operativity.")
(defconst tr/gc-cons-startup-threshold-mb 2048
  "Number of MB of consing between garbage collection during startup.")

(defun tr/restore-garbage-collection ()
  "Restore GC consing threshold to `tr/gc-cons-standard-threshold-mb'."
  (setq gc-cons-threshold (* tr/gc-cons-standard-threshold-mb 1024 1024)))

;; set high garbage collection consing threshold during startup
(setq gc-cons-threshold (* tr/gc-cons-startup-threshold-mb 1024 1024))

;; restore garbage collection after init
(add-hook 'after-init-hook 'tr/restore-garbage-collection)

;; garbage collect when idle
(run-with-idle-timer 2 t 'garbage-collect)

;;;; Temporary disable file handler at startup

(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun tr/reset-file-name-handler-alist ()
  "Reset filename handlers to default value."
  (setq file-name-handler-alist default-file-name-handler-alist))

(add-hook 'after-init-hook 'tr/reset-file-name-handler-alist)

;;;; Disable package.el at startup

(setq package-enable-at-startup nil)

;;;; Early UI cleanups

;; frame resize seems to be very expensive, disable it
(setq frame-inhibit-implied-resize t)
(setq inhibit-default-init t)
(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'fundamental-mode)

;;;; Move backups elsewhere

(setq default-directory "~/")
(setq backup-directory-alist `(("." . "~/.cache/emacs/var/backup/")))

;;; early-init.el ends here
