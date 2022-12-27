;;; mod-editing.el --- Editing module -*- lexical-binding: t -*-

;; Copyright (C) 2022 Tommaso Rossi

;; Author: Tommaso Rossi <tommaso.rossi1@protonmail.com

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Editing module. TODO refactor in other modules / rename.

;;; Code:

;;;; Text selection and navigation

;; increases the selected region by semantic units
(+install-package 'expand-region)
(+define-key (kbd "C-ò") #'er/expand-region)

;;;; Scratch buffers

(+install-package '(scratch-el
			        :type git
			        :host github
                    :repo "tomrss/scratch.el"))
(with-eval-after-load 'scratch
  (setq scratch-search-fn #'consult-ripgrep)
  (scratch-persist-mode +1))

(eval-and-compile
  (define-prefix-command 'scratch-key-map))
(define-key scratch-key-map (kbd "n") #'scratch-new)
(define-key scratch-key-map (kbd "t") #'scratch-titled)
(define-key scratch-key-map (kbd "o") #'scratch-open)
(define-key scratch-key-map (kbd "r") #'scratch-search)
(+define-key (kbd "C-c s") scratch-key-map)

(provide 'mod-editing)
;;; mod-editing.el ends here
