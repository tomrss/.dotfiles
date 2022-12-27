;;; mod-completions.el --- Completions module -*- lexical-binding: t -*-

;; Copyright (C) 2022 Tommaso Rossi

;; Author: Tommaso Rossi <tommaso.rossi1@protonmail.com

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Module for customizing org-mode.

;;; Code:

(with-eval-after-load 'org
  ;; setup visual fill
  (add-hook 'org-mode-hook (lambda () (+setup-visual-fill 100)))
  ;; org babel languages
  (require 'org-tempo)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
	 (python . t)
	 (shell . t)))

  ;; templates for adding code snippets
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("yml" . "src yaml"))
  (add-to-list 'org-structure-template-alist '("json" . "src json"))

  ;; some defaults
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-edit-src-content-indentation 2)
  (setq org-hide-block-startup nil)
  (setq org-src-preserve-indentation nil)
  (setq org-hide-leading-stars t)
  (setq org-adapt-indentation nil)
  (setq org-startup-folded 'content)
  (setq org-cycle-separator-lines 2)
  (setq org-return-follows-link t)
  (setq org-startup-truncated nil)
  (setq org-startup-with-inline-images t)

  ;; auto tangle on save
  (defun +org-auto-tangle ()
	"Set hook for auto tangling org files on save."
	(let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle)))

  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'after-save-hook #'+org-auto-tangle 0 t))))

;; ;;;; Org roam

;; (+install-package 'org-roam)
;; (defvar +org-roam-base-dir)
;; (setq org-roam-v2-ack t)
;; (+define-key (kbd "C-c n l") #'org-roam-buffer-toggle)
;; (+define-key (kbd "C-c n f") #'org-roam-node-find)
;; (+define-key (kbd "C-c n i") #'org-roam-node-insert)
;; (with-eval-after-load 'org-roam
;;   (unless (file-exists-p +org-roam-base-dir)
;;     (make-directory +org-roam-base-dir))
;;   (setq org-roam-directory +org-roam-base-dir)
;;   (setq org-roam-completion-everywhere t)
;;   (define-key org-mode-map (kbd "C-i") #'completion-at-point)
;;   (org-roam-setup))

(provide 'mod-org)
;;; mod-org.el ends here
