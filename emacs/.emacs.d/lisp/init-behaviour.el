;;; init-behaviour.el --- Common behaviour configuration -*- lexical-binding: t -*-

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

;; This package provides a configuration for common behaviour,
;; like parenthentesis features, syntax checking, autocompletions
;; on point, minibuffer completions, helps and docs enhancements.

;; Both ivy and selectrum are used.  Selectrum works almost everywhere
;; a completion is required as it adds advices on the builtin completion
;; functions, and it does not require to override functions or change
;; common key bindings, like ivy and counsel do.  However,selectrum seems
;; to have no enhancements for isearch, grep and ripgrep.
;; So in those cases ivy and swiper are used.

;;; System requirements:

;; none

;;; Key bindings:

;; C-c C-d: `helpful-at-point'.  Show helpful documentation at point.
;; C-h s:   `helpful-symbol'.  Show docs of symbol.
;; <tab>:   `company-complete-selection'.  Complete with selected item in autocompletion list.

;;; Code:

;; parenthentesis highlighting for prog modes
(show-paren-mode t)
(add-hook 'prog-mode-hook `(lambda () (electric-pair-mode 1)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; use y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; god mode can reduce the Ctrl key presses
;; TODO: give evil mode a try?
(use-package god-mode
  ;; :init
  ;; (god-mode-all)
  :config
  (add-to-list 'god-exempt-major-modes 'vterm-mode)
  (global-set-key (kbd "<escape>") #'god-mode-all))

;; delete trailing whitespace before save
(defun tr/delete-trailing-whitespace ()
  "Delete trailing whitespace except for Markdown and TeX modes."
  (when (not (or (derived-mode-p 'markdown-mode
				 'tex-mode
				 'latex-mode)))
    (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'tr/delete-trailing-whitespace)

;; syntax checking with flycheck
(use-package flycheck
  :init
  (global-flycheck-mode t))

(use-package flycheck-inline
  :requires flycheck
  :hook (flycheck-mode . flycheck-inline-mode))

;; (with-eval-after-load 'flycheck
;;   (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))

;; completions with company
(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 2)
  (setq completion-style '(partial-completion substring emacs27))
  :bind
  (:map company-active-map ("<tab>" . company-complete-selection)))

(use-package company-quickhelp
  :hook
  (company-mode . company-quickhelp-mode))

;; selectrum is a solution for incremental narrowing.
;; Here it is preferred to Ivy due to simplicity of configuration
;; and cleanliness of results, and for the fact that it works out
;; of the box for every function that uses minibuffer autocompletion
(use-package selectrum
  :init
  (selectrum-mode +1)
  :config
  (setq selectrum-count-style 'current/matches))

;; just for cases not covered by selectrum, most notably eshell history
;; and projectile functions (like `projectile-find-file' or `projectile-ripgrep')
(use-package ivy
  :custom (ivy-count-format "(%d/%d) "))

(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-r") 'swiper-backward)

;; seems useless in this build:
;; (use-package ivy-rich
;;   :init
;;   (ivy-rich-mode 1))

(use-package counsel
  :custom counsel-linux-app-format-function #'counsel-linux-app-format-function-name-pretty)

;; prescient can sort items by usage
(use-package prescient
  :config
  (prescient-persist-mode +1))

;; enhance company autocomplete results
(use-package company-prescient
  :requires company
  :config
  (company-prescient-mode +1))

;; enhance selectrum results
(use-package selectrum-prescient
  :after (:all selectrum prescient)
  :requires selectrum
  :config
  (selectrum-prescient-mode +1))

;; enhance ivy and counsel results
;; TODO this is really annoying when showing eshell history,
;; in which you probably want just basic launch command time
;; sorting and not most frequent usage sorting.
(use-package ivy-prescient
  :after (:all ivy prescient counsel)
  :requires ivy
  :config
  (ivy-prescient-mode +1))

;; marginalia shows docs in minibuffer
(use-package marginalia
  :init
  (marginalia-mode)
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light)))

;; show possible commands with key bindings
(use-package which-key
  :init
  (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5))

;; enhance helping and documentation
(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ("C-h s" . helpful-symbol)
  ("C-c C-d" . helpful-at-point))

;; code snippets
(use-package yasnippet
  :config (yas-global-mode))

(use-package yasnippet-snippets)

(use-package ivy-yasnippet
  :bind
  ("C-<tab>" . ivy-yasnippet))

;; allow actions for minibuffer items
;;(use-package embark
;;  :bind ("M-RET" . embark-act))

(provide 'init-behaviour)
;;; init-behaviour.el ends here
