;;; init-javascript.el --- JavaScript and TypeScript configuration -*- lexical-binding: t -*-

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

;; This package provides a configuration for JavaScript and TypeScript
;; with focus on web development.
;; *WARNING* this config is highly untested and requires further research.

;;; Key bindings:
;; TODO

;;; Code:

;; TODO verify, file copied from https://mihamina.rktmb.org/2020/08/emacs-typescript-development.html

(defun setup-tide-mode ()
  (interactive)
  ;;  (setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /tmp/tss.log"))
  (tide-setup)
  (if (file-exists-p (concat tide-project-root "node_modules/typescript/bin/tsserver"))
      (setq tide-tsserver-executable "node_modules/typescript/bin/tsserver"))
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (setq tide-format-options '(:indentSize 2 :tabSize 2 :insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))
  (local-set-key (kbd "C-c d") 'tide-documentation-at-point)
  (company-mode +1)
  (setq company-minimum-prefix-length 1))

(use-package tide
  :ensure t
  :config
  (progn
    (company-mode +1)
    ;; aligns annotation to the right hand side
    (setq company-tooltip-align-annotations t) ;; todo move from here
    (add-hook 'typescript-mode-hook #'setup-tide-mode)
    (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))))

;; use web-mode + tide-mode for javascript instead
(use-package js2-mode
  :ensure t
  :config
  (progn
    (add-hook 'js2-mode-hook #'setup-tide-mode)
    ;; configure javascript-tide checker to run after your default javascript checker
    (setq js2-basic-offset 2)
    (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
    (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))))

;; (add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(use-package json-mode
  :ensure t
  :config
  (progn
    (flycheck-add-mode 'json-jsonlint 'json-mode)
    (add-hook 'json-mode-hook 'flycheck-mode)
    (setq js-indent-level 2)
    (add-to-list 'auto-mode-alist '("\\.json" . json-mode))))

(use-package web-mode
  :ensure t
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.js"     . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html"   . web-mode))
    ;; this magic incantation fixes highlighting of jsx syntax in .js files
    (setq web-mode-content-types-alist
          '(("jsx" . "\\.js[x]?\\'")))
    (add-hook 'web-mode-hook
              (lambda ()
                (setq web-mode-code-indent-offset 2)
                (when (string-equal "tsx" (file-name-extension buffer-file-name))
                  (setup-tide-mode))
                (when (string-equal "jsx" (file-name-extension buffer-file-name))
                  (setup-tide-mode))
                (when (string-equal "js" (file-name-extension buffer-file-name))
                  (progn
                    (setup-tide-mode)
                    (with-eval-after-load 'flycheck
                      (flycheck-add-mode 'typescript-tslint 'web-mode)
                      (flycheck-add-mode 'javascript-tide 'web-mode))))))
    ))

(provide 'init-javascript)
;;; init-javascript.el ends here
