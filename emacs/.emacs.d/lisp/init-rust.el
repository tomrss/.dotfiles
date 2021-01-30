;;; init-rust.el --- Rust support -*- lexical-binding: t -*-

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

;; This package provides support for the Rust programming language.
;; It configures rust mode with support for racer (rust code completion
;; utility) and cargo (rust package manager) with hooks for company and
;; flycheck for enhancing completion and syntax checking.

;;; System requirements:

;; rust
;; cargo
;; racer
;; rustfmt

;;; Key bindings:

;; C-c C-t: racer-describe.  Show documentation at point.

;;; Code:
(use-package rust-mode
  :bind (:map rust-mode-map
              (("C-c C-t" . racer-describe)
               ("TAB" .  company-indent-or-complete-common)))
  :config
  (progn
    (use-package flycheck-rust)
    (use-package cargo
      :hook (rust-mode . cargo-minor-mode))
    (use-package racer
      :hook (rust-mode . racer-mode)
      :config
      (progn
        (add-hook 'racer-mode-hook #'company-mode)
        (add-hook 'racer-mode-hook #'eldoc-mode)))

    (add-hook 'rust-mode-hook 'flycheck-mode)
    (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)

    ;; format rust buffers on save using rustfmt
    (add-hook 'before-save-hook
              (lambda ()
                (when (eq major-mode 'rust-mode)
                  (rust-format-buffer))))))

(provide 'init-rust)
;;; init-rust.el ends here
