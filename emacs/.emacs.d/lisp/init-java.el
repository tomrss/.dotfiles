;;; init-java.el --- Java programming -*- lexical-binding: t -*-

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

;; This package provides configuration for project, files and buffer management,
;; most notably with dired basic configuration, magit git client,
;; projectile for project-based functions and treemacs for project tree view.

;;; System requirements:

;;; Key bindings:

;;; Code:

;; TODO parse lombok version from pom
;; (defvar lombok-jar-path
;;       (expand-file-name "~/.m2/repository/org/projectlombok/lombok/1.18.10/lombok-1.18.10.jar"))

(use-package lsp-java
  :config
  (add-hook 'java-mode-hook 'lsp))
  ;; (setq lsp-java-vmargs `("-XX:+UseStringDeduplication"
  ;; 			  ,(concat "-javaagent:" lombok-jar-path)
  ;; 			  ,(concat "-Xbootclasspath/a:" lombok-jar-path)
  ;; 			  "--add-modules=ALL-SYSTEM")))

(use-package dap-mode
  :after lsp-mode
  :config (dap-auto-configure-mode))

(use-package dap-java
  :ensure nil)

(provide 'init-java)
;;; init-java.el ends here
