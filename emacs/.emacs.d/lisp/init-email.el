;;; init-email.el --- Email configuration -*- lexical-binding: t -*-

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

;; This package provides configuration for email with mu4e.

;;; System requirements:

;; mu, mu4e: mu AUR package.  yay -S mu

;;; Key bindings:

;; none

;;; Code:

(use-package mu4e
  :ensure nil				; needs to be installed as OS package alongside mu
  :custom
  ((mu4e-maildir "~/Mail")
   (mu4e-get-mail-command "mbsync -a")
   (mu4e-change-filenames-when-moving t))
  :config
  (setq mu4e-maildir-shortcuts
	'(("/Inbox"  . ?i)
	  ("/Sent"   . ?s)
	  ("/Drafts" . ?d)
	  ("/Trash"  . ?t))))
;;  (add-hook 'mu4e-view-mode-hook 'variable-pitch-mode))


(provide 'init-email)
;;; init-email.el ends here
