;;; init-messaging.el --- Configuration for messaging services-*- lexical-binding: t -*-

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

;; This package provides configuration for messaging, notably with the
;; telega telegram client.

;;; System requirements:

;; TDlib (cmake is needed):
;; git clone --depth 1 https://github.com/tdlib/td.git && cd td
;; mkdir build && cd build && cmake ../
;; make -j<n_cores>
;; sudo make install

;;; Key bindings:

;; none

;;; Code:

;; (use-package unicode-fonts
;;   :custom
;;   (unicode-fonts-skip-font-groups '(low-quality-glyphs))
;;   :config
;;   (unicode-fonts-setup))

(use-package tracking
  :defer t
  :after telega
  :config
  (setq tracking-faces-priorities '(all-the-icons-pink
                                    all-the-icons-lgreen
                                    all-the-icons-lblue))
  (setq tracking-frame-behavior nil))

(use-package telega
  :commands telega
  :config
  (setq telega-use-tracking-for '(any pin unread)
        telega-chat-use-markdown-version nil
        telega-emoji-use-images t
        telega-completing-read-function #'ivy-completing-read
        telega-msg-rainbow-title nil
        telega-chat-fill-column 79))

(defun tr/telega-completions-setup ()
  "Setup completions for telega."
  (set (make-local-variable 'company-backends)
       (append (list telega-emoji-company-backend
                     'telega-company-username
                     'telega-company-hashtag)
	       (when (telega-chat-bot-p telega-chatbuf--chat)
		 '(telega-company-botcmd))))
  (company-mode 1))

(add-hook 'telega-chat-mode-hook 'tr/telega-completions-setup)

(use-package emojify
  :after telega
  :commands (telega emojify-mode)
  :hook
  (telega-chat-mode . emojify-mode)
  (telega-root-mode . emojify-mode))

(defun tr/emoji-telega-hook ()
  "Wrapper for `emojify-mode' for activating only in telega modes."
  (when (or (eq major-mode 'telega-root-mode)
	    (eq major-mode 'telega-chat-mode))
    (emojify-mode 1)))

(add-hook 'company-search-mode-hook 'tr/emoji-telega-hook)

(provide 'init-messaging)
;;; init-messaging.el ends here
