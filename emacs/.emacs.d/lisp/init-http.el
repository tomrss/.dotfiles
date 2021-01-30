;;; init-http.el --- Emacs 27+ configuration -*- lexical-binding: t -*-

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

;; This package provides a configuration for HTTP client.
;;
;; This file is almost entirely copied from
;; <https://github.com/purcell/emacs.d/blob/master/lisp/init-http.el>

;;; Code:
(use-package httprepl
  :config
  (push '("image" . image) httprepl-content-type-alist)
  (push '(image . ((lambda (b) (with-current-buffer b
                                 (image-mode)
                                 b))))
        httprepl-content-type-middleware-alist))

(use-package restclient
  :config
  (add-to-list 'auto-mode-alist '("\\.rest\\'" . 'restclient-mode)))

(defun sanityinc/restclient ()
  "Work with `rest' in the *restclient* buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*restclient*")
    (restclient-mode)
    (pop-to-buffer (current-buffer))))


(provide 'init-http)
;;; init-http.el ends here
