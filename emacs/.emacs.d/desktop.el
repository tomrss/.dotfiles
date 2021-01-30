;;; desktop.el --- Emacs as desktop environment -*- lexical-binding: t -*-

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

;; This package configures Emacs as a desktop environment through
;; the EXWM package.

;; TODO code is almost entirely copied from:
;; https://github.com/daviwil/emacs-from-scratch

;;; Code:

(defun tr/exwm-rename-buffer ()
  "Name EXWM buffers after EXWM class (X window process)."
  (exwm-workspace-rename-buffer exwm-class-name))

(defun tr/init-dashboard ()
  "Init dashboard (mutuated from `dashboard-setup-startup-hook')."
  (dashboard-insert-startupify-lists)
  (switch-to-buffer dashboard-buffer-name)
  (goto-char (point-min))
  (redisplay)
  (run-hooks 'dashboard-after-initialize-hook))

(defun tr/exwm-init-hook ()
  "Init hook for EXWM."
  ;; Make workspace 1 be the one where we land at startup
  ;; (exwm-workspace-switch-create 1)
  ;; (exwm-workspace-delete 0)

  ;; Open eshell by default (already done in emacs)
  ;;(eshell)
  (tr/init-dashboard)

  ;; Show battery status and date in the mode line
  (display-battery-mode 1)
  (setq display-time-default-load-average nil)
  (setq display-time-format "%a %d-%b %H:%M")
  (display-time-mode))

(defun tr/switch-to-dashboard (frame)
  "Switch to dashboard in FRAME."
  (switch-to-buffer "*dashboard*"))

(use-package exwm
  :config

  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'tr/exwm-rename-buffer)

  ;; startup hook
  (add-hook 'exwm-init-hook #'tr/exwm-init-hook)

  ;; TODO: this seems a bit hacky
  (add-to-list 'after-make-frame-functions 'tr/switch-to-dashboard)

  ;; Run xmodmap
  ;; (start-process-shell-command "xmodmap" nil "xmodmap ~/.emacs.d/exwm/Xmodmap")

  ;; Set the screen resolution (update this to be the correct resolution for your screen!)
  ;; (require 'exwm-randr)
  ;; (exwm-randr-enable)
  ;; (start-process-shell-command "xrandr" nil "xrandr --output Virtual-1 --primary --mode 2048x1152 --pos 0x0 --rotate normal")

  ;; Load the system tray before exwm-init
  ;; (require 'exwm-systemtray)
  ;; (exwm-systemtray-enable)

  ;; These keys should always pass through to Emacs
  (setq exwm-input-prefix-keys
    '(?\C-x
      ?\C-u
      ?\C-h
      ?\M-x
      ?\M-`
      ?\M-&
      ?\M-:))

  ;; Ctrl+Q will enable the next key to be sent directly
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  (setq exwm-input-global-keys
        `(
          ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
          ([?\s-r] . exwm-reset)

          ;; Move between windows
          ([s-left] . windmove-left)
	  ([?\s-b] . windmove-left)

          ([s-right] . windmove-right)
	  ([?\s-f] . windmove-right)

          ([s-up] . windmove-up)
	  ([?\s-p] . windmove-up)

          ([s-down] . windmove-down)
	  ([?\s-n] . windmove-down)

          ;; Launch applications via shell command
          ([?\s-&] . (lambda (command)
		       (interactive (list (read-shell-command "$ ")))
		       (start-process-shell-command command nil command)))

          ;; Switch workspace
          ([?\s-w] . exwm-workspace-switch)

          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

  (exwm-input-set-key (kbd "s-d") 'counsel-linux-app)

  (exwm-enable))

(use-package desktop-environment
  :after exwm
  :config (desktop-environment-mode)
  :custom
  (desktop-environment-screenshot-command "maim screenshot_$(date +%s).png")
  (desktop-environment-brightness-small-increment "2%+")
  (desktop-environment-brightness-small-decrement "2%-")
  (desktop-environment-brightness-normal-increment "5%+")
  (desktop-environment-brightness-normal-decrement "5%-"))

(defun tr/pixel-delta (dimension-pixel ratio)
  "Get pixel delta from DIMENSION-PIXEL scaled by RATIO."
  (car (cl-truncate
	(* (- ratio 1) dimension-pixel))))

(defun tr/exwm-floating-resize-by-ratio (ratio)
  "Resize current floating window by RATIO."
  (interactive "nRatio: ")
  (let ((width-delta (tr/pixel-delta (frame-pixel-width) ratio))
	(height-delta (tr/pixel-delta (frame-pixel-height) ratio)))
    (exwm-layout-enlarge-window-horizontally width-delta)
    (exwm-layout-enlarge-window height-delta)))

(defun tr/exwm-floating-resize-to (width height)
  "Resize current floating window to fixed WIDTH and HEIGHT."
  (interactive "nWidth: \nnHeight: ")
  (let ((width-delta (- width (frame-pixel-width)))
	(height-delta (- height (frame-pixel-height))))
    (exwm-layout-enlarge-window-horizontally width-delta)
    (exwm-layout-enlarge-window height-delta)))

(provide 'desktop)
;;; desktop.el ends here
