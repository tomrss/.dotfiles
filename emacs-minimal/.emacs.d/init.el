(menu-bar-mode -1)
(toggle-scroll-bar -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq default-frame-alist '((fullscreen . maximized)))

(setq ring-bell-function 'ignore)
;; remove graphical dialog box and keep it keyboard driven
(setq use-dialog-box nil)
;; silent native compilation warning
(setq native-comp-async-report-warnings-errors 'silent)

;;; Manage backups

;; disable lockfiles
(setq create-lockfiles nil)

;; put backups in one folder
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; change some backup default
(setq backup-by-copying t)
(setq delete-old-versions t)
(setq kept-new-versions 4)
(setq kept-old-versions 2)
(setq version-control t)

;; custom file to temp file (practically disable `customize')
(setq custom-file
      (if (boundp 'server-socket-dir)
		  (expand-file-name "custom.el" server-socket-dir)
		(expand-file-name (format "emacs-custom-%s.el" (user-uid))
						  temporary-file-directory)))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(defun +install-package (package)
  (unless (package-installed-p package)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install package)))

;;;; Key bindings

;; define one centralized minor mode holding custom keys
(defvar customized-keys-minor-mode-map (make-sparse-keymap)
  "Keymap for the customized-keys-minor-mode.")

(define-minor-mode customized-keys-minor-mode
  "A minor mode so that custom key settings override major modes."
  :init-value t
  :lighter "")

(customized-keys-minor-mode +1)

(defun +define-key (key def)
  "Define customized KEY with definition DEF."
  (define-key customized-keys-minor-mode-map key def))

(setq history-length 100)
(savehist-mode +1)

(fset 'yes-or-no-p 'y-or-n-p)
;; recent files
(recentf-mode +1)
(+define-key (kbd "C-x f") #'recentf-open-files)

;; reopen file at same point
(save-place-mode +1)
;; keep all buffers updated if external program change content
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode +1)
;; share system clipboard
(setq save-interprogram-paste-before-kill t)
(setq kill-do-not-save-duplicates t)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))

(column-number-mode +1)

(setq fast-but-imprecise-scrolling t)
(setq scroll-margin 1)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq scroll-preserve-screen-position 1)

(load-theme 'modus-vivendi)

(setq inhibit-startup-echo-area-message "tomrss")

(with-eval-after-load 'dired
  ;; dired defualts
  (setq dired-auto-revert-buffer t)
  (setq dired-dwim-target t)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'top)
  (setq dired-create-destination-dirs 'ask)
  (setq dired-listing-switches "-agho --group-directories-first")
  (setq delete-by-moving-to-trash t)

  ;; hide/show dotfiles in Dired (taken from 'dired-hide-dotfiles' package)
  ;; TODO could be done by hooking a setq on dired-listing-switches ?
  (defun dired-hide-dotfiles--hide ()
	"Hide all dot-files in the current `dired' buffer."
	(let ((inhibit-message t))
      (dired-mark-files-regexp "^\\."))
	(dired-do-kill-lines nil "Hidden %d dotfile%s."))

  (define-minor-mode dired-hide-dotfiles-mode
	"Toggle `dired-hide-dotfiles-mode'"
	:init-value nil
	:lighter " !."
	:group 'dired
	(if dired-hide-dotfiles-mode
		(progn
          (add-hook 'dired-after-readin-hook 'dired-hide-dotfiles--hide)
          (dired-hide-dotfiles--hide))
      (remove-hook 'dired-after-readin-hook 'dired-hide-dotfiles--hide)
      (revert-buffer)))

  (define-key dired-mode-map (kbd "H") #'dired-hide-dotfiles-mode))

(with-eval-after-load 'proced
  (setq proced-auto-update-interval 5)
  (proced-toggle-auto-update 1))

(with-eval-after-load 'org
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

;;; Stateful window layout

(winner-mode +1)
(+define-key (kbd "C-c w") 'winner-undo)
(+define-key (kbd "C-c W") 'winner-redo)

;;; Window selection and navigation

(+define-key (kbd "M-o") #'other-window)

(+define-key (kbd "M-h") #'windmove-left)
(+define-key (kbd "M-j") #'windmove-down)
(+define-key (kbd "M-k") #'windmove-up)
(+define-key (kbd "M-l") #'windmove-right)

(+define-key (kbd "C-M-h") #'shrink-window-horizontally)
(+define-key (kbd "C-M-j") #'shrink-window)
(+define-key (kbd "C-M-k") #'enlarge-window)
;; (+define-key (kbd "C-M-l") 'recenter-other-window)
(+define-key (kbd "C-M-l") 'enlarge-window-horizontally)

(+define-key (kbd "C-M-S-h") #'windmove-swap-states-left)
(+define-key (kbd "C-M-S-j") #'windmove-swap-states-down)
(+define-key (kbd "C-M-S-k") #'windmove-swap-states-up)
(+define-key (kbd "C-M-S-l") #'windmove-swap-states-right)

;;; Workspaces (tab-bar-mode)

(setq tab-bar-show nil)
(setq tab-bar-new-tab-choice "*scratch*")
(tab-bar-mode +1)

;; use `ibuffer' instead of buffer list
(+define-key (kbd "C-x C-b") 'ibuffer)

;;; vterm

(+install-package 'vterm)
(with-eval-after-load 'vterm
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (setq vterm-max-scrollback 10000)
  (setq vterm-kill-buffer-on-exit t))

;; add as project popup shell
(defun +project-vterm ()
  "Start term in the current project's root directory.
If a buffer already exists for running a shell in the project's root,
switch to it.  Otherwise, create a new shell buffer.
With \\[universal-argument] prefix arg, create a new inferior shell buffer even
if one already exists."
  (interactive)
  (let* ((default-directory (project-root (project-current t)))
         (default-project-vterm-name (project-prefixed-buffer-name "vterm"))
         (vterm-buffer (get-buffer default-project-vterm-name)))
    (if (and vterm-buffer (not current-prefix-arg))
        (pop-to-buffer-same-window vterm-buffer)
      (let ((vterm-buffer-name (generate-new-buffer-name default-project-vterm-name)))
        (vterm)))))

(define-key project-prefix-map (kbd "t") #'+project-vterm)

;;; Eshell

(with-eval-after-load 'em-alias
  (dolist
      (alias
       '(("q"     "exit")
		 ("f"     "find-file $1")
		 ("ff"    "find-file $1")
		 ("d"     "dired $1")
		 ("pd"    "proced $1")
		 ("rg"    "rg --color=always $*")
		 ("l"     "ls -lh $*")
		 ("ll"    "ls -lah $*")
		 ("git"   "git --no-pager $*")
		 ("gg"    "magit-status")
		 ("clear" "clear-scrollback")
		 ("u"     "eshell-up $1")))	; see section below for `eshell-up' command and package
    (add-to-list 'eshell-command-aliases-list alias))
  (eshell-write-aliases-list))

(with-eval-after-load 'em-hist
  (add-hook 'eshell-pre-command-hook #'eshell-save-some-history)
  (setq eshell-history-size 10000
		eshell-history-ignoredups t
		eshell-input-filter #'eshell-input-filter-initial-space
		;; don't record command in history if prefixed with whitespace
		eshell-input-filter #'eshell-input-filter-initial-space)
  (eshell-hist-initialize))

;; eshell visual commands
(with-eval-after-load 'em-term
  (dolist (cmd '("htop" "vim" "nvim"))
    (add-to-list 'eshell-visual-commands cmd)))

;; eshell defaults and generic conf
(with-eval-after-load 'eshell
  (setenv "PAGER" "cat")
  ;; truncate buffer for performance
  ;; (add-to-list 'eshell-output-filter-functions #'eshell-truncate-buffer)
  ;; use TRAMP
  (add-to-list 'eshell-modules-list 'eshell-tramp)

  (setq password-cache t
		password-cache-expiry 3600)

  (setq eshell-buffer-maximum-lines 10000
		eshell-scroll-to-bottom-on-input 'all
		eshell-scroll-to-bottom-on-output 'all
		eshell-kill-processes-on-exit t
		eshell-glob-case-insensitive t
		eshell-error-if-no-glob t)

  ;; directory navigation
  (defun eshell-up-closest-parent-dir (file)
    "Find the closest parent directory of a file.
Argument FILE the file to find the closest parent directory for."
    (file-name-directory
     (directory-file-name
      (expand-file-name file))))

  (defun eshell-up-find-parent-dir (path &optional match)
    "Find the parent directory based on the user's input.
Argument PATH the source directory to search from.
Argument MATCH a string that identifies the parent directory to search for."
    (let ((closest-parent (eshell-up-closest-parent-dir path)))
      (if match
          (let ((case-fold-search nil))
            (locate-dominating-file
			 closest-parent
             (lambda (parent)
               (let ((dir (file-name-nondirectory
                           (expand-file-name
                            (directory-file-name parent)))))
				 (if (string-match match dir)
                     dir
                   nil)))))
		closest-parent)))

  (defun eshell-up (&optional match)
    "Go to a specific parent directory in eshell.
Argument MATCH a string that identifies the parent directory to go
to."
    (interactive)
    (let* ((path default-directory)
           (parent-dir (eshell-up-find-parent-dir path match)))
      (when parent-dir
        (eshell/cd parent-dir)))))

;; highlight mathing parentesis
(show-paren-mode +1)
(electric-pair-mode +1)
;; propably these tree lines can be collapsed in one set face attribute
(set-face-background 'show-paren-match nil)
(set-face-foreground 'show-paren-match "red")
(set-face-italic 'show-paren-match t)

;;; Enhance compilation buffer

;; colorize compilation buffer
(with-eval-after-load 'compilation-mode
  (require 'ansi-color))

(defun +colorize-compilation-buffer ()
  "Support ANSI colors in compilation buffer."
  (when (eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))

(add-hook 'compilation-filter-hook #'+colorize-compilation-buffer)

;; follow output with scroll in compilation buffer
(setq compilation-scroll-output t)

;;; Indentation

;; human tab with
(add-hook 'prog-mode-hook (lambda () (setq tab-width 4)))
;; indent with spaces
(setq-default indent-tabs-mode nil)

;;;; Completion framework

;; help and docs in minibuffer
(+install-package 'marginalia)
(marginalia-mode +1)

;;; Completion styles and functions

;; completion style (how completion candidates are narrowed)
(+install-package 'orderless)
(setq orderless-matching-styles
      '(orderless-literal orderless-initialism orderless-regexp))
(setq orderless-component-separator "[ +]+")
(setq completion-styles '(orderless))

;;; Minibuffer completions

;; completion UI
(+install-package 'vertico)

(vertico-mode +1)

;; TODO use builtin vc
(+install-package 'magit)


(add-hook 'after-init-hook (lambda () (message "Initialized in %s" (emacs-init-time))))
