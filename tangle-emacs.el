(defun tangle-emacs ()
  "Tangle emacs literate configuration."
  (require 'org)
  (require 'org-tempo)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)))
  (let ((org-confirm-babel-evaluate nil))
    ;; TODO parameterize file
    (org-babel-tangle-file "emacs/Emacs.org")))

(tangle-emacs)
