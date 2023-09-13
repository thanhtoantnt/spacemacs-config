;;; packages.el --- toan layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: thanhtoantnt <thanhtoantnt@loris-81>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `toan-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `toan/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `toan/pre-init-PACKAGE' and/or
;;   `toan/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst toan-packages
  '(
    swiper
    super-save
    monky
    comment-dwim-2
    )
  "The list of Lisp packages required by the toan layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")


(defun toan/init-packages ()
  (use-package swiper)
  (use-package comment-dwim-2)
  (use-package monky)
  (use-package super-save)
  (use-package column-marker)
  )

(defun toan/init-smartparents ()
  (smartparens-mode 1)
  (display-line-numbers-mode 1)
  )

(defun check-sub-string (sub-string super-string)
  (and super-string
       sub-string
       (string-match-p (regexp-quote sub-string) super-string)))

(defun toan/compile ()
  "Find the closest Makefile and compile."
  (interactive)
  ;; (when (get-buffer "*compilation*") (kill-buffer "*compilation*"))
  (defun find-make-dir (dir)
    (cond ((check-sub-string ".." dir) "./")
          ((file-exists-p (expand-file-name "Makefile" dir)) dir)
          ((file-exists-p (expand-file-name "build/Makefile" dir))
           (expand-file-name "build" dir))
          (t (find-make-dir (expand-file-name ".." dir)))) )
  ;; save editing buffers
  (let ((root (projectile-project-root)))
    (save-some-buffers (and root (not compilation-ask-about-save))
                       (lambda ()
                         (projectile-project-buffer-p (current-buffer) root))))
  (when (check-sub-string "make" compile-command)
    (let ((make-dir (expand-file-name (find-make-dir default-directory))))
      (setq compile-command (format "make -k -C %s" make-dir))))
  (call-interactively 'compile)

  )

(defun toan/config-keys ()
  (global-set-key (kbd "M-o") 'swiper)
  (global-set-key (kbd "M-;") 'comment-dwim-2)
  (global-set-key (kbd "M-|") 'split-window-right)
  ;; NOTE: Cannot override the binding
  (global-set-key (kbd "M-_") 'split-window-below)
  (global-set-key (kbd "C-/") 'undo-tree-redo)
  (global-set-key (kbd "C-o") 'evil-window-next)
  (global-set-key (kbd "M--") 'evil-window-delete)
  (global-set-key (kbd "C-x m") 'helm-mini)
  (global-set-key (kbd "C-x g") 'magit)
  (global-set-key (kbd "C-c C-v") 'toan/compile)
)




;;; packages.el ends here
