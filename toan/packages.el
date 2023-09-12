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


(defun toan/init-swiper ()
  (use-package swiper))

(defun toan/init-comment-dwim-2 ()
  (use-package comment-dwim-2))

(defun toan/init-monky ()
  (use-package monky))

(defun toan/init-super-save ()
  (use-package super-save))

(defun toan/init-column-marker ()
  (use-package column-marker))

;;; packages.el ends here
