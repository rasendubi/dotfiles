;;;  -*- lexical-binding: t; -*-
;;;
;;; This file is auto-generated from "README.org"
;;;

(defvar rasen/dotfiles-directory
  (file-name-as-directory
   (expand-file-name ".." (file-name-directory (file-truename user-init-file))))
  "The path to the dotfiles directory.")

(require 'org)
(require 'ob-tangle)
(require 'el-patch)

(org-babel-load-file (expand-file-name "README.org" rasen/dotfiles-directory))
