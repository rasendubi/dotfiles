;; [[file:~/dotfiles/emacs.org::*Bootstrap][Bootstrap:1]]
;;; init.el --- The start of my configuration
;;; Commentary:
;;; Code:

(package-initialize)

(defvar rasen/dotfiles-directory
  (file-name-as-directory
   (expand-file-name ".." (file-name-directory (file-truename user-init-file))))
  "The path to the dotfiles directory.")

(require 'org-install)
(require 'ob-tangle)

(org-babel-load-file (expand-file-name "emacs.org" rasen/dotfiles-directory))
;;; init.el ends here
;; Bootstrap:1 ends here
