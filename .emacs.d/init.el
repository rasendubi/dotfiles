;; [[file:~/dotfiles/emacs.org::*Bootstrap][Bootstrap:1]]
;;
;; This file is auto-generated from "emacs.org"
;;

(defvar rasen/dotfiles-directory
  (file-name-as-directory
   (expand-file-name ".." (file-name-directory (file-truename user-init-file))))
  "The path to the dotfiles directory.")

(require 'org-install)
(require 'ob-tangle)

;; [[[[file:~/dotfiles/emacs.org::patch-ob-tangle][patch-ob-tangle]]][patch-ob-tangle]]
(require 'el-patch)
;; org-babel fixes to tangle ALL matching sections
(defun rasen/map-regex (regex fn)
  "Map the REGEX over the BUFFER executing FN.

FN is called with the match-data of the regex.

Returns the results of the FN as a list."
  (save-excursion
    (goto-char (point-min))
    (let (res)
      (save-match-data
        (while (re-search-forward regex nil t)
          (let ((f (match-data)))
            (setq res
                  (append res
                          (list
                           (save-match-data
                             (funcall fn f))))))))
      res)))

(el-patch-feature ob-core)
(el-patch-defun org-babel-expand-noweb-references (&optional info parent-buffer)
  "Expand Noweb references in the body of the current source code block.

For example the following reference would be replaced with the
body of the source-code block named `example-block'.

<<example-block>>

Note that any text preceding the <<foo>> construct on a line will
be interposed between the lines of the replacement text.  So for
example if <<foo>> is placed behind a comment, then the entire
replacement text will also be commented.

This function must be called from inside of the buffer containing
the source-code block which holds BODY.

In addition the following syntax can be used to insert the
results of evaluating the source-code block named `example-block'.

<<example-block()>>

Any optional arguments can be passed to example-block by placing
the arguments inside the parenthesis following the convention
defined by `org-babel-lob'.  For example

<<example-block(a=9)>>

would set the value of argument \"a\" equal to \"9\".  Note that
these arguments are not evaluated in the current source-code
block but are passed literally to the \"example-block\"."
  (let* ((parent-buffer (or parent-buffer (current-buffer)))
         (info (or info (org-babel-get-src-block-info 'light)))
         (lang (nth 0 info))
         (body (nth 1 info))
         (ob-nww-start org-babel-noweb-wrap-start)
         (ob-nww-end org-babel-noweb-wrap-end)
         (new-body "")
         (nb-add (lambda (text) (setq new-body (concat new-body text))))
         index source-name evaluate prefix)
    (with-temp-buffer
      (setq-local org-babel-noweb-wrap-start ob-nww-start)
      (setq-local org-babel-noweb-wrap-end ob-nww-end)
      (insert body) (goto-char (point-min))
      (setq index (point))
      (while (and (re-search-forward (org-babel-noweb-wrap) nil t))
        (save-match-data (setf source-name (match-string 1)))
        (save-match-data (setq evaluate (string-match "(.*)" source-name)))
        (save-match-data
          (setq prefix
                (buffer-substring (match-beginning 0)
                                  (save-excursion
                                    (beginning-of-line 1) (point)))))
        ;; add interval to new-body (removing noweb reference)
        (goto-char (match-beginning 0))
        (funcall nb-add (buffer-substring index (point)))
        (goto-char (match-end 0))
        (setq index (point))
        (funcall
         nb-add
         (with-current-buffer parent-buffer
           (save-restriction
             (widen)
             (mapconcat ;; Interpose PREFIX between every line.
              #'identity
              (split-string
               (if evaluate
                   (let ((raw (org-babel-ref-resolve source-name)))
                     (if (stringp raw) raw (format "%S" raw)))
                 (or
                  ;; Retrieve from the Library of Babel.
                  (nth 2 (assoc-string source-name org-babel-library-of-babel))
                  ;; Return the contents of headlines literally.
                  (save-excursion
                    (when (org-babel-ref-goto-headline-id source-name)
                      (org-babel-ref-headline-body)))
                  ;; Find the expansion of reference in this buffer.
                  (save-excursion
                    (goto-char (point-min))
                    (let* ((name-regexp
                            (org-babel-named-src-block-regexp-for-name
                             source-name))
                           (comment
                            (string= "noweb"
                                     (cdr (assq :comments (nth 2 info)))))
                           (c-wrap
                            (lambda (s)
                              ;; Comment, according to LANG mode,
                              ;; string S.  Return new string.
                              (unless org-babel-tangle-uncomment-comments
                                (with-temp-buffer
                                  (funcall (org-src-get-lang-mode lang))
                                  (comment-region (point)
                                                  (progn (insert s) (point)))
                                  (org-trim (buffer-string))))))
                           (expand-body
                            (lambda (i)
                              ;; Expand body of code blocked
                              ;; represented by block info I.
                              (let ((b (if (org-babel-noweb-p (nth 2 i) :eval)
                                           (org-babel-expand-noweb-references i)
                                         (nth 1 i))))
                                (if (not comment) b
                                  (let ((cs (org-babel-tangle-comment-links i)))
                                    (concat (funcall c-wrap (car cs)) "\n"
                                            b "\n"
                                            (funcall c-wrap (cadr cs)))))))))
                      (if (and (re-search-forward name-regexp nil t)
                               (not (org-in-commented-heading-p)))
                          (el-patch-swap
                            (funcall expand-body
                                     (org-babel-get-src-block-info 'light))
                            ;; Found a source block named SOURCE-NAME.
                            ;; Assume it is unique; do not look after
                            ;; `:noweb-ref' header argument.
                            (mapconcat
                             #'identity
                             (rasen/map-regex name-regexp
                                              (lambda (md)
                                                (funcall expand-body
                                                         (org-babel-get-src-block-info 'light))))
                             "\n"))
                        ;; Though luck.  We go into the long process
                        ;; of checking each source block and expand
                        ;; those with a matching Noweb reference.
                        (let ((expansion nil))
                          (org-babel-map-src-blocks nil
                            (unless (org-in-commented-heading-p)
                              (let* ((info (org-babel-get-src-block-info 'light))
                                     (parameters (nth 2 info)))
                                (when (equal source-name
                                             (cdr (assq :noweb-ref parameters)))
                                  (push (funcall expand-body info) expansion)
                                  (push (or (cdr (assq :noweb-sep parameters))
                                            "\n")
                                        expansion)))))
                          (when expansion
                            (mapconcat #'identity
                                       (nreverse (cdr expansion))
                                       ""))))))
                  ;; Possibly raise an error if named block doesn't exist.
                  (if (or org-babel-noweb-error-all-langs
                          (member lang org-babel-noweb-error-langs))
                      (error "%s could not be resolved (see \
`org-babel-noweb-error-langs')"
                             (org-babel-noweb-wrap source-name))
                    "")))
               "[\n\r]")
              (concat "\n" prefix))))))
      (funcall nb-add (buffer-substring index (point-max))))
    new-body))
;; patch-ob-tangle ends here

(org-babel-load-file (expand-file-name "emacs.org" rasen/dotfiles-directory))
;; Bootstrap:1 ends here
