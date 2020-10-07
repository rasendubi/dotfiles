;;
;; This file is auto-generated from "README.org"
;;

(defvar rasen/dotfiles-directory
  (file-name-as-directory
   (expand-file-name ".." (file-name-directory (file-truename user-init-file))))
  "The path to the dotfiles directory.")

(require 'org-install)
(require 'ob-tangle)

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
         (comment (string= "noweb" (cdr (assq :comments (nth 2 info)))))
         (noweb-re (format "\\(.*?\\)\\(%s\\)"
                           (with-current-buffer parent-buffer
                             (org-babel-noweb-wrap))))
         (cache nil)
         (c-wrap
          (lambda (s)
            ;; Comment string S, according to LANG mode.  Return new
            ;; string.
            (unless org-babel-tangle-uncomment-comments
              (with-temp-buffer
                (funcall (org-src-get-lang-mode lang))
                (comment-region (point)
                                (progn (insert s) (point)))
                (org-trim (buffer-string))))))
         (expand-body
          (lambda (i)
            ;; Expand body of code represented by block info I.
            (let ((b (if (org-babel-noweb-p (nth 2 i) :eval)
                         (org-babel-expand-noweb-references i)
                       (nth 1 i))))
              (if (not comment) b
                (let ((cs (org-babel-tangle-comment-links i)))
                  (concat (funcall c-wrap (car cs)) "\n"
                          b "\n"
                          (funcall c-wrap (cadr cs))))))))
         (expand-references
          (lambda (ref cache)
            (pcase (gethash ref cache)
              (`(,last . ,previous)
               ;; Ignore separator for last block.
               (let ((strings (list (funcall expand-body last))))
                 (dolist (i previous)
                   (let ((parameters (nth 2 i)))
                     ;; Since we're operating in reverse order, first
                     ;; push separator, then body.
                     (push (or (cdr (assq :noweb-sep parameters)) "\n")
                           strings)
                     (push (funcall expand-body i) strings)))
                 (mapconcat #'identity strings "")))
              ;; Raise an error about missing reference, or return the
              ;; empty string.
              ((guard (or org-babel-noweb-error-all-langs
                          (member lang org-babel-noweb-error-langs)))
               (error "Cannot resolve %s (see `org-babel-noweb-error-langs')"
                      (org-babel-noweb-wrap ref)))
              (_ "")))))
    (replace-regexp-in-string
     noweb-re
     (lambda (m)
       (with-current-buffer parent-buffer
         (save-match-data
           (let* ((prefix (match-string 1 m))
                  (id (match-string 3 m))
                  (evaluate (string-match-p "(.*)" id))
                  (expansion
                   (cond
                    (evaluate
                     ;; Evaluation can potentially modify the buffer
                     ;; and invalidate the cache: reset it.
                     (setq cache nil)
                     (let ((raw (org-babel-ref-resolve id)))
                       (if (stringp raw) raw (format "%S" raw))))
                    ;; Retrieve from the Library of Babel.
                    ((nth 2 (assoc-string id org-babel-library-of-babel)))
                    ;; Return the contents of headlines literally.
                    ((org-babel-ref-goto-headline-id id)
                     (org-babel-ref-headline-body))
                    ;; Look for a source block named SOURCE-NAME.  If
                    ;; found, assume it is unique; do not look after
                    ;; `:noweb-ref' header argument.
                    ((org-with-point-at 1
                       (let ((r (org-babel-named-src-block-regexp-for-name id)))
                         (and (re-search-forward r nil t)
                              (not (org-in-commented-heading-p))
                              (el-patch-swap
                                (funcall expand-body
                                         (org-babel-get-src-block-info t))
                                (mapconcat
                                 #'identity
                                 (rasen/map-regex r
                                                  (lambda (md)
                                                    (funcall expand-body
                                                             (org-babel-get-src-block-info t))))
                                 "\n"))))))
                    ;; All Noweb references were cached in a previous
                    ;; run.  Extract the information from the cache.
                    ((hash-table-p cache)
                     (funcall expand-references id cache))
                    ;; Though luck.  We go into the long process of
                    ;; checking each source block and expand those
                    ;; with a matching Noweb reference.  Since we're
                    ;; going to visit all source blocks in the
                    ;; document, cache information about them as well.
                    (t
                     (setq cache (make-hash-table :test #'equal))
                     (org-with-wide-buffer
                      (org-babel-map-src-blocks nil
                        (if (org-in-commented-heading-p)
                            (org-forward-heading-same-level nil t)
                          (let* ((info (org-babel-get-src-block-info t))
                                 (ref (cdr (assq :noweb-ref (nth 2 info)))))
                            (push info (gethash ref cache))))))
                     (funcall expand-references id cache)))))
             ;; Interpose PREFIX between every line.
             (mapconcat #'identity
                        (split-string expansion "[\n\r]")
                        (concat "\n" prefix))))))
     body t t 2)))

(org-babel-load-file (expand-file-name "README.org" rasen/dotfiles-directory))
