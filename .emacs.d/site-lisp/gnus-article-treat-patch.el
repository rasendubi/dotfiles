;; Gnus addon to beautify patch-like emails. This uses a "ft/" prefix for
;; everything to avoid clashing with anything upstream. That prefix can be
;; savely s,ft/,,'d - if this is to be submitted to the gnus developers.

(require 'diff-mode)

(add-hook 'gnus-part-display-hook 'ft/gnus-article-treat-patch)

;; Colour handling and faces
(defun ft/gnus-colour-line (use-face)
  "Set text overlay to `use-face' for the current line."
  (overlay-put (make-overlay (point-at-bol) (point-at-eol)) 'face use-face))

(make-face 'ft/gnus-three-dashes)
(set-face-attribute 'ft/gnus-three-dashes nil :foreground "brightblue")
(make-face 'ft/gnus-scissors)
(set-face-attribute 'ft/gnus-scissors nil :foreground "brown")
(make-face 'ft/gnus-diff-index)
(set-face-attribute 'ft/gnus-diff-index nil :foreground "brightmagenta")
(make-face 'ft/gnus-diff-hunk)
(set-face-attribute 'ft/gnus-diff-hunk nil :foreground "brightblue")
(make-face 'ft/gnus-diff-equals)
(set-face-attribute 'ft/gnus-diff-equals nil :foreground "brightmagenta")
(make-face 'ft/gnus-commit-message)
(set-face-attribute 'ft/gnus-commit-message nil :foreground "white")
(make-face 'ft/gnus-diff-stat-file)
(set-face-attribute 'ft/gnus-diff-stat-file nil :foreground "yellow")
(make-face 'ft/gnus-diff-stat-bar)
(set-face-attribute 'ft/gnus-diff-stat-bar nil :foreground "magenta")
(make-face 'ft/gnus-diff-stat-num)
(set-face-attribute 'ft/gnus-diff-stat-num nil :foreground "white")
(make-face 'ft/gnus-diff-misc)
(set-face-attribute 'ft/gnus-diff-misc nil :foreground "magenta")
(make-face 'ft/gnus-commit-comment)
(set-face-attribute 'ft/gnus-commit-comment nil :inherit 'default)
(make-face 'ft/gnus-diff-header)
(set-face-attribute 'ft/gnus-diff-header nil :inherit 'diff-header)
(make-face 'ft/gnus-diff-add)
(set-face-attribute 'ft/gnus-diff-add nil :inherit 'diff-added)
(make-face 'ft/gnus-diff-remove)
(set-face-attribute 'ft/gnus-diff-remove nil :inherit 'diff-removed)

;; Pseudo-headers
(defvar ft/gnus-article-patch-pseudo-headers
  '(("^Acked-by: "      'gnus-header-name 'gnus-header-from)
    ("^C\\(c\\|C\\): "  'gnus-header-name 'gnus-header-from)
    ("^From: "          'gnus-header-name 'gnus-header-from)
    ("^Link: "          'gnus-header-name 'gnus-header-from)
    ("^Reported-by: "   'gnus-header-name 'gnus-header-from)
    ("^Reviewed-by: "   'gnus-header-name 'gnus-header-from)
    ("^Signed-off-by: " 'gnus-header-name 'gnus-header-from)
    ("^Subject: "       'gnus-header-name 'gnus-header-from)
    ("^Suggested-by: "  'gnus-header-name 'gnus-header-from))
  "List of lists of regular expressions (with two face names)
which are used to determine the highlighting of pseudo headers in
the commit message (such as \"Signed-off-by:\").

The first face if used to highlight the header's name; the second
highlights the header's value.")

(defun ft/gnus-pseudo-header-get (line)
  "Check if `line' is a pseudo header, and if so return its enty in
`ft/gnus-article-patch-pseudo-headers'."
  (catch 'done
    (dolist (entry ft/gnus-article-patch-pseudo-headers)
      (let ((regex (car entry)))
        (if (string-match regex line)
            (throw 'done entry))))
    (throw 'done '())))

(defun ft/gnus-pseudo-header-p (line)
  "Returns `t' if `line' looks like a pseudo-header; `nil' otherwise.

`ft/gnus-article-patch-pseudo-headers' is used to determine what a pseudo-header
is."
  (if (eq (ft/gnus-pseudo-header-get line) '()) nil t))

(defun ft/gnus-pseudo-header-colour (line)
  "Colourise a pseudo-header line."
  (let ((data (ft/gnus-pseudo-header-get line)))
    (if (eq data '())
        nil
      (let* ((s (point-at-bol))
             (e (point-at-eol))
             (colon (re-search-forward ":"))
             (value (+ colon 1)))
        (overlay-put (make-overlay s colon) 'face (nth 1 data))
        (overlay-put (make-overlay value e) 'face (nth 2 data))))))

;; diff-stat
(defun ft/gnus-diff-stat-colour (line)
  "Colourise a diff-stat line."
  (let ((s (point-at-bol))
        (e (point-at-eol))
        (bar (- (re-search-forward "|") 1))
        (num (- (re-search-forward "[0-9]") 1))
        (pm (- (re-search-forward "\\([+-]\\|$\\)") 1)))

    (overlay-put (make-overlay s (- bar 1)) 'face 'ft/gnus-diff-stat-file)
    (overlay-put (make-overlay bar (+ bar 1)) 'face 'ft/gnus-diff-stat-bar)
    (overlay-put (make-overlay num pm) 'face 'ft/gnus-diff-stat-num)

    (goto-char pm)
    (let* ((plus (looking-at "\\+"))
           (regex (if plus "-+" "\\++"))
           (brk (if plus
                    (re-search-forward "-" e t)
                  (re-search-forward "\\+" e t)))
           (first-face (if plus 'ft/gnus-diff-add 'ft/gnus-diff-remove))
           (second-face (if plus 'ft/gnus-diff-remove 'ft/gnus-diff-add)))

      (if (eq brk nil)
          (overlay-put (make-overlay pm e) 'face first-face)
        (progn
          (setq brk (- brk 1))
          (overlay-put (make-overlay pm brk) 'face first-face)
          (overlay-put (make-overlay brk e) 'face second-face))))))

(defun ft/gnus-diff-stat-summary-colour (line)
  "Colourise a diff-stat summary-line."
  (let* ((e (point-at-eol))
         (plus (- (re-search-forward "(\\+)" e t) 2))
         (minus (- (re-search-forward "(-)" e t) 2)))
    (overlay-put (make-overlay plus (+ plus 1)) 'face 'ft/gnus-diff-add)
    (overlay-put (make-overlay minus (+ minus 1)) 'face 'ft/gnus-diff-remove)))

(defun ft/gnus-diff-stat-line-p (line)
  "Return `t' if `line' is a diff-stat line; `nil' otherwise."
  (string-match "^ *[^ ]+[^|]+| +[0-9]+\\( *\\| +[+-]+\\)$" line))

(defun ft/gnus-diff-stat-summary-p (line)
  "Return `t' if `line' is a diff-stat summary-line; `nil' otherwise."
  (string-match "^ *[0-9]+ file\\(s\\|\\) changed,.*insertion.*deletion" line))

;; unified-diffs
(defun ft/gnus-diff-header-p (line)
  "Returns `t' if `line' looks like a diff-header; `nil' otherwise."
  (cond
   ((string-match "^\\(\\+\\+\\+\\|---\\) " line) t)
   ((string-match "^diff -" line) t)
   (t nil)))

(defun ft/gnus-index-line-p (line)
  "Returns `t' if `line' looks like an index-line; `nil' otherwise."
  (cond
   ((string-match "^Index: " line) t)
   ((string-match "^index [0-9a-f]+\\.\\.[0-9a-f]+" line) t)
   (t nil)))

(defun ft/gnus-hunk-line-p (line)
  "Returns `t' if `line' looks like a hunk-line; `nil' otherwise."
  (string-match "^@@ -[0-9]+,[0-9]+ \\+[0-9]+,[0-9]+ @@" line))

(defun ft/gnus-atp-misc-diff-p (line)
  "Return `t' if `line' is a \"misc line\" with respect to patch
treatment; `nil' otherwise."
  (let ((patterns '("^new file"
                    "^RCS file:"
                    "^retrieving revision ")))
    (catch 'done
      (dolist (regex patterns)
        (if (string-match regex line)
            (throw 'done t)))
      (throw 'done nil))))

(defun ft/gnus-atp-looks-like-diff (line)
  "Return `t' if `line' looks remotely like a line from a unified
diff; `nil' otherwise."
  (or (ft/gnus-index-line-p line)
      (ft/gnus-diff-header-p line)
      (ft/gnus-hunk-line-p line)))

;; miscellaneous line handlers
(defun ft/gnus-scissors-line-p (line)
  "Returns `t' if `line' looks like a scissors-line; `nil' otherwise."
  (cond
   ((string-match "^\\( *--* *\\(8<\\|>8\\)\\)+ *-* *$" line) t)
   (t nil)))

;; Patch mail detection
(defvar ft/gnus-article-patch-conditions nil
  "List of conditions that will enable patch treatment.  String
values will be matched as regular expressions within the currently
processed part.  Non-string value are supposed to be code fragments,
which determine whether or not to do treatment: The code needs to
return `t' if treatment is wanted.")

(defun ft/gnus-part-want-patch-treatment ()
  "Run through `ft/gnus-article-patch-conditions' to determine whether
patch treatment is wanted or not. Return `t' or `nil' accordingly."
  (catch 'done
    (dolist (entry ft/gnus-article-patch-conditions)
      (cond
       ((stringp entry)
        (if (re-search-forward entry nil t)
            (throw 'done t)))
       (t
        (if (eval entry)
            (throw 'done t)))))
      (throw 'done nil)))


;; The actual article treatment code
(defun ft/gnus-article-treat-patch-state-machine ()
  "Implement the state machine which colourises a part of an article
if it looks patch-like.

The state machine works like this:

  0a. The machinery starts at the first line of the article's body. Not
      the header lines. We don't care about header lines at all.

  0b. The whole thing works line by line. It doesn't do any forward or
      backward looks.

  1. Initially, we assume, that what we'll see first is part of the
     patch's commit-message. Hence this first initial state is
     \"commit-message\". There are several ways out of this state:

       a) a scissors line is found (see 2.)
       b) a pseudo-header line is found (see 3.)
       c) a three-dashes line is found (see 4.)
       d) something that looks like the start of a unified diff is
          found (see 7.)

  2. A scissors line is something that looks like a pair of scissors running
     through a piece of paper. Like this:

      ------ 8< ----- 8< ------

     or this:

      ------------>8-----------

     The function `ft/gnus-scissors-line-p' decides whether a line is a
     scissors line or not. After a scissors line was treated, the machine
     will switch back to the \"commit-mesage\" state.

  3. This is very similar to a scissors line. It'll just return to the old
     state after its being done. The `ft/gnus-pseudo-header-p' function
     decides if a line is a pseudo header. The line will be appropriately
     coloured.

  4. A three-dashes line is a line that looks like this: \"---\". It's the
     definite end of the \"commit-message\" state. The three dashes line is
     coloured and the state switches to \"commit-comment\". (See 5.)

  5. Nothing in \"commit-comment\" will appear in the generated commit (this
     is git-am specific semantics, but it's useful, so...). It may contain
     things like random comments or - promimently - a diff stat. (See 6.)

  6. A diff stat provides statistics about how much changed in a given commit
     by files and by whole commit (in a summary line). Two functions
     `ft/gnus-diff-stat-line-p' and `ft/gnus-diff-stat-summary-p' decide if a
     line belongs to a diff stat. It's coloured appropriately and the state
     switches back to \"commit-comment\".

  7. There is a function `ft/gnus-unified-diff-line-p' which will cause the
     state to switch to \"unified-diff\" state from either \"commit-message\"
     or \"commit-comment\". In this mode there can be a set of lines types:

       a) diff-header lines (`ft/gnus-diff-header-p')
       b) index lines (`ft/gnus-index-line-p')
       c) hunk lines (`ft/gnus-hunk-line-p')
       d) equals line (\"^==*$\")
       e) context lines (\"^ \")
       f) add lines (\"^\\+\")
       g) remove lines (\"^-\")
       h) empty lines (\"^$\")

     This state runs until the end of the part."
  (catch 'ft/gnus-atp-done
    (let ((state 'commit-message)
          line do-not-move)

      (while t
        ;; Put the current line into an easy-to-handle string variable.
        (setq line
              (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
        (setq do-not-move nil)

        ;; Switched state machine. The "real" states are `commit-message',
        ;; `commit-comment' and `unified-diff'. The other "states" are only
        ;; single-line colourisations that return to their respective parent-
        ;; state. Each state may (throw 'ft/gnus-atp-done) to leave the state-
        ;; machine immediately.
        (setq state
              (cond

               ((eq state 'commit-message)
                (cond
                 ((ft/gnus-scissors-line-p line)
                  (ft/gnus-colour-line 'ft/gnus-scissors)
                  'commit-message)
                 ((ft/gnus-pseudo-header-p line)
                  (ft/gnus-pseudo-header-colour line)
                  'commit-message)
                 ((string= line "---")
                  (ft/gnus-colour-line 'ft/gnus-three-dashes)
                  'commit-comment)
                 ((ft/gnus-atp-looks-like-diff line)
                  (setq do-not-move t)
                  'unified-diff)
                 (t
                  (ft/gnus-colour-line 'ft/gnus-commit-message)
                  'commit-message)))

               ((eq state 'commit-comment)
                (cond
                 ((ft/gnus-diff-stat-line-p line)
                  (ft/gnus-diff-stat-colour line)
                  'commit-comment)
                 ((ft/gnus-diff-stat-summary-p line)
                  (ft/gnus-diff-stat-summary-colour line)
                  'commit-comment)
                 ((ft/gnus-atp-looks-like-diff line)
                  (setq do-not-move t)
                  'unified-diff)
                 (t
                  (ft/gnus-colour-line 'ft/gnus-commit-comment)
                  'commit-comment)))

               ((eq state 'unified-diff)
                (cond
                 ((ft/gnus-diff-header-p line)
                  (ft/gnus-colour-line 'ft/gnus-diff-header)
                  'unified-diff)
                 ((ft/gnus-index-line-p line)
                  (ft/gnus-colour-line 'ft/gnus-diff-index)
                  'unified-diff)
                 ((ft/gnus-hunk-line-p line)
                  (ft/gnus-colour-line 'ft/gnus-diff-hunk)
                  'unified-diff)
                 ((string-match "^==*$" line)
                  (ft/gnus-colour-line 'ft/gnus-diff-equals)
                  'unified-diff)
                 ((string-match "^$" line)
                  'unified-diff)
                 ((string-match "^ " line)
                  (ft/gnus-colour-line 'ft/gnus-diff-context)
                  'unified-diff)
                 ((ft/gnus-atp-misc-diff-p line)
                  (ft/gnus-colour-line 'ft/gnus-diff-misc)
                  'unified-diff)
                 ((string-match "^\\+" line)
                  (ft/gnus-colour-line 'ft/gnus-diff-add)
                  'unified-diff)
                 ((string-match "^-" line)
                  (ft/gnus-colour-line 'ft/gnus-diff-remove)
                  'unified-diff)
                 (t 'unified-diff)))))

        (if (not do-not-move)
            (if (> (forward-line) 0)
                (throw 'ft/gnus-atp-done t)))))))

(defun ft/gnus-article-treat-patch ()
  "Highlight mail parts, that look like patches (well, usually
they *are* patches - or possibly, when you take git's format-patch output,
entire commit exports - including comments).  This treatment assumes the
use of unified diffs.  Here is how it works:

The most fancy type of patch mails look like this:

  From: ...
  Subject: ...
  Other-Headers: ...

  Body text, which can be reflecting the commit message but may
  optionally be followed by a so called scissors line, which
  looks like this (in case of a scissors line, the text above is
  not part of the commit message):

  -------8<----------

  If there really was a scissors line, then it's usually
  followed by repeated mail-headers. Which do not *have* to
  be the same as the one from the sender.

  From: ...
  Subject: ...

  More text. Usually part of the commit message. Likely
  multiline.  What follows may be an optional diffstat. If
  there is one, it's usually preceded by a line that contains
  only three dashes and nothing more. Before the diffstat,
  however, there may be a set of pseudo headers again, like
  these:

  Acked-by: Mike Dev <md@other.tld>
  Signed-off-by: Joe D. User <jdu@example.com>

  ---
  ChangeLog                    |    5 ++++-
  1 file changed, 4 insertions(+), 1 deletions(-)

  Now, there is again room for optional text, which is not
  part of the actual commit message. May be multiline. Actually,
  anything between the three-dashes line and the diff content
  is ignored as far as the commit message goes.

  Now for the actual diff part.  I want this to work for as
  many unified diff formats as possible.  What comes to mind
  is the format used by git and the format used by cvs and
  quilt.

  CVS style looks like this:

  Index: foo/bar.c
  ============================================================
  --- boo.orig/foo/bar.c       2010-02-24 ....
  +++ boo/foo/bar.c            2010-02-28 ....
  @@ -1823,7 +1823,7 @@
  <hunk>

  There may be multiple hunks. Each file gets an \"Index:\" and
  equals line.  Now the git format looks like this:

  diff --git a/ChangeLog b/ChangeLog
  index 6ffbc8c..36e5c17 100644
  --- a/ChangeLog
  +++ b/ChangeLog
  @@ -3,6 +3,9 @@
  <hunk>

  Again, there may be multiple hunks.

  When all hunks and all files are done, there may be additional
  text below the actual text.

And that's it.

You may define the look of several things: pseudo headers, scissor
lines, three-dashes-line, equals lines, diffstat lines, diffstat
summary. Then there is added lines, removed lines, context lines,
diff-header lines and diff-file-header lines, for which we are
borrowing the highlighting faces for from `diff-mode'."
  (if (ft/gnus-part-want-patch-treatment)
      (save-excursion
        (progn
          (let ((inhibit-read-only t))
            (goto-char (point-min))
            (ft/gnus-article-treat-patch-state-machine))))))

(provide 'gnus-article-treat-patch)
