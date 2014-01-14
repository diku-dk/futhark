;;; l0-mode.el --- major mode for editing L0 source files

;; Copyright (C) DIKU, University of Copenhagen
;;   Written by Troels Henriksen (athas@sigkill.dk) in 2013.
;;   Improved by Niels G. W. Serup (ngws@metanohi.name) in 2014.

;;; Commentary:
;; This mode provides syntax highlighting and automatic indentation
;; for L0 source files.  There is sadly yet no automatic recompilation
;; or interactive environment, mostly because there is no l0ci yet.
;; Change keybindings in `l0-mode-map` and add hooks to
;; `l0-mode-hook`.
;;
;; Provided this file is loaded, l0-mode is automatically engaged when
;; opening .l0 files.

;;; Code:

(require 'cl)

(defvar l0-mode-hook nil
  "Hook for L0 mode - run whenever the mode is entered.")

(defvar l0-mode-map
  (let ((map (make-keymap)))
    map)
  "Keymap for L0 major mode.")

(defcustom l0-indent-level 2
  "Indentation of blocks in L0."
  :group 'l0
  :type '(integer))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.l0\\'" . l0-mode))

(defconst l0-keywords
  '("if" "then" "else" "let" "loop" "in" "with" "int" "real" "bool" "char"
    "fun" "fn" "for" "do" "do" "op" "not" "pow"
    "iota" "shape" "replicate" "reshape" "transpose" "map" "reduce" "zip" "unzip"
    "scan" "split" "concat" "filter" "redomap"
    "mapT" "reduceT" "scanT" "filterT" "redomapT"
    "empty" "copy")
  "A list of L0 keywords.")

(defconst l0-font-lock-keywords-1
  `((,(concat "\\<" (regexp-opt l0-keywords t) "\\>") . font-lock-builtin-face)
    ("\\<\\w*\\>" . font-lock-variable-name-face)
    )
  "Minimal highlighting expressions for L0 mode.")

(defconst l0-font-lock-keywords-2
  (append l0-font-lock-keywords-1
          '(
            ;; Character literals
            ("'\\([^\\'\n]\\|\\\\[a-z']\\)'" . font-lock-constant-face)
            ;; Boolean literals
            ("\\<\\(True\\|False\\)\\>" . font-lock-constant-face)
            ;; Numeric literals; both integers and floating-point.
            ("-?\\([0-9]+\\|\\([0-9]+\\(\\.[0-9]*\\)?\\|\\.[0-9]+\\)\\([eE][\+\~]?[0-9]+\\)?\\)" . font-lock-constant-face)
            ))
  "As `l0-font-lock-keywords-1`, but also highlight constants.")

(defvar l0-font-lock-keywords l0-font-lock-keywords-2
  "Default highlighting expressions for L0 mode.")

(defvar l0-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Define the // line comment syntax.
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table used in `l0-mode'.")


;;; Indentation

(defun l0-indent-line ()
  "Indent current line as L0 code."
  (interactive)
  (let ((savep (> (current-column) (current-indentation)))
    (indent (or (l0-calculate-indentation)
                (current-indentation))))
    (if savep
        (save-excursion (indent-line-to indent))
      (indent-line-to indent))))

;; Keywords on which to base indentation.
(cl-defstruct l0-indkwd name level pos)

(defun l0-indkwd-column (k)
  (save-excursion
    (goto-char (l0-indkwd-pos k))
    (current-column)))

(defun l0-indkwd-linum (k)
  (line-number-at-pos (l0-indkwd-pos k)))

(defun l0-indkwd-compare (a b)
  "Compare two positions of keywords to see which keyword should
be indented against.  Prefer the keywords closest to you and with
least existing indentation."
  (or (> (l0-indkwd-linum a) (l0-indkwd-linum b))
      (and (= (l0-indkwd-linum a) (l0-indkwd-linum b))
           (< (l0-indkwd-column a) (l0-indkwd-column b)))))

(defun l0-backward-part ()
  "Try to jump back one sexp.  The net effect seems to be that it
works ok."
  (ignore-errors (backward-sexp 1) t))

(defun l0-calculate-indentation ()
  "Calculate the indentation for the current line.  In general,
prefer as little indentation as possible to visually separate the
code, but use more detailed indentation with \"if\", \"then\",
and \"else\", and \"let\", \"loop\", and \"in\"."
  (let ((parse-sexp-lookup-properties t)
        (parse-sexp-ignore-comments t))
    (save-excursion
      (beginning-of-line-text)
      (or (cond

           ;; Don't touch comments.
           ((looking-at "//")
            (current-column))

           ;; Function definitions to column 0.
           ((looking-at "fun\\>")
            0)

           ;; Closing paren and comma indents to opening paren.
           ((looking-at ",\\|\\s)")
            (ignore-errors
              (backward-up-list 1)
              (current-column)))

           ;; Make "in" align to nearest "let" or "loop".
           ((looking-at "in")
            (when
                ;; Do not use the first-on-line variant, as we're
                ;; interested in a matching "let" or "loop", not in a
                ;; short indentation.
                (or (l0-find-keyword-backward "let")
                    (l0-find-keyword-backward "loop"))
              (current-column)))

           ;; Make "then" align to nearest "if".
           ((looking-at "then")
            (when (l0-find-keyword-first-on-line-backward "if")
              (current-column)))

           ;; Make "else if" align to nearest "else" or "if", instead
           ;; of nearest "then", to avoid very long lines if chained.
           ((looking-at "else[[:space:]]+if")
            (let ((p
                   (or
                    (search-backward "else" 0 t)
                    (search-backward "if" 0 t))))
              (when p
                (goto-char p)
                (current-column))))

           ;; Make "else" align to nearest "then".
           ((looking-at "else")
            (when (l0-find-keyword-first-on-line-backward "then")
              (current-column))))

          ;; Otherwise, if the previous line ends with "in", indent to
          ;; the matching "let" or "loop".
          (save-excursion
            (when (and (l0-backward-part)
                       (looking-at "in")
                       (or (l0-find-keyword-backward "let")
                           (l0-find-keyword-backward "loop")))
              (current-column)))

          ;; Otherwise, try to align to a control keyword if the
          ;; previous line does not end with a comma.
          (when (not (save-excursion
                       (when (l0-backward-part)
                         (end-of-line)
                         (goto-char (1- (point)))
                         (looking-at ","))))
            (let ((indkwds (list
                            ;; Don't further indent lines after "in".
                            (make-l0-indkwd :name "in" :level 0)
                            ;; In general, "do" shouldn't be the first
                            ;; word on a line, but don't fret over it if
                            ;; it is.
                            (make-l0-indkwd :name "do" :level 0)
                            ;; Don't consider "let" heavy lines reason
                            ;; to further indent.
                            (make-l0-indkwd :name "let" :level 0)
                            ;; The rest is more obvious.
                            (make-l0-indkwd :name "else" :level 1)
                            (make-l0-indkwd :name "then" :level 1)
                            (make-l0-indkwd :name "loop" :level 1)
                            (make-l0-indkwd :name "for" :level 1)
                            (make-l0-indkwd :name "fn" :level 1)
                            (make-l0-indkwd :name "fun" :level 1))))
              (mapc (lambda (k)
                      (save-excursion
                        (when (l0-find-keyword-first-on-line-backward (l0-indkwd-name k))
                          (setf (l0-indkwd-pos k) (point))))) indkwds)
              (let ((indkwd-best
                     (car (sort (remove-if
                                 (lambda (k) (null (l0-indkwd-pos k)))
                                 indkwds)
                                'l0-indkwd-compare))))
                (when indkwd-best
                  (save-excursion
                    (goto-char (l0-indkwd-pos indkwd-best))
                    (+ (* l0-indent-level (l0-indkwd-level indkwd-best))
                       (current-column)))))))

          ;; Otherwise, try to align to the top element in an array
          ;; literal or similar structure.
          (when (ignore-errors (backward-up-list 1) t)
            (goto-char (1+ (point)))
            (or
             (save-excursion
               ;; If the top element is not on the same line as the
               ;; opening paren, use 1 indent level.
               (when (re-search-forward "[^[:space:]]" (line-end-position) t)
                 (goto-char (1- (point)))
                 (current-column)))
             (+ l0-indent-level (1- (current-column)))))

          ;; In all remaining cases (what are those?), keep the
          ;; current indentation.
          ))))

(defun l0-find-keyword-first-on-line-backward (word &optional is-at-in)
  "Do the same as `l0-find-keyword-backward', except if one line
  has several matching keywords, set the mark at the first one."
  (let ((is-at-in (or is-at-in (looking-at "in"))))
    (when (l0-find-keyword-backward word is-at-in)
      (let ((p (point)))
        (while (and (when (l0-find-keyword-backward word is-at-in)
                      (not (= (point) p)))
                    (= (line-number-at-pos) (line-number-at-pos p)))
          (setq p (point)))
        (goto-char p))
      t)))

(defun l0-find-keyword-backward (word &optional is-at-in)
  "Find a keyword before the current position.  Set mark and
return t if found; return nil otherwise."
  (let ((oldp (point))
        ;; We need to count "if"s and "else"s to properly indent.
        (missing-ifs 0)
        ;; The same with "let", "loop" and "in", though only if we
        ;; start at "in".
        (missing-outs 0)
        ;; "in" is special, since starting at it means aligning it to
        ;; the correct "let" or "loop", which means counting.  In all
        ;; other cases, we just want the least space-using
        ;; indentation.
        (orig-in (or is-at-in (looking-at "in")))
        ;; Only look in the current paren-delimited code.
        (startp (or (ignore-errors
                      (save-excursion
                        (backward-up-list 1)
                        (point)))
                      -1)))
    (while (and (not (bobp))
                (> (point) startp)
                (l0-backward-part)
                (or (not (or (looking-at word)
                             (looking-at "fun")))
                    (> missing-ifs 0)
                    (> missing-outs 0)
                    ))
      (cond ((looking-at "if")
             (setq missing-ifs (max 0 (1- missing-ifs))))
            ((looking-at "else")
             (incf missing-ifs))
            (orig-in
              (cond
               ((looking-at "let")
                (setq missing-outs (max 0 (1- missing-outs))))
               ((looking-at "loop")
                (setq missing-outs (max 0 (1- missing-outs))))
               ((looking-at "in")
                (incf missing-outs))))))
    (if (looking-at word)
        t
      (goto-char oldp)
      nil)))


(define-derived-mode l0-mode fundamental-mode "L0"
  "Major mode for editing L0 source files."
  :syntax-table l0-mode-syntax-table
  (set (make-local-variable 'font-lock-defaults) '(l0-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'l0-indent-line)
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-padding) " "))

(provide 'l0-mode)

;;; l0-mode.el ends here
