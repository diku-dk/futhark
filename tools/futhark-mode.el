;;; futhark-mode.el --- major mode for editing Futhark source files

;; Copyright (C) DIKU, University of Copenhagen
;;   Written by Troels Henriksen (athas@sigkill.dk) in 2013.
;;   Improved by Niels G. W. Serup (ngws@metanohi.name) in 2014.

;;; Commentary:
;; This mode provides syntax highlighting and automatic indentation
;; for Futhark source files.  There is sadly yet no automatic
;; recompilation or interactive environment, mostly because there is
;; no good futharki yet.  Change keybindings in `futhark-mode-map` and add
;; hooks to `futhark-mode-hook`.
;;
;; Provided this file is loaded, futhark-mode is automatically engaged when
;; opening .l0 files.

;;; Code:

(require 'cl)

(defvar futhark-mode-hook nil
  "Hook for Futhark mode - run whenever the mode is entered.")

(defvar futhark-mode-map
  (let ((map (make-keymap)))
    map)
  "Keymap for Futhark major mode.")

(defcustom futhark-indent-level 2
  "Indentation of blocks in Futhark."
  :group 'futhark
  :type '(integer))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.l0\\'" . futhark-mode))

(defconst futhark-keywords
  '("if" "then" "else" "let" "loop" "in" "with" "int" "real" "bool" "char"
    "fun" "fn" "for" "do" "do" "op" "not" "pow"
    "iota" "shape" "replicate" "reshape" "transpose" "map" "reduce" "zip" "unzip"
    "scan" "split" "concat" "filter" "redomap"
    "mapT" "reduceT" "scanT" "filterT" "redomapT"
    "empty" "copy")
  "A list of Futhark keywords.")

(defconst futhark-font-lock-keywords-1
  `((,(concat "\\<" (regexp-opt futhark-keywords t) "\\>") . font-lock-builtin-face)
    ("\\<\\w*\\>" . font-lock-variable-name-face)
    )
  "Minimal highlighting expressions for Futhark mode.")

(defconst futhark-font-lock-keywords-2
  (append futhark-font-lock-keywords-1
          '(
            ;; Character literals
            ("'\\([^\\'\n]\\|\\\\[a-z']\\)'" . font-lock-constant-face)
            ;; Boolean literals
            ("\\<\\(True\\|False\\)\\>" . font-lock-constant-face)
            ;; Numeric literals; both integers and floating-point.
            ("-?\\([0-9]+\\|\\([0-9]+\\(\\.[0-9]*\\)?\\|\\.[0-9]+\\)\\([eE][\+\~]?[0-9]+\\)?\\)" . font-lock-constant-face)
            ))
  "As `futhark-font-lock-keywords-1`, but also highlight constants.")

(defvar futhark-font-lock-keywords futhark-font-lock-keywords-2
  "Default highlighting expressions for Futhark mode.")

(defvar futhark-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Define the // line comment syntax.
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table used in `futhark-mode'.")


;;; Indentation

(defun futhark-indent-line ()
  "Indent current line as Futhark code."
  (interactive)
  (let ((savep (> (current-column) (current-indentation)))
    (indent (or (futhark-calculate-indentation)
                (current-indentation))))
    (if savep
        (save-excursion (indent-line-to indent))
      (indent-line-to indent))))

;; Keywords on which to base indentation.
(defstruct futhark-indkwd name level pos)

(defun futhark-indkwd-column (k)
  (save-excursion
    (goto-char (futhark-indkwd-pos k))
    (current-column)))

(defun futhark-indkwd-linum (k)
  (line-number-at-pos (futhark-indkwd-pos k)))

(defun futhark-indkwd-compare (a b)
  "Compare two positions of keywords to see which keyword should
be indented against.  Prefer the keywords closest to you and with
least existing indentation."
  (or (> (futhark-indkwd-linum a) (futhark-indkwd-linum b))
      (and (= (futhark-indkwd-linum a) (futhark-indkwd-linum b))
           (< (futhark-indkwd-column a) (futhark-indkwd-column b)))))

(defun futhark-backward-part ()
  "Try to jump back one sexp.  The net effect seems to be that it
works ok."
  (ignore-errors (backward-sexp 1) t))

(defun futhark-calculate-indentation ()
  "Calculate the indentation for the current line.  In general,
prefer as little indentation as possible to visually separate the
code, but use more detailed indentation with \"if\", \"then\",
and \"else\", and \"let\", \"loop\", and \"in\"."
  (let ((parse-sexp-lookup-properties t)
        (parse-sexp-ignore-comments t)
        (oldp (point)))
    (save-excursion
      (beginning-of-line-text)

      ;; Align comment to next non-comment line.
      (when (looking-at "//")
        (forward-comment (buffer-size)))

      (or (cond

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
                (or (futhark-find-keyword-backward "let")
                    (futhark-find-keyword-backward "loop"))
              (current-column)))

           ;; Make "then" align to nearest "if".
           ((looking-at "then")
            (when (futhark-find-keyword-first-on-line-backward "if")
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
            (when (futhark-find-keyword-first-on-line-backward "then")
              (current-column))))

          ;; Otherwise, if the previous line ends with "in", indent to
          ;; the matching "let" or "loop".
          (save-excursion
            (when (and (futhark-backward-part)
                       (looking-at "in")
                       (or (futhark-find-keyword-backward "let")
                           (futhark-find-keyword-backward "loop")))
              (current-column)))

          ;; Otherwise, if the previous line ends with "=", indent to
          ;; the matching "let" if it's close.
          (save-excursion
            (when (and (futhark-backward-part)
                       (looking-at "=")
                       (let ((linum (line-number-at-pos)))
                         (when (futhark-find-keyword-backward "let")
                           (= linum (line-number-at-pos)))))
              (+ futhark-indent-level (current-column))))

          ;; Otherwise, try to align to a control keyword if the
          ;; previous line does not end with a comma.
          (when (not (save-excursion
                       (when (futhark-backward-part)
                         (end-of-line)
                         (goto-char (1- (point)))
                         (looking-at ","))))
            (let ((indkwds (list
                            ;; Don't further indent lines after "in".
                            (make-futhark-indkwd :name "in" :level 0)
                            ;; In general, "do" shouldn't be the first
                            ;; word on a line, but don't fret over it if
                            ;; it is.
                            (make-futhark-indkwd :name "do" :level 0)
                            ;; Don't consider "let" heavy lines reason
                            ;; to further indent.
                            (make-futhark-indkwd :name "let" :level 0)
                            ;; The rest is more obvious.
                            (make-futhark-indkwd :name "else" :level 1)
                            (make-futhark-indkwd :name "then" :level 1)
                            (make-futhark-indkwd :name "loop" :level 1)
                            (make-futhark-indkwd :name "for" :level 1)
                            (make-futhark-indkwd :name "fn" :level 1)
                            (make-futhark-indkwd :name "fun" :level 1))))
              (mapc (lambda (k)
                      (save-excursion
                        (when (futhark-find-keyword-first-on-line-backward (futhark-indkwd-name k))
                          (setf (futhark-indkwd-pos k) (point))))) indkwds)
              (let ((indkwd-best
                     (car (sort (remove-if
                                 (lambda (k) (null (futhark-indkwd-pos k)))
                                 indkwds)
                                'futhark-indkwd-compare))))
                (when indkwd-best
                  (save-excursion
                    (goto-char (futhark-indkwd-pos indkwd-best))
                    (+ (* futhark-indent-level (futhark-indkwd-level indkwd-best))
                       (current-column)))))))

          ;; Otherwise, try to align to the top element in an array
          ;; literal or similar structure.
          (when (ignore-errors (backward-up-list 1) t)
            (goto-char (1+ (point)))
            (or
             (save-excursion
               (when (re-search-forward "[^[:space:]]" (line-end-position) t)
                 (goto-char (1- (point)))
                 (current-column)))
             ;; If the top element is not on the same line as the opening paren,
             ;; use 1 indent level relative to the previous line.
             (save-excursion
               (goto-char oldp)
               (forward-line -1)
               (beginning-of-line-text)
               (+ futhark-indent-level (current-column)))
             ))

          ;; In all remaining cases (what are those?), keep the current
          ;; indentation.
          ))))

(defun futhark-find-keyword-first-on-line-backward (word &optional is-at-in)
  "Do the same as `futhark-find-keyword-backward', except if one line
  has several matching keywords, set the mark at the first one."
  (let ((is-at-in (or is-at-in (looking-at "in"))))
    (when (futhark-find-keyword-backward word is-at-in)
      (let ((p (point)))
        (while (and (when (futhark-find-keyword-backward word is-at-in)
                      (not (= (point) p)))
                    (= (line-number-at-pos) (line-number-at-pos p)))
          (setq p (point)))
        (goto-char p))
      t)))

(defun futhark-find-keyword-backward (word &optional is-at-in)
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
        (topp (or (ignore-errors
                    (save-excursion
                      (backward-up-list 1)
                      (point)))
                  -1)))
    (while (and (not (bobp))
                (> (point) topp)
                (futhark-backward-part)
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
    (if (and (looking-at word)
             (not (= (point) oldp)))
        t
      (goto-char oldp)
      nil)))


(define-derived-mode futhark-mode fundamental-mode "Futhark"
  "Major mode for editing Futhark source files."
  :syntax-table futhark-mode-syntax-table
  (set (make-local-variable 'font-lock-defaults) '(futhark-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'futhark-indent-line)
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-padding) " "))

(provide 'futhark-mode)

;;; futhark-mode.el ends here
