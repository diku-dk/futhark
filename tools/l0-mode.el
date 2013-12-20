;;; l0-mode.el --- major mode for editing L0 source files

;; Copyright (C) DIKU, University of Copenhagen
;;   Written by Troels Henriksen (athas@sigkill.dk) in 2013.

;;; Commentary:
;; This mode provides syntax highlighting and (broken) automatic
;; indentation for L0 source files.  There is sadly yet no automatic
;; recompilation or interactive environment, mostly because there is
;; no l0ci yet.  Change keybindings in `l0-mode-map` and add hooks to
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
            ("~?\\([0-9]+\\|\\([0-9]+\\(\\.[0-9]*\\)?\\|\\.[0-9]+\\)\\([eE][\+\~]?[0-9]+\\)?\\)" . font-lock-constant-face)
            ))
  "As `l0-font-lock-keywords-1`, but also highlight constants.")

(defvar l0-font-lock-keywords l0-font-lock-keywords-2
  "Default highlighting expressions for L0 mode.")

(defun l0-indent-line ()
  "Indent current line as L0 code."
  (interactive)
  (let ((savep (> (current-column) (current-indentation)))
	(indent (or (l0-calculate-indentation)
                    (current-indentation))))
    (if savep
	(save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun l0-calculate-indentation ()
  (let ((parse-sexp-lookup-properties t)
        (parse-sexp-ignore-comments t))
   (save-excursion
     (beginning-of-line-text)
     ;; Indentation for comments alone on a line, matches the
     ;; proper indentation of the next line.
     (when (looking-at "//") (forward-comment 100000))
     (or
      ;; Function definitions to column 0
      (when (looking-at "fun\\>") 0)
      ;; Closing paren indents to opening paren.
      (when (looking-at "\\s)")
        (skip-syntax-forward ")")
        (backward-sexp 1)
        (current-column))
      ;; If at an if, and the previous line ends in an else, align to
      ;; the corresponding if.
      (and (looking-at "if")
           (save-excursion
             (backward-sexp 1)
             (looking-at "else")
             (when (l0-find-keyword-backwards "if")
               (current-column))))
      (and (looking-at "then")
           (when (l0-find-keyword-backwards "if")
             (current-column)))
      (and (looking-at "else")
           (when (l0-find-keyword-backwards "then")
             (current-column)))
      (when (l0-find-keyword-backwards "then\\|else")
        (+ (current-column) l0-indent-level))
      (when (looking-at "in")
        (l0-find-keyword-backwards "let")
        (current-column))
      (when (l0-find-keyword-backwards "in\\|let")
        (if (looking-at "in")
            (when (l0-find-keyword-backwards "let")
              (current-column))
          (+ (current-column) l0-indent-level)))
      ;; If nothing else, indent to the function body.
      (when (l0-find-keyword-backwards "fun")
        (+ (current-column) l0-indent-level))))))

(defun l0-find-keyword-backwards (word)
  (let ((old (point))
        (missing-ifs 0))
    (while (and (not (bobp))
                (l0-backward-sexp)
                (or (not (or (looking-at word)
                             (looking-at "fun")))
                    (> missing-ifs 0)))
      (cond ((looking-at "if")
             (setq missing-ifs (max 0 (1- missing-ifs))))
            ((looking-at "else")
             (incf missing-ifs))))
    (if (looking-at word)
        t
      (goto-char old)
      nil)))

(defun l0-backward-to ()
  (ignore-errors
    (backward-sexp 1)
    (while (not (or (bopb)))
      (backward-sexp 1))
    t))

(defvar l0-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Define the // line comment syntax.
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table used in `l0-mode'.")

(define-derived-mode l0-mode fundamental-mode "L0"
  "Major mode for editing L0 source files."
  :syntax-table l0-mode-syntax-table
  (set (make-local-variable 'font-lock-defaults) '(l0-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'l0-indent-line)
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-padding) " "))

(provide 'l0-mode)

;;; l0-mode.el ends here
