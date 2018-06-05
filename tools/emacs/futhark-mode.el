;;; futhark-mode.el --- major mode for editing Futhark source files

;; Copyright (C) DIKU 2013-2017, University of Copenhagen
;;
;; URL: https://github.com/diku-dk/futhark
;; Keywords: languages
;; Version: 0.1
;; Package-Requires: ((cl-lib "0.5"))

;; This file is not part of GNU Emacs.

;;; License:
;; ICS <https://github.com/diku-dk/futhark/blob/master/LICENSE>

;;; Commentary:
;; Futhark is a small programming language designed to be compiled to
;; efficient GPU code.  This Emacs mode provides syntax highlighting
;; and conservative automatic indentation for Futhark source code.  A
;; simple flycheck definition is also included.
;;
;; Files with the ".fut" extension are automatically handled by this mode.
;;
;; For extensions: Define local keybindings in `futhark-mode-map'.  Add startup
;; functions to `futhark-mode-hook'.

;;; Code:

(require 'cl-lib)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.fut\\'" . futhark-mode))

(defvar futhark-mode-hook nil
  "Hook for `futhark-mode'.  Is run whenever the mode is entered.")

(defvar futhark-mode-map
  (make-keymap)
  "Keymap for `futhark-mode'.")


;;; Highlighting

(let (
      (ws "[[:space:]\n]*")
      (ws1 "[[:space:]\n]+")
      )

  ;; FIXME: Backslash should also be a keyword (for anonymous functions), but
  ;; Emacs Lisp is stupid.
  (defconst futhark-keywords
    '("if" "then" "else" "let" "loop" "in" "with" "type"
      "val" "entry" "for" "while" "do"
      "empty" "unsafe" "default" "include" "import" "module" "open" "local" "assert")
    "All Futhark keywords.")

  (defconst futhark-builtin-functions
    '("zip" "unzip" "map" "reduce"
      "reduce_comm" "scan" "filter" "partition" "scatter" "stream_map"
      "stream_map_per" "stream_red" "stream_map_per" "stream_seq")
    "All Futhark builtin SOACs, functions, and non-symbolic operators.")

  (defconst futhark-numeric-types
    '("i8" "i16" "i32" "i64"
      "u8" "u16" "u32" "u64"
      "f32" "f64")
    "A list of Futhark numeric types.")

  (defconst futhark-builtin-types
    (cons "bool" futhark-numeric-types)
    "A list of Futhark types.")

  (defconst futhark-booleans
    '("true" "false")
    "All Futhark booleans.")

  (defconst futhark-number
    (concat "-?"
            "\\<\\(?:"
            (concat "\\(?:"
                    "\\(?:0[xX]\\)"
                    "[0-9a-fA-F]+"
                    "\\(?:\\.[0-9a-fA-F]+\\)?"
                    "\\(?:[pP][+-]?[0-9]+\\)?"
                    "\\|"
                    "[0-9]+"
                    "\\(?:\\.[0-9]+\\)?"
                    "\\)"
                    )
            "\\(?:i8\\|i16\\|i32\\|i64\\|u8\\|u16\\|u32\\|u64\\|f32\\|f64\\)?"
            "\\)\\>")
    "All numeric constants, including hex float literals.")

  (defconst futhark-var
    (concat "\\(?:" "[_'[:alnum:]]+" "\\)")
    "A regex describing a Futhark variable.")

  (defconst futhark-operator
    (concat "["
            "-+*/%!<>=&|@"
            "]" "+"))

  (defconst futhark-non-tuple-type
    (concat "\\(?:"
            "\\*" "?"
            "\\(?:"
            "\\["
            "\\(?:"
            ""
            "\\|"
            futhark-var
            "\\)"
            "\\]"
            "\\)" "*"
            futhark-var
            "\\)"
            )
    "A regex describing a Futhark type which is not a tuple")

  ;; This does not work with nested tuple types.
  (defconst futhark-tuple-type
    (concat "\\(?:"
            "("
            "\\(?:" ws futhark-non-tuple-type ws "," "\\)" "*"
            ws futhark-non-tuple-type ws
            ")"
            "\\)"
            )
    "A regex describing a Futhark type which is a tuple")

  (defconst futhark-type
    (concat "\\(?:"
            futhark-non-tuple-type
            "\\|"
            futhark-tuple-type
            "\\)"
            )
    "A regex describing a Futhark type")


  (defvar futhark-font-lock
    `(

      ;; Variable and tuple declarations.
      ;;; Lets.
      ;;;; Primitive values.
      (,(concat "let" ws1
                "\\(" futhark-var "\\)")
       . '(1 font-lock-variable-name-face))
      ;;;; Tuples.  FIXME: It would be nice to highlight only the variable names
      ;;;; inside the parantheses, and not also the commas.
      (,(concat "let" ws1 "("
                "\\(" "[^)]+" "\\)")
       . '(1 font-lock-variable-name-face))
      ;;; Function parameters.
      (,(concat "\\(" futhark-var "\\)" ws ":")
       . '(1 font-lock-variable-name-face))

      ;; Keywords.
      (,(regexp-opt futhark-keywords 'words)
       . font-lock-keyword-face)

      ;; Types.
      ;;; Type aliases.  FIXME: It would be nice to highlight also the right
      ;;; hand side.
      (,(concat "type" ws1 "\\(" futhark-type "\\)")
       . '(1 font-lock-type-face))
      ;;; Function parameters types and return type.
      (,(concat ":" ws "\\(" "[^=,)]+" "\\)")
       . '(1 font-lock-type-face))
      ;;; Builtin types.
      (,(regexp-opt futhark-builtin-types 'words)
       . font-lock-type-face)

      ;; Builtins.
      ;;; Functions.
      ;;;; Builtin functions.
      (,(regexp-opt futhark-builtin-functions 'words)
       . font-lock-builtin-face)
      ;;; Operators.
      (,futhark-operator
       . font-lock-builtin-face)

      ;; Constants.
      ;;; Booleans.
      (,(regexp-opt futhark-booleans 'words)
       . font-lock-constant-face)

      (,(concat "\\(" futhark-number "\\)")
       . font-lock-constant-face)

      )
    "Highlighting expressions for Futhark.")
  )

(defvar futhark-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Define the -- line comment syntax.
    (modify-syntax-entry ?- ". 123" st)
    (modify-syntax-entry ?\n ">" st)
    ;; Make apostrophe and underscore be part of variable names.
    ;; Technically, they should probably be part of the symbol class,
    ;; but it works out better for some of the regexpes if they are part
    ;; of the word class.
    (modify-syntax-entry ?' "w" st)
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?\\ "_" st)
    st)
  "Syntax table used in `futhark-mode'.")


;;; Indentation

(defvar futhark-indent-level 2
  "The basic indent level for `futhark-mode'.")

(defun futhark-indent-line ()
  "Indent current line as Futhark code."
  (let ((savep (> (current-column) (current-indentation)))
        (indent (or (futhark-calculate-indentation)
                    (current-indentation))))
    (if savep ; The cursor is beyond leading whitespace.
        (save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun futhark-calculate-indentation ()
  "Calculate the indentation for the current line.
In general, prefer as little indentation as possible."
  (let ((parse-sexp-lookup-properties t)
        (parse-sexp-ignore-comments t))

    (save-excursion
      (futhark-beginning-of-line-text)

      ;; The following code is fickle and deceptive.  Don't change it
      ;; unless you kind of know what you're doing!
      (or

       ;; Align comment to next non-comment line.
       (and (looking-at comment-start)
            (forward-comment (count-lines (point-min) (point)))
            (current-column))

       ;; Align global definitions and headers to nearest module definition or
       ;; column 0.
       (and (or (futhark-looking-at-word "entry")
                (futhark-looking-at-word "type")
                (futhark-looking-at-word "val")
                (futhark-looking-at-word "module")
                (futhark-looking-at-word "include")
                (futhark-looking-at-word "import"))
            (or
             (save-excursion
               (and
                (ignore-errors (backward-up-list 1) t)
                (looking-at "{")
                (or
                 (futhark-keyword-backward "module")
                 (futhark-keyword-backward "open")
                 (and
                  (ignore-errors (backward-up-list 1) t)
                  (or
                   (futhark-keyword-backward "module")
                   (futhark-keyword-backward "open"))))
                (+ futhark-indent-level (current-column))))
             0))

       ;; Align closing parentheses and commas to the matching opening
       ;; parenthesis.
       (save-excursion
         (and (looking-at (regexp-opt '(")" "]" ",")))
              (ignore-errors
                (backward-up-list 1)
                (current-column))))

       ;; Align closing curly brackets to the matching opening 'module'
       ;; keyword.
       (save-excursion
         (and (looking-at "}")
              (ignore-errors
                (backward-up-list 1)
                (or
                 (save-excursion
                   (ignore-errors
                     (and
                      (backward-up-list 1)
                      (looking-at "(")
                      (futhark-keyword-backward "module")
                      (current-column))))
                 (and
                  (futhark-keyword-backward "module")
                  (current-column))))))

       ;; If the previous code line ends with "=", align to the matching "let"
       ;; or "loop" column plus one indent level.
       (save-excursion
         (and (futhark-backward-part)
              (looking-at "=[[:space:]]*$")
              (let ((m
                     (futhark-max
                      (save-excursion
                        (futhark-keyword-backward "let"))
                      (save-excursion
                        (futhark-keyword-backward "loop")))))
                (and (not (eq nil m))
                     (goto-char m)
                     (+ (current-column) futhark-indent-level)))))

       ;; Don't align "let" if the previous line is blank (be conservative!).
       (save-excursion
         (and (futhark-looking-at-word "let")
              (let ((cur (current-column)))
                (save-excursion
                  (forward-line -1)
                  (and
                   (futhark-is-empty-line)
                   cur)))))

       ;; Align "in", "let", or "loop" to the closest previous "let" or "loop".
       (save-excursion
         (and (or (futhark-looking-at-word "in")
                  (futhark-looking-at-word "let")
                  (futhark-looking-at-word "loop"))
              (let ((m
                     (futhark-max
                      (save-excursion
                        (futhark-keyword-backward "let"))
                      (save-excursion
                        (futhark-keyword-backward "loop")))))
                (and (not (eq nil m))
                     (goto-char m)
                     (current-column)))))

       ;; Otherwise, if the previous code line ends with "in" align to
       ;; the matching "let" or "loop" column.
       (save-excursion
         (and (futhark-backward-part)
              (looking-at "\\<in[[:space:]]*$")
              (let ((m
                     (futhark-max
                      (save-excursion
                        (futhark-keyword-backward "let"))
                      (save-excursion
                        (futhark-keyword-backward "loop")))))
                (and (not (eq nil m))
                     (goto-char m)
                     (current-column)))))

       ;; Align "then" to nearest "else if" or "if".
       (save-excursion
         (and (futhark-looking-at-word "then")
              (futhark-keyword-backward "if")
              (or
               (let ((curline (line-number-at-pos)))
                 (save-excursion
                   (and (futhark-backward-part)
                        (= (line-number-at-pos) curline)
                        (futhark-looking-at-word "else")
                        (current-column))))
               (current-column))))

       ;; Align "else" to nearest "then" or "else if" or "if".
       (save-excursion
         (and (futhark-looking-at-word "else")
              (let ((m
                     (futhark-max
                      (save-excursion
                        (and
                         (futhark-keyword-backward "then")
                         (futhark-is-beginning-of-line-text)
                         (point)))
                      (save-excursion
                        (let ((pos0 (futhark-keyword-backward "if")))
                          (or
                           (let ((curline (line-number-at-pos)))
                             (and (futhark-backward-part)
                                  (= (line-number-at-pos) curline)
                                  (futhark-looking-at-word "else")
                                  (point)))
                           pos0))))))
                (and (not (eq nil m))
                     (goto-char m)
                     (current-column)))))

       ;; Align general content inside parentheses to the first general
       ;; non-space content.
       (save-excursion
         (when (ignore-errors (backward-up-list 1) t)
              (forward-char 1)
              (futhark-goto-first-text)
              (and
               (not (futhark-is-looking-at-keyword))
               (current-column))))

       ;; Otherwise, keep the user-specified indentation level.
       ))))

(defun futhark-min (a b)
  "Like `min', but also accepts nil values in A and B."
  (or (and (eq nil a) b)
      (and (eq nil b) a)
      (and (not (eq nil a))
           (not (eq nil b))
           (min a b))))

(defun futhark-max (a b)
  "Like `max', but also accepts nil values in A and B."
  (or (and (eq nil a) b)
      (and (eq nil b) a)
      (and (not (eq nil a))
           (not (eq nil b))
           (max a b))))

(defun futhark-beginning-of-line-text ()
  "Move to the beginning of the non-whitespace text on this line."
  (beginning-of-line)
  (futhark-goto-first-text))

(defun futhark-goto-first-text ()
  "Skip over whitespace."
  (while (looking-at "[[:space:]\n]")
    (forward-char)))

(defun futhark-is-beginning-of-line-text ()
  "Check if point is at the first word on a line."
  (=
   (point)
   (save-excursion
     (futhark-beginning-of-line-text)
     (point))))

(defun futhark-is-empty-line ()
  "Check if the line of the current point is empty.
It is considered empty if the line consists of zero or more
whitespace characters."
  (let ((cur (line-number-at-pos)))
    (futhark-beginning-of-line-text)
    (not (= cur (line-number-at-pos)))))

(defun futhark-is-looking-at-keyword ()
  "Check if we are currently looking at a keyword."
  (cl-some 'futhark-looking-at-word futhark-keywords))

(defun futhark-backward-part ()
  "Try to jump back one sexp.
The net effect seems to be that it works ok."
  (and (not (bobp))
       (ignore-errors (backward-sexp 1) t)))

(defun futhark-looking-at-word (word)
  "Do the same as `looking-at', but also check for blanks around WORD."
  (looking-at (concat "\\<" word "\\>")))

(defun futhark-back-actual-line ()
  "Go back to the first non-empty line, or return nil trying."
  (let (bound)
    (while (and (not (bobp))
                (forward-line -1)
                (progn (beginning-of-line)
                       (setq bound (point))
                       (end-of-line)
                       t)
                (ignore-errors
                  (re-search-backward "^[[:space:]]*$" bound))))))

(defun futhark-keyword-backward (word)
  "Go to a keyword WORD before the current position.
Set mark and return t if found; return nil otherwise."
  ;; FIXME: Support nested let-chains.  This used to work, but was removed
  ;; because the code was too messy.
  (let (;; Only look in the current paren-delimited code if present.
        (startp (point))
        (topp (or (save-excursion (ignore-errors
                                    (backward-up-list 1)
                                    (point)))
                  (max
                   (or (save-excursion (futhark-keyword-backward-raw "fun"))
                       0)
                   (or (save-excursion (futhark-keyword-backward-raw "entry"))
                       0))))
        (result nil))

    (while (and (not result)
                (futhark-backward-part)
                (>= (point) topp))

      (if (futhark-looking-at-word word)
          (setq result (point))))

    (or result
        (progn
          (goto-char startp)
          nil))))

(defun futhark-keyword-backward-raw (word)
  "Go to a keyword WORD before the current position.
Ignore any program structure."
  (let ((pstart (point)))
    (while (and (futhark-backward-part)
                (not (futhark-looking-at-word word))))
    (and (futhark-looking-at-word word)
         (point))))


;;; flycheck

(require 'flycheck nil t) ;; no error if not found
(when (featurep 'flycheck)
  (flycheck-define-checker futhark
    "A Futhark syntax and type checker.
See URL `https://github.com/diku-dk/futhark'."
    :command ("futhark" "-t" source-inplace)
    :modes 'futhark-mode
    :error-patterns
    ((error line-start "Error at " (file-name) ":" line ":" column "-"
            (one-or-more not-newline) ":" (message (one-or-more anything))
            "If you find")
     (error (message "lexical error") " at line " line ", column " column)
     (warning line-start "Warning at " (file-name) ":"
              line ":" column "-" (one-or-more digit) ":" (one-or-more digit) ":" ?\n
              (message (one-or-more (and (one-or-more (not (any ?\n))) ?\n)))
              line-end)))
  (add-to-list 'flycheck-checkers 'futhark))

;;; Actual mode declaration

;;;###autoload
(define-derived-mode futhark-mode fundamental-mode "Futhark"
  "Major mode for editing Futhark source files."
  :syntax-table futhark-mode-syntax-table
  (setq-local font-lock-defaults '(futhark-font-lock))
  (setq-local indent-line-function 'futhark-indent-line)
  (setq-local indent-region-function nil)
  (setq-local comment-start "--")
  (setq-local comment-start-skip "--[ \t]*")
  (setq-local paragraph-start (concat " *-- |\\| ==$\\|" page-delimiter))
  (setq-local paragraph-separate (concat " *-- ==$\\|" page-delimiter))
  (setq-local comment-padding " "))

(provide 'futhark-mode)

;;; futhark-mode.el ends here
