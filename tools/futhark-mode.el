;;; futhark-mode.el --- major mode for editing Futhark source files

;; Copyright (C) DIKU 2013-2016, University of Copenhagen
;;   Written by Troels Henriksen (athas@sigkill.dk) in 2013.
;;   Improved by Niels G. W. Serup (ngws@metanohi.name) in 2014.
;;   Improved by Rasmus Wriedt Larsen in 2015.
;;   Improved by Niels G. W. Serup again in 2016.

;;; Commentary:
;; This mode provides syntax highlighting and automatic indentation for
;; Futhark source files.  There is sadly yet no automatic recompilation
;; or interactive environment, mostly because there is no good futharki
;; yet.
;;
;; This mode provides the following features for Futhark source files:
;;
;;   + syntax highlighting
;;   + automatic indentation
;;   + experimental flycheck support (currently disabled)
;;
;; To load futhark-mode automatically on Emacs startup, put this file in
;; your load path and require the mode, e.g. something like this:
;;
;;   (add-to-list 'load-path "~/.emacs.d/futhark-mode")
;;   (require 'futhark-mode)
;;
;; In this case, you have to create the directory
;; "~/.emacs.d/futhark-mode" and store this file in that directory.
;;
;; This will also tell your Emacs that ".fut" files are to be handled by
;; futhark-mode.
;;
;; Define your local keybindings in `futhark-mode-map', and add startup
;; functions to `futhark-mode-hook'.


;;; Code:

;;;; Basics.

(require 'cl) ; incf, some

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.fut\\'" . futhark-mode))

(defvar futhark-mode-hook nil
  "Hook for futhark-mode.  Is run whenever the mode is entered.")

(defvar futhark-mode-map
  (make-keymap)
  "Keymap for futhark-mode.")

(defconst futhark-keywords
  '("if" "then" "else" "let" "loop" "in" "with" "type"
    "fun" "entry" "fn" "for" "while" "do" "do" "op" "not"
    "empty" "unsafe" "default" "include")
  "All Futhark keywords.")

(defconst futhark-builtin-functions
  '("pow" "iota" "shape" "replicate" "reshape" "rotate" "transpose" "map"
    "reduce" "reduceComm" "zip" "unzip" "zipWith" "scan" "split"
    "concat" "filter" "partition" "redomap" "empty" "copy" "size"
    "write")
  "All Futhark builtin SOACs, functions, and non-symbolic operators.")

(defconst futhark-builtin-operators
  '("+" "*" "-" "/" "%" "//" "%%" "==" "!=" "<" "<=" "**" "^" "&"
    "|" ">>" "<<" ">>>")
  "All Futhark builtin symbolic operators.")

(defconst futhark-builtin-types
  '("i8" "i16" "i32" "i64"
    "u8" "u16" "u32" "u64"
    "f32" "f64"
    "int" "real" "bool")
  "A list of Futhark types.")

(defconst futhark-booleans
  '("True" "False")
  "All Futhark booleans.")

(defconst futhark-var
  "[_'[:alnum:]]+"
  "A regex describing a Futhark variable.")

(defconst futhark-type
  (concat "\\*?" "\\(?:"
          "\\(?:"

          "\\(?:" "\\[" "\\(?:"
          ""
          "\\|"
          futhark-var
          "\\)" "\\]" "\\)*" futhark-var

          "\\)" "\\|" "\\(?:"

          "([^)]+)"

          "\\)"
          "\\)")
;  (concat "\\(?:\\(?:\\[[^]]+\\]\\)\\|\\(?:([^)]+)\\)\\|" futhark-var "\\)")
  "A regex describing a Futhark type, built-in or user-specified.
Does not recognise nested tuples or nested arrays.")


;;; Highlighting

(defvar futhark-font-lock
  `(

    ;; Function declarations
    (,(concat "\\(?:fun\\|entry\\)[[:space:]\n]+" futhark-type
              "[[:space:]\n]+\\(" futhark-var "\\)")
     . '(1 font-lock-function-name-face))

    ;; Variable and tuple declarations
    ;;; Lets
    (,(concat "let[[:space:]\n]+\\(" futhark-var "\\)")
     . '(1 font-lock-variable-name-face))
    (,(concat "let[[:space:]\n]+(\\([^)]+\\)")
     . '(1 font-lock-variable-name-face))
    ;;; Function parameters
    (,(concat "[(,][[:space:]\n]*" futhark-type "[[:space:]]+\\("
              futhark-var "\\)")
     . '(1 font-lock-variable-name-face))

    ;; Keywords
    (,(regexp-opt futhark-keywords 'words)
     . font-lock-keyword-face)

    ;; Types
    ;;; Type aliases
    (,(concat "type[[:space:]]+\\(" futhark-type "\\)")
     . '(1 font-lock-type-face))
    (,(concat "type[[:space:]]+" futhark-type "[[:space:]]*=[[:space:]]*"
              "\\(" futhark-type "\\)")
     . '(1 font-lock-type-face))
    ;;; Function return type
    (,(concat "fu?n[[:space:]]+\\(" futhark-type "\\)")
     . '(1 font-lock-type-face))
    ;;; Function parameters types
    (,(concat "[(,][[:space:]\n]*\\(" futhark-type "\\)[[:space:]]+"
              futhark-var)
     . '(1 font-lock-type-face))
    ;;; Builtins
    (,(regexp-opt futhark-builtin-types 'words)
     . font-lock-type-face)

    ;; Builtins
    ;;; Functions
    (,(regexp-opt futhark-builtin-functions 'words)
     . font-lock-builtin-face)
    ;;; Operators
    (,(regexp-opt futhark-builtin-operators)
     . font-lock-builtin-face)

    ;; Constants
    ;;; Booleans
    (,(regexp-opt futhark-booleans 'words)
     . font-lock-constant-face)

    )
  "Highlighting expressions for Futhark.")

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
    st)
  "Syntax table used in `futhark-mode'.")


;;; Indentation

(defvar futhark-indent-level 2
  "The indent level for futhark-mode.")

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
In general, prefer as little indentation as possible, and make block
constituents match each other's indentation."
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

       ;; Align function definitions to column 0.
       (and (or (futhark-looking-at-word "fun")
                (futhark-looking-at-word "entry"))
            0)

       ;; Align closing parentheses and commas to opening
       ;; parenthesis.
       (save-excursion
         (and (looking-at (regexp-opt '(")" "]" ",")))
              (ignore-errors
                (backward-up-list 1)
                (current-column))))

       ;; Align "in" to nearest "let" or "loop".
       (save-excursion
         (and (futhark-looking-at-word "in")
              (futhark-find-closest-of-keywords-backward '("let" "loop"))
              (current-column)))

       ;; Align "then" to nearest "if" or "else if".
       (save-excursion
         (and (futhark-looking-at-word "then")
              (futhark-find-keyword-backward "if")
              (or
               (let ((curline (line-number-at-pos)))
                 (save-excursion
                   (and (futhark-backward-part)
                        (= (line-number-at-pos) curline)
                        (futhark-looking-at-word "else")
                        (current-column))))
               (current-column))))

       ;; Align "else" to nearest "then" or "if ... then" or "else if
       ;; ... then"
       (save-excursion
         (and (futhark-looking-at-word "else")
              (futhark-find-keyword-backward "then")
              (or
               (let ((curline (line-number-at-pos)))
                 (save-excursion
                   (and (futhark-find-keyword-backward "if")
                        (= (line-number-at-pos) curline)
                        (or (save-excursion
                              (and (futhark-backward-part)
                                   (= (line-number-at-pos) curline)
                                   (futhark-looking-at-word "else")
                                   (current-column)))
                            (current-column)))))
               (current-column))))

       ;; Align "=" to nearest "let" or "loop".
       (save-excursion
         (and (looking-at "=[^=]")
              (futhark-find-closest-of-keywords-backward '("let" "loop"))
              (current-column)))

       ;; Otherwise, if the previous code line ends with "then" or
       ;; "else", align to the starting column plus one indent level.
       (save-excursion
         (and (futhark-backward-part)
              (or (looking-at "\\<then[[:space:]]*$")
                  (looking-at "\\<else[[:space:]]*$"))
              (progn (futhark-beginning-of-line-text) t)
              (+ (current-column) futhark-indent-level)))

       ;; Otherwise, if the previous keyword is "loop", align to the
       ;; matching column plus one indent level.
       (save-excursion
         (and (futhark-first-keyword-backward)
              (futhark-looking-at-word "loop")
              (+ (current-column) futhark-indent-level)))

       ;; Otherwise, if the previous code line ends with "do", align to
       ;; the matching "while" or "for" or "loop" column (whatever is
       ;; first on the line) plus one indent level.
       (save-excursion
         (and (futhark-backward-part)
              (looking-at "\\<do[[:space:]]*$")
              (or (and (futhark-find-closest-of-keywords-backward '("for" "while"))
                       (futhark-is-beginning-of-line-text)
                       (+ (current-column) futhark-indent-level))
                  (and (futhark-find-keyword-backward "loop")
                       (futhark-is-beginning-of-line-text)
                       (+ (current-column) futhark-indent-level)))))

       ;; Otherwise, if the previous code line ends with "=", align to
       ;; the matching "let" or "loop" column plus one indent level.
       (save-excursion
         (and (futhark-backward-part)
              (looking-at "=[[:space:]]*$")
              (futhark-find-closest-of-keywords-backward '("let" "loop"))
              (+ (current-column) futhark-indent-level)))

       ;; Otherwise, if the previous code line ends with "in" align to
       ;; the matching "let" or "loop" column.
       (save-excursion
         (and (futhark-backward-part)
              (looking-at "\\<in[[:space:]]*$")
              (futhark-find-closest-of-keywords-backward '("let" "loop"))
              (current-column)))

       ;; Otherwise, if the previous code line ends with "=>", align to
       ;; the matching "fn" column plus one indent level.
       (save-excursion
         (and (futhark-backward-part)
              (looking-at "=>[[:space:]]*$")
              (futhark-find-keyword-backward "fn")
              (+ (current-column) futhark-indent-level)))

       ;; Otherwise, if the line starts with "let" or "loop", align to a
       ;; previous "let" or "loop".
       (save-excursion
         (and (or (looking-at "let")
                  (looking-at "loop"))
              (futhark-find-closest-of-keywords-backward '("let" "loop"))
              (current-column)))

       ;; Otherwise, if the line starts with "let" or "loop", and the above rule
       ;; did not result in anything, align to a previous "unsafe".
       (save-excursion
         (and (or (looking-at "let")
                  (looking-at "loop"))
              (futhark-find-keyword-backward "unsafe")
              (current-column)))

       ;; Otherwise, if inside a parenthetical structure, align to its
       ;; start element if present, otherwise the parenthesis + 1.
       (save-excursion
         (and (ignore-errors (backward-up-list 1) t)
              (ignore-errors (forward-char) t)
              (let ((linum (line-number-at-pos)))
                (or (save-excursion (and (ignore-errors (forward-sexp) t)
                                         (= (line-number-at-pos) linum)
                                         (ignore-errors (backward-sexp) t)
                                         (current-column)))
                    (current-column)))))

       ;; Otherwise, if the previous keyword is "fun", align to a single
       ;; indent level.
       (and
        (let ((first-keyword (save-excursion (futhark-first-keyword-backward))))
          (or (string= "fun" first-keyword)
              (string= "entry" first-keyword)))
        futhark-indent-level)

       ;; Otherwise, align to the previous non-empty line.
       (save-excursion
         (and (progn
                (futhark-back-actual-line)
                (futhark-beginning-of-line-text)
                t)
              (current-column)))

       ))))

(defun futhark-beginning-of-line-text ()
  "Move to the beginning of the text on this line.
Contrary to `beginning-of-line-text', consider any non-whitespace
character to be text."
  (beginning-of-line)
  (while (looking-at " ")
    (forward-char)))

(defun futhark-is-beginning-of-line-text ()
  "Check if point is at the first word on a line."
  (=
   (point)
   (save-excursion
     (futhark-beginning-of-line-text)
     (point))))

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
  (while (and (not (bobp))
              (forward-line -1)
              (progn (beginning-of-line)
                     (setq bound (point))
                     (end-of-line)
                     t)
              (ignore-errors
                (re-search-backward "^[[:space:]]*$" bound)))))

(defun futhark-find-closest-of-keywords-backward (words)
  "Find the closest of keywords WORDS before the current position.
Set mark and return t if found; return nil otherwise."
  (setq ps (mapcar
            (lambda (word)
              (save-excursion
                (or (and (futhark-find-keyword-backward word)
                         (point))
                    -1)))
            words))
  (setq ps-sorted (sort ps (lambda (a b) (< a b))))
  (setq p-closest (car (last ps-sorted)))
  (or (and (not (= -1 p-closest))
           (goto-char p-closest)
           t)
      nil))

(defun futhark-find-keyword-backward (word)
  "Find a keyword WORD before the current position.
Set mark and return t if found; return nil otherwise."
  (let ((pstart (point))
        ;; We need to count "if"s, "then"s, and "else"s to properly
        ;; indent.
        (if-else-count 0)
        (then-else-count 0)
        ;; The same with "let", "loop", and "in".
        (let-in-count 0)
        (just-had-let nil)
        ;; Only look in the current paren-delimited code.
        (topp (save-excursion (or (ignore-errors
                                    (backward-up-list 1)
                                    (point))
                                  (max
                                   (or (save-excursion
                                         (futhark-find-keyword-backward-raw "fun"))
                                       0)
                                   (or (save-excursion
                                         (futhark-find-keyword-backward-raw "entry"))
                                       0))
                                  )))
        (result nil)
        )

    (cond ((futhark-looking-at-word "else")
           (incf if-else-count)
           (incf then-else-count))
          ((futhark-looking-at-word "in")
           (incf let-in-count))
          )

    (while (and (not result)
                (futhark-backward-part)
                (>= (point) topp))

      (cond ((futhark-looking-at-word "if")
             (setq if-else-count (max 0 (1- if-else-count))))
            ((futhark-looking-at-word "then")
             (setq then-else-count (max 0 (1- then-else-count))))
            ((futhark-looking-at-word "else")
             (incf if-else-count)
             (incf then-else-count))
            ((and (or (futhark-looking-at-word "let")
                      (futhark-looking-at-word "loop"))
                  (not just-had-let))
             (setq just-had-let t)
             (setq let-in-count (max 0 (1- let-in-count))))
            ((futhark-looking-at-word "in")
             (setq just-had-let nil)
             (incf let-in-count))
            ((futhark-looking-at-word "do")
             (setq just-had-let nil))
            )

      (when (and (futhark-looking-at-word word)
                 (or (and (string= "if" word)
                          (= 0 let-in-count)
                          (= 0 if-else-count))
                     (and (string= "then" word)
                          (= 0 let-in-count)
                          (= 0 then-else-count))
                     (and (string= "else" word)
                          (= 0 let-in-count))
                     (and (or (string= "let" word)
                              (string= "loop" word))
                          (= 0 let-in-count))
                     (and
                      (not (string= "if" word))
                      (not (string= "then" word))
                      (not (string= "else" word))
                      (not (string= "let" word))
                      (not (string= "loop" word)))
                     ))
        (setq result (point))
        ))

    (if result
        result
      (goto-char pstart)
      nil)
    ))

(defun futhark-find-keyword-backward-raw (word)
  "Find a keyword WORD before the current position.
Ignore any program structure."
  (let ((pstart (point)))
    (while (and (futhark-backward-part)
                (not (futhark-looking-at-word word))))
    (if (futhark-looking-at-word word)
        (point)
      (goto-char pstart)
      nil)))

(defun futhark-first-keyword-backward ()
  "Going backwards, find the first Futhark keyword."
  (while (and (futhark-backward-part)
              (not (some 'futhark-looking-at-word futhark-keywords))))

  (some (lambda (kwd)
          (and (futhark-looking-at-word kwd)
               kwd))
        futhark-keywords))


;;; flycheck

;; TODO: This doesn't really work well, probably because of the compiler.

;; (require 'flycheck nil t) ;; no error if not found
;; (when (featurep 'flycheck)
;;   (flycheck-define-checker futhark
;;     "A Futhark syntax and type checker.
;; See URL `https://github.com/HIPERFIT/futhark'."
;;     :command ("futhark" source)
;;     :modes 'futhark-mode
;;     :error-patterns
;;     ((error line-start (message) "at " (file-name) ":" line ":" column "-")
;;      (error line-start "No extra lines")))
;;   (add-to-list 'flycheck-checkers 'futhark))


;;; The silly section

(defvar futhark-danger-zone-path nil
  "A path to a sound file to be played when writing the `unsafe' keyword.
If nil, no sound will be played.")
;; For example, you can enter this in your Emacs init file:
;;
;;    (setq futhark-danger-zone-path "/path/to/danger-zone.wav")
;;
;; You may have to restart your Emacs.

(defun futhark-check-unsafe (begin end length)
  "Play a sound if the user has just written the `unsafe' keyword.
Ignore BEGIN, END, and LENGTH (present to satisfy Emacs)."
  (if (and
       (string= major-mode "futhark-mode")
       futhark-danger-zone-path)
      (save-excursion
        (ignore-errors (backward-sexp 1) t)
        (if (looking-at "\\<unsafe\\>")
            (futhark-play-sound-file-in-background
             futhark-danger-zone-path)))))

(defun futhark-play-sound-file-in-background (path)
  "Play the sound in PATH in the background."
  ;; It would be nice to just use `play-sound-file', but that function
  ;; blocks.
  (start-process "futhark-sound" nil "mplayer" path))


;;; Actual mode declaration

(define-derived-mode futhark-mode fundamental-mode "Futhark"
  "Major mode for editing Futhark source files."
  :syntax-table futhark-mode-syntax-table
  (set (make-local-variable 'font-lock-defaults) '(futhark-font-lock))
  (set (make-local-variable 'indent-line-function) 'futhark-indent-line)
  (set (make-local-variable 'indent-region-function) nil)
  (set (make-local-variable 'comment-start) "--")
  (set (make-local-variable 'comment-padding) " ")
  (add-hook 'after-change-functions 'futhark-check-unsafe nil))

(provide 'futhark-mode)

;;; futhark-mode.el ends here
