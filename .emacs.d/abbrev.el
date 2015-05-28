(defun backward-char-3 ()
  (interactive)
  (backward-char 3)
  )

(define-abbrev-table 'pike-mode-abbrev-table '(
    ("while" "while" c-electric-continued-statement 0)
    ("else" "else" c-electric-continued-statement 0)
    ))

(define-abbrev-table 'idl-mode-abbrev-table '(
    ))

(define-abbrev-table 'objc-mode-abbrev-table '(
    ("while" "while" c-electric-continued-statement 0)
    ("else" "else" c-electric-continued-statement 0)
    ))

(define-abbrev-table 'c++-mode-abbrev-table '(
    ("while" "while" c-electric-continued-statement 0)
    ("else" "else" c-electric-continued-statement 0)
    ("catch" "catch" c-electric-continued-statement 0)
    ))

(define-abbrev-table 'c-mode-abbrev-table '(
    ("while" "while" c-electric-continued-statement 0)
    ("else" "else" c-electric-continued-statement 0)
    ))

(define-abbrev-table 'java-mode-abbrev-table '(
    ("while" "while" c-electric-continued-statement 0)
    ("finally" "finally" c-electric-continued-statement 0)
    ("else" "else" c-electric-continued-statement 0)
    ("catch" "catch" c-electric-continued-statement 0)
    ("pu" "public" nil 0)
    ("pr" "private" nil 0)
    ("pe" "protected" nil 0)
    ("st" "static" nil 0)
    ("fi" "final" nil 0)
    ("ex" "extends" nil 0)
    ("im" "implements" nil 0)
    ("cl" "class" nil 0)
    ("sout" "System.out.println(\"\");" backward-char-3 0)
    ("serr" "System.err.println(\"\");" backward-char-3 0)
    ("St" "String" nil 0)
    ))

(define-abbrev-table 'occur-mode-abbrev-table '(
    ))

(define-abbrev-table 'text-mode-abbrev-table '(
    ))

(define-abbrev-table 'lisp-interaction-mode-abbrev-table '(
    ))

(define-abbrev-table 'emacs-lisp-mode-abbrev-table '(
    ))

(define-abbrev-table 'lisp-mode-abbrev-table '(
    ))

(define-abbrev-table 'fundamental-mode-abbrev-table '(
    ))

(define-abbrev-table 'global-abbrev-table '(
    ))

