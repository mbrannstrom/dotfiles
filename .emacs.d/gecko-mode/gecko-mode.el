;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   A major mode for editing Gecko schemas
;;
;;   Version: 1.0
;;   Date: 2012-12-03
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   Copyright (c) 2012, Pantor Engineering AB
;;   All rights reserved.
;;   
;;   Redistribution and use in source and binary forms, with or
;;   without modification, are permitted provided that the following
;;   conditions are met:
;;   
;;   * Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer. 
;;
;;   * Redistributions in binary form must reproduce the above
;;     copyright notice, this list of conditions and the following
;;     disclaimer in the documentation and/or other materials provided
;;     with the distribution.
;;
;;   * Neither the name of Pantor Engineering AB nor the names of its
;;     contributors may be used to endorse or promote products derived
;;     from this software without specific prior written permission.
;;   
;;   THIS   SOFTWARE   IS PROVIDED BY    THE   COPYRIGHT  HOLDERS  AND
;;   CONTRIBUTORS "AS IS"   AND ANY  EXPRESS OR  IMPLIED   WARRANTIES,
;;   INCLUDING,  BUT  NOT LIMITED  TO,   THE  IMPLIED  WARRANTIES   OF
;;   MERCHANTABILITY    AND  FITNESS  FOR   A  PARTICULAR  PURPOSE ARE
;;   DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS
;;   BE  LIABLE   FOR ANY    DIRECT, INDIRECT,   INCIDENTAL,  SPECIAL,
;;   EXEMPLARY, OR CONSEQUENTIAL DAMAGES  (INCLUDING, BUT NOT  LIMITED
;;   TO, PROCUREMENT  OF  SUBSTITUTE GOODS OR  SERVICES;  LOSS OF USE,
;;   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
;;   ANY THEORY OF  LIABILITY, WHETHER IN CONTRACT,  STRICT LIABILITY,
;;   OR  TORT (INCLUDING NEGLIGENCE OR  OTHERWISE) ARISING  IN ANY WAY
;;   OUT OF  THE  USE OF   THIS  SOFTWARE,  EVEN IF ADVISED   OF   THE
;;   POSSIBILITY OF SUCH DAMAGE.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Example setup for your ~/.emacs file:
;;
;;   (autoload 'gecko-mode "gecko-mode")
;;   (setq auto-mode-alist       
;;         (cons '("\\.gecko\\'" . gecko-mode) auto-mode-alist))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'font-lock)

(defvar gecko-indent-level 2 "The Gecko indentation level.")

(defvar gecko-decls
  (mapcar (lambda (kw) (concat "\\b\\(?:" kw "\\)\\b"))
	  '("namespace"))
  "Gecko special forms")

(defvar gecko-types
  (mapcar (lambda (kw) (concat "\\b%\\s *\\(?:" kw "\\)\\b"))
	  '("i8" "u8" "i16" "u16" "i32" "u32" "i64" "u64"  
	    "f64" "decimal" "date" "timeOfDayMilli" "timeOfDayMicro" "timeOfDayNano"
	    "millitime" "microtime" "nanotime" "bool" "string" "object"))
  "Gecko types")

(defvar gecko-keywords
  (mapcar (lambda (kw) (concat "\\b\\(?:" kw "\\)\\b"))
	  '("type"))
  "Gecko keywords")

(defun gecko-make-regexp-choice (operands)
  "(op1 op2 ...) -> \"\\(op1\\|op2\\|...\\)\""
  (let ((result "\\("))
    (mapcar (lambda (op) (setq result (concat result op "\\|"))) operands)
    (concat (substring result 0 -2) "\\)")))

(defconst gecko-light-blue-color "#9292C9")
(defconst gecko-dark-blue-color "#3A3A7B")
(defconst gecko-green-color "#257A25")
(defconst gecko-grey-color "#666666")

(defconst gecko-sky-blue-color "#ACACFC")
(defconst gecko-dark-green-color "#00AD00")
(defconst gecko-light-green-color "#70F170")

(defgroup gecko-highlighting-faces nil
  "Faces for Gecko syntax highlighting."
  :group 'font-lock-highlighting-faces)

(defface gecko-annot-face
  `((((class color) (background light)) 
     (:foreground ,gecko-green-color :italic t))
    (((class color) (background dark :italic t)) 
     (:foreground ,gecko-light-green-color)))
  "Face used to highlight annotations."
  :group 'gecko-highlighting-faces)

(defface gecko-type-face
  `((((class color) (background light)) (:foreground ,gecko-grey-color))
    (((class color) (background dark)) (:foreground ,gecko-sky-blue-color)))
  "Face used to highlight references."
  :group 'gecko-highlighting-faces)

;; Font lock treats face names differently in GNU Emacs and XEmacs
;; The following defvars is a workaround

(defvar italic 'italic)
(defvar default 'default)
(defvar font-lock-preprocessor-face 'font-lock-preprocessor-face)
(defvar gecko-annot-face 'gecko-annot-face)
(defvar gecko-type-face 'gecko-type-face)

(defvar gecko-font-lock-keywords
  (list
   (list 
    (concat (gecko-make-regexp-choice gecko-decls) "\\s *\\([^ \t\n]+\\)")
    '(1 font-lock-keyword-face) '(2 font-lock-constant-face))

   (list (gecko-make-regexp-choice gecko-keywords) 1 font-lock-keyword-face)

   '("\\(?:\\s +\\|,\\|=\\)\\([a-zA-Z_][a-zA-Z0-9_]*\\(?::[a-zA-Z_][a-zA-Z0-9_]*\\)?\\)\\(?:\\s *?\\*\\)?\\(?:\\s *\\[\\s *\\]\\)?\\(?:\\s \\|\n\\)+[a-zA-Z_][a-zA-Z0-9_]*"
     1 gecko-type-face)

   '("^[a-zA-Z_][a-zA-Z0-9_]*" 0 font-lock-function-name-face)

   '("^\\([a-zA-Z_][a-zA-Z0-9_:]*\\)\\(?:\\s \\|\n\\)*<-" 1 gecko-type-face t)

   '(":\\(?:\\s \\|\n\\)+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1 gecko-type-face)

   '("\\([a-zA-Z_][a-zA-Z0-9_]*\\):\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 
     1 font-lock-constant-face t)

   '("\\(string\\)\\s *\\(([^)]+)\\)"
     (1 gecko-type-face)
     (2 font-lock-string-face))
   
   '("\\(@[a-zA-Z_][a-zA-Z0-9_:]*\\)\\(?:\\s \\|\n\\)*\\(?:=\\(?:\\s \\|\n\\)*\\)?"
     1 gecko-annot-face t)

   '("|\\(?:\\s \\|\n\\)*\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
     1 font-lock-string-face t)
   '("\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\(?:\\s \\|\n\\)*|"
     1 font-lock-string-face t)
   )
  "Gecko Highlighting")

(defun gecko-find-column (first start is-incr-annot)
  "Find which column to indent to." 

  ;; FIXME: backward-sexp doesn't work with unbalanced braces in comments

  (let* (column
	 pos
	 ;; Find start of enclosing block or assignment
	 (token
	  (catch 'done
	    (while (setq pos (re-search-backward "=\\|->\\|<-" (point-min) t))
	      (let ((c (match-string 0)))
		(beginning-of-line)
		(re-search-forward "\\S ")
		(setq column (- (current-column) 1))
		(beginning-of-line)
		(cond
		 ;; Don't match inside comments or annots
		 ;; FIXME: Should exclude matches inside string literals too
		 ((re-search-forward "#" pos t) (beginning-of-line))
		 ((string= c "<-") (throw 'done 'annot))
		 ((re-search-forward "@" pos t) (beginning-of-line))
		 ;; Skip block
		 ((string= c "=") (throw 'done 'def))
		 ((string= c "->") (throw 'done 'def))
		 (t (throw 'done 'annot))))))))

    (cond
     ((not pos) 0)

     ;; Check if first preceding non-whitespace character was an operator
     ;; If not, this is most likely a top level clause.

     ((or (eq token 'def))
      (goto-char start)
      (if (and (setq pos (find-prev-grammar-char))
	       (or
		(member (match-string 0) '("," "=" ":" "|"))
		(string= (buffer-substring (- pos 1) (+ pos 1)) "<-")
		(string= (buffer-substring (- pos 1) (+ pos 1)) "->")))
	  (+ column gecko-indent-level)
	column))
     (is-incr-annot
      (goto-char pos)
      (beginning-of-line)
      (search-forward "<-" (+ pos 2) t)
      (- (current-column) 2))
     (t 0))))

(defun find-prev-grammar-char ()
  "Finds the first non ws char that is not part of a comment"
  (save-excursion
    (let ((pos -1) start)
      (while (< pos 0)
	(setq pos (re-search-backward "[^ \t\n]" (point-min) t))
	(cond ((not pos) (setq pos (point-min)))
	      ((setq start (search-backward "#" (find-start-of-line pos) t))
	       (setq pos -1)
	       (goto-char start))))
      pos)))

(defun find-start-of-line (pos)
  "Finds a start of line before pos"
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (point)))

(defun get-end-of-line ()
  "Finds the end of the current line"
  (save-excursion (end-of-line) (point)))

(defun gecko-indent-line ()
  "Indents the current line."
  (interactive)
  (let ((orig-point (point)))
    (beginning-of-line)

    (let* ((is-incr-annot 
	    (save-excursion (re-search-forward "^\\s *<-" (get-end-of-line) t)))
	   (beg-of-line (point))
	   (pos (re-search-forward "\\(\\S \\|\n\\)" (point-max) t))
	   (first (match-string 0))
	   (start (match-beginning 0))
	   (col (- (current-column) 1)))

      (goto-char beg-of-line)

      (let ((indent-column (gecko-find-column first start is-incr-annot)))
	(goto-char beg-of-line)

	(cond
	 ;; Only modify buffer if the line must be reindented
	 ((not (= col indent-column))
	  (if (not (or (null pos)
		       (= beg-of-line start)))
	      (kill-region beg-of-line start))

	  (goto-char beg-of-line)
	  
	  (while (< 0 indent-column)
	    (insert " ")
	    (setq indent-column (- indent-column 1))))

	 ((< orig-point start) (goto-char start))
	 (t (goto-char orig-point)))))))


(defun gecko-electric-brace (arg)
  (interactive "*P")
  (self-insert-command (prefix-numeric-value arg))
  (gecko-indent-line)
  (let ((p (point)))
    (when (save-excursion
	    (beginning-of-line)
	    (let ((pos (re-search-forward "\\S " (point-max) t)))
	      (and pos (= (- pos 1) p))))
      (forward-char-command))))

(defvar gecko-mode-map () "Keymap used in Gecko mode.")
(when (not gecko-mode-map)
  (setq gecko-mode-map (make-sparse-keymap))
  (define-key gecko-mode-map "\C-c\C-c" 'comment-region)
  (define-key gecko-mode-map "]" 'gecko-electric-brace)
  (define-key gecko-mode-map "[" 'gecko-electric-brace))

(defun gecko-mode ()
  "Major mode for editing Extensible Contact Notation (Gecko) documents.
\\{gecko-mode-map}"
  (interactive)

  (kill-all-local-variables)

  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'gecko-indent-line)
  
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(gecko-font-lock-keywords nil nil nil nil))
  
  (use-local-map gecko-mode-map)

  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)
  
  (setq comment-start "#"
	comment-end ""
	comment-start-skip "\\([ \n\t]+\\)##?[ \n\t]+")
  
  (let ((gecko-syntax-table (copy-syntax-table)))
    (modify-syntax-entry ?# "<   " gecko-syntax-table)
    (modify-syntax-entry ?\n ">   " gecko-syntax-table)
    (modify-syntax-entry ?\^m ">   " gecko-syntax-table)
    (modify-syntax-entry ?\\ "w   " gecko-syntax-table)
    (modify-syntax-entry ?' "\"   " gecko-syntax-table)
    (modify-syntax-entry ?. "w   " gecko-syntax-table)
    (modify-syntax-entry ?- "w   " gecko-syntax-table)
    (modify-syntax-entry ?_ "w   " gecko-syntax-table)
    (set-syntax-table gecko-syntax-table))
  
  (setq mode-name "Gecko"
	major-mode 'gecko-mode)
  (run-hooks 'gecko-mode-hook))

(provide 'gecko-mode)
