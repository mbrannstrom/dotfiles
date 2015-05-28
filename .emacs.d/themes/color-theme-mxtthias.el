(defun color-theme-mxtthias ()
  (interactive)
  (color-theme-install
   '(color-theme-mxtthias
   ((background-color . "#000000")
   (background-mode . dark)
   (cursor-color . "#FFFFFF")
   (foreground-color . "#FFFFFF"))
   (modeline ((t (:background "#000000" :foreground "#FFFFFF" ))))
   (modeline-inactive ((t (:background "#000000" :foreground "#FFFFFF"))))
   (default ((t (nil))))

   (bold ((t (:bold t))))
   (bold-italic ((t (:italic t :bold t))))
   (italic ((t (:italic t))))
   (underline ((t (:underline t))))
   (highlight ((t (:background "#2C2C2C"))))

   ; comments
   (font-lock-comment-face ((t (:foreground "#125E8A" :italic t))))
   (font-lock-comment-delimiter-face ((t (:foreground "#125E8A" :italic t))))
   (font-lock-doc-face ((t (:foreground "#125E0A" :italic t))))

   ; strings
   (font-lock-string-face ((t (:foreground "#37E01D"))))

   ; keywords
   (font-lock-keyword-face ((t (:foreground "#FFAA00" :bold t))))

   ; built-in functions
   (font-lock-builtin-face ((t (:foreground "#D6749B" :bold t))))

   ; function and variable names
   (font-lock-function-name-face ((t (:foreground "#FFFFFF" :bold t))))
   (font-lock-variable-name-face ((t (:foreground "#749BD6"))))
   ;(font-lock-variable-name-face ((t (:foreground "#FFFFFF"))))

   ; types and constants
   (font-lock-type-face ((t (:foreground "#FFFFFF"))))
   (font-lock-constant-face ((t (:foreground "#FFFFFF"))))

   ; preprocessor directives
   (font-lock-preprocessor-face ((t (:foreground "#FFAA00"))))

   ; negation characters
   (font-lock-negation-char-face ((t (:foreground "red"))))

   ; warnings
   (font-lock-warning-face ((t (:foreground "red")))))))