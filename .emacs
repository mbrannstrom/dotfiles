(add-to-list 'load-path "~/.emacs.d")

;; Environment ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; system related paths and stuff goes here
(load-file "~/.emacs.d/environment.el") 

;; Misc ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(standard-display-european 1)
(set-input-mode nil nil 0 7)

;; Mac cmd is meta
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

; Status bar
(setq column-number-mode t)
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(setq truncate-partial-width-windows nil)

(set-scroll-bar-mode 'right) ; scroll bar to the right
;(tool-bar-mode) ; hide toolbar

(setq inhibit-startup-message t) ; hide welcome screen

(setq-default indent-tabs-mode nil) ; never use tabs to indent code
(setq-default tab-width 8)

; what is this?
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Decoration ;;;;;;;;;;;;;;;;;;;;;;;;;;
(put 'eval-expression 'disabled nil)
(setq font-lock-maximum-decoration t)
(global-font-lock-mode t)

;; PC-style selection with shift ;;;;;;;;;;;;;;
;(pc-selection-mode)
;(set-face-foreground 'region "white")
;(set-face-background 'region "#4040cc")

;; Abbrevations ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default abbrev-mode t)
(setq abbrev-file-name "~/.emacs.d/abbrev.el")
(read-abbrev-file abbrev-file-name)

;; PHP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq cc-imenu-php-generic-expression t)
(setq php-mode-force-pear t)
(setq php-font-lock-keywords-3 t)

;; Perl ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\|ppp\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

;; Erlang ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; apply erlang mode to .erl, .hrl and .yaws
;(add-to-list 'load-path "~/git/klarna/OTP/install/R14B03/lib/erlang/lib/tools-2.6.6.4/emacs")
;(autoload 'erlang-mode "erlang.el" "" t)
;(add-to-list 'auto-mode-alist '("\\.[eh]rl$" . erlang-mode))
;(add-to-list 'auto-mode-alist '("\\.yaws$" . erlang-mode))

;; Distel
;(if distel-path
;    (progn
;      (add-to-list 'load-path distel-path)
;      (require 'distel)
;      (distel-setup)
;     ))

;(defun my-erlang-mode-hook ()
;  ;; customize keys
;  (local-set-key [return] 'newline-and-indent)
;  (setq erlang-indent-level 2)
;  (linum-mode)
;  (setq whitespace-style
;        '(face lines tabs tab-mark empty trailing))
;  (whitespace-mode)
;
;  )
;(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)

;; Emacs 23 on mac os x ;;;;;;;;;;;;;;;;;;;;;;;;;;
;(setq mac-option-key-is-meta nil)
;(setq mac-command-key-is-meta t)
;(setq mac-command-modifier 'meta)
;(setq mac-option-modifier nil)


;; Blink protocol ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'blink-mode "blink-mode" "" t)
(setq auto-mode-alist       
      (cons '("\\.blink\\'" . blink-mode) auto-mode-alist))


;; Markdown ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
;;(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;; Groovy ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

;;; make Groovy mode electric by default.
(add-hook 'groovy-mode-hook
          '(lambda ()
             (require 'groovy-electric)
             (groovy-electric-mode)))

;; global key bindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun rev ()(interactive) (revert-buffer nil t nil))
(global-set-key (kbd "C-x C-r") 'rev) ; revert buffer, no questions

(global-set-key (kbd "M-_") 'dabbrev-expand) ; M-/ does not work on Mac
(global-set-key '[(meta g)] 'goto-line) ; Goto line
(global-set-key '[(control backspace)] 'backward-kill-word)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((c-hanging-comment-ender-p) (erlang-indent-level . 2) (erlang-indent-level . 4)))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
