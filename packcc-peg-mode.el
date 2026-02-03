;;; packcc-peg-mode.el --- Major mode for editing PackCC PEG files -*- lexical-binding: t -*-

;; Copyright (C) 2024 Rongsong Shen

;; Author: Rongsong Shen
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: languages, tools, packcc, peg
;; URL: https://github.com/shen390s/packcc-peg-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Major mode for editing PackCC parsing expression grammar (PEG) files.

;;; Code:

(require 'cc-mode)

(defgroup packcc-peg nil
  "Major mode for editing PackCC PEG files."
  :group 'languages
  :prefix "packcc-peg-")

(defcustom packcc-peg-indent-offset 2
  "Number of spaces for each indentation step."
  :type 'integer
  :group 'packcc-peg
  :safe 'integerp)

(defcustom packcc-peg-use-c-mode-indentation t
  "Use C mode indentation for code blocks."
  :type 'boolean
  :group 'packcc-peg)

(defface packcc-peg-directive-face
  '((t :inherit font-lock-preprocessor-face :weight bold))
  "Face for PackCC directives."
  :group 'packcc-peg)

(defface packcc-peg-rule-name-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for rule names."
  :group 'packcc-peg)

(defface packcc-peg-operator-face
  '((t :inherit font-lock-keyword-face))
  "Face for PEG operators."
  :group 'packcc-peg)

(defface packcc-peg-predicate-face
  '((t :inherit font-lock-builtin-face :slant italic))
  "Face for predicates (&, !)."
  :group 'packcc-peg)

(defconst packcc-peg-directives
  '("%prefix" "%value" "%auxil" "%scope" "%include" "%earlysource"
    "%source" "%header" "%common" "%precedence" "%left" "%right"
    "%nonassoc")
  "PackCC directives.")

(defconst packcc-peg-operators
  '("<-" "/" "&" "!" "*" "+" "?" "~" ". ")
  "PEG operators.")

(defconst packcc-peg-keywords
  '("error" "void")
  "PackCC keywords.")

(defvar packcc-peg-syntax-table
  (let ((table (make-syntax-table)))
    ;; C/C++ style comments
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    
    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)
    
    ;; Brackets
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    
    ;; Punctuation
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?; "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?, "." table)
    (modify-syntax-entry ?. "." table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?_ "_" table)
    
    table)
  "Syntax table for packcc-peg-mode.")

(defvar packcc-peg-font-lock-keywords
  `(
    ;; Directives
    (,(regexp-opt packcc-peg-directives 'symbols) . 'packcc-peg-directive-face)
    
    ;; Rule names (before <-)
    ("^\\s-*\\([[:alpha:]_][[:alnum:]_]*\\)\\s-*<-"
     1 'packcc-peg-rule-name-face)
    
    ;; Operators
    (,(regexp-opt packcc-peg-operators) . 'packcc-peg-operator-face)
    
    ;; Predicates
    ("[&!]" . 'packcc-peg-predicate-face)
    
    ;; Keywords
    (,(regexp-opt packcc-peg-keywords 'symbols) . font-lock-keyword-face)
    
    ;; Character classes [a-z]
    ("\\[\\(?:[^]\n]\\|\\\\]\\)*\\]" . font-lock-string-face)
    
    ;; Literal strings
    ("\"\\(?:[^\"\\]\\|\\\\.\\)*\"" . font-lock-string-face)
    
    ;; Character literals
    ("'[^'\\]\\|\\\\.'" . font-lock-string-face)
    
    ;; C code blocks
    ("{\\([^}]\\|\\\\.\\)*}" . font-lock-type-face)
    
    ;; Variables in actions
    ("$[0-9]+\\b\\|$[a-zA-Z_][a-zA-Z0-9_]*" . font-lock-variable-name-face)
    
    ;; Type annotations
    ("<[^>]+>" . font-lock-type-face)
    )
  "Font-lock keywords for packcc-peg-mode.")

(defun packcc-peg-indent-line ()
  "Indent current line in packcc-peg-mode."
  (interactive)
  (let ((indent 0)
        (pos (point)))
    (save-excursion
      (beginning-of-line)
      (cond
       ;; Inside C code block
       ((nth 8 (syntax-ppss))
        (when packcc-peg-use-c-mode-indentation
          (let ((c-indent-offset packcc-peg-indent-offset))
            (c-indent-line))))
       
       ;; Rule definition
       ((looking-at "^\\s-*[a-zA-Z_]")
        (setq indent 0))
       
       ;; Continuation of previous line
       (t
        (forward-line -1)
        (when (not (bobp))
          (end-of-line)
          (backward-char)
          (cond
           ;; After opening brace
           ((looking-at "{")
            (setq indent (+ (current-indentation) packcc-peg-indent-offset)))
           ;; After rule operator
           ((looking-at "\\s-*<-")
            (setq indent (+ (current-indentation) packcc-peg-indent-offset)))
           ;; Default: maintain same indentation
           (t
            (setq indent (current-indentation))))))))
    (when (> indent 0)
      (indent-line-to indent))
    (when (< (point) pos)
      (goto-char pos))))

(defun packcc-peg-syntax-propertize-function (start end)
  "Apply syntax properties between START and END for packcc-peg-mode."
  (goto-char start)
  (funcall
   (syntax-propertize-rules
    ;; Treat C code blocks as strings for syntax highlighting purposes
    ("\\({\\)[^}]*\\(}\\)"
     (1 "(|") (2 ")|")))
   start end))

(defvar packcc-peg-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'comment-region)
    (define-key map (kbd "C-c C-u") 'uncomment-region)
    (define-key map (kbd "C-c C-f") 'packcc-peg-format-buffer)
    (define-key map (kbd "C-c C-v") 'packcc-peg-validate)
    map)
  "Keymap for packcc-peg-mode.")

(defun packcc-peg-format-buffer ()
  "Format the current PackCC PEG buffer."
  (interactive)
  (message "Formatting PackCC PEG file...")
  ;; Basic formatting: indentation
  (indent-region (point-min) (point-max))
  (message "Formatting done."))

(defun packcc-peg-validate ()
  "Validate the current PackCC PEG file."
  (interactive)
  (if (buffer-file-name)
      (let ((file (buffer-file-name)))
        (compile (format "packcc %s" (shell-quote-argument file))))
    (message "Buffer is not visiting a file.")))

(defun packcc-peg-imenu-setup ()
  "Set up imenu for packcc-peg-mode."
  (setq imenu-generic-expression
        '((nil "^\\s-*\\([[:alpha:]_][[:alnum:]_]*\\)\\s-*<-" 1))))

(defun packcc-peg-electric-brace (arg)
  "Insert a brace and maybe add newline and indent."
  (interactive "*P")
  (self-insert-command (prefix-numeric-value arg))
  (when (and electric-indent-mode
             (eolp)
             (save-excursion
               (beginning-of-line)
               (looking-at "^\\s-*[^{]*{")))
    (newline)
    (indent-according-to-mode)
    (save-excursion
      (newline)
      (indent-according-to-mode))))

;;;###autoload
(define-derived-mode packcc-peg-mode prog-mode "PackCC-PEG"
  "Major mode for editing PackCC PEG files."
  :group 'packcc-peg
  :syntax-table packcc-peg-syntax-table
  
  (setq-local font-lock-defaults
              '(packcc-peg-font-lock-keywords
                nil
                nil
                nil
                nil
                (font-lock-syntactic-face-function
                 . packcc-peg-font-lock-syntactic-face-function)))
  
  (setq-local indent-line-function 'packcc-peg-indent-line)
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "//+\\s-*")
  (setq-local comment-multi-line t)
  
  (setq-local syntax-propertize-function #'packcc-peg-syntax-propertize-function)
  
  (setq-local electric-indent-chars
              (append electric-indent-chars '(?{ ?})))
  
  (add-hook 'imenu-setup-hook 'packcc-peg-imenu-setup nil t)
  
  ;; Set up electric brace insertion
  (local-set-key (kbd "{") 'packcc-peg-electric-brace)
  
  ;; Set up adaptive fill
  (setq-local adaptive-fill-mode t)
  
  ;; Set up whitespace mode
  ;;(whitespace-mode 1)
  
  (message "PackCC PEG mode enabled"))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.peg\\'" . packcc-peg-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.packcc\\'" . packcc-peg-mode))

(defun packcc-peg-font-lock-syntactic-face-function (state)
  "Return face for syntactic highlighting in packcc-peg-mode.
STATE is the parser state."
  (if (nth 3 state)  ; Inside a string
      font-lock-string-face
    (if (nth 4 state)  ; Inside a comment
        font-lock-comment-face
      font-lock-preprocessor-face)))

(defun packcc-peg-insert-rule-template ()
  "Insert a rule template at point."
  (interactive)
  (let ((rule-name (read-string "Rule name: ")))
    (insert rule-name " <- \n")
    (save-excursion
      (insert "  ;\n")
      (insert "\n"))
    (indent-according-to-mode)))

(defun packcc-peg-insert-action-template ()
  "Insert an action template at point."
  (interactive)
  (insert "{\n")
  (insert "  $$ = $1;\n")
  (insert "}\n")
  (indent-according-to-mode))

(provide 'packcc-peg-mode)

;;; packcc-peg-mode.el ends here
