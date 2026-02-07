(require 'polymode)
(require 'peg-mode)

(define-hostmode poly-peg-hostmode :mode 'peg-mode)

(defun my-poly-balanced-tail-matcher (_)
  (save-excursion
    (let ((depth 1))
      (while (and (> depth 0) (re-search-forward "[{}]" nil t))
        (let ((state (syntax-ppss)))
          (unless (or (nth 3 state) (nth 4 state)) ;; Not in string or comment
            (if (string= (match-string 0) "{")
                (setq depth (1+ depth))
              (setq depth (1- depth))))))
      (when (= depth 0)
        (cons (match-beginning 0) (match-end 0))))))

(define-innermode poly-c-innermode
  :mode 'c-mode
  :indent-offset 2
  :head-matcher "{"
  :tail-matcher 'my-poly-balanced-tail-matcher 
  :head-mode 'host
  :tail-mode 'host)

;;;#autoload
(define-polymode poly-peg-mode
  :hostmode 'poly-peg-hostmode
  :innermodes '(poly-c-innermode))


(provide 'poly-peg-mode)
