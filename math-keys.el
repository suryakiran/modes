(provide 'books/math-keys)

(books/defkeymap math-keys-mode-keymap "Keymap used in math-keys mode")

(define-minor-mode math-keys-mode
  "Some Documentation"
  :lighter " mk"
  :keymap math-keys-mode-keymap
  (math-keys/startup-or-shutdown)
  )

(defun math-keys/startup-or-shutdown ()
  (if (symbol-value 'math-keys-mode)
      (math-keys/startup)
    (math-keys/shutdown)
    ))

(defun math-keys/startup ()
  (key-chord-mode 1)
  )

(defun math-keys/shutdown ()
  (key-chord-mode -1)
  )

(defun math-keys/sign-limits (key)
  (interactive)
  (let ((macro (format "\\%s_{}^{}" (symbol-name key))))
    (insert macro)
    (backward-char 4)
    ))

(defun math-keys/cal ()
  (interactive)
  (let ((char (skg/char-under-cursor)))
    (unless (s-blank? (s-trim char))
      (set-mark (point))
      (goto-char (1+ (point)))
      (wrap-region-with "\\mathcal{" "}"))
    ))

(defun math-keys/prime ()
  (interactive)
  (goto-char (1+ (point)))
  (insert "^{\\prime}")
  (save-buffer))

(defun math-keys/dollar-word ()
  (interactive)
  (books/as-word "\\^_{}"
    (unless (region-active-p) (er/mark-word))
    (wrap-region-with "$" "$")
    (goto-char (1+ (point)))
    (search-forward "$")
    ))

(cl-defun math-keys/insert (&key l r)
  (let ((s))
    (and l r
         (progn
           (setq s (format "\\left%s \\right%s" l r))
           (insert s)
           (when (search-backward (format "\\left%s" l) (point-min) t 1)
             (goto-char (1+ (match-end 0))
                        ))
           (save-buffer))
         )))
         

(defun math-keys/parens ()
  (interactive)
  (pp 'tinere)
  (math-keys/insert :l "(" :r ")"))

(defun math-keys/brackets ()
  (interactive)
  (math-keys/insert :l "{" :r "}"))

(defun math-keys/sq-brackets ()
  (interactive)
  (math-keys/insert :l "[" :r "]"))

(define-key math-keys-mode-keymap (kbd "s-s") '(lambda () (interactive) (math-keys/sign-limits 'sum)))

(define-key math-keys-mode-keymap (kbd "s-<kp-6>") '(lambda () (interactive) (insert "\\rightarrow")))
(define-key math-keys-mode-keymap (kbd "s-<kp-4>") '(lambda () (interactive) (insert "\\leftarrow")))

(key-chord-define math-keys-mode-keymap (kbd "44") 'math-keys/dollar-word)
(key-chord-define math-keys-mode-keymap (kbd "aa") '(lambda () (interactive) (insert "\\alpha")))
(key-chord-define math-keys-mode-keymap (kbd "bb") '(lambda () (interactive) (insert "\\beta")))
(key-chord-define math-keys-mode-keymap (kbd "gg") '(lambda () (interactive) (insert "\\gamma")))
(key-chord-define math-keys-mode-keymap (kbd "dd") '(lambda () (interactive) (insert "\\delta")))
(key-chord-define math-keys-mode-keymap (kbd "ee") '(lambda () (interactive) (insert "\\epsilon")))
(key-chord-define math-keys-mode-keymap (kbd "ll") '(lambda () (interactive) (insert "\\lambda")))
(key-chord-define math-keys-mode-keymap (kbd "le") '(lambda () (interactive) (insert " {\\varleq} ")))
(key-chord-define math-keys-mode-keymap (kbd "ge") '(lambda () (interactive) (insert " {\\vargeq} ")))
(key-chord-define math-keys-mode-keymap (kbd "((") 'math-keys/parens)

(key-chord-define math-keys-mode-keymap (kbd "mr") '(lambda () (interactive) (insert "\\mathbb{R}")))
(key-chord-define math-keys-mode-keymap (kbd "mp") 'math-keys/prime)
(key-chord-define math-keys-mode-keymap (kbd "mc") 'math-keys/cal)

;; (define-key math-keys-mode-keymap (kbd "<f1>") 'help)
