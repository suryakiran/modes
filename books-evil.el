(provide 'books/evil)


(books/defkeymap books/evil-mode-keymap "Keymap used in books/evil mode")

(define-minor-mode books/evil-mode
  "Some Documentation"
  :lighter " vi"
  :keymap books/evil-mode-keymap
  (books/evil/startup-or-shutdown)
  )

(defun books/evil/startup-or-shutdown ()
  (if (symbol-value 'books/evil-mode)
      (books/evil/startup)
    (books/evil/shutdown)
    ))

(defun books/evil/startup ()
  )

(defun books/evil/shutdown ()
  )

(define-key books/evil-mode-keymap (kbd "l") 'right-char)
(define-key books/evil-mode-keymap (kbd "h") 'left-char)
(define-key books/evil-mode-keymap (kbd "j") 'next-line)
(define-key books/evil-mode-keymap (kbd "k") 'previous-line)
(define-key books/evil-mode-keymap (kbd "x") '(lambda () (interactive) (delete-forward-char 1)))
(define-key books/evil-mode-keymap (kbd "0") 'back-to-indentation)

(define-key books/evil-mode-keymap (kbd "L") '(lambda () (interactive) (execute-kbd-macro [S-right])))
(define-key books/evil-mode-keymap (kbd "H") '(lambda () (interactive) (execute-kbd-macro [S-left])))
(define-key books/evil-mode-keymap (kbd "J") '(lambda () (interactive) (execute-kbd-macro [S-down])))
(define-key books/evil-mode-keymap (kbd "K") '(lambda () (interactive) (execute-kbd-macro [S-up])))

(define-key books/evil-mode-keymap (kbd "u") 'undo)
