(provide 'books/math-overlay)
;; (books/require 'overlay)

(books/defkeymap books/math-overlay-mode-keymap "Keymap used in books/math-overlay mode")

(define-minor-mode books/math-overlay-mode
  "Some Documentation"
  :lighter " bmo"
  :keymap books/math-overlay-mode-keymap
  (books/math-overlay/startup-or-shutdown)
  )

(defun books/math-overlay/startup-or-shutdown ()
  (if (symbol-value 'books/math-overlay-mode)
      (books/math-overlay/startup)
    (books/math-overlay/shutdown)
    ))

(defun books/math-overlay/startup ()
  )

(defun books/math-overlay/shutdown ()
  )

;; (define-key books/math-overlay-mode-keymap (kbd "<f1>") 'help)
