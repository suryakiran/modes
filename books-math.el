(books/defkeymap books/math-mode-keymap "Keymap used in books/math mode")
(books/require 'math-keys)
(require 'helm)

(define-minor-mode books/math-mode
  "Some Documentation"
  :lighter " math"
  :keymap books/math-mode-keymap
  (books/math/startup-or-shutdown)
  )

(defun books/math/startup-or-shutdown ()
  (if (symbol-value 'books/math-mode)
      (books/math/startup)
    (books/math/shutdown)
    ))

(defun books/math/startup ()
  )

(defun books/math/shutdown ()
  )

(defun math/superscript ()
  (interactive)
  (math/super-sub '^)
  )

(defun math/subscript ()
  (interactive)
  (math/super-sub '_)
  )

(defun math/super-sub (sym)
  (let ((beg (format "%s{" (symbol-name sym))))
    (unless (region-active-p)
      (set-mark (point))
      (goto-char (1+ (point)))
      )
    (wrap-region-with beg "}")
    (save-buffer)
    ))

(defun math/select-from-previous-dollars ()
  (interactive)
  (let ((items) (text))
    (skg/search-file-do "\\$[^$]*\\$"
      (add-to-list 'items (match-string-no-properties 0)))
    (setq text (helm :sources '((name . "Select from list")
                                (candidates . items)
                                (action . identity))))
    (when text (insert text))
    ))

(defun math/display-math ()
  (interactive)
  (unless (region-active-p)
    (skg/select-line))
  (wrap-region-with "\\[\n" "\n\\]")
  (save-buffer))

(define-key books/math-mode-keymap (kbd "<f12>") 'math-keys-mode)
(define-key books/math-mode-keymap (kbd "s-<kp-8>") 'math/superscript)
(define-key books/math-mode-keymap (kbd "s-<kp-2>") 'math/subscript)
(define-key books/math-mode-keymap (kbd "s-m") 'math/display-math)
(define-key books/math-mode-keymap (kbd "s-.") '(lambda () (interactive) (insert " {\\cdot} ")))
(define-key books/math-mode-keymap (kbd "C-s-a") '(lambda () (interactive) (insert " {\\alpha} ")))
(define-key books/math-mode-keymap (kbd "C-s-b") '(lambda () (interactive) (insert " {\\beta} ")))
(define-key books/math-mode-keymap (kbd "C-s-g") '(lambda () (interactive) (insert " {\\gamma} ")))
(define-key books/math-mode-keymap (kbd "C-s-t") '(lambda () (interactive) (insert " {\\theta} ")))
(define-key books/math-mode-keymap (kbd "C-s-p") '(lambda () (interactive) (insert " {\\pi} ")))
(define-key books/math-mode-keymap (kbd "s-<prior>") 'math/select-from-previous-dollars)


(defun books/math/standard-sp-triggers (mode)
  (books/deftag "B" "\\mathbf{" "}" (books/major-mode))
  (books/deftag "n" "\\textnormal{" "}" (books/major-mode))
  (books/deftag "r" "\\mathrm{" "}" (books/major-mode))
  )


(defmacro math/book-mode (mode-str book &rest body)
  (declare (indent 1) (debug t))
  (let (
        (mode (intern (format "books/%s-mode" mode-str)))
        (hook (intern (format "books/%s-mode-hook" mode-str)))
        )
    (add-hook hook (lambda ()
                     (books/math-mode 1)
                     ))
    `(progn
       (books/new-mode ,mode-str latex-mode ,book ,@body
                       (turn-off-wrap-region-mode)
                       (books/math/standard-sp-triggers ',mode)))
    
    ))

(provide 'books/math)
