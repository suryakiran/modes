(provide 'books/section)

(defvar books/section-regions () "Regions of sections")
(make-variable-buffer-local 'books/section-regions)

(books/defkeymap books/section-mode-keymap "Keymap used in books/section mode")

(define-minor-mode books/section-mode
  "Some Documentation"
  :lighter " bs"
  :keymap books/section-mode-keymap
  (books/section/startup-or-shutdown)
  )

(defun books/section/startup-or-shutdown ()
  (if (symbol-value 'books/section-mode)
      (books/section/startup)
    (books/section/shutdown)
    ))

(defun books/section/clear ()
  (widen)
  (setq books/section-regions nil)
  (remove-list-of-text-properties (point-min) (point-max) '(section-number section-index))
  (save-buffer)
  )

(defun books/section/create-list ()
  (let ((starts) (ends))
    (books/section/clear)
    (skg/search-file-do "\\\\section{"
      (add-to-list 'starts (match-beginning 0) t))
    (setq ends (-slice starts 1))
    (add-to-list 'ends (point-max) t)
    (setq books/section-regions (-zip-with 'cons starts ends))
    (--map-indexed (progn
                     (put-text-property (car it) (cdr it) 'section-number (1+ it-index))
                     (put-text-property (car it) (cdr it) 'section-index  it-index)
                     ) books/section-regions)
    (save-buffer)
    ))

(defun books/section/startup ()
  (books/section/create-list)
  )

(defun books/section/shutdown ()
  (books/section/clear)
  )

(defun books/section/goto (type)
  (let ((cur-idx) (cur-num) (prev) (narrow))
    (books/section/create-list)
    (setq cur-num (get-text-property (point) 'section-number))
    (setq cur-idx (get-text-property (point) 'section-index))

    (unless cur-idx
      (setq cur-num 0)
      (setq cur-idx -1)
      )
    
    (setq prev (1- cur-idx))

    (when (< prev 0)
      (setq prev 0))

    (cond
     ((eq type 'first) (setq narrow (-first-item books/section-regions)))
     ((eq type 'last)  (setq narrow (-last-item books/section-regions)))
     ((eq type 'next)  (setq narrow (nth-value cur-num books/section-regions)))
     ((eq type 'prev)  (setq narrow (nth-value prev books/section-regions)))
     )

    (when narrow
      (narrow-to-region (car narrow) (cdr narrow)))
    ))

(define-key books/section-mode-keymap (kbd "<home>") '(lambda () (interactive) (books/section/goto 'first)))
(define-key books/section-mode-keymap (kbd "<end>") '(lambda () (interactive) (books/section/goto 'last)))
(define-key books/section-mode-keymap (kbd "<prior>") '(lambda () (interactive) (books/section/goto 'prev)))
(define-key books/section-mode-keymap (kbd "<next>") '(lambda () (interactive) (books/section/goto 'next)))
