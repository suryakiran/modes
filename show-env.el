(books/defkeymap books/show-env-mode-keymap "Keymap used in books/show-env mode")
(defvar books/show-env/regex nil "Regex of environments to show")
(make-variable-buffer-local 'books/show-env/regex)

(define-minor-mode books/show-env-mode
  "Some Documentation"
  :lighter " show-env"
  :keymap books/show-env-mode-keymap
  (books/show-env/startup-or-shutdown)
  )

(defun books/show-env/startup-or-shutdown ()
  (if (symbol-value 'books/show-env-mode)
      (books/show-env/startup)
    (books/show-env/shutdown)
    ))

(defun books/show-env/startup ()
  (unless books/show-env/regex
    (books/show-env-mode -1))
  )

(defun books/show-env/shutdown ()
  (widen)
  )

(defun books/show-env/get-current (regions)
  (let ((current))
    (save-buffer)
    (if (buffer-narrowed-p)
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward (format "\\\\begin{%s}" books/show-env/regex) (point-max) t)
            (setq current (point)))
          )
      (setq current (point))
      )
    (--find-index (and (< (car it) current) (> (cdr it) current)) regions)
    ))

(defun books/show-env/show (type)
  (let ((regions) (narrow) (current))
    (save-restriction
      (widen)
      (setq regions (reverse (books/environment-regions books/show-env/regex)))
      )

    (setq current (books/show-env/get-current regions))
    
    
    (cond
     ((eq type 'last)  (setq narrow (-last-item regions)))
     ((eq type 'first) (setq narrow (-first-item regions)))
     ((eq type 'next)  (setq narrow (nth-value (1+ current) regions)))
     ((eq type 'prev)  (setq narrow (nth-value (1- current) regions)))
     )

    (widen)
    (when narrow (narrow-to-region (car narrow) (cdr narrow)))
    ))

(define-key books/show-env-mode-keymap (kbd "s-<home>") '(lambda () (interactive) (books/show-env/show 'first)))
(define-key books/show-env-mode-keymap (kbd "s-<next>") '(lambda () (interactive) (books/show-env/show 'next)))
(define-key books/show-env-mode-keymap (kbd "s-<prior>") '(lambda () (interactive) (books/show-env/show 'prev)))
(define-key books/show-env-mode-keymap (kbd "s-<end>") '(lambda () (interactive) (books/show-env/show 'last)))

;; (key-chord-define books/show-env-mode-keymap (kbd "ll") 'books/show-env/newline)
