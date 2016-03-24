(books/require 'overlay)

(provide 'books/books-highlight)

(books/defface books/highlight/face "Face used to highlight regions" books
               (:background "yellow"))

(defun highlight/create-overlay (begin end)
  (let ((olay (make-overlay begin end (current-buffer) nil t)))
    (overlay-put olay 'face 'books/highlight/face)
    (overlay-put olay 'category 'highlight)
    (and books/highlight/keymap (overlay-put olay 'keymap books/highlight/keymap))
    olay
    ))

(defun highlight/get-overlay (&optional pt)
  (car (books/overlays-at-point 'category 'highlight pt)))

(defun highlight/delete-region ()
  (interactive)
  (let ((olay (highlight/get-overlay)))
    (and olay
         (delete-region (overlay-start olay) (overlay-end olay))
         (remove-overlays (point-min) (point-max) 'category 'highlight)
         (save-buffer))
    ))

(defun highlight/wrap (open close)
  (books/do-in-overlay 'highlight
                       (goto-char (point-min))
                       (insert open)
                       (goto-char (point-max))
                       (insert close)
                       (save-buffer))
  )

(defun highlight/enumerate ()
  (interactive)
  (highlight/wrap-env "enumerate"))

(defun highlight/wrap-env (env)
  (highlight/wrap (format "\\begin{%s}\n" env) (format "\n\\end{%s}\n" env)))

(defun highlight/itemize ()
  (interactive)
  (highlight/wrap-env "itemize"))

(defun highlight/code ()
  (interactive)
  (highlight/wrap-env "Code"))

(defun highlight/include-previous-line ()
  (interactive)
  (let ((new-begin) (olay (highlight/get-overlay)))
    (save-excursion
      (goto-char (overlay-start olay))
      (goto-char (1- (line-beginning-position)))
      (setq new-begin (line-beginning-position))
      )
    (move-overlay olay new-begin (overlay-end olay))
    (highlight/show-highlight-top)
    ))

(defun highlight/include-next-line ()
  (interactive)
  (let ((new-end) (olay (highlight/get-overlay)))
    (save-excursion
      (goto-char (overlay-end olay))
      (goto-char (1+ (line-end-position)))
      (setq new-end (line-end-position))
      )
    (move-overlay olay (overlay-start olay) new-end)
    (highlight/show-highlight-bottom)
    ))

(defun highlight/toggle-line-select ()
  (interactive)
  (let ((olay (car (books/overlays-at-point 'category 'highlight))))
    (if olay
        (delete-overlay olay)
      (progn
        (highlight/create-overlay (line-beginning-position) (line-end-position))
        ))
    ))

(defun highlight/to-end-of-line ()
  (interactive)
  (let ((olay (car (books/overlays-at-point 'category 'highlight))))
    (unless olay
      (highlight/create-overlay (point) (line-end-position)))
    ))

(defun highlight/to-beginning-of-line ()
  (interactive)
  (let ((olay (car (books/overlays-at-point 'category 'highlight))))
    (unless olay
      (highlight/create-overlay (line-beginning-position) (point)))
    ))

(defun highlight/paste-highlights ()
  (interactive)
  (let ((olays (books/overlays 'category 'highlight t)) (texts))
    (setq texts (--map (buffer-substring-no-properties (overlay-start it) (overlay-end it)) olays))
    (--map (delete-region (overlay-start it) (overlay-end it)) (reverse olays))
    (insert (s-join "\n" texts))
    (save-buffer)
    ))

(defun highlight/current-env ()
  (interactive)
  (let ((olay (car (books/overlays-at-point 'category 'highlight))))
    (when olay (delete-overlay olay))
    (LaTeX-mark-environment)
    (highlight/create-overlay (region-beginning) (region-end))
    ))

(defun highlight/show-highlight (type)
  (let ((fun)
        (win (selected-window))
        (olay (car (books/overlays-at-point 'category 'highlight)))
        )
    (cl-case type
      ('top (setq fun 'overlay-start))
      ('bottom (setq fun 'overlay-end)))
    
    (and fun olay win
         (progn
           (setq pos (funcall fun olay))
           (unless (pos-visible-in-window-p pos win)
             (set-window-start win (- pos 10) t))
           ))
    ))

(defun highlight/show-highlight-top ()
  (interactive)
  (highlight/show-highlight 'top))

(defun highlight/show-highlight-bottom ()
  (interactive)
  (highlight/show-highlight 'bottom))


(eval-after-load 'monokai-theme '(progn
                                   (custom-theme-set-faces
                                    'monokai
                                    '(books/highlight/face ((t (:foreground "black" :background "rosy brown"))))
                                    )))
