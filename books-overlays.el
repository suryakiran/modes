(provide 'books/overlays)

(defun books/overlays (property value &optional sort)
  (let ((overlays) (all-overlays (overlays-in (point-min) (point-max))))
    (setq overlays (--filter (let ((val))
                               (setq val (overlay-get it property))
                               (and val (eq val value))) all-overlays))
    (when sort
      (setq overlays (--sort (< (overlay-start it) (overlay-start other)) overlays)))
    overlays
    ))

(defun books/overlay-sort (overlays)
  (--sort (< (overlay-start it) (overlay-start other)) overlays))

(defun books/overlays-at-point (property &optional value pt)
  (let* ((all-overlays) (overlays))
    (unless pt (setq pt (point)))
    (setq all-overlays (overlays-at pt))
    (setq overlays (--filter (overlay-get it property) all-overlays))
    (when value
      (setq overlays (--filter (let ((val))
                                 (setq val (overlay-get it property) overlays)
                                 (eq val value)) overlays)))
    overlays))

(defun books/overlay-property (property &optional pt)
  (let ((overlays (books/overlays-at-point property nil pt)))
    (when overlays
      (overlay-get (car overlays) property))
    ))

(defun books/overlays-in-range (start end property value)
  (let ((filtered) (olays (overlays-in start end)))
    (setq filtered (books/overlay-sort
                    (--filter (eq (overlay-get it property) value) olays)))
    filtered))

(defun books/overlays-in (range-or-overlay property value)
  (let ((start) (end))
    (cond
     ((consp range-or-overlay)
      (setq start (car range-or-overlay))
      (setq end (cdr range-or-overlay)))
     
     ((overlayp range-or-overlay)
      (setq start  (overlay-start range-or-overlay))
      (setq end (overlay-end range-or-overlay)))
     )
    (and start end
         (books/overlays-in-range start end property value))
    ))

(defun books/remove-other-overlays-in (this-category other-category)
  (books/do-in-overlay this-category
                       (remove-overlays (point-min) (point-max) 'category other-category)))

(defun books/next-overlay (property value)
  (let ((overlays (books/overlays property value t))
        (cur (books/overlays-at-point property value))
        (next-overlay)
        )
    (setq cur (and cur (listp cur) (car cur)))
    (when cur
      (setq next-overlay
            (--first (> (overlay-start it) (overlay-start cur)) overlays))
      )
    next-overlay))

(defun books/prev-overlay (property value)
  (let ((overlays (books/overlays property value t))
        (cur (books/overlays-at-point property value))
        (prev-overlay)
        )
    (setq cur (and cur (listp cur) (car cur)))
    (when cur
      (setq prev-overlay
            (--last (> (overlay-start cur) (overlay-start it)) overlays))
      )
    prev-overlay))

(defun books/overlays/first (property value)
  (let* ((overlays (books/overlays property value t))
         (overlay (-first-item overlays))
         )
    overlay))

(defun books/overlays/last (property value)
  (let* ((overlays (books/overlays property value t))
         (overlay (-last-item overlays))
         )
    overlay))

(defmacro books/do-in-overlay (category &rest body)
  `(let ((_overlay (car (books/overlays-at-point 'category ,category))))
     (when _overlay
       (save-excursion
         (save-restriction
           (narrow-to-region (overlay-start _overlay) (overlay-end _overlay))
           ,@body
           )
         ))
     ))
