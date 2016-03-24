(provide 'books/bib)

(books/defkeymap books/bib-mode-keymap "Keymap used in books/bib mode")
(books/defkeymap books/bib-mode-overlay-keymap "Keymap used in books/bib mode overlays")
(books/defface books/bib-face "Face used to display bib citation" bib
               (:foreground "white" :background "light coral"))
(defvar books/bib/regex "\\[\\([A-Za-z][^]]*\\)\\]" "citation regex")
(defvar books/bib/overlays nil "All Overlays")
(make-variable-buffer-local 'books/bib/overlays)

(define-minor-mode books/bib-mode
  "Some Documentation"
  :lighter " bb"
  :keymap books/bib-mode-keymap
  (books/bib/startup-or-shutdown)
  )

(defun books/bib/create-overlay (start end)
  (let ((overlay (make-overlay start end (current-buffer) nil t)))
    (overlay-put overlay 'face 'books/bib-face)
    (overlay-put overlay 'books/bib t)
    (overlay-put overlay 'keymap books/bib-mode-overlay-keymap)
    (add-to-list 'books/bib/overlays overlay t)
    overlay))

(defun books/bib/list-citations ()
  (books/bib/clear)
  (skg/search-file-do books/bib/regex
    (books/bib/create-overlay (match-beginning 0) (match-end 0))
    ))

(defun books/bib/clear ()
  (remove-overlays (point-min) (point-max) 'books/bib t)
  (setq books/bib/overlays nil)
  )

(defun books/bib/shutdown ()
  (books/bib/clear)
  )

(defun books/bib/startup ()
  (books/bib/list-citations)
  )

(defun books/bib/startup-or-shutdown ()
  (if (symbol-value 'books/bib-mode)
      (books/bib/startup)
    (books/bib/shutdown)
    ))

(defun books/bib/goto (type)
  (let ((sorted) (sorted-reverse) (next) (prev))
    (when books/bib/overlays
      (setq sorted (--map (overlay-start it) books/bib/overlays))
      (setq sorted (--sort (< it other) sorted))
      (setq sorted-reverse (reverse sorted))
      (setq next (--find-index (> it (point)) sorted))
      (setq prev (--find-index (< it (point)) sorted-reverse))

      (when next
        (setq next (nth-value next sorted)))

      (when prev
        (setq prev (nth-value prev sorted-reverse)))

      (cond
       ((eq type 'first) (goto-char (overlay-start (-first-item books/bib/overlays))))
       ((eq type 'next)  (when next (goto-char next)))
       ((eq type 'next)  (when prev (goto-char prev)))
       ))
    ))

(defun books/bib/current-overlay ()
  (let* ((overlays (overlays-at (point)))
         (overlays (--filter (overlay-get it 'books/bib) overlays))
         )
    (when overlays
      (car overlays))
    ))

(defun books/bib/cite ()
  (interactive)
  (let ((overlay (books/bib/current-overlay)))
    (when overlay
      (save-excursion
        (save-restriction
          (narrow-to-region (overlay-start overlay) (overlay-end overlay))
          (replace-string "[" "" nil (point-min) (point-max))
          (replace-string "]" "" nil (point-min) (point-max))
          (mark-whole-buffer)
          (wrap-region-with "\\cite{" "}")
          (save-buffer)
          ))
      (books/bib/list-citations)
      (books/bib/goto 'next))
    ))

(define-key books/bib-mode-keymap (kbd "<home>") '(lambda () (interactive) (books/bib/goto 'first)))
(define-key books/bib-mode-keymap (kbd "<next>") '(lambda () (interactive) (books/bib/goto 'next)))
(define-key books/bib-mode-keymap (kbd "<prior>") '(lambda () (interactive) (books/bib/goto 'prev)))
(define-key books/bib-mode-overlay-keymap (kbd "r") 'books/bib/cite)
