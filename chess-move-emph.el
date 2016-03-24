(books/require 'books/chess-line)

(books/defkeymap books/chess-move-emph-mode-map  "Keymap to set move emphasis")
(define-minor-mode books/chess-move-emph-mode
  "Minor mode to set move emphasis"
  :lighter " emph"
  :keymap books/chess-move-emph-mode-map
  (cme/startup-and-shutdown)
  )

(defun cme/startup-and-shutdown ()
  (if (symbol-value 'books/chess-move-emph-mode)
      (cme/mode-startup)
    (cme/mode-shutdown)
    ))

(defun cme/mode-shutdown ()
  ;; (highline-mode -1)
  (remove-overlays (point-min) (point-max)))

(defun cme/mode-startup ()
  (let ((regex "\\(ncg\\|ml\\){[^}]*}"))
    ;; (highline-mode 1)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regex (point-max) t)
        (cme/overlay-mainline (match-beginning 0) (match-end 0)))
      )))

(defun cme/overlay-mainline (begin end)
  (let ((ol (make-overlay begin end (current-buffer) nil t)))
    (overlay-put ol 'mainline t)
    (overlay-put ol 'keymap books/chess-move-emph-mode-map)
    ))

(defun cme/overlays (&optional start)
  (let ((olays))
    (unless start (setq start (point-min)))
    (setq olays (overlays-in start (point-max)))
    (setq olays (--filter (and (overlay-get it 'mainline) (< (overlay-start it) (overlay-end it))) olays))
    olays))

(defun cme/next-mainline ()
  (interactive)
  (let ((start (point-min)) (olay (car (overlays-at (point)))) (olays))
    (when olay
      (setq start (overlay-end olay)))
    (setq olays (cme/overlays start))
    (when olays
      (setq olay (--min-by (> (overlay-start it) (overlay-start other)) olays))
      (goto-char (overlay-start olay)))
    ))

(defun cme/goto-end ()
  (interactive)
  (let ((olay (car (overlays-at (point)))))
    (when olay
      (goto-char (1- (overlay-end olay))))
    ))

(defun cme/goto-start ()
  (interactive)
  (let ((olay (car (overlays-at (point)))))
    (when olay
      (goto-char (overlay-start olay))
      (re-search-forward "{"))
    ))

(define-key books/chess-move-emph-mode-map (kbd "<kp-add>") 'cme/next-mainline)
(define-key books/chess-move-emph-mode-map (kbd "<end>") 'cme/goto-end)
(define-key books/chess-move-emph-mode-map (kbd "<home>") 'cme/goto-start)
(define-key books/chess-move-emph-mode-map (kbd "1") 'cl/good-move)
(define-key books/chess-move-emph-mode-map (kbd "`") 'cl/bad-move)
