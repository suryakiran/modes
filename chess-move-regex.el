(provide 'books/chess-move-regex)
(require 'books/chess)
(require 'pcre2el)
(require 'regexp-opt)
(require 'thingatpt)
(require 'dash)
(books/require 'books/chess-line)

(defconst cmr/chess/num "[0-9]+")
(defconst cmr/castle "O-O\\(-O\\)?")
(defconst cmr/chess/pawn-promote "=[QNRB]")
(defconst cmr/chess/regular-move "[KQRNB]?[a-h]?x?[a-h][1-8]")
(defconst cmr/chess/rook-knight-special-move "[RN][1-8a-h]x?[a-h][1-8]")
(defconst cmr/chess/move (format "\\(%s\\|%s\\|%s\\)" cmr/castle cmr/chess/rook-knight-special-move cmr/chess/regular-move))
(defconst cmr/chess/move-emph "[+!?]+")
(defconst cmr/chess/move-promote-emph
  (format "%s\\(%s\\)?\\(%s\\)?" cmr/chess/move cmr/chess/pawn-promote cmr/chess/move-emph))
(defconst cmr/chess/white-only-move
  (format "%s\\. ?%s" cmr/chess/num cmr/chess/move-promote-emph))
(defconst cmr/chess/white-black-move
  (format "%s \\(%s\\)?" cmr/chess/white-only-move cmr/chess/move-promote-emph))
(defconst cmr/chess/black-only-move
  (format "%s\\.\\.\\.%s" cmr/chess/num cmr/chess/move-promote-emph))
(defconst cmr/chess/reg
  (format "\\(%s *\\|%s *\\|%s\\)"
          cmr/chess/black-only-move cmr/chess/white-black-move cmr/chess/white-only-move))
(defconst cmr/dirty/move
  (format "\\(\\.\\.\\.\\)?%s\\([—-]%s\\)?" cmr/chess/move-promote-emph cmr/chess/move-promote-emph))

(books/defface cmr/variation-face "Face used to show variation moves" cmr
               (:foreground "blue" :background "yellow"))

(books/defkeymap cmr/overlay-keymap "Keymap used on overlays")
(books/defkeymap books/chess-move-regex-mode-map "Keymap used in cmr minor mode")

(define-minor-mode books/chess-move-regex-mode
  "Minor mode to mark chess moves using regex"
  :lighter " cmr"
  :keymap books/chess-move-regex-mode-map
  (cmr/startup-or-shutdown)
  )

(defun cmr/startup-or-shutdown ()
  (if (symbol-value 'books/chess-move-regex-mode)
      (cmr/startup)
    (cmr/shutdown)
    ))

(defun cmr/startup ()
  (save-excursion
    (cmr/variations)
    (cmr/cleanup-overlays)
    ))

(defun cmr/shutdown ()
  (remove-overlays (point-min) (point-max))
  (remove-list-of-text-properties (point-min) (point-max) '(mainline variation dirty do-not-touch))
  )

(defun cmr/overlay (begin end)
  (let ((ol (make-overlay begin end (current-buffer) nil t)))
    (overlay-put ol 'keymap cmr/overlay-keymap)
    (overlay-put ol 'face 'cmr/variation-face)
  ol))

(defun cmr/change-dirty-texts (ranges)
  (--map (progn
           (books/replace-text-in-range (car it) (cdr it) (chess/move-from-text-as-string it))) ranges)
  (cmr/fill-paragraphs)
  (save-buffer))
    
(defun cmr/write-dirties ()
  (interactive)
  (let ((start (point-min)) (ranges) (start) (end) (done))
    (remove-overlays (point-min) (point-max))
    (remove-text-properties (point-min) (point-max) '(dirty nil))
    (save-buffer)
    (cmr/show-dirties)
    (setq start (point-min))
    (while (not done)
      (setq start (text-property-any start (point-max) 'dirty t))
      (if start
          (progn
            (setq end (next-single-property-change start 'dirty))
            (add-to-list 'ranges (cons start end))
            (setq start end)
            (and (equal end (point-max)) (setq done t))
            )
        (setq done t))
      )
    (remove-overlays (point-min) (point-max))
    (remove-text-properties (point-min) (point-max) '(dirty nil))
    (save-buffer)
    (cmr/change-dirty-texts ranges)
    ))

(defun cmr/variations ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward cmr/chess/reg (point-max) t)
      (unless (eq (get-text-property (point) 'mainline) t)
        (setq ol (cmr/overlay (match-beginning 0) (match-end 0)))))
    ))

(defun cmr/cleanup-overlays ()
  (let* ((new-ol) (ol (cmr/first-overlay)) (range) (cur-ol) (start) (end))
    (while ol
      (setq range (cmr/overlay-range))
      (setq start (car range))
      (setq end (cdr range))
      (goto-char end)
      (when (equal (char-before end) ? )
        (setq end (1- end)))
      (remove-overlays (car range) (cdr range))
      (cmr/overlay start end)
      (setq ol (cmr/next-overlay)))
    ))

(defun cmr/first-overlay ()
  (interactive)
  (cmr/next-overlay (point-min)))

(defun cmr/next-overlay (&optional start)
  (let ((olay))
    (unless start (setq start (point)))
    (setq olays (overlays-in start (point-max)))
    (setq olays (--filter (< (overlay-start it) (overlay-end it)) olays))
    (when olays
      (setq olay (--min-by (> (overlay-start it) (overlay-start other)) olays))
      (goto-char (overlay-start olay)))
    olay))

(defun cmr/overlay-range ()
  (let ((ol (car (overlays-at (point))))
        (start)
        (end)
        )
    (when (overlayp ol)
      (setq start (overlay-start ol)))

    (save-excursion
      (when start
        (while (overlayp ol)
          (setq end (overlay-end ol))
          (goto-char end)
          (setq ol (car (overlays-at (point)))))
        (when end
          (cons start end))))
    ))

(defun cmr/current-overlay ()
  (let ((olays (overlays-at (point))))
    (when olays (car olays))
    ))

(defun cmr/wrap-overlay (type)
  (let* ((olay (cmr/current-overlay))
         )
    (when olay
      (save-excursion
        (goto-char (overlay-end olay))
        (set-mark (overlay-start olay))
        (wrap-region-with (format "\\%s{" (symbol-name type)) "}")
        (unless (eq type 'dirty)
          (chess/dot-after-move-number)))
      (save-buffer))
    ))

(defun cmr/visit-next-overlay ()
  (interactive)
  (let ((olays (overlays-at (point))) (start (point)))
    (when olays
      (setq start (overlay-end (car olays))))
    (cmr/next-overlay start)
    ))

(defun cmr/wrap-overlays ()
  (interactive)
  (let ((ol (cmr/first-overlay)))
    (while ol
      (cmr/wrap-overlay 'vone)
      (setq ol (cmr/visit-next-overlay)))
    (remove-overlays (point-min) (point-max))
    ))

(defun cmr/extend-overlay-by-a-char ()
  (interactive)
  (let* ((overlay (cmr/current-overlay))
         (start (overlay-start overlay))
         (end (overlay-end overlay)))
    (save-excursion
      (cmr/overlay start (1+ end))
      (delete-overlay overlay)
      )
  ))

(defun cmr/show-dirties ()
  (interactive)
  (cmr/shutdown)
  (cm/set-mainline-text-properties 'do-not-touch)
  (cm/set-variation-text-properties 'do-not-touch)
  (save-buffer)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward cmr/dirty/move (point-max) t)
      (unless (get-text-property (point) 'do-not-touch)
        (cmr/overlay (match-beginning 0) (match-end 0))
        (add-text-properties (match-beginning 0) (match-end 0) '(dirty t))))
    (save-buffer)
    ))

(defun cmr/replace-text-in-dirties ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (cdr (assoc 'dirty cl/moves-type-alist)) (point-max) t)
      (books/replace-text-in-range (match-beginning 0) (match-end 0)
                                   (chess/move-from-text-as-string (cons (match-beginning 3) (match-end 3)) 'dirty))
      )
    (save-buffer)
    ))

(define-key books/chess-move-regex-mode-map (kbd "<home>") 'cmr/first-overlay)
(define-key books/chess-move-regex-mode-map (kbd "<return>") 'cmr/visit-next-overlay)
(define-key books/chess-move-regex-mode-map (kbd "v") '(lambda () (interactive) (cmr/wrap-overlay 'vone)))
(define-key books/chess-move-regex-mode-map (kbd "m") '(lambda () (interactive) (cmr/wrap-overlay 'ml)))
(define-key books/chess-move-regex-mode-map (kbd "d") '(lambda () (interactive) (cmr/wrap-overlay 'dirty)))
(define-key books/chess-move-regex-mode-map (kbd "n") '(lambda () (interactive) (cmr/wrap-overlay 'ncg)))
(define-key books/chess-move-regex-mode-map (kbd "h") '(lambda () (interactive) (cmr/wrap-overlay 'ncgh)))
(define-key books/chess-move-regex-mode-map (kbd "C-d") 'cmr/show-dirties)
(define-key books/chess-move-regex-mode-map (kbd "<kp-add>") 'cmr/extend-overlay-by-a-char)
(define-key books/chess-move-regex-mode-map (kbd "H-d") 'cmr/replace-text-in-dirties)

;; (eval-after-load 'tango-2-theme
;;   '(progn
;;      (custom-theme-set-faces
;;       'tango-2
;;       '(chess-move-last-mainline-face ((t (:foreground "dark red"))))
;;       '(header-line ((t (:box (:line-width 2 :color "grey75" :style released-button) :foreground "dark red" :background "dark grey")))) 
;;       )))


;; (eval-after-load 'tango-theme
;;   '(progn
;;      (custom-theme-set-faces
;;       'tango
;;       '(chess-move-face ((t (:foreground "#a020f0"))))
;;       '(chess-move-num-face ((t (:foreground "dark red"))))
;;       '(chess-move-emph-face ((t (:foreground "dark green"))))
;;       '(header-line ((t (:box (:line-width 2 :color "grey75" :style released-button) :background "dark grey")))) 
;;       '(chess-move-last-mainline-face ((t (:foreground "NavyBlue"))))
;;       )))
