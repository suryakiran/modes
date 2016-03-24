(books/require 'books/chess)

(books/defface chess/vone/mouse-face "Face used to show variations when mouse is over the variation" vone
               (:foreground "blue" :background "yellow"))

(books/defkeymap vone/overlay-keymap "Keymap for vone overlays")
(books/defkeymap chess/vone-mode-map "Keymap used in changing variation styles")
(define-minor-mode chess/vone-mode
  "Mode to change variation styles"
  :lighter " vone"
  :keymap chess/vone-mode-map
  (chess/vone/startup-or-shutdown)
  )

(defvar chess/vone/overlay-fg-colors nil)
(defvar vone/regex "\\\\\\(vone\\|vtwo\\|vthree\\|vfour\\|vfive\\|vall\\|vmain\\){\\([^}]*\\)}"
  "Regex to search variation")

(defalias 'fg-colors 'chess/vone/overlay-fg-colors)

(defun shutdown-vone-mode ()
  (remove-overlays (point-min) (point-max))
  ;; (highline-mode -1)
  )

(defun chess/vone/overlay (start end)
  (let ((ol (make-overlay start end (current-buffer) nil t)))
    (overlay-put ol 'keymap vone/overlay-keymap)
    (overlay-put ol 'priority 0)
    (overlay-put ol 'category 'variation)
    (overlay-put ol 'variation t)
    (overlay-put ol 'mouse-face 'chess/vone/mouse-face)
    ;; (overlay-put ol 'face 'chess/vone/mouse-face)
    (save-buffer)
  ))

(defun startup-vone-mode ()
  (let ((regex vone/regex))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regex (point-max) t)
        (chess/vone/overlay (match-beginning 0) (match-end 0))
        ))
    (vone/highlight-paren-region)
    ;; (highline-mode 1)
    (save-buffer)
    ))

(defun chess/vone/startup-or-shutdown ()
  (if (symbol-value 'chess/vone-mode)
      (startup-vone-mode)
    (shutdown-vone-mode)))

(defun chess/vone/reset-fg-colors ()
  (setq fg-colors
        '("yellow" "DeepPink" "cyan" "MediumPurple1" "SpringGreen1"
          "DarkOrange" "HotPink1" "RoyalBlue1" "OliveDrab" "red" "green")))

(defun chess/vone/next-overlay-color ()
  (let ((color (-first-item fg-colors)))
    (setq fg-colors (-remove-at 0 fg-colors))
    (setq fg-colors (-insert-at (length fg-colors) color fg-colors))
    color))
        

(defun vone/highlight-paren-region ()
  (interactive)
  (save-excursion
    (let ((beg) (end) (ol))
      (cond
       ((looking-at "(") (backward-char 1))
       (t (re-search-backward "(" (point-min) t))
       )

      (chess/vone/reset-fg-colors)
      (while (re-search-forward "(" (point-max) t)
        (backward-char 1)
        (setq beg (point))
        (forward-list)
        (setq end (point))
        (setq text (buffer-substring-no-properties beg end))
        (when (s-contains? "\\vone" text)
          (setq ol (make-overlay beg end (current-buffer) nil t))
          (overlay-put ol 'priority 10)
          (overlay-put ol 'face (list (list ':foreground (chess/vone/next-overlay-color))))
          )))
    ))

(defun vone/overlays (&optional start)
  (let ((all-olays) (vone-olays))
    (unless start (setq start (point-min)))
    (setq all-olays (overlays-in start (point-max)))
    (setq all-lays (--filter (< (overlay-start it) (overlay-end it)) all-olays))
    (setq vone-olays (--filter (overlay-get it 'variation) all-olays))
    vone-olays))

(defun vone/overlays/type-variation ()
  (let ((olay (car (overlays-at (point) t))))
    (and olay (eq (overlay-get olay 'category) 'variation) olay)
    ))


(defun vone/next-overlay ()
  (interactive)
  (let ((olay) (olays) (start (point)))
    (setq olay (car (overlays-at (point) t)))
    (and olay (eq (overlay-get olay 'category) 'variation)
         (setq start (overlay-end olay)))
    (setq olays (vone/overlays start))
    (when olays
      (setq olay (--min-by (> (overlay-start it) (overlay-start other)) olays))
      (goto-char (overlay-start olay)))
    ))

(defun vone/set-variation (type)
  (let ((olay (vone/overlays/type-variation)))
    (and olay (replace-regexp "\\(vone\\|vtwo\\|vthree\\|vfour\\|vfive\\|vmain\\|vall\\|vopt\\)"
                              (symbol-name type) nil (overlay-start olay) (overlay-end olay)))
    (save-buffer)
    ))

(define-key chess/vone-mode-map (kbd "<f1>") 'chess/cycle-variations)
(define-key chess/vone-mode-map (kbd "<kp-add>") 'chess/next-variation)
;; (define-key chess/vone-mode-map (kbd "<f3>") 'chess/vone/highlight-paren-region)
(define-key chess/vone-mode-map (kbd "<f4>") 'books/remove-overlays)
(define-key chess/vone-mode-map (kbd "z") '(lambda () (interactive) (undo)))

(define-key vone/overlay-keymap (kbd "<kp-add>") 'vone/next-overlay)
(define-key vone/overlay-keymap (kbd "<tab>") 'vone/next-overlay)
(define-key vone/overlay-keymap (kbd "<kp-1>") '(lambda () (interactive) (vone/set-variation 'vone)))
(define-key vone/overlay-keymap (kbd "<kp-2>") '(lambda () (interactive) (vone/set-variation 'vtwo)))
(define-key vone/overlay-keymap (kbd "<kp-3>") '(lambda () (interactive) (vone/set-variation 'vthree)))
(define-key vone/overlay-keymap (kbd "<kp-4>") '(lambda () (interactive) (vone/set-variation 'vfour)))
(define-key vone/overlay-keymap (kbd "<kp-5>") '(lambda () (interactive) (vone/set-variation 'vfive)))
(define-key vone/overlay-keymap (kbd "<kp-6>") '(lambda () (interactive) (vone/set-variation 'vall)))
(define-key vone/overlay-keymap (kbd "<kp-7>") '(lambda () (interactive) (vone/set-variation 'vopt)))
(define-key vone/overlay-keymap (kbd "<kp-8>") '(lambda () (interactive) (vone/set-variation 'vmain)))
