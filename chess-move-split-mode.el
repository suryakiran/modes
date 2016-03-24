(require 'wrap-region)

(books/defkeymap chess-move-split-mode-keymap "Keymap used in chess-move-split mode")

(define-minor-mode chess-move-split-mode
  "Some Documentation"
  :lighter " cms"
  :keymap chess-move-split-mode-keymap
  (chess-move-split/startup-or-shutdown)
  )

(defun chess-move-split/startup-or-shutdown ()
  (if (symbol-value 'chess-move-split-mode)
      (chess-move-split/startup)
    (chess-move-split/shutdown)
    ))

(defun chess-move-split/startup ()
  )

(defun chess-move-split/shutdown ()
  )

(defun cms/at-end ()
  (let ((p (point)) (m (mark)))
    (or (= p (point-max)) (= m (point-max)))))

(defun cms/mark-next-move ()
  (interactive)
  (books/mark-next-word)
  (when (looking-back "[[:space:]]+[0-9]*")
    (books/mark-next-word))
  (catch 'loop-exit
    (while (not (cms/at-end))
      (if (not (looking-at "\\([[:space:]]\\|\n\\)+"))
          (books/mark-next-word)
        (throw 'loop-exit nil))))
  )

(defun cms/wrap-region (type)
  (let ((left (format "\\%s{" (symbol-name type))) (right "}") (text) (move-number) (black-move) (rem-text))
    (when (region-active-p)
      (setq text (buffer-substring-no-properties (region-beginning) (region-end)))
      (setq move-number (cms/last-move-number text))
      (save-excursion
        (unless (looking-at (format "\n*[[:space:]]*%d\\." (1+ move-number)))
          (setq black-move t)))
      (save-restriction
        (narrow-to-region (region-beginning) (region-end))
        (replace-string "\n" " " nil (point-min) (point-max))
        (replace-regexp "\\([0-9]\\)\\.\\([OKQRNBa-h]\\)" "\\1. \\2" nil (point-min) (point-max))
        (replace-regexp "\\.\\.\\. +" "..." nil (point-min) (point-max))
        (replace-regexp " +" " " nil (point-min) (point-max))
        (replace-regexp "^ +" "" nil (point-min) (point-max))
        (mark-whole-buffer)
        
        (wrap-region-with left right)
        (goto-char (point-max))
        )
      (setq rem-text (s-trim (buffer-substring-no-properties (point) (point-max))))
      (when (> (length rem-text) 0)
        (newline) (insert "%") (newline)
        (cms/delete-leading-whitespace)
        (save-excursion
          (when black-move
            (insert (format "%d..." move-number))
            (cms/no-space-after-three-dots)
            )))
      (save-buffer)
      )))

(defun cms/no-space-after-three-dots ()
    (goto-char (point-min))
    (while (re-search-forward "\\.\\.\\. " (point-max) t)
      (replace-match "...")))

(defun cms/delete-leading-whitespace ()
  (let ((line (thing-at-point 'line)))
    (setq line (s-trim-left line))
    (books/replace-text-in-range (line-beginning-position) (line-end-position) line)
  ))

(defun cms/space-after-dot ()
  (interactive)
  (replace-regexp "[{}]" "" nil (point-min) (point-max))
  (chess/cleanup-move-number (point-min) (point-max))
  (save-buffer)
  )

(defun cms/last-move-number (text)
  (let ((start) (num))
  (while (string-match "\\([0-9]+\\)\\." text start)
    (setq num (string-to-number (match-string 1 text)))
    (setq start (match-end 0))
    )
  num))

(defun cms/wrap-newgame-hide-moves ()
  (interactive)
  (cms/wrap-region 'ncgh))

(defun cms/wrap-newgame ()
  (interactive)
  (cms/wrap-region 'ncg))

(defun cms/wrap-mainline ()
  (interactive)
  (cms/wrap-region 'ml))

(define-key chess-move-split-mode-keymap (kbd "f") 'cms/mark-next-move)
(define-key chess-move-split-mode-keymap (kbd "w") 'chess/white-won)
(define-key chess-move-split-mode-keymap (kbd "b") 'chess/black-won)
(define-key chess-move-split-mode-keymap (kbd "d") 'chess/game-draw)
(define-key chess-move-split-mode-keymap (kbd "h") 'cms/wrap-newgame-hide-moves)
(define-key chess-move-split-mode-keymap (kbd "n") 'cms/wrap-newgame)
(define-key chess-move-split-mode-keymap (kbd "m") 'cms/wrap-mainline)
(define-key chess-move-split-mode-keymap (kbd "C-d") 'kill-whole-line)
(define-key chess-move-split-mode-keymap (kbd "i") '(lambda () (interactive) (chess/board-after-move) (newline)))

(provide 'chess-move-split)
