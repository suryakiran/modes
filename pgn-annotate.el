(defconst pgn-annotate/regex
  "\\\\\\(pgnannotate\\)\\({\\([^}]*\\)}\\)")

(books/defkeymap books/pgn-annotate-paste-mode-map "Keymap used in pasting pgn annotations")
(define-minor-mode books/pgn-annotate-paste-mode
  "Mode to paste pgn annotations"
  :lighter " pgn-annotate"
  :keymap books/pgn-annotate-paste-mode-map
  (books/pgn-annotate-paste-startup-or-shutdown)
  )

(defun books/pgn-annotate-paste-startup-or-shutdown ()
  (if (symbol-value 'books/pgn-annotate-paste-mode)
      (startup-pgn-annotate-paste-mode)
    (shutdown-pgn-annotate-paste-mode)))

(defun pgn/annotate/cleanup ()
  (interactive)
  (let ((start) (end) (annotations))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "\\\\pgnannotate *{" (point-max) t)
        (backward-char)
        (setq start (point))
        (forward-list)
        (setq end (point)))
      (and start end
           (save-restriction
             (narrow-to-region start end)
             (replace-regexp "\\$[0-9]+" "" nil (point-min) (point-max))
             (replace-regexp " +)" ")" nil (point-min) (point-max))
             (replace-string "... " "..." nil (point-min) (point-max))
             (replace-regexp " +" " " nil (point-min) (point-max))
             (goto-char (point-min))
             (while (re-search-forward "(" (point-max) t)
               (backward-char)
               (setq start (point))
               (forward-list)
               (setq end (point))
               (add-to-list 'annotations (buffer-substring-no-properties start end) t))
             (setq annotations (--map (replace-regexp-in-string "[()]" "" it) annotations))
             (books/replace-text-in-range (point-min) (point-max) (format "{%s}" (s-join " " annotations)))
             (save-buffer)
             ))
      )
    ))

(defun pgn/annotate/delete ()
  (interactive)
  (let ((start) (end))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "\\\\pgnannotate *{" (point-max) t)
        (setq start (match-beginning 0))
        (backward-char)
        (forward-list)
        (setq end (point))
        (delete-region start end)
        (save-buffer))
      )))

(defun startup-pgn-annotate-paste-mode ()
  (pgn/annotate/cleanup)
  )

(defun shutdown-pgn-annotate-paste-mode ()
  (pgn/annotate/delete)
  )

;; (define-key books/chess-pgn-mode-map (kbd "<f12>") 'pgn/show-next-pgn)
;; (define-key books/chess-pgn-mode-map (kbd "h") 'pgn/wrap-newgame-hide-moves)
;; (define-key books/chess-pgn-mode-map (kbd "n") 'pgn/wrap-newgame)
;; (define-key books/chess-pgn-mode-map (kbd "<kp-enter>") 'pgn/wrap-mainline)
;; (define-key books/chess-pgn-mode-map (kbd "`") 'pgn/space-after-dot)
;; (define-key books/chess-pgn-mode-map (kbd "<f1>") 'pgn/mark-next-move)
;; (define-key books/chess-pgn-mode-map (kbd "<kp-add>") '(lambda () (interactive) (chess/board-after-move) (newline)))
;; (define-key books/chess-pgn-mode-map (kbd "z") 'undo)
;; (define-key books/chess-pgn-mode-map (kbd "<escape>") '(lambda () (interactive) (kill-buffer)))

;; (define-key books/chess-pgn-mode-map (kbd "s-a") 'pgn/remove-annotations)
;; (define-key books/chess-pgn-mode-map (kbd "s-n") 'pgn/goto-next-game)
;; (define-key books/chess-pgn-mode-map (kbd "s-1") 'pgn/goto-first-game)
;; (define-key books/chess-pgn-mode-map (kbd "s-0") 'pgn/goto-last-game)

;; (define-key books/pgn-paste-mode-map (kbd "<f1>") 'pgn/insert-next-move)
;; (define-key books/pgn-paste-mode-map (kbd "<f2>") 'pgn/cleanup-pgn-info-in-buffer)
;; (define-key books/pgn-paste-mode-map (kbd "<delete>") 'kill-whole-line)
;; (define-key books/pgn-paste-mode-map (kbd "b") 'chess/black-won)
;; (define-key books/pgn-paste-mode-map (kbd "w") 'chess/white-won)
;; (define-key books/pgn-paste-mode-map (kbd "d") 'chess/game-draw)
;; (define-key books/pgn-paste-mode-map (kbd "r") 'bam)
;; (define-key books/pgn-paste-mode-map (kbd "z") 'pgn/undo-paste)

(setq auto-mode-alist 
      (append '(
                ("\\.pgn\\'" . books/chess-pgn-mode))
              auto-mode-alist))

(provide 'books/chess-pgn-annotate)
