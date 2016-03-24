(books/require 'evil)
(books/require 'show-book)

(defvar pgn/game nil "Entire game as string")
(defvar pgn/index 0 "Index of current pgn segment to be pasted")

(defconst pgn/regex
  "\\\\\\(pgn\\)\\({\\([^}]*\\)}\\)")

(books/new-mode "chess-pgn" fundamental-mode "PGN"
                (require 'books/chess-line)
                (setq font-lock-defaults '(cl/keywords))
                (turn-off-smartparens-mode)
                (turn-on-wrap-region-mode)
                )


(defun pgn/reset-segments ()
  (setq pgn/index 0)
  (setq pgn/game '())
  )

(defun pgn/save-info ()
  (interactive)
  (when (region-active-p)
    (pgn/reset-segments)
    (setq pgn/game (--remove (string-equal it "%") (s-lines (buffer-substring-no-properties
                                                             (region-beginning) (region-end)))))
    ))

(defun pgn/cleanup-current ()
  (let ((range) (text))
  (goto-char (point-min))
  (while (re-search-forward "\\\\\\(ncgh\\|ncg\\|ml\\){\\([^}]*\\)}" (point-max) t)
    (setq range (cons (match-beginning 2) (match-end 2)))
    (setq text (match-string-no-properties 2))
    (setq text (replace-regexp-in-string "[[:space:]]+" " " text))
    (books/replace-text-in-range (car range) (cdr range) text)
    )
  (goto-char (point-min))
  (flush-lines "^[[:space:]]*$")
  (setq buf (buffer-substring-no-properties (point-min) (point-max)))
  (setq pgn/game (--remove (string-equal it "%") (s-lines buf)))
  ))

(defun pgn/show-next-pgn ()
  (interactive)
  (when (buffer-narrowed-p)
    (pgn/cleanup-current)
    (goto-char (point-max))
    (widen)
    (replace-regexp "[59][DZB])?" "N" nil (point-min) (point-max))
    (save-buffer))
  (if (re-search-forward pgn/regex (point-max) t)
      (progn
        (narrow-to-region (match-beginning 2) (match-end 2))
        (goto-char (point-min))
        )
    (kill-buffer)))

(defun pgn/no-space-after-three-dots ()
    (goto-char (point-min))
    (while (re-search-forward "\\.\\.\\. " (point-max) t)
      (replace-match "...")))

(defun pgn/delete-leading-whitespace ()
  (let ((line (thing-at-point 'line)))
    (setq line (s-trim-left line))
    (books/replace-text-in-range (line-beginning-position) (line-end-position) line)
  ))

(defun pgn/wrap-region (type)
  (let ((left (format "\\%s{" (symbol-name type))) (right "}") (text) (move-number) (black-move) (rem-text))
    (when (region-active-p)
      (setq text (buffer-substring-no-properties (region-beginning) (region-end)))
      (setq move-number (pgn/last-move-number text))
      (save-excursion
        (unless (looking-at (format "[[:space:]]*%d." (1+ move-number)))
          (setq black-move t)))
      (wrap-region-with left right)
      (setq rem-text (s-trim (buffer-substring-no-properties (point) (point-max))))
      (when (> (length rem-text) 0)
        (newline) (insert "%") (newline)
        (pgn/delete-leading-whitespace)
        (save-excursion
          (when black-move
            (insert (format "%d..." move-number))
            (pgn/no-space-after-three-dots)
            )))
      (save-buffer)
      )))

(defun pgn/wrap-newgame-hide-moves ()
  (interactive)
  (pgn/wrap-region 'ncgh))

(defun pgn/wrap-newgame ()
  (interactive)
  (pgn/wrap-region 'ncg))

(defun pgn/wrap-mainline ()
  (interactive)
  (pgn/wrap-region 'ml))

(defun pgn/space-after-dot ()
  (interactive)
  (replace-regexp "[{}]" "" nil (point-min) (point-max))
  (chess/cleanup-move-number (point-min) (point-max))
  (save-buffer)
  )

(defun pgn/last-move-number (text)
  (let ((start) (num))
  (while (string-match "\\([0-9]+\\)\\." text start)
    (setq num (string-to-number (match-string 1 text)))
    (setq start (match-end 0))
    )
  num))

(defun pgn/at-end ()
  (let ((p (point)) (m (mark)))
    (or (= p (point-max)) (= m (point-max)))))

(defun pgn/mark-next-move ()
  (interactive)
  (books/mark-next-word)
  (when (looking-back "[[:space:]]+[0-9]*")
    (books/mark-next-word))
  (catch 'loop-exit
    (while (not (pgn/at-end))
      (if (not (looking-at "[[:space:]]+"))
          (books/mark-next-word)
        (throw 'loop-exit nil))))
  )

(defun pgn/remove-text-in-parens (left-paren)
  (let ((begin) (end))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward left-paren (point-max) t)
        (backward-char 1)
        (setq begin (point))
        (forward-list)
        (setq end (point))
        (delete-region begin end)
        ))
    ))

(defun pgn/get-moves-regions ()
  (let ((begin) (end) (regions '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^1\\." (point-max) t)
        (setq begin (match-beginning 0))
        (if (re-search-forward "^\\[Event" (point-max) t)
            (setq end (1- (match-beginning 0)))
          (setq end (point-max)))
        (add-to-list 'regions (cons begin end))))
    regions))

(defun pgn/next-set ()
  (let ((ret))
    (when pgn/game
      (setq ret (nth pgn/index pgn/game))
      (setq ret (s-trim ret))
      (when (length ret)
        (setq pgn/index (1+ pgn/index))))
    ret))

(books/defkeymap books/pgn-paste-mode-map "Keymap used in pasting pgn info")
(define-minor-mode books/pgn-paste-mode
  "Mode to paste pgn segments"
  :lighter " pgn-paste"
  :keymap books/pgn-paste-mode-map
  ;; (books/chess-book-mode -1)
  (books/pgn-paste-startup-or-shutdown)
  )

(defun books/pgn-paste-startup-or-shutdown ()
  (if (symbol-value 'books/pgn-paste-mode)
      (startup-pgn-paste-mode)
    (shutdown-pgn-paste-mode)))

(defun startup-pgn-paste-mode ()
  (let ((begin) (end))
    (books/evil-mode t)
    (show-page-mode t)
    (if (region-active-p)
        (pgn/save-info)
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "\\\\ncg{" (point-max) t)
          (setq begin (match-beginning 0))
          (while (re-search-forward "\\\\ml{[^}]*}" (point-max) t)
            (setq end (match-end 0))))
        (and begin end (progn
                         (set-mark begin)
                         (pgn/save-info)))
        ))
    ))

(defun shutdown-pgn-paste-mode ()
  (pgn/cleanup-pgn-info-in-buffer)
  (pgn/reset-segments)
  (books/evil-mode -1)
  )

(defun pgn/undo-paste ()
  (interactive)
  (unless (eq pgn/index 0)
    (undo)
    (setq pgn/index (1- pgn/index))
  ))

(defun pgn/insert-next-move ()
  (interactive)
  (let ((line))
    (setq line (pgn/next-set))
    (when line
      (if (s-contains? "Board" line)
          (insert (format "%s\n%%\n" line))
        (insert (format "%%\n%s\n%%\n" line)))
      )))

(defun pgn/cleanup-pgn-info-in-buffer ()
  (interactive)
  (let ((begin) (end) (first) (second))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "\\\\ncg{" (point-max) t)
        (setq first (match-beginning 0))
        (when (re-search-forward "\\\\ncg{" (point-max) t)
          (setq second (match-beginning 0))))
      (and first second
           (save-excursion
             (goto-char (point-min))
             (setq begin first)
             (while (re-search-forward "\\\\ml{" second t)
               (setq end (line-end-position)))
             (and begin end
                  (progn
                    (delete-region begin end)
                    (pgn/reset-segments)
                    (books/pgn-paste-mode -1)
                    (books/chess-book-mode +1)
                    (save-buffer)))
             ))
      )))

(defun pgn/remove-text-in-parens (left-paren)
  (let ((begin) (end))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward left-paren (point-max) t)
        (backward-char 1)
        (setq begin (point))
        (forward-list)
        (setq end (point))
        (delete-region begin end)
        ))
    ))

(defun pgn/get-moves-regions ()
  (let ((begin) (end) (regions '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^1\\." (point-max) t)
        (setq begin (match-beginning 0))
        (if (re-search-forward "^\\[Event" (point-max) t)
            (setq end (1- (match-beginning 0)))
          (setq end (point-max)))
        (add-to-list 'regions (cons begin end))))
    regions))

(defun pgn/remove-annotations ()
  (interactive)
  (let ((begin) (end))
  (pgn/remove-text-in-parens "(")
  (pgn/remove-text-in-parens "{")
  (replace-regexp "\\$[0-9]+ *" " " nil (point-min) (point-max))
  (replace-regexp "[0-9]+\\.\\.\\." "" nil (point-min) (point-max))
  (replace-regexp "^ +" "" nil (point-min) (point-max))
  (replace-regexp " +$" "" nil (point-min) (point-max))
  (--map (flush-lines "^ *$" (car it) (cdr it)) (pgn/get-moves-regions))
  (--map (progn
           (goto-char (cdr it))
           (newline)) (pgn/get-moves-regions))
  (set-fill-column 90)
  
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^1\\." (point-max) t)
      (fill-paragraph t)))
  (save-buffer)
  ))

(defun pgn/goto-next-game ()
  (interactive)
  (let ((recenter-positions '(middle)))
  (when (re-search-forward "^1\\." (point-max) t)
    (recenter-top-bottom))
  ))

(defun pgn/goto-first-game ()
  (interactive)
  (goto-char (point-min))
  (pgn/goto-next-game))

(defun pgn/goto-last-game ()
  (interactive)
  (let ((recenter-positions '(middle)))
    (goto-char (point-max))
    (when (re-search-backward "^1\\." (point-min) t)
      (recenter-top-bottom))
    ))

;; nag Numeric Annotation Glyph
(defun pgn/nag-to-annotation (save &optional region)
  (let ((start) (end))
    (if region
        (progn
          (setq start (car region))
          (setq end (cdr region)))
      (setq start (point-min))
      (setq end (point-max)))
    
    (save-restriction
      (narrow-to-region start end)
      (replace-regexp " *\\$1" "!" nil (point-min) (point-max))
      (replace-regexp " *\\$2" "?" nil (point-min) (point-max))
      (replace-regexp " *\\$3" "!!" nil (point-min) (point-max))
      (replace-regexp " *\\$4" "??" nil (point-min) (point-max))
      (replace-regexp " *\\$5" "!?" nil (point-min) (point-max))
      (replace-regexp " *\\$6" "?!" nil (point-min) (point-max))
      (replace-string "#" "\\#" nil (point-min) (point-max))
      )
    (when save (save-buffer))
    ))

(define-key books/chess-pgn-mode-map (kbd "<f12>") 'pgn/show-next-pgn)
(define-key books/chess-pgn-mode-map (kbd "h") 'pgn/wrap-newgame-hide-moves)
(define-key books/chess-pgn-mode-map (kbd "n") 'pgn/wrap-newgame)
(define-key books/chess-pgn-mode-map (kbd "<kp-enter>") 'pgn/wrap-mainline)
(define-key books/chess-pgn-mode-map (kbd "`") 'pgn/space-after-dot)
(define-key books/chess-pgn-mode-map (kbd "<f1>") 'pgn/mark-next-move)
(define-key books/chess-pgn-mode-map (kbd "<kp-add>") '(lambda () (interactive) (chess/board-after-move) (newline)))
(define-key books/chess-pgn-mode-map (kbd "z") 'undo)
(define-key books/chess-pgn-mode-map (kbd "<escape>") '(lambda () (interactive) (kill-buffer)))

(define-key books/chess-pgn-mode-map (kbd "s-a") 'pgn/remove-annotations)
(define-key books/chess-pgn-mode-map (kbd "s-n") 'pgn/goto-next-game)
(define-key books/chess-pgn-mode-map (kbd "s-1") 'pgn/goto-first-game)
(define-key books/chess-pgn-mode-map (kbd "s-0") 'pgn/goto-last-game)

(define-key books/pgn-paste-mode-map (kbd "f") 'pgn/insert-next-move)
(define-key books/pgn-paste-mode-map (kbd "<f2>") 'pgn/cleanup-pgn-info-in-buffer)
(define-key books/pgn-paste-mode-map (kbd "<delete>") 'kill-whole-line)
(define-key books/pgn-paste-mode-map (kbd "b") 'chess/black-won)
(define-key books/pgn-paste-mode-map (kbd "w") 'chess/white-won)
(define-key books/pgn-paste-mode-map (kbd "d") 'chess/game-draw)
(define-key books/pgn-paste-mode-map (kbd "r") 'bam)
(define-key books/pgn-paste-mode-map (kbd "z") 'pgn/undo-paste)

(setq auto-mode-alist 
      (append '(
                ("\\.pgn\\'" . books/chess-pgn-mode))
              auto-mode-alist))



(books/defkeymap chess-pgn-to-move-mode-keymap "Keymap used in chess-pgn-to-move mode")

(define-minor-mode chess-pgn-to-move-mode
  "Some Documentation"
  :lighter " cptm"
  :keymap chess-pgn-to-move-mode-keymap
  (chess-pgn-to-move/startup-or-shutdown)
  )

(defun chess-pgn-to-move/startup-or-shutdown ()
  (if (symbol-value 'chess-pgn-to-move-mode)
      (chess-pgn-to-move/startup)
    (chess-pgn-to-move/shutdown)
    ))

(defun chess-pgn-to-move/startup ()
  )

(defun chess-pgn-to-move/shutdown ()
  )

(define-key chess-pgn-to-move-mode-keymap (kbd "h") 'pgn/wrap-newgame-hide-moves)
(define-key chess-pgn-to-move-mode-keymap (kbd "n") 'pgn/wrap-newgame)
(define-key chess-pgn-to-move-mode-keymap (kbd "<kp-enter>") 'pgn/wrap-mainline)
(define-key chess-pgn-to-move-mode-keymap (kbd "`") 'pgn/space-after-dot)
(define-key chess-pgn-to-move-mode-keymap (kbd "<f1>") 'pgn/mark-next-move)
(define-key chess-pgn-to-move-mode-keymap (kbd "<kp-add>") '(lambda () (interactive) (chess/board-after-move) (newline)))
(define-key chess-pgn-to-move-mode-keymap (kbd "z") 'undo)
(define-key chess-pgn-to-move-mode-keymap (kbd "b") 'chess/black-won)
(define-key chess-pgn-to-move-mode-keymap (kbd "w") 'chess/white-won)
(define-key chess-pgn-to-move-mode-keymap (kbd "d") 'chess/game-draw)


(provide 'books/chess-pgn)
