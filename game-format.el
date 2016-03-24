(books/require 'books/chess)
(books/require 'books/chess-line)
(books/require 'books/chess-pgn)

(defconst chess/game-format/regex "\\\\gameformat *{")

(books/defkeymap chess/game-format-mode-keymap "Keymap used in chess/game-format mode")

(define-minor-mode chess/game-format-mode
  "Some Documentation"
  :lighter " gf"
  :keymap chess/game-format-mode-keymap
  (chess/game-format/startup-or-shutdown)
  )

(defun chess/game-format/startup-or-shutdown ()
  (if (symbol-value 'chess/game-format-mode)
      (chess/game-format/startup)
    (chess/game-format/shutdown)
    ))

(defun chess/game-format/shutdown ()
  (let ((ranges))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward chess/game-format/regex (point-max) t)
        (add-to-list 'ranges (cons (match-beginning 0) (match-end 0)))
        (backward-char)
        (forward-list)
        (add-to-list 'ranges (cons (1- (point)) (point)))
        )
      (--map (delete-region (car it) (cdr it)) ranges)
      (save-buffer))
    ))

(defun chess/game-format/ranges ()
  (let ((start) (end) (ranges))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward chess/game-format/regex (point-max) t)
        (setq start (point))
        (backward-char)
        (forward-list)
        (setq end (1- (point)))
        (add-to-list 'ranges (cons start end))
        ))
    ranges))
              

(defun chess/game-format/startup ()
  (let ((ranges (chess/game-format/ranges)))
    (--map (chess/game-format/replace-game-format-text (car it) (cdr it)) ranges)
    ))

(defun chess/game-format/replace-game-format-text (begin end)
  (save-restriction
    (narrow-to-region begin end)
    (chess/game-format/mark-text-regions)
    (books/replace-text-in-range (point-min) (point-max) (chess/game-format/output-text))
    (pgn/nag-to-annotation t)
    ))

(defun chess/game-format/mark-text-in-parens (type &rest properties)
  (let ((start) (end))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward type (point-max) t)
        (setq start (point))
        (backward-char)
        (forward-list)
        (setq end (1- (point)))
        (--map (put-text-property start end it t) (-flatten properties)))
      (save-buffer))
    ))
  
(defun chess/game-format/mark-text-regions ()
  (let ((brace-ranges) (paren-ranges))
    (remove-list-of-text-properties (point-min) (point-max) '(mainline comment variation))
    (put-text-property (point-min) (point-max) 'mainline t)
    (setq brace-ranges (books/get-paren-ranges "{" (point-min) (point-max)))
    (setq paren-ranges (books/get-paren-ranges "(" (point-min) (point-max)))
    (--map (progn
             (put-text-property (car it) (cdr it) 'variation t)
             (remove-text-properties (car it) (cdr it) '(mainline nil)))
           paren-ranges)
    (--map (progn
             (put-text-property (car it) (cdr it) 'comment t)
             (remove-text-properties (car it) (cdr it) '(variation nil mainline nil)))
           brace-ranges)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "[{}()]" (point-max) t)
        (remove-list-of-text-properties (1- (point)) (point) '(mainline variation comment))
        (put-text-property (1- (point)) (point) 'delim t)))
    (save-buffer)
    ))
    
(defun chess/game-format/text-in-range (start end prop)
  (unless (eq prop 'delim)
    (let ((text))
      (setq text (buffer-substring-no-properties start end))
      (setq text (s-replace "\n" " " text))
      (setq text (s-trim text))
      (when (s-present? text)
        (cond
         ((eq prop 'mainline) (format "\n\n\\ml{%s}\n\n" text))
         ((eq prop 'variation) (format "\\vone{%s}" text))
         ((eq prop 'comment) text))
        ))
    ))

(defun chess/game-format/property-at-point (cur-point)
  (let* ((properties (text-properties-at cur-point))
         (mainline (-elem-index 'mainline properties))
         (variation (-elem-index 'variation properties))
         (comment (-elem-index 'comment properties))
         (delim (-elem-index 'delim properties))
         )
    (cond
     (mainline 'mainline)
     (variation 'variation)
     (comment 'comment)
     (delim 'delim)
     )
    ))

(defun chess/game-format/output-text ()
  (let ((cur-prop) (cur-pos) (next-pos) (text) (output))
    (setq cur-pos (point-min))
    (while cur-pos
      (setq cur-prop (chess/game-format/property-at-point cur-pos))
      (setq next-pos (next-single-property-change cur-pos cur-prop))
      (when next-pos
        (setq text (chess/game-format/text-in-range cur-pos next-pos cur-prop))
        (and text (add-to-list 'output text t)))
      (setq cur-pos next-pos))
    
    (and output (chess/game-format/write-to-string output))
    ))

(defun chess/game-format/write-to-file (lines)
  (with-temp-file "skg.output"
    (insert (s-join " " lines))
    (pgn/nag-to-annotation nil))
  (with-current-buffer "skg.output"
    (skg/revert-buffer))
  )

(defun chess/game-format/write-to-string (lines)
  (let ((regex (cdr (assoc 'mainline cl/moves-type-alist))) (text)
        (number-reg "^[0-9]+\\.") (last))
    (with-temp-buffer
      (insert (s-join " " lines))
      (replace-regexp " +\\([.,]\\)" "\\1" nil (point-min) (point-max))
      (replace-regexp "\\.\\.\\. *" "..." nil (point-min) (point-max))
      (set-fill-column 90)
      (fill-region (point-min) (point-max) nil nil)

      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "ml{" (point-max) t)
          (replace-match "ncg{")
          (setq last (car (s-slice-at "[wb]" (car (chess/last-move-num)))))
          (while (re-search-forward regex (point-max) t)
            (setq text (match-string-no-properties 3))
            (unless (string-match-p number-reg text)
              ;; (message (format "Text is %s" text))
              (setq text (format "%s...%s" last text))
              (replace-match (format "\\\\\\1{%s}" text)))
            (setq last (car (s-slice-at "[wb]" (car (chess/last-move-num))))))
          ))
      (buffer-string))
    ))

(defun ielm/test ()
  (let ((start) (end) (cur-prop) (cur-pos) (next-pos))
    (with-current-buffer "test.tex"
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward chess/game-format/regex (point-max) t)
          (setq start (point))
          (backward-char)
          (forward-list)
          (backward-char)
          (setq end (point))))
      (ielm/test2 start end))
    ))

(defun chess/game-format/invalid-vone ()
  (interactive)
  (re-search-forward "\\\\vone{[KQRNBa-h"))

(define-key chess/game-format-mode-keymap (kbd "<f5>") 'chess/game-format/invalid-vone)

(provide 'books/chess/game-format)
