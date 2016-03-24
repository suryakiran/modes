(provide 'books/chess-moves)
(require 'books/chess)
(require 'pcre2el)
(require 'regexp-opt)
(require 'thingatpt)
(require 'ht)

(defvar books/chess-moves-mode/old-mode-line-format () "Old modeline format")

(defvar chess-move-num-face 'chess-move-num-face "Face name used for move numbers")
(defvar chess-move-face 'chess-move-face "Face name used for moves")
(defvar chess-move-emph-face 'chess-move-emph-face "Face name used for move emphasis")
(defvar chess-move-last-mainline-face 'chess-move-last-mainline-face "Face used to display on modeline")

(defface chess-move-num-face
  '((t (:foreground "green")))
  "Face used to show chess move numbers"
  :group 'chess
  )

(defface chess-move-face
  '((t (:foreground "gold")))
  "Face used to show chess moves"
  :group 'chess
  )

(defface chess-move-emph-face
  '((t (:foreground "deep sky blue")))
  "Face used to show chess move emphasis"
  :group 'chess
  )

(defface chess-move-last-mainline-face
  '((t (:foreground "deep sky blue")))
  "Face used to show chess move emphasis"
  :group 'chess
  )

(defconst
  cm/move-regex
  "\\([RN][1-8]x?\\|[RN][a-h]x?\\|[KQNRB]x?\\|[a-h]x\\)?[a-h][1-8]")

(defconst
  cm/castle-move-regex
  "O-O\\(-O\\)?")

(defconst
  cm/move-number-regex
  "\\([0-9]+\\)\\.\\(\\.\\.\\| ?\\)")

(defconst
  cm/next-mainline-variation-regex
  "\\\\\\(ncg\\|ml\\|vone\\){[^}]*}"
  )

(defconst
  cm/next-mainline-regex
  "\\\\\\(ncg\\|ml\\){\\([^}]*\\)}"
  )

(defconst
  cm/next-variation-regex
  "\\\\\\(vone\\|vtwo\\|vthree\\|vfour\\|vfive\\|vall\\|vmain\\){[^}]*}"
  )

(defconst
  cm/dirty-move-regex
  "\\\\dirty{\\([^}]*\\)}"
  )

(setplist 'cm/move-type-regex
          '(
            mainline cm/next-mainline-regex
            variation cm/next-variation-regex
          ))
          

(setq cm/keywords (list
                   (cons cm/move-number-regex chess-move-num-face)
                   '("[\\?!\\+]" . chess-move-emph-face)
                   (cons cm/move-regex  chess-move-face)
                   (cons cm/castle-move-regex  chess-move-face)
                   ))

(setq chess-pieces "kqrnb")

(defun cm/to-next-move-number ()
  (interactive)
  (re-search-forward cm/move-number-regex (point-max) t)
  )

(defun cm/prev-mainline-variation ()
  (interactive)
  (when (= (point) (point-max)) (backward-char 1))
  (widen)
  (save-buffer)
  (if (re-search-backward cm/next-mainline-variation-regex (point-min) t)
      (progn
        (narrow-to-region (match-beginning 0) (match-end 0))
        (goto-char (point-min)))
    (kill-buffer)))

(defun cm/dirty ()
  (interactive)
  (get-text-property (point-min) 'dirty))

(defun cm/mainline ()
  (interactive)
  (let ((res))
    (save-excursion
      (goto-char (point-min))
      (setq res (re-search-forward "\\(ml\\|ncg\\){" (point-max) t))
      )
    res))

(defun cm/variation ()
  (interactive)
  (let ((res))
    (save-excursion
      (goto-char (point-min))
      (setq res (re-search-forward "v\\(one\\|two\\|three\\|four\\|five\\|all\\){" (point-max) t))
      )
    res))

(defun cm/change-dirty-text-to-move ()
  (interactive)
  (goto-char (point-min))
  (when (re-search-forward cm/dirty-move-regex (point-max) t)
    (replace-match "\\1"))
  (goto-char (point-max))
  (set-mark (point-min))
  (chess/move-from-text)
  (deactivate-mark)
  (remove-text-properties (point-min) (point-max) '(dirty nil))
  (save-buffer))

(defun cm/next-mainline-variation ()
  (interactive)
  (when (= (point) (point-min)) (forward-char 1))
  (if (cm/dirty)
      (progn
        (cm/change-dirty-text-to-move)
        (widen)
        (if (re-search-forward cm/dirty-move-regex (point-max) t)
            (progn
              (narrow-to-region (match-beginning 0) (match-end 0))
              (goto-char (point-min)))
          (kill-buffer)
          ))
    (progn
      (let ((regex))
        (cond
         ((cm/mainline) (setq regex cm/next-mainline-regex))
         ((cm/variation) (setq regex cm/next-variation-regex))
         )

        (widen)
        (save-buffer)
        (when regex
          (if (re-search-forward regex (point-max) t)
              (progn
                (narrow-to-region (match-beginning 0) (match-end 0))
                (goto-char (point-min)))
            (kill-buffer)))
        ))
    ))

(defun cm/to-upcase ()
  (when (-contains? (append chess-pieces nil) (char-before))
    (upcase-region (1- (point)) (point)))) 

(defun cm/to-next-move ()
  (interactive)
  (let ((cur-face (get-text-property (point) 'face)) (move) (exhausted) (cur-point (point)))
    (if (not (eq cur-face 'chess-move-face))
        (progn
          (setq move (text-property-any (point) (point-max) 'face 'chess-move-face)))
      (progn
        (while (and (not move) (not exhausted))
          (progn
            (setq cur-point (next-single-property-change cur-point 'face))
            (if (not cur-point) (setq exhausted t)
              (progn
                (when (eq (get-text-property cur-point 'face) 'chess-move-face)
                  (setq move cur-point)))))))
      )
    (when move (goto-char move))
    ))

(defun cm/board-after-move ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (forward-list)
    (books/new-line)
    (insert "%")
    (books/new-line)
    (chess/board-after-move)
    ))

(defun cm/three-dots-for-first-move ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward cm/move-number-regex (point-max) t)
      (goto-char (match-end 1))
      (insert " ")
      (backward-char)
      (chess/insert-three-dots))
    ))

(defun cm/to-next-pawn ()
  (interactive)
  (skg/case-sensitive
   (when (re-search-forward "[a-h][1-8a-hx]" (point-max) t)
     (goto-char (match-beginning 0)))
     ))

(defun cm/overwrite-current-char ()
  (interactive)
  (progn
    (overwrite-mode t)
    (self-insert-command 1)
    (overwrite-mode -1)
    ))

(defun cm/latest-mainline ()
  (interactive)
  (let ((ml "nil") (move) (beg) (end) (move-number-regex (format "[{ ]%s" cm/move-number-regex)))
    (save-excursion
      (save-restriction
        (widen)
        (when (re-search-backward cm/next-mainline-regex (point-min) t)
          (setq beg (match-beginning 0))
          (setq end (match-end 0))
          (goto-char (match-end 0))
          (setq beg (re-search-backward move-number-regex beg t))
          (setq ml (format "Last Move: %s" (buffer-substring-no-properties (1+ beg) (1- end))))
          )))
    (propertize ml 'face 'chess-move-last-mainline-face)))

(defun cm/modify-mode-line ()
  (add-to-list 'header-line-format '(:eval (cm/latest-mainline)) t))

(defun cm/replace-chars ()
  (interactive)
  (skg/case-sensitive
      (cond
       
       ((looking-at "[ce]") (cycle-chars "ec"))
       ((looking-at "[BN]")
        (progn
          (books/replace-text-in-range (point) (1+ (point)) "43.")
          (chess/dot-after-move-number)
          ))
       ((looking-at "[58]") (cycle-chars "85"))
       )))

(books/new-mode "chess-moves" fundamental-mode "Chess Moves"
                (setq font-lock-defaults '(cm/keywords))
                (setq books/chess-moves-mode/old-mode-line-format mode-line-format)
                (cm/modify-mode-line)
                ;; (persp-mode -1)
                (yas-minor-mode -1)
                (add-hook 'post-self-insert-hook 'cm/to-upcase nil 'local)
                )

(eval-after-load 'books/chess-moves
  '(progn
     (define-key books/chess-moves-mode-map (kbd "<f1>") 'cm/replace-chars)
     (define-key books/chess-moves-mode-map (kbd "<f2>") 'cm/next-mainline-variation)
     (define-key books/chess-moves-mode-map (kbd "S-<f2>") 'cm/prev-mainline-variation)
     (define-key books/chess-moves-mode-map (kbd "<f3>") 'cm/to-next-move)
     (define-key books/chess-moves-mode-map (kbd "<f4>") 'cm/to-next-pawn)
     (define-key books/chess-moves-mode-map (kbd "<f6>") 'cm/board-after-move)
     (define-key books/chess-moves-mode-map (kbd "k") 'cm/overwrite-current-char)
     (define-key books/chess-moves-mode-map (kbd "q") 'cm/overwrite-current-char)
     (define-key books/chess-moves-mode-map (kbd "r") 'cm/overwrite-current-char)
     (define-key books/chess-moves-mode-map (kbd "n") 'cm/overwrite-current-char)
     (define-key books/chess-moves-mode-map (kbd "b") 'cm/overwrite-current-char)
     (define-key books/chess-moves-mode-map (kbd "C-b") '(lambda () (interactive) (insert "b")))
     (define-key books/chess-moves-mode-map (kbd "<tab>") 'cm/to-next-move-number)
     (define-key books/chess-moves-mode-map (kbd "s-d") 'cm/three-dots-for-first-move)
     ))

(eval-after-load 'odersky-theme
  '(progn
     (custom-theme-set-faces
      'odersky
      '(chess-move-last-mainline-face ((t (:foreground "spring green"))))
      )))

(eval-after-load 'tango-2-theme
  '(progn
     (custom-theme-set-faces
      'tango-2
      '(chess-move-last-mainline-face ((t (:foreground "dark red"))))
      '(header-line ((t (:box (:line-width 2 :color "grey75" :style released-button) :foreground "dark red" :background "dark grey")))) 
      )))


(eval-after-load 'tango-theme
  '(progn
     (custom-theme-set-faces
      'tango
      '(chess-move-face ((t (:foreground "#a020f0"))))
      '(chess-move-num-face ((t (:foreground "dark red"))))
      '(chess-move-emph-face ((t (:foreground "dark green"))))
      '(header-line ((t (:box (:line-width 2 :color "grey75" :style released-button) :background "dark grey")))) 
      '(chess-move-last-mainline-face ((t (:foreground "NavyBlue"))))
      )))
