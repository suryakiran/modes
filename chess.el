(provide 'books/chess)
(require 'pcre2el)
(require 'regexp-opt)
(require 'thingatpt)
(books/require 'master-book)
(books/require 'chess-players)
(books/require 'chess-hooks-and-functions)
(books/require 'chess-pgn)
(books/require 'highlight)

(defvar books/highlight/keymap nil "Keymap used in highlight regions")
(make-variable-buffer-local 'books/highlight/keymap)

(defun chess/wrap-region ()
  (interactive)
  (let* ((key (this-command-keys))
         (mode (or (books/major-mode) major-mode))
         (text (cdr (assoc key chess/moves-type-alist)))
         (fun) (pkey))
    (if (region-active-p)
        (progn
          (and key (setq pkey (ht-get chess/post-move-keys key)))
          (and pkey (setq fun (plist-get (ht-get chess/wrap-functions mode) pkey)))
          (wrap-region-with (format "\\%s{" text) "}")
          (and fun (fboundp fun) (funcall fun pkey))
          (save-buffer)
          )
      (progn
        (self-insert-command 1))
      )
    ))

(setq move-number (rxt-pcre-to-elisp "(^\\d*)\\.?"))
(setq dot-or-dots "\\.{1,3}")
(setq move-reg (rxt-pcre-to-elisp "[KQRNB]x?[a-h][1-8]|[a-h](x[a-h])?([1-7]|8=?[KQBNR]?)"))
(setq short-castle "O-O")
(setq long-castle "O-O-O")
(setq move-num-reg (rxt-pcre-to-elisp "(^\\d*\\.*)"))

(defun chess/wrap-variation ()
  )

(defun chess/vone ()
  (interactive)
  )

(defadvice books/mark-next-word (around books-mark-next-word-around-advice)
  (progn
    (books/as-word "?!+"
      ad-do-it
      )))

(ad-activate 'books/mark-next-word)

(defun chess/parenthesis-range ()
  (let (
        (start) (end) (range)
        )
  (save-excursion
    (unless (looking-at "{") (re-search-backward "{"))
    (setq start (point))
    (re-search-forward "}" (point-max) t)
    (backward-char)
    (setq end (point))
    (setq range (cons (+ start 1) end ))
    )
  range))

(defun chess/wrap-with-tag (tag)
  (let ((key (assoc tag chess/moves-type-alist)))
    (when key
      (sp-wrap-with-pair (format "\\%s{" (cdr key)))
      (chess/post-move-function tag 'wrap))
    ))

(defun chess/mainline ()
  (interactive)
  (unless (region-active-p) (skg/select-line))
  (save-restriction
    (narrow-to-region (region-beginning) (region-end))
    (when (comment-only-p (point-min) (point-max))
      (skg/operate-on-buffer 'comment-or-uncomment-region))
    (replace-string "\n" " " nil (point-min) (point-max))
    (mark-whole-buffer)
    (chess/wrap-with-tag "m")
  ))

  (defun chess/insert-three-dots ()
  (interactive)
  (let ((start (point)) (end))
    (save-excursion
      (setq end (- (re-search-forward "[[:alpha:]]") 1)))
    (books/replace-text-in-range start end "...")
    ))

(defun chess/remove-spaces (start end)
  (interactive "r")
  (let ((text))
    (setq text (s-replace-all '((" " . "")) (buffer-substring-no-properties start end)))
    (delete-region start end)
    (insert text)
    ))

(defun chess/cleanup ()
  (interactive)
  (save-excursion
    (replace-string "++" "\\#" nil (point-min) (point-max))
    (replace-string ". ... " "..." nil (point-min) (point-max))
    (replace-string "...." "..." nil (point-min) (point-max))
    (replace-string "0-0-0" "O-O-O" nil (point-min) (point-max))
    (replace-string "0-0" "O-O" nil (point-min) (point-max))
    ))


(defun is-first-move-by-black (s)
  (s-contains? "..." s))

(defun first-char (s)
  (char-to-string (aref s 0)))

(defun last-char (s)
  (char-to-string (aref s (- (length s) 1))))

(defun has-castling? (s)
  (s-contains? "O-O" s))

(defun has-good-move (s)
  (s-contains? "!" s))

(defun chess/move-number-index-end (text)
  (when (string-match move-num-reg text 0)
    (match-end 1)))

(defun chess/split-move-by-players (m)
  (let (
        (idx) (black-start)
        (white-move) (black-move) (res '())
        (text m) (start 0) (move-num)
        (move-idx (chess/move-number-index-end m))
        )

    (when move-idx
      (setq move-num (substring m 0 move-idx))
      (setq text (substring m move-idx (length m))))

    (if (has-castling? text)
        (progn
        (setq res (split-castling-move text))
        (push move-num res))
      
      (progn
        (setq idx (string-match move-reg text start))
        (setq black-start (string-match move-reg text (match-end 0)))

        (if black-start
            (progn
              (setq white-move (substring-no-properties text 0 black-start))
              (setq black-move (substring-no-properties text black-start (length text)))
              (push black-move res))
          (progn
            (setq white-move text)
            ))
          (push white-move res)
          (push move-num res)
          ))
    (setq res (s-join " " res))
    (setq res (s-replace "... " "..." res))
    res))

(defun chess/split-moves-by-numbers (s)
  (let* (
         (ss s)
         (first-move-number (chess/get-first-move-number ss))
         (num first-move-number)
         (res '()) (beg 0) (end)
         )

    (setq end (s-index-of (format "%d." num) ss))
    
    (while end
      (setq num (1+ num))
      (setq end (s-index-of (format "%d." num) ss))
      (if end
          (progn
            (push (substring-no-properties ss beg end) res)
            (setq ss (substring-no-properties ss end (length ss))))
        (push ss res))
      )
    (setq res (reverse res))
  res))

(defun split-castling-move (s)
  (let (
        (res '()) (idx)
        (idx-long (s-index-of "O-O-O" s))
        (idx-short (s-index-of "O-O" s))
        )    
    (if idx-long
        (if (= idx-long 0) ; long castling
            (progn
              (setq idx (string-match move-reg s 5))
              (when idx
                (push (substring-no-properties s idx (length s)) res))
              (push (substring-no-properties s 0 idx) res)
              )
          (progn
            (push (substring-no-properties s idx-long (length s)) res)
            (push (substring-no-properties s 0 idx-long) res)
            ))
      (if (= idx-short 0) ; Short Castling
          (progn
            (setq idx (string-match move-reg s 3))
            (when idx
              (push (substring-no-properties s idx (length s)) res))
            (push (substring-no-properties s 0 idx) res)
            )
        (progn
          (push (substring-no-properties s idx-short (length s)) res)
          (push (substring-no-properties s 0 idx-short) res)
          ))
      )
    res))

(defun chess/make-moves-in-parenthesis ()
  (interactive)
  (let ((range (chess/parenthesis-range)))
    (chess/make-moves (car range) (cdr range))
  ))

(defun chess/last-move-num ()
  (let ((start) (end) (white-move-number) (black-move-number) (ret) (hide))
    (save-excursion
      (setq start (re-search-backward "\\(ml\\|ncg\\|ncgh\\|hidemoves\\){" (point-min) t))
      (setq hide (match-string-no-properties 1))
      (setq end (re-search-forward "}"))
      (save-excursion
        (if (re-search-backward "[{ ]\\([0-9]+\\)\\. " start t)
            (progn
              (setq white-move-number (match-string-no-properties 1))
              (goto-char (match-end 0))
              (if (re-search-forward " [a-hBKNQR]" end t)
                  (setq ret (format "%sb" white-move-number))
                (setq ret (format "%sw" white-move-number))
                ))
          (when (re-search-backward "{\\([0-9]+\\)\\.\\.\\." start t)
            (setq ret (format "%sb" (match-string-no-properties 1))))
          )))
    (and ret (cons ret hide))
  ))

(defun chess/board-after-move ()
  (interactive)
  (let* ((ret (chess/last-move-num)) (hide (cdr ret)) (move (car ret)))
    (when ret
      (if (or (string-equal hide "ncgh") (string-equal hide "hidemoves"))
          (insert (format "\\BoardAfterMoveNoLabel{%s}" move))
        (insert (format "\\BoardAfterMove{%s}" move)))
      (execute-kbd-macro [return ?%])
      (save-buffer)
      )
    ))

(defun chess/last-name (input)
  (books/no-accented-string (-last-item (split-string input)))
  )

(defun chess/cleanup-move-number (beg end)
  (let ((reg "\\([0-9]\\.\\)\\([a-hKQRBNO]\\)") (str (buffer-substring-no-properties beg end)))
    (setq str (replace-regexp-in-string reg "\\1 \\2" str))
    (books/replace-text-in-range beg end str)
  ))

(defun chess/space-after-dot ()
  (interactive)
  (books/apply-on-paren-region 'chess/cleanup-move-number)
  )

(defun chess/make-moves (start end)
  (interactive "r")
  (let (
        (text (buffer-substring-no-properties start end))
        (moves) 
        (moves-list '()) (moves-list-as-string)
        )
    (setq text (chess/cleanup-moves text))
    (setq text (s-replace-all '(("{" . "") ("}" . "")) text))
    (setq moves (chess/split-moves-by-numbers text))
    (dolist (move moves)
      (push (chess/split-move-by-players move) moves-list)
      )
    (setq moves-list (reverse moves-list))
    (setq moves-list-as-string (s-join " " moves-list))
    (delete-region start end)
    (insert moves-list-as-string)
  ))

(defun chess/cleanup-moves (s)
  (let ((text s))
    (setq text (s-replace-all '((" " . "")) text))
    (setq text (s-replace-all
                '(
                  ("++" . "\\\\#")
                  ("0-0-0" . "O-O-O")
                  ("0-0" . "O-O")
                  ("l" . "1")
                  ) text))
    (setq text
          (replace-regexp-in-string
           (rxt-pcre-to-elisp "\\.\\.\\.*")
           "..." text))
      text))

(defun chess/get-first-move-number (text)
  (let (
       (move-num)
        )
      (when (string-match move-number text 0)
        (setq move-num (string-to-number (match-string 1 text))))
      move-num))

(defun chess/emphasize-text ()
  (interactive)
  (let ((range) (input))
    (if (looking-at "{")
        (progn
          (books/cycle-words (list "Move" "textbf" "emph" "underline"))
          (re-search-forward "{") (re-search-backward "{"))
      (progn
        (unless (region-active-p) (er/mark-word))
        (wrap-region-with "\\Move{" "}")
        (re-search-backward "{")
        ))
    ))

(defvar chess/replace-chars-hook nil "Replace chars hook")
(defvar chess/mft-after-word-select-hook nil "Functions to run after selecting word in move-from-text")
(defvar chess/mft-after-change-hook nil "Functions to run after changing the text in move-from-text")

(defun chess/select-to-end-of-line ()
  (interactive)
  (let ((start (point)) (end (line-end-position)))
    (save-excursion
      (goto-char (line-end-position))
      (if (looking-back "[\\.,]")
          (progn
            (backward-char)
            (setq end (point))
            )))
    (goto-char end)
    (set-mark start)
    ))

(defun chess/replace-chars ()
  (interactive)
  (let ((current-point (point)) (out) (char-under-cursor (books/char-under-cursor))
        (word-under-cursor (thing-at-point 'word))
        (variation-list (list "vone" "vtwo" "vthree" "vfour" "vfive" "vall"))
        )
    (skg/case-sensitive
        (cond

         ((-contains? variation-list word-under-cursor)
          (books/cycle-words variation-list))

         ((looking-at "^ *$")
          (progn
            (save-excursion
              (goto-char (line-beginning-position))
              (insert "%")
              )))

         ((looking-at "'") (delete-char 1))
         ((looking-at "\"") (delete-char 1))
         ((looking-at "¡¥") (delete-char 1))
         ((looking-at "'") (delete-char 1))
         ((looking-at "¢X") (delete-char 1))
         ((looking-at "\\*") (delete-char 1))
         ((looking-at "<") (delete-char 1))
         ((looking-at "\\.") (delete-char 1))
         ((looking-at ")") (delete-char 1))
         ((looking-at "[ce]") (cycle-chars "ec"))

         ((looking-at "£á")
          (progn
            (books/replace-text-in-range current-point (+ 1 current-point) "B")))
         
         ((looking-at "B[[:space:]]")
          (progn
            (books/replace-text-in-range current-point (+ 1 current-point) "9.")))

         ((looking-at-p "Q\\([[:digit:]]\\)")
          (progn
            (books/replace-text-in-range current-point (+ 1 current-point) "Qb")))
         
         ((looking-at "43")
          (progn
            (books/replace-text-in-range current-point (+ 2 current-point) "N")))

         ((s-equals? char-under-cursor " ")
          (progn
            (chess/insert-three-dots)))

         ((looking-at "g")
          (progn
            (books/replace-text-in-range current-point (+ 1 current-point) "Q")))
         
         ((looking-at "[0-9]+")
          (progn
            (setq out (match-string-no-properties 0))
            (books/replace-text-in-range current-point (+ 1 current-point)
                                         (format "%s." out))))
         
         ))
    ))

(defun chess/move-from-text-as-string (range &optional type)
  (let ((ret))
  (setq ret (buffer-substring-no-properties (car range) (cdr range)))
  (skg/case-sensitive
      (setq ret (s-replace "K" "{\\kg}" ret))
    (setq ret (s-replace "B" "{\\bp}" ret))
    (setq ret (s-replace "R" "{\\rk}" ret))
    (setq ret (s-replace "N" "{\\kt}" ret))
    (setq ret (s-replace "Q" "{\\qn}" ret))
    (setq ret (s-replace "x" "{\\capturesymbol}" ret))
    (setq ret (s-replace books/dash "-" ret))
    )
  (if (and type (eq type 'vone))
      (format "{\\color{VoneColor} %s}" ret)
    (format "{\\color{MoveColor} %s}" ret))
  ))

(defun chess/move-from-text ()
  (interactive)
  (let* ((range) (new-str))
    (books/as-word ".+"
      (unless (region-active-p) (er/mark-word))
      (run-hooks 'chess/mft-after-word-select-hook)
      (setq range (cons (region-beginning) (region-end)))
      (setq new-str (chess/move-from-text-as-string range))
      (books/replace-text-in-range (car range) (cdr range) new-str))
    )
  (run-hooks 'chess/mft-after-change-hook))

(defun chess/line-mainline ()
  (interactive)
  (unless (region-active-p) (skg/select-line))
  (wrap-region-trigger 0 "m")
  )

(defun chess/line-variation ()
  (interactive)
  (skg/select-line)
  (wrap-region-trigger 0 "v")
  )

(defun chess/next-mainline-or-variation ()
  (interactive)
  (re-search-forward "\\(ml\\|vone\\|vtwo\\|vthree\\|vfour\\|vfive\\|vall\\|vplain\\|vmain\\){"))

(defun chess/next-variation ()
  (interactive)
  (re-search-forward "\\(vone\\||vtwo\\|vthree\\|vfour\\|vfive\\|vall\\|vplain\\|vmain\\){"))

(defun chess/prev-mainline-or-variation ()
  (interactive)
  (re-search-backward "\\(ml\\|vone\\|vtwo\\|vthree\\|vfour\\|vfive\\|vall\\|vplain\\|vmain\\){"))

(defun chess/see-position ()
  (interactive)
  (insert "(see \\hyperref[position:\\thechapter-1]{position})")
  (backward-list)
  (re-search-forward "thechapter-")
  )

(defun chess/name-to-symbol ()
  (interactive)
  (save-excursion
    (skg/case-sensitive
     (replace-regexp "rook" "{\\\\rk}" nil (point-min) (point-max)) 
     (replace-regexp "pawn" "{\\\\pn}" nil (point-min) (point-max)) 
     (replace-regexp "king" "{\\\\kg}" nil (point-min) (point-max)) 
     (replace-regexp "queen" "{\\\\qn}"  nil (point-min) (point-max)) 
     (replace-regexp "bishop" "{\\\\bp}" nil (point-min) (point-max)) 
     (replace-regexp "knight" "{\\\\kt}" nil (point-min) (point-max)) 
     (replace-string "\u2014+" "\\BBetter" nil (point-min) (point-max))
     (replace-string "-+" "\\BBetter" nil (point-min) (point-max))
     (replace-string "+\u2014" "\\WBetter" nil (point-min) (point-max))
     (replace-string "+-" "\\WBetter" nil (point-min) (point-max))
     (replace-string "#" "\\#" nil (point-min) (point-max))
     (replace-regexp "\\([[:alpha:]]\\){\\\\kg}" "\\1king" nil (point-min) (point-max))
     )))

(defun cm/set-dirty-text-properties ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward cm/dirty-move-regex (point-max) t)
      (add-text-properties (match-beginning 0) (match-end 0) '(dirty t)))
    (save-buffer)
  ))

(defun cm/set-mainline-text-properties (&rest additional-properties)
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward cm/next-mainline-regex (point-max) t)
      (add-text-properties (match-beginning 0) (match-end 0) '(mainline t))
      (--map (put-text-property (match-beginning 0) (match-end 0) it t) (-flatten additional-properties))
      )
    (save-buffer)
    ))

(defun cm/set-variation-text-properties (&rest additional-properties)
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward cm/next-variation-regex (point-max) t)
      (add-text-properties (match-beginning 0) (match-end 0) '(variation t))
      (--map (put-text-property (match-beginning 0) (match-end 0) it t) (-flatten additional-properties))
      )
    (save-buffer)
    ))

(defun chess/single-line-mainline ()
  (interactive)
  (unless (region-active-p) (skg/select-line))
  (save-restriction
    (narrow-to-region (region-beginning) (region-end))
    (when (comment-only-p (point-min) (point-max))
      (skg/operate-on-buffer 'comment-or-uncomment-region))
    (replace-string "\n" " " nil (point-min) (point-max))
    (mark-whole-buffer)
    (wrap-region-trigger 0 "m")
  ))

(defun chess/show-only-dirty-moves ()
  (interactive)
  (let ((old-buf (current-buffer))
        (buf (clone-indirect-buffer (format "%s-<indirect>" (buffer-name)) nil)))
    (when buf
      (switch-to-buffer buf)
      (goto-char (point-min))
      (books/chess-moves-mode)
      (cm/set-dirty-text-properties)
      (when (re-search-forward cm/dirty-move-regex (point-max) t)
        (progn
          (narrow-to-region (match-beginning 0) (match-end 0))
          (goto-char (point-min))))
      )))
  
(defun chess/create-moves-buffer-from-this (type)
  (interactive)
  (let ((old-buf (current-buffer))
        (buf (clone-indirect-buffer (format "%s-<indirect>" (buffer-name)) nil))
        (regex (symbol-value (get 'cm/move-type-regex type))))
    (when buf
      (switch-to-buffer buf)
      (goto-char (point-min))
      (books/chess-moves-mode)
      (when (re-search-forward regex (point-max) t)
        (progn
          (narrow-to-region (match-beginning 0) (match-end 0))
          (goto-char (point-min)))))
    ))

(defun chess/half-point ()
  (interactive)
  (insert "$\\sfrac{1}{2}$"))

(defun chess/insert-game ()
  (interactive)
  (let* ((file (buffer-file-name)) (dir (f-dirname file)))
  (save-excursion
    (goto-char (point-max))
    (insert-file (f-join dir "game.tex"))
    (save-buffer))
  ))

(defun chess/cycle-variations ()
  (interactive)
  (let ((word-under-cursor (thing-at-point 'word))
        (variation-list (list "vone" "vtwo" "vthree" "vfour" "vfive" "vall")))
    (when (-contains? variation-list word-under-cursor)
      (books/cycle-words variation-list))
    ))

(defun chess/white-won ()
  (interactive)
  (insert "\n%\n\\WhiteWon\n%\n"))

(defun chess/black-won ()
  (interactive)
  (insert "\n%\n\\BlackWon\n%\n"))

(defun chess/game-draw ()
  (interactive)
  (insert "\n%\n\\GameDraw\n%\n"))

(defun chess/dummy-vone ()
  (interactive)
  (when (region-active-p) (delete-region (region-beginning) (region-end)))
  (insert "\\vone{ } "))

(defun chess/dummy-dirty ()
  (interactive)
  (insert "\\dirty{ } "))

(defun chess/vone-dirty-interchange ()
  (interactive)
  (let ((other) (range (bounds-of-thing-at-point 'word)) (w (thing-at-point 'word)))
    (when (s-equals-p w "dirty") (setq other "vone"))
    (when (s-equals-p w "vone") (setq other "dirty"))
    (when other
      (books/replace-text-in-range (car range) (cdr range) other)
      (save-buffer))
    ))
  
(books/defkeymap books/chess-book-mode-map "Keymap for chess books minor mode")
(define-key books/chess-book-mode-map (kbd "<f1>") 'chess/replace-chars)
(define-key books/chess-book-mode-map (kbd "<f12>") 'books/pgn-paste-mode)
(define-key books/chess-book-mode-map (kbd "C-<f12>") 'chess/show-pgns)
(define-key books/chess-book-mode-map (kbd "s-h") 'chess/half-point)
(define-key books/chess-book-mode-map (kbd "H-a") 'chess/vone-dirty-interchange)

(--map (define-key books/chess-book-mode-map (kbd (car it)) 'chess/wrap-region) chess/moves-type-alist)


(define-minor-mode books/chess-book-mode
  "Mode to write chess books"
  :lighter " chess"
  :keymap books/chess-book-mode-map
  )


(defun chess/d-to-position ()
  (interactive)
  (let ((reg "(D)") (count 0))
    (skg/search-file-do reg
      (setq count (1+ count))
      (replace-match (format "(see \\\\hyperref[position:\\\\thechapter-%d]{position})" count))
      )
    (save-buffer)
    ))

(defmacro chess/book-mode (mode-str book &rest body)
  (declare (indent 1) (debug t))
  (let (
        (mode (intern (format "books/%s-mode" mode-str)))
        (hook (intern (format "books/%s-mode-hook" mode-str)))
        )
    (add-hook hook (lambda ()
                     (books/chess-book-mode 1)
                     ))
    `(progn
       (books/new-mode ,mode-str latex-mode ,book ,@body))
    
    ))

(defalias 'bam 'chess/board-after-move)

(books/defkeymap chess/edit-mode-keymap "Keymap used in chess/edit mode")

(define-prefix-command 'chess-edit-mode-map)
(define-key chess-edit-mode-map (kbd "v") 'chess/dummy-vone)
(define-key chess-edit-mode-map (kbd "d") 'chess/dummy-dirty)
(define-key chess-edit-mode-map (kbd "k") 'highlight/to-end-of-line)
(define-key chess-edit-mode-map (kbd "h") 'highlight/to-beginning-of-line)
(define-key chess-edit-mode-map (kbd "p") 'highlight/paste-highlights)
(define-key chess-edit-mode-map (kbd "j") '(lambda () (interactive) (insert-char ?j)))
(define-key chess-edit-mode-map (kbd "w") '(lambda () (interactive) (insert "White")))
(define-key chess-edit-mode-map (kbd "b") '(lambda () (interactive) (insert "Black")))
(define-key chess-edit-mode-map (kbd "s") 'highlight/toggle-line-select)
(define-key chess-edit-mode-map (kbd "e") 'highlight/current-env)

(define-minor-mode chess/edit-mode
  "Some Documentation"
  :lighter " ce"
  :keymap chess/edit-mode-keymap
  (chess/edit/startup-or-shutdown)
  )

(defun chess/edit/startup-or-shutdown ()
  (if (symbol-value 'chess/edit-mode)
      (chess/edit/startup)
    (chess/edit/shutdown)
    ))

(defun chess/edit/startup ()
  (define-key chess/edit-mode-keymap (kbd "j") 'chess-edit-mode-map)
  (show-page-mode t)
  (setq books/highlight/keymap chess/highlight-keymap)
  )

(defun chess/edit/shutdown ()
  (define-key chess/edit-mode-keymap (kbd "j") nil)
  (setq books/highlight/keymap nil)
  )

(books/defkeymap chess/highlight-keymap "Keymap used in chess-highlight regions")

(define-key chess/highlight-keymap (kbd "d") 'highlight/delete-region)
(define-key chess/highlight-keymap (kbd "e") 'highlight/enumerate)
(define-key chess/highlight-keymap (kbd "i") 'highlight/itemize)
(define-key chess/highlight-keymap (kbd "p") 'highlight/include-previous-line)
(define-key chess/highlight-keymap (kbd "n") 'highlight/include-next-line)
(define-key chess/highlight-keymap (kbd "C-i") 'books/itemize-line)
