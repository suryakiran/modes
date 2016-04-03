(require 'pcre2el)
(require 'thingatpt)
(require 'key-chord)
(require 'ht)
(books/require 'overlays)
(books/require 'narrow)
(books/require 'evil)
(books/require 'show-book)

(defconst chess-algebraic-pieces-regexp "[RNBKQ]")

;; jww (2008-09-01): use rx here, like in chess-ics
(defconst cl/move-regex
  (format (concat "\\("
		    "O-O\\(-O\\)?\\|"
		    "\\(%s?\\)"
		    "\\([a-h]?[1-8]?\\)"
		    "\\([x-]?\\)"
		    "\\([a-h][1-8]\\)"
		    "\\(=\\(%s\\)\\)?"
		  "\\)"
		  "\\([#+]\\)?")
	  chess-algebraic-pieces-regexp
	  chess-algebraic-pieces-regexp)
  "A regular expression that matches all possible algebraic moves.
This regexp handles both long and short form.")

(defvar cl/cleanup-mainline-hook nil"Hook to cleanup mainline moves")
(defvar cl/cleanup-variation-hook nil "Hook to cleanup variation moves")
(defvar cl/cleanup-dirty-hook nil "Hook to cleanup dirty moves")
(defvar cl/replace-chars-hook nil "Hook to replace individual characters")
(defvar cl/modify-mode-line-hook nil "Hook to show status on modeline")

(defconst
  cl/next-mainline-regex
  "\\\\\\(ncg\\|ml\\){\\([^}]*\\)}"
  )


(defconst cl/move-num-regex "\\([0-9]+\\)\\.\\(\\.\\.\\| \\)")
(defconst cl/move-emph-regex "[\\?!\\+]")

(books/defface cl/move-num-face "Face used to show chess move numbers" chess-line
               (:foreground "green"))

(books/defface cl/move-face "Face used to show chess moves" chess-line
               (:foreground "gold"))

(books/defface cl/move-emph-face "Face used to show chess move emphasis" chess-line
               (:foreground "deep sky blue"))

(defun cl/propertize-text (beg end old-len)
  (cl/propertize-whole-text)
  ;; (cl/auto-capitalize)
  )

(defun cl/auto-capitalize ()
  (when (eq this-command 'self-insert-command)
    (let* ((key (this-command-keys)) (piece-key (cl/char-exists-in-string key "kqrn")))
      (cond
       (piece-key
        (upcase-region (1- (point)) (point))
        (unless (looking-at "}")
          (delete-char 1)))

       ((string-equal key "i")
        (books/replace-text-in-range (1- (point)) (point) "B")
        (forward-char)
        (unless (looking-at "}") (delete-char 1)))
        ))
    ))

(setq cl/keywords (list
                   (cons cl/move-num-regex cl/move-num-face)
                   (cons cl/move-emph-regex cl/move-emph-face)
                   (cons cl/move-regex  cl/move-face)
                   ))


(defun cl/latest-mainline ()
  (interactive)
  (let ((ml) (move) (beg) (end) (move-number-regex (format "[{ ]%s" cl/move-num-regex)))
    (save-excursion
      (save-restriction
        (widen)
        (when (re-search-backward cl/next-mainline-regex (point-min) t)
          (setq beg (match-beginning 0))
          (setq end (match-end 0))
          (goto-char (match-end 0))
          (setq beg (re-search-backward move-number-regex beg t))
          (setq ml (format "Last Move: %s" (buffer-substring-no-properties (1+ beg) (1- end))))
          )))
    ml))
    ;; (propertize ml 'face 'anand/last-mainline-face)))


(defun cl/modify-mode-line ()
  (run-hooks 'cl/modify-mode-line-hook))

(books/new-mode "chess-line" fundamental-mode "Chess Line"
                (setq font-lock-defaults '(cl/keywords))
                (cl/modify-mode-line)
                (turn-off-wrap-region-mode)
                (add-hook 'after-change-functions 'cl/propertize-text t t)
                )

(books/new-mode "chess-mainline" books/chess-line-mode "Mainline")
(books/new-mode "chess-variation" books/chess-line-mode "Variation")
(books/new-mode "chess-dirty" books/chess-line-mode "Dirty")
(books/new-mode "chess-vone-dirty" books/chess-line-mode "VDirty")

(defun chess-line-mode-hook ()
  (turn-off-wrap-region-mode)
  (smartparens-mode -1)
  (books/master-book-mode -1)
  )

(add-hook 'books/chess-mainline-mode-hook 'chess-line-mode-hook)
(add-hook 'books/chess-variation-mode-hook 'chess-line-mode-hook)


(defun cl/char-exists-in-string (char string)
  (-elem-index (string-to-char char) (append string nil)))


(defun cl/repeat-last-square ()
  (interactive)
  (let ((ls))
    (save-excursion
      (when (re-search-backward "\\([a-h][1-8]\\)" (point-min) t)
        (setq ls (match-string-no-properties 0))))
    (when ls (insert ls))
    ))

(defun cl/repeat-last-move ()
  (interactive)
  (let ((regex "\\(\\.\\.\\.\\| \\)\\([a-hKQRNBO][^ ]*\\) ") (text)) 
    (unless (looking-back " ") (insert " "))
    (save-excursion
      (when (re-search-backward regex (point-min) t)
        (setq text (match-string-no-properties 2))))
    (when text (insert text))
    ))

(defun cl/cur-face ()
  (get-text-property (point) 'face))

(defun cl/cur-field ()
  (get-text-property (point) 'field))

(defun cl/goto-next (field)
  )

(defun cl/goto (pos)
  (when pos
    (goto-char pos)
    pos))

(defun cl/goto-next-field ()
  (cl/goto (next-single-property-change (point) 'field)))

(defun cl/goto-prev-field ()
  (let ((beg (field-beginning)) (end (field-end)) (cur (point)))
    (unless (equal end cur)
      (cl/goto (field-beginning)))
  (cl/goto (previous-single-property-change (point) 'field))
  ))

(defun cl/goto-next-sub-move ()
  (interactive)
  (let ((field (cl/cur-field)))
    (and (cl/goto-next-field) (eq field 'black-move) (cl/goto-next-field))
    ))

(defun cl/goto-prev-sub-move ()
  (interactive)
  (let ((field (cl/cur-field)))
    (and (cl/goto-prev-field) (eq field 'white-move) (cl/goto-prev-field))
  ))

(defun cl/goto-next-move ()
  (interactive)
  (let ((field (cl/cur-field)) (cur-pos (point)) (pos))
    (cl/goto-next-field)
    (setq pos (cl/goto (text-property-any (point) (point-max) 'field 'white-move)))
    (unless pos (goto-char cur-pos))
  ))

(defun cl/goto-prev-move ()
  (interactive)
  (let ((field (cl/cur-field)) (cur-pos (point)) (pos) (num))
    (cond
     ((eq field 'black-move) (setq num 1))
     ((eq field 'white-move) (setq num 3))
     ((eq field 'move-num)   (setq num 2))
     )
    (when num
      (dolist (i (number-sequence 1 num))
        (setq pos (cl/goto-prev-field)))
      (unless pos (cl/goto cur-pos)))
    ))

(defun cl/propertize-whole-text ()
  (interactive)
  (save-excursion
    (remove-text-properties (point-min) (point-max) '(field nil))
    (let ((cur-pos) (pos) (str))
      (goto-char (point-min))
      (setq cur-pos (point))
      (catch 'loop-exit
        (while t
          (setq pos (next-single-property-change cur-pos 'face))
          (unless pos (throw 'loop-exit t))

          (setq str (buffer-substring-no-properties cur-pos pos))
          (add-text-properties cur-pos pos '(field move-num))
          (goto-char pos)
          (setq cur-pos (point))
          (setq pos (re-search-forward " " (point-max) t))
          (unless pos (throw 'loop-exit t))
          (if (s-ends-with? "..." str)
              (add-text-properties cur-pos pos '(field black-move))
            (progn
              (add-text-properties cur-pos pos '(field white-move))
              (setq cur-pos pos)
              (setq pos (re-search-forward " " (point-max) t))
              (unless pos (throw 'loop-exit t))
              (add-text-properties cur-pos pos '(field black-move))
              (setq cur-pos pos)
              )))))
    ))

(setq cl/moves-type-alist
      '((mainline . "\\\\\\(ncg\\|ml\\)\\({\\([^}]*\\)}\\)")
        (variation .  "\\\\\\(vone\\|vtwo\\|vthree\\|vfour\\|vfive\\|vall\\)\\({\\([^}]*\\)}\\)")
        (dirty . "\\\\\\(dirty\\)\\({\\([^}]*\\)}\\)")
        (vone . "\\\\\\(vonedirty\\)\\({\\([^}]*\\)}\\)")
        ))


(defun cl/show-only-chess-moves (type)
  (let ((narrow-buffer) (item (assoc type cl/moves-type-alist)) (reg))
    (when item
      (setq reg (cdr item))
      (when (buffer-narrowed-p)
        (setq narrow-buffer t)
        ;; (replace-string "#" "\\#" nil (point-min) (point-max))
        (goto-char (point-max))
        (widen)
        (when (or (eq type 'dirty) (eq type 'vone))
            (cl/change-dirty-text-to-move type))
        (save-buffer))
      (if (re-search-forward reg (point-max) t)
          (progn
            (narrow-to-region (match-beginning 2) (match-end 2))
            (goto-char (point-min))
            (when (or (eq type 'mainline) (eq type 'variation))
              (cl/propertize-whole-text)
              (save-buffer))
            )
        (when narrow-buffer (kill-buffer)))
      )))

(defun cl/show-mainlines ()
  (interactive)
  (books/restricted-view 'books/chess-mainline-mode "mainline"
                         '(lambda () (cl/show-only-chess-moves 'mainline))))


(defun cl/show-variations ()
  (interactive)
  (books/restricted-view 'books/chess-variation-mode "var"
                         '(lambda () (cl/show-only-chess-moves 'variation))))

(defun cl/show-dirty-moves ()
  (interactive)
  (books/restricted-view 'books/chess-dirty-mode "dirty"
                         '(lambda () (cl/show-only-chess-moves 'dirty))))
                            
(defun cl/show-vone-dirty-moves ()
  (interactive)
  (books/restricted-view 'books/chess-vone-dirty-mode "vdirty"
                         '(lambda () (cl/show-only-chess-moves 'vone))))

(defun cl/clear-buffer-to-default()
  (interactive)
  (books/replace-text-in-range (point-min) (point-max) "{}")
  (goto-char (1+ (point-min)))
  )

(defun cl/cleanup-moves ()
  (interactive)
  (let ((text (buffer-substring-no-properties (1+ (point-min)) (1- (point-max)))))
    (cond
     ((derived-mode-p 'books/chess-mainline-mode) (run-hook-with-args 'cl/cleanup-mainline-hook text))
     ((derived-mode-p 'books/chess-variation-mode) (run-hook-with-args 'cl/cleanup-variation-hook text))
     ((derived-mode-p 'books/chess-dirty-mode) (run-hook-with-args 'cl/cleanup-dirty-hook text))
     )))

(defun cl/bishop ()
  (interactive)
  (insert "B")
  (unless (looking-at "}")
    (delete-char 1)))

(defun cl/short-castle ()
  (interactive)
  (insert "O-O "))

(defun cl/long-castle ()
  (interactive)
  (insert "O-O-O "))

(defun cl/paren-at-end ()
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (when (not (looking-back "}"))
      (insert "}"))
    ))

(defun cl/insert-move (text)
  (let ((p (point)) (new-point))
    (save-excursion
      (when (re-search-forward "[ }]" (point-max) t)
        (setq new-point (1- (match-end 0)))
        (books/replace-text-in-range p new-point text))
      )
    (forward-char (length text))
    ))

(defun cl/parse-it ()
  (interactive)
  (let* (
         (cur-state (parse-partial-sexp (point-min) (point)))
         (level (car cur-state))
         (pos (cadr cur-state))
         (move)
        )
    (save-excursion
      (while pos
        (goto-char pos)
        (if (looking-back "\\\\\\(ml\\|vone\\)")
            (progn
              (setq move (match-string-no-properties 1))
              (setq pos nil))
          (progn
            (backward-char)
            (setq cur-state (parse-partial-sexp (point-min) (point)))
            (setq pos (cadr cur-state))))
          ))
      move))

(defun cl/bad-move ()
  (interactive)
  (cl/insert-move "?"))

(defun cl/good-move ()
  (interactive)
  (cl/insert-move "!"))

(defun cl/check-mate ()
  (interactive)
  (cl/insert-move "\\#"))

(defun cl/check ()
  (interactive)
  (cl/insert-move "+"))

(defun cl/to-next-move ()
  (interactive)
  (when (re-search-forward "[ \\.][a-hKQRNB]" (point-max) t)
    (backward-char 1)))

(defun cl/break-move (text)
  (let ((move (cl/parse-it)))
    (if move (insert (format "} %s \\%s{" text move))
      (insert (format "} %s {" text)))
    ))

(defun cl/just-break ()
  (interactive)
  (cl/break-move ""))

(defun cl/insert-move-description (text)
  (insert (format " {\\bfseries\\%s} " text)))

(defun cl/only-move ()
  (interactive)
  (cl/insert-move-description "onlymove"))

(defun cl/white-decisive-advantage ()
  (interactive)
  (cl/insert-move-description "WBetter"))

(defun cl/black-decisive-advantage ()
  (interactive)
  (cl/insert-move-description "BBetter"))

(defun cl/white-slightly-better ()
  (interactive)
  (cl/insert-move-description "wbetter"))

(defun cl/black-slightly-better ()
  (interactive)
  (cl/insert-move-description "bbetter"))

(defun cl/white-upper-hand ()
  (interactive)
  (cl/insert-move-description "wBetter"))

(defun cl/black-upper-hand ()
  (interactive)
  (cl/insert-move-description "bBetter"))

(defun cl/attack ()
  (interactive)
  (cl/insert-move-description "attack"))

(defun cl/equal ()
  (interactive)
  (cl/insert-move-description "equal"))

(defun cl/compensation ()
  (interactive)
  (cl/insert-move-description "compensation"))

(defun cl/with-initiative()
  (interactive)
  (cl/insert-move-description "initiative"))

(defun cl/bishops ()
  (interactive)
  (cl/insert-move-description "bishops"))

(defun cl/opposite-bishops ()
  (interactive)
  (cl/insert-move-description "oppositebishops"))

(defun cl/time-trouble ()
  (interactive)
  (cl/insert-move-description "timetrouble"))

(defun cl/same-bishops ()
  (interactive)
  (cl/insert-move-description "samebishops"))

(defun cl/with-idea ()
  (interactive)
  (cl/insert-move-description "withidea"))

(defun cl/unclear ()
  (interactive)
  (cl/insert-move-description "unclear"))

(defun cl/betteris ()
  (interactive)
  (cl/insert-move-description "betteris"))

(defun cl/square-bracket-and-variation ()
  (interactive)
  (cl/break-move "[\\vone{}]"))

(defun cl/paren-and-variation ()
  (interactive)
  (cl/break-move "(\\vone{})"))

(defun cl/variation ()
  (interactive)
  (cl/break-move "\\vone{}"))

(defun cl/white-won ()
  (interactive)
  (insert " {\\WhiteWon}"))

(defun cl/black-won ()
  (interactive)
  (insert " {\\BlackWon}"))


(eval-after-load 'whiteboard-theme
  '(progn
     (custom-theme-set-faces
      'whiteboard
      '(cl/move-num-face ((t (:foreground "deep sky blue"))))
      '(cl/move-face ((t (:foreground "dark magenta"))))
      '(cl/move-emp-face ((t (:foreground "forest green"))))
      )))

(add-hook 'books/chess-line-mode-hook 'books/narrow-mode)
(setq auto-mode-alist 
      (append '(
                ("\\.clm\\'" . books/chess-mainline-mode))
              auto-mode-alist))



(books/defface cmc/mainline-face "Face used to show chess mainlines" chess-move-correction
               (:foreground "red"))

(books/defface cmc/variation-face "Face used to show chess variations" chess-move-correction
               (:foreground "green"))

(books/defface cmc/dirty-face "Face used to show dirty moves" chess-move-correction
               (:foreground "blue"))

(defvar cmc/move-type 'mainline)
(make-variable-buffer-local 'cmc/move-type)

(defconst cmc/regex
  "\\\\%s{\\([^}]*\\)}")
  
(defconst cmc/regex/mainline
  (format cmc/regex "\\(ml\\|ncg\\|ncgh\\)"))

(defconst cmc/regex/variation
  (format cmc/regex "\\(vone\\|vtwo\\|vthree\\|vfour\\|vfive\\|vall\\|vmain\\)"))

(defconst cmc/regex/dirty
  (format cmc/regex "\\(dirty\\)"))

(defvar cmc/move-type/regex (ht-create 'eq))
(ht-set! cmc/move-type/regex 'mainline cmc/regex/mainline)
(ht-set! cmc/move-type/regex 'variation cmc/regex/variation)
(ht-set! cmc/move-type/regex 'dirty cmc/regex/dirty)

(defvar cmc/move-type/face (ht-create 'eq))
(ht-set! cmc/move-type/face 'mainline cmc/mainline-face)
(ht-set! cmc/move-type/face 'variation cmc/variation-face)
(ht-set! cmc/move-type/face 'dirty cmc/dirty-face)

(books/defkeymap chess-move-correction-overlay-keymap "Keymap used in chess-move-correction overlays")

(defun cmc/last-move ()
  (let ((ret))
  (save-excursion
    (when (re-search-backward "\\(^\\|[{ ]\\)\\([0-9]+\\).\\(\\.\\.\\| ?\\)" (point-min) t)
      (setq ret (string-to-number (match-string-no-properties 2)))))
      ret))

(defun cmc/black-move-present (move-num)
  (let* ((square "[a-hKQRNBO]")
         (regex (format "%d\\.\\(\\.\\.%s\\| %s[^ ]* %s\\)" move-num square square square))
         )
    (save-excursion
      (re-search-backward regex (point-min) t))
    ))

(defun cmc/remove-next-move ()
  (interactive)
  (let ((olays) (olay) (cur (point)))
    (unless olays
      (setq olays (books/overlays-at-point 'category 'mainline)))
    (unless olays
      (setq olays (books/overlays-at-point 'category 'variation)))
    (unless olays
      (setq olays (books/overlays-at-point 'category 'dirty)))
    (unless olays
      (setq (books/overlays-at-point 'category 'vonedirty)))

    (when olays
      (setq olay (car olays))
      (save-restriction
        (narrow-to-region (overlay-start olay) (overlay-end olay))
        (kill-word 1)
        (save-buffer)))
    ))
  
(defun cmc/replace-chars ()
  (interactive)
  (let ((mode) (fun))
    (setq mode (or (books/major-mode) major-mode))
    (setq fun (plist-get (ht-get chess/other-functions mode) :replace-chars))
    (and fun (fboundp fun) (funcall fun (skg/char-under-cursor) (point)))
    (save-buffer)
    ))

(defun cmc/change (from to)
  (let ((pt) (olay (cmc/get-chess-move-overlay)))
    (when olay
      (save-excursion
        (goto-char (overlay-start olay))
        (setq pt (point))
        (and
         (search-backward (format "%s{" from) (point-min) t)
         (= pt (match-end 0))
         (replace-match (format "%s{" to))
         )
        (save-buffer)
        ))
    ))
      
(defun cmc/next-move-number ()
  (interactive)
  (let* ((last-move (cmc/last-move)) (black-present (cmc/black-move-present last-move)) (next-move))
    (when last-move (setq next-move (1+ last-move))
          (unless (looking-back " ") (insert " "))
          (when black-present
            (insert (format "%d. " next-move))))
    ))

(defun cmc/next-overlay ()
  (interactive)
  (let (
        (move-type)
        (olays) (cur-olay)
        (next-overlay)
        )
    (setq olays (books/overlays 'keymap chess-move-correction-overlay-keymap))
    (and olays (setq cur-olay (car olays)))
    (when cur-olay
      (setq move-type (overlay-get cur-olay 'category))
      (setq next-overlay (books/next-overlay 'category move-type))
      )
    (when next-overlay
      (goto-char (overlay-start next-overlay)))
  ))

(defun cmc/prev-overlay ()
  (interactive)
  (let (
        (move-type)
        (olays) (cur-olay)
        (prev-overlay)
        )
    (setq olays (books/overlays 'keymap chess-move-correction-overlay-keymap))
    (and olays (setq cur-olay (car olays)))
    (when cur-olay
      (setq move-type (overlay-get cur-olay 'category))
      (setq prev-overlay (books/prev-overlay 'category move-type))
      )
    (when prev-overlay
      (goto-char (overlay-start prev-overlay)))
    ))

(defun cmc/new-overlay (start end type)
  (let (
        (ol (make-overlay start end (current-buffer) nil t))
        (face (ht-get cmc/move-type/face type))
        )
    (overlay-put ol 'category type)
    (overlay-put ol 'priority 0)
    (overlay-put ol 'face face)
    (overlay-put ol 'keymap chess-move-correction-overlay-keymap)
  ))

(defun cmc/new-overlay-in-range (range type)
  (cmc/new-overlay (car range) (cdr range) type))

(defun cmc/create-overlays (move-type)
  (let ((ol)
        (regex (ht-get cmc/move-type/regex move-type))
        )
    (when regex
      (while (skg/search-file-do regex
               (cmc/new-overlay (match-beginning 2) (match-end 2) move-type)
               ))
      )
    ))

(defun cmc/get-chess-move-overlay ()
  (let* (
         (types '(mainline variation dirty vonedirty))
         (type (--first (books/overlays-at-point 'category it) types))
         (olays)
         )
    
    (when type
      (setq olays (books/overlays-at-point 'category type))
      (car olays))
    ))
    
(defun dot-after-move-number-in-region (start end)
  (let ((first-move-number) (reg) (repl "%d. ") (next-move-number) (text) (next-move-regex))  
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (replace-string "\n" " " nil (point-min) (point-max))
        (goto-char (point-min))
        (setq text (buffer-substring-no-properties (point-min) (point-max)))
        (setq first-move-number (chess/get-first-move-number text))
        (setq next-move-number first-move-number)
        (when (looking-at "{") (forward-char))
        (save-excursion
          (when (or (looking-at (format "%d\\.\\.\\." first-move-number))
                    (looking-at (format "%d\\. " first-move-number)))
            (setq next-move-number (1+ next-move-number))))
        (while (re-search-forward (format repl next-move-number) (point-max) t)
          (setq next-move-number (1+ next-move-number)))
        
        (setq reg (format "%s " next-move-number))
        (while (re-search-forward reg (point-max) t)
          (unless (looking-at "\\.")
            (replace-match (format repl next-move-number)))
          (setq next-move-number (1+ next-move-number))
          (setq reg (format " %s" next-move-number))
          (setq repl " %d.")
          )
        (replace-regexp "\\([ 0-9]\\)\\.\\([a-hKQRBN]\\)" "\\1. \\2" nil (point-min) (point-max))
        ))
    ))

(defun dot-after-move-number-in-region-pre (start end)
  (let ((new-start) (new-end))
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (setq new-start (1+ (point)))
        (forward-list)
        (setq new-end (1- (point)))
        ))
    (and new-start new-end (dot-after-move-number-in-region new-start new-end))
    ))


(defun cmc/dot-after-move-number ()
  (interactive)
  (let ((olay (cmc/get-chess-move-overlay)))
    (if olay
        (dot-after-move-number-in-region (overlay-start olay) (overlay-end olay))
      (save-excursion
        (books/apply-on-paren-region 'dot-after-move-number-in-region-pre)))
    ))
      
(defun cmc/three-dots-for-first-move ()
  (interactive)
  (let ((olay (cmc/get-chess-move-overlay)))
    (when olay
      (save-excursion
        (save-restriction
          (narrow-to-region (overlay-start olay) (overlay-end olay))
          (goto-char (point-min))
          (when (re-search-forward "[0-9]+\\(\\.\\| \\)" (point-max) t)
            (goto-char (match-end 0))
            (backward-char)
            (chess/insert-three-dots))
          )
        ))
    ))

(defun cmc/create-new-variation ()
  (let* (
         (mode (books/major-mode))
         (fun (plist-get (ht-get chess/wrap-functions mode) :variation))
         )
    (save-excursion
      (save-restriction
        (narrow-to-region (region-beginning) (region-end))
        (mark-whole-buffer)
        (wrap-region-with "\\vone{" "}")
        (goto-char (point-max))
        (when fun (funcall fun :variation))
        ))
    (cmc/refresh-overlays 'variation)
    (save-buffer)
    ))

 (defun cmc/split-variation ()
   (insert "\\vone{")
   (cmc/refresh-overlays 'variation)
  )

(defun cmc/merge-ml ()
  (interactive)
  (let ((text) (regex (ht-get cmc/move-type/regex 'mainline)))
    (when (region-active-p)
      (save-excursion
        (save-restriction
          (narrow-to-region (region-beginning) (region-end))
          (replace-regexp "%" "" nil (point-min) (point-max))
          (goto-char (point-min))
          (while (re-search-forward regex (point-max) t)
            (replace-match "\\2" t))
          (replace-string "\n" " " nil (point-min) (point-max))
          (replace-regexp "^ *" "" nil (point-min) (point-max))
          (replace-regexp " *$" "" nil (point-min) (point-max))
          (replace-regexp " [0-9]+\\.\\.\\." " " nil (point-min) (point-max))
          (replace-regexp " +" " " nil (point-min) (point-max))
          (mark-whole-buffer)
          (wrap-region-with "%\n\\ml{" "}\n%\n")
          ))
      )))

(defun cmc/new-variation ()
  (interactive)
  (if (region-active-p)
      (cmc/create-new-variation)
    (cmc/split-variation)
    ))

(defun cmc/first (type)
  (let ((overlay (books/overlays/first 'category type)))
    (when overlay (goto-char (overlay-start overlay)))
  ))

(defun cmc/last (type)
  (let ((overlay (books/overlays/last 'category type)))
    (when overlay (goto-char (overlay-start overlay)))
  ))

(defun cmc/new (type)
  (let ((mode (or (books/major-mode) major-mode))
        (fun)
        )
    (setq fun (plist-get (ht-get chess/wrap-functions mode) :dirty))
    
    (if (region-active-p)
        (progn
          (wrap-region-with "\\dirty{" "}")
          (cmc/refresh-overlays 'dirty)
          (and fun (fboundp fun) (funcall fun :dirty))
          )
      (progn
        (self-insert-command 1))
      )
    ))

(defun cmc/next-field ()
  (interactive)
  (let ((olay (cmc/get-chess-move-overlay)))
    (and olay (re-search-forward "\\(\\.\\| +\\)[A-Za-z]" (overlay-end olay) t)
         (backward-char))
    ))

(defun cmc/refresh-overlays (type)
  (remove-overlays (point-min) (point-max) 'category type)
  (cmc/create-overlays type))

(books/defkeymap chess-move-correction-mainline-mode-keymap "Keymap used in chess-move-correction-mainline mode")

(define-minor-mode chess-move-correction-mainline-mode
  "Some Documentation"
  :lighter " cmcm"
  :keymap chess-move-correction-mainline-mode-keymap
  (chess-move-correction-mainline/startup-or-shutdown)
  )

(defun chess-move-correction-mainline/startup-or-shutdown ()
  (if (symbol-value 'chess-move-correction-mainline-mode)
      (chess-move-correction-mainline/startup)
    (chess-move-correction-mainline/shutdown)
    ))

(defun chess-move-correction-mainline/startup ()
  (cmc/create-overlays 'mainline)
  (smartparens-mode -1)
  (wrap-region-mode t)
  (show-page-mode t)
  )

(defun chess-move-correction-mainline/shutdown ()
  (remove-overlays (point-min) (point-max) 'category 'mainline)
  (show-page-mode -1)
  (smartparens-mode t)
  (wrap-region-mode -1)
  (save-buffer)
  )

(books/defkeymap chess-move-correction-variation-mode-keymap "Keymap used in chess-move-correction-variation mode")

(define-minor-mode chess-move-correction-variation-mode
  "Some Documentation"
  :lighter " cmcv"
  :keymap chess-move-correction-variation-mode-keymap
  (chess-move-correction-variation/startup-or-shutdown)
  )

(defun chess-move-correction-variation/startup-or-shutdown ()
  (if (symbol-value 'chess-move-correction-variation-mode)
      (chess-move-correction-variation/startup)
    (chess-move-correction-variation/shutdown)
    ))

(defun chess-move-correction-variation/startup ()
  (show-page-mode t)
  (smartparens-mode -1)
  (wrap-region-mode t)
  (cmc/create-overlays 'variation)
  )

(defun chess-move-correction-variation/shutdown ()
  (remove-overlays (point-min) (point-max) 'category 'variation)
  (show-page-mode -1)
  (smartparens-mode t)
  (wrap-region-mode -1)
  (save-buffer)
  )

(books/defkeymap chess-move-correction-dirty-mode-keymap "Keymap used in chess-move-correction-dirty mode")

(define-minor-mode chess-move-correction-dirty-mode
  "Some Documentation"
  :lighter " cmcd"
  :keymap chess-move-correction-dirty-mode-keymap
  (chess-move-correction-dirty/startup-or-shutdown)
  )

(defun chess-move-correction-dirty/startup-or-shutdown ()
  (if (symbol-value 'chess-move-correction-dirty-mode)
      (chess-move-correction-dirty/startup)
    (chess-move-correction-dirty/shutdown)
    ))

(defun chess-move-correction-dirty/startup ()
  (show-page-mode t)
  (smartparens-mode -1)
  (wrap-region-mode t)
  (cmc/create-overlays 'dirty)
  )

(defun chess-move-correction-dirty/shutdown ()
  (remove-overlays (point-min) (point-max) 'category 'dirty)
  (show-page-mode -1)
  (smartparens-mode t)
  (wrap-region-mode -1)
  (save-buffer)
  )

(defun chess-move-correction-dirty/convert-dirty-to-text (type)
  (let ((regex (ht-get cmc/move-type/regex type))
        (text)
        (range)
        )
    (skg/search-file-do regex
      (setq range (cons (match-beginning 2) (match-end 2)))
      (setq text (s-trim (match-string-no-properties 2)))
      (books/replace-text-in-range (match-beginning 0) (match-end 0)
                                   (chess/move-from-text-as-string range type))
      )
    (save-buffer)
    ))

     

(books/defkeymap chess-move-create-vone-mode-keymap "Keymap used in chess-move-create-vone mode")

(define-minor-mode chess-move-create-vone-mode
  "Some Documentation"
  :lighter " cvone"
  :keymap chess-move-create-vone-mode-keymap
  (chess-move-create-vone/startup-or-shutdown)
  )

(defun chess-move-create-vone/startup-or-shutdown ()
  (if (symbol-value 'chess-move-create-vone-mode)
      (chess-move-create-vone/startup)
    (chess-move-create-vone/shutdown)
    ))

(defun chess-move-create-vone/startup ()
  (books/evil-mode t)
  (show-page-mode t)
  )

(defun chess-move-create-vone/shutdown ()
  (books/evil-mode -1)
  (show-page-mode -1)
  )

(define-key chess-move-create-vone-mode-keymap (kbd "i") 'books/mark-next-word)


(eval-after-load 'books/chess-line
  '(progn
     (define-key books/chess-line-mode-map (kbd "<return>")  'cl/next-move-number)
     (define-key books/chess-line-mode-map (kbd "<s-right>") 'cl/goto-next-sub-move)
     (define-key books/chess-line-mode-map (kbd "<s-left>")  'cl/goto-prev-sub-move)
     (define-key books/chess-line-mode-map (kbd "<s-up>")    'cl/goto-next-move)
     (define-key books/chess-line-mode-map (kbd "<s-down>")  'cl/goto-prev-move)
     (define-key books/chess-line-mode-map (kbd "<tab>") 'cl/cleanup-moves)
     (define-key books/chess-line-mode-map (kbd "C-d") 'chess/dot-after-move-number)
     (define-key books/chess-line-mode-map (kbd "<f1>") 'cl/replace-chars)
     (define-key books/chess-line-mode-map (kbd "s") 'cl/short-castle)
     (define-key books/chess-line-mode-map (kbd "l") 'cl/long-castle)
     (define-key books/chess-line-mode-map (kbd "'") 'cl/good-move)
     (define-key books/chess-line-mode-map (kbd "/") 'cl/bad-move)
     (define-key books/chess-line-mode-map (kbd "o") 'cl/only-move)
     (define-key books/chess-line-mode-map (kbd "m") 'cl/check-mate)
     (define-key books/chess-line-mode-map (kbd "p") 'cl/check)
     (define-key books/chess-line-mode-map (kbd "v") 'chess/dummy-variation)
     (define-key books/chess-line-mode-map (kbd "z") (lambda () (interactive) (undo)))
     (define-key books/chess-line-mode-map (kbd "<escape>") (lambda () (interactive) (kill-buffer)))
     (define-key books/chess-line-mode-map (kbd "s-d") 'cl/clear-buffer-to-default)
     (define-key books/chess-line-mode-map (kbd "<home>") 'cl/to-next-move)

     (define-key books/chess-line-mode-map (kbd "<kp-1>") 'cl/white-slightly-better)
     (define-key books/chess-line-mode-map (kbd "<kp-2>") 'cl/white-upper-hand)
     (define-key books/chess-line-mode-map (kbd "<kp-3>") 'cl/white-decisive-advantage)
     (define-key books/chess-line-mode-map (kbd "<kp-4>") 'cl/black-slightly-better)
     (define-key books/chess-line-mode-map (kbd "<kp-5>") 'cl/black-upper-hand)
     (define-key books/chess-line-mode-map (kbd "<kp-6>") 'cl/black-decisive-advantage)
     (define-key books/chess-line-mode-map (kbd "<kp-7>") 'cl/white-won)
     (define-key books/chess-line-mode-map (kbd "<kp-8>") 'cl/black-won)
     (define-key books/chess-line-mode-map (kbd "<kp-subtract>") 'cl/just-break)
     (define-key books/chess-line-mode-map (kbd "s-a") 'cl/attack)
     (define-key books/chess-line-mode-map (kbd "s-c") 'cl/compensation)
     (define-key books/chess-line-mode-map (kbd "s-i") 'cl/with-idea)
     (define-key books/chess-line-mode-map (kbd "s-e") 'cl/equal)
     (define-key books/chess-line-mode-map (kbd "s-t") 'cl/with-initiative)
     (define-key books/chess-line-mode-map (kbd "s-u") 'cl/unclear)
     (define-key books/chess-line-mode-map (kbd "s-l") 'cl/time-trouble)
     (define-key books/chess-line-mode-map (kbd "s-b") 'cl/betteris)

     (define-key books/chess-line-mode-map (kbd ",") 'cl/repeat-last-move)
     (define-key books/chess-line-mode-map (kbd "<up>") 'cl/repeat-last-square)
     
     (define-key books/chess-line-mode-map (kbd "<f5>") 'cl/square-bracket-and-variation)
     (define-key books/chess-line-mode-map (kbd "<f6>") 'cl/variation)
     (define-key books/chess-line-mode-map (kbd "<f7>") 'cl/paren-and-variation)
     
     (define-key books/chess-mainline-mode-map (kbd "<kp-add>")
       (lambda () (interactive) (cl/show-only-chess-moves 'variation)))
       
     (define-key books/chess-variation-mode-map (kbd "<kp-add>")
       (lambda () (interactive) (cl/show-only-chess-moves 'variation)))
     
     (define-key books/chess-dirty-mode-map (kbd "<kp-add>")
       (lambda () (interactive) (cl/show-only-chess-moves 'dirty)))
  
     (define-key books/chess-vone-dirty-mode-map (kbd "<kp-add>")
       (lambda () (interactive) (cl/show-only-chess-moves 'vone)))

     ))

(define-key chess-move-correction-mainline-mode-keymap (kbd "C-<prior>") '(lambda () (interactive) (cmc/first 'mainline)))
(define-key chess-move-correction-mainline-mode-keymap (kbd "C-<next>") '(lambda () (interactive) (cmc/last 'mainline)))

(define-key chess-move-correction-variation-mode-keymap (kbd "C-<prior>") '(lambda () (interactive) (cmc/first 'variation)))
(define-key chess-move-correction-variation-mode-keymap (kbd "C-<next>") '(lambda () (interactive) (cmc/last 'variation)))
(define-key chess-move-correction-variation-mode-keymap (kbd "C-d") '(lambda () (interactive) (cmc/change "vone" "dirty")))
(define-key chess-move-correction-variation-mode-keymap (kbd "C-v") '(lambda () (interactive) (cmc/change "dirty" "vone")))
(define-key chess-move-correction-variation-mode-keymap (kbd "v") 'cmc/new-variation)

(define-key chess-move-correction-dirty-mode-keymap (kbd "C-<prior>") '(lambda () (interactive) (cmc/first 'dirty)))
(define-key chess-move-correction-dirty-mode-keymap (kbd "C-<next>") '(lambda () (interactive) (cmc/last 'dirty)))
(define-key chess-move-correction-dirty-mode-keymap (kbd "C-v") '(lambda () (interactive) (cmc/change "dirty" "vone")))
(define-key chess-move-correction-dirty-mode-keymap (kbd "C-d") '(lambda () (interactive) (cmc/change "vone" "dirty")))
(define-key chess-move-correction-dirty-mode-keymap (kbd "d") '(lambda () (interactive) (cmc/new 'dirty)))
(define-key chess-move-correction-dirty-mode-keymap (kbd "C-<f5>")
  '(lambda () (interactive)
     (chess-move-correction-dirty/convert-dirty-to-text 'dirty)))

(define-key chess-move-correction-overlay-keymap (kbd "H-d") 'cmc/dot-after-move-number)
(define-key chess-move-correction-overlay-keymap (kbd "<return>") 'cmc/next-move-number)
(define-key chess-move-correction-overlay-keymap (kbd "<tab>") 'cmc/replace-chars)
(define-key chess-move-correction-overlay-keymap (kbd "C-<next>") 'cmc/next-overlay)
(define-key chess-move-correction-overlay-keymap (kbd "C-<prior>") 'cmc/prev-overlay)
(define-key chess-move-correction-overlay-keymap (kbd "u") 'undo)

(define-key chess-move-correction-overlay-keymap (kbd "]") 'cmc/next-overlay)
(define-key chess-move-correction-overlay-keymap (kbd "[") 'cmc/prev-overlay)

(define-key chess-move-correction-overlay-keymap (kbd "i") '(lambda () (interactive) (insert "B")))
(define-key chess-move-correction-overlay-keymap (kbd "k") '(lambda () (interactive) (insert "K")))
(define-key chess-move-correction-overlay-keymap (kbd "q") '(lambda () (interactive) (insert "Q")))
(define-key chess-move-correction-overlay-keymap (kbd "n") '(lambda () (interactive) (insert "N")))
(define-key chess-move-correction-overlay-keymap (kbd "r") '(lambda () (interactive) (insert "R")))
(define-key chess-move-correction-overlay-keymap (kbd "C-k") 'cmc/remove-next-move)
(define-key chess-move-correction-overlay-keymap (kbd "s-d") 'cmc/three-dots-for-first-move)
(define-key chess-move-correction-overlay-keymap (kbd "d") '(lambda () (interactive) (delete-forward-char 1)))
(define-key chess-move-correction-overlay-keymap (kbd "D") '(lambda () (interactive) (insert-char ?d)))
(define-key chess-move-correction-overlay-keymap (kbd "p") '(lambda () (interactive) (insert-char ?+)))
(define-key chess-move-correction-overlay-keymap (kbd ";") 'cmc/next-field)


(eval-after-load 'alect-light-theme
  '(progn
     (custom-theme-set-faces
      'alect-light
      '(cmc/variation-face ((t (:foreground "dark cyan"))))
      )))

(defun cl/change-dirty-text-to-move (type)
  (let ((pos (re-search-backward (cdr (assoc type cl/moves-type-alist)) (point-min) t))
        (range) (text-range) (text))
    (when pos
      (setq range (cons (match-beginning 0) (match-end 0)))
      (setq text-range (cons (match-beginning 3) (match-end 3)))
      (if (and (eq type 'vone) (s-blank? (s-trim (buffer-substring-no-properties (car text-range) (cdr text-range)))))
          (delete-region (car range) (cdr range))
        (books/replace-text-in-range (car range) (cdr range)
                                   (chess/move-from-text-as-string text-range type))
        ))
    ))

(provide 'books/chess-move-correction)
