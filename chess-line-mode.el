(require 'pcre2el)
(require 'thingatpt)
(require 'key-chord)
(require 'books/narrow)
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

(defun cl/last-move ()
  (let ((ret))
  (save-excursion
    (when (re-search-backward "\\(^\\|[{ ]\\)\\([0-9]+\\).\\(\\.\\.\\| ?\\)" (point-min) t)
      (setq ret (string-to-number (match-string-no-properties 2)))))
      ret))

(defun cl/black-move-present (move-num)
  (let* ((square "[a-hKQRNBO]")
         (regex (format "%d\\.\\(\\.\\.%s\\| %s[^ ]* %s\\)" move-num square square square))
         )
    (save-excursion
      (re-search-backward regex (point-min) t))
    ))

(defun cl/char-exists-in-string (char string)
  (-elem-index (string-to-char char) (append string nil)))

(defun cl/next-move-number ()
  (interactive)
  (let* ((last-move (cl/last-move)) (black-present (cl/black-move-present last-move)) (next-move))
    (when last-move (setq next-move (1+ last-move))
          (unless (looking-back " ") (insert " "))
          (when black-present
            (insert (format "%d. " next-move))))
    ))

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

(defun cl/three-dots-for-first-move ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "[0-9]+\\(\\.\\| \\)" (point-max) t)
      (goto-char (match-end 0))
      (backward-char)
      (chess/insert-three-dots))
    ))

(defun cl/cleanup-moves ()
  (interactive)
  (let ((text (buffer-substring-no-properties (1+ (point-min)) (1- (point-max)))))
    (cond
     ((derived-mode-p 'books/chess-mainline-mode) (run-hook-with-args 'cl/cleanup-mainline-hook text))
     ((derived-mode-p 'books/chess-variation-mode) (run-hook-with-args 'cl/cleanup-variation-hook text))
     ((derived-mode-p 'books/chess-dirty-mode) (run-hook-with-args 'cl/cleanup-dirty-hook text))
     )))

(defun cl/replace-chars ()
  (interactive)
  (let ((mode) (fun))
    (setq mode (or (books/major-mode) major-mode))
    (setq fun (plist-get (get 'chess/other-functions mode) :replace-chars))
    (and fun (fboundp fun) (funcall fun (skg/char-under-cursor) (point)))
    (save-buffer)
    ))

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

(eval-after-load 'books/chess-line
  '(progn
     (define-key books/chess-line-mode-map (kbd "<return>")  'cl/next-move-number)
     (define-key books/chess-line-mode-map (kbd "<s-right>") 'cl/goto-next-sub-move)
     (define-key books/chess-line-mode-map (kbd "<s-left>")  'cl/goto-prev-sub-move)
     (define-key books/chess-line-mode-map (kbd "<s-up>")    'cl/goto-next-move)
     (define-key books/chess-line-mode-map (kbd "<s-down>")  'cl/goto-prev-move)
     (define-key books/chess-line-mode-map (kbd "<tab>") 'cl/cleanup-moves)
     (define-key books/chess-line-mode-map (kbd "C-d") 'chess/dot-after-move-number)
     (define-key books/chess-line-mode-map (kbd "<f3>") 'cl/three-dots-for-first-move)
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

     (define-key books/chess-line-mode-map (kbd "i") (lambda () (interactive) (insert "B")))
     (define-key books/chess-line-mode-map (kbd "k") (lambda () (interactive) (insert "K")))
     (define-key books/chess-line-mode-map (kbd "q") (lambda () (interactive) (insert "Q")))
     (define-key books/chess-line-mode-map (kbd "n") (lambda () (interactive) (insert "N")))
     (define-key books/chess-line-mode-map (kbd "r") (lambda () (interactive) (insert "R")))
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

(provide 'books/chess-line)
