(provide 'books/new-fen)
(require 'ht)
(books/require 'overlays)

(defvar fen/regex (ht-create 'eq))
(defvar fen/face (ht-create 'eq))
(defvar fen/defaults (ht-create 'equal))

(books/defface fen/bg-face "Face used to display white pieces" fen (:background "dark gray"))
(books/defface fen/white-piece-face "Face used to display white pieces" fen (:foreground "white"))
(books/defface fen/black-piece-face "Face used to display black pieces" fen (:foreground "white"))
(books/defface fen/empty-square-face "Face used to display empty squares" fen (:foreground "yellow"))
(books/defface fen/field-sep-face "Face used to display field separator" fen (:weight bold))
(books/defface fen/error-face "Face used to display errors" fen (:foreground "white" :background "red" :weight bold))
(books/defface fen/edit-face "Face used to indicate field is editable" fen (:background "gray20"))

(ht-set! fen/regex 'fen/black "[kqrnbp]")
(ht-set! fen/regex 'fen/white "[KQRNBP]")
(ht-set! fen/regex 'fen/sep "/")
(ht-set! fen/regex 'fen/empty "[1-8]")
(ht-set! fen/regex 'fen/error "[^1-8kKqQrRbBnNpP/]")

(ht-set! fen/face 'fen/black fen/black-piece-face)
(ht-set! fen/face 'fen/white fen/white-piece-face)
(ht-set! fen/face 'fen/sep fen/field-sep-face)
(ht-set! fen/face 'fen/empty fen/empty-square-face)
(ht-set! fen/face 'fen/error fen/error-face)

(ht-set! fen/defaults 1 "RNBQKBNR")
(ht-set! fen/defaults 2 "PPPPPPPP")
(ht-set! fen/defaults 3 "8")
(ht-set! fen/defaults 4 "8")
(ht-set! fen/defaults 5 "8")
(ht-set! fen/defaults 6 "8")
(ht-set! fen/defaults 7 "pppppppp")
(ht-set! fen/defaults 8 "rnbqkbnr")

(books/defkeymap fen-minor-mode-keymap "Keymap used in fen mode")

(define-minor-mode fen-mode
  "Some Documentation"
  :lighter " fen"
  :keymap fen-minor-mode-keymap
  (fen/startup-or-shutdown)
  )

(defun fen/char-overlay (type start end)
  (let ((regex (ht-get fen/regex type))
        (face (ht-get fen/face type))
        (olay)
        )
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (skg/case-sensitive
            (while (re-search-forward regex (point-max) t)
              (setq olay (make-overlay (1- (point)) (point) (current-buffer) nil t))
              (overlay-put olay 'category type)
              (overlay-put olay 'face face)))
        ))
    ))

(defun fen/field-overlays (start end)
  (let ((first) (last) (olay) (num 8))
    (when fen
      (save-excursion
        (save-restriction
          (narrow-to-region start end)
          (goto-char (point-min))
          (setq first (point))
          (while (re-search-forward "/" (point-max) t)
            (setq last (1- (point)))
            (setq olay (make-overlay first last (current-buffer) nil t))
            (overlay-put olay 'category 'fen-field)
            (overlay-put olay 'field-num num)
            (setq num (1- num))
            (setq first (point))
            )
          (setq olay (make-overlay first end (current-buffer) nil t))
          (overlay-put olay 'category 'fen-field)
          (overlay-put olay 'field-num num)
          )))
    ))

(defun fen/overlay (start end)
  (let ((fen (make-overlay start end (current-buffer) nil t)))
    (overlay-put fen 'face 'fen/bg-face)
    (overlay-put fen 'category 'fen)
    (overlay-put fen 'keymap fen-minor-mode-keymap)
    (fen/field-overlays start end)
    (fen/char-overlay 'fen/black start end)
    (fen/char-overlay 'fen/white start end)
    (fen/char-overlay 'fen/sep start end)
    (fen/char-overlay 'fen/empty start end)
    (fen/char-overlay 'fen/error start end)
    ))

(defun fen/refresh-overlays (&optional start end)
  (unless start (setq start (point-min)))
  (unless end (setq end (point-max)))
  (remove-overlays start end)
  (fen/overlay start end)
  )

(defun fen/startup-or-shutdown ()
  (if (symbol-value 'fen-mode)
      (fen/startup)
    (fen/shutdown)
    ))

(defun fen/after-change (begin end len)
  (books/do-in-overlay 'fen
                       (fen/refresh-overlays)
                       ))

(defun fen/before-change (begin end)
  (let (
        (first) (last)
        (olay (car (books/overlays-at-point 'category 'fen-edit)))
        )
    (when olay
      (books/do-in-overlay 'fen
                           (setq first (overlay-start olay))
                           (setq last (overlay-end olay))
                           (delete-region first last)
                           (fen/refresh-overlays)
                           ))
    ))

(defun fen/startup ()
  (skg/search-file-do "[Ff]en{\\([^}]*\\)}"
    (fen/overlay (match-beginning 1) (match-end 1))
    )
  (key-chord-mode t)
  (add-hook 'after-change-functions 'fen/after-change nil t)
  (add-hook 'before-change-functions 'fen/before-change nil t)
  )

(defun fen/shutdown ()
  (remove-overlays (point-min) (point-max))
  (remove-hook 'after-change-functions 'fen/after-change t)
  (remove-hook 'before-change-functions 'fen/before-change t)
  (key-chord-mode -1)
  )

(defun fen/clear-current-field ()
  (interactive)
  (let ((field (car (books/overlays-at-point 'category 'fen-field))))
    (when field
      (books/do-in-overlay 'fen
                           (books/replace-text-in-range (overlay-start field) (overlay-end field) "8")
                           (fen/refresh-overlays)
                           (save-buffer)
                           )
      )
    ))

(defun fen/get-fields ()
  (let ((fen (car (books/overlays-at-point 'category 'fen)))
        (fields))
    (when fen
      (save-excursion
        (save-restriction
          (narrow-to-region (overlay-start fen) (overlay-end fen))
          (setq fields (books/overlays 'category 'fen-field t))
          ))
      )
    fields))

(defun fen/allow-edit (olay)
  (let ((edit (make-overlay (overlay-start olay) (overlay-end olay) (current-buffer) nil t)))
    (overlay-put edit 'category 'fen-edit)
    (overlay-put edit 'face fen/edit-face)
    ))

(defun fen/ends-with-number (text)
  (s-matches? "[[:digit]]$" text))

(defun fen/update-fen-field (text)
  (let (
        (rv text)
        (rem-fields 8)
        (cur-num 0)
        (new-text)
        (len (length text))
        )
    (if (fen/ends-with-number text)
        (setq new-text (s-left (- len 1) text))
      (setq new-text text))
    (when (> (length new-text) 0)
      (dolist (c (append new-text nil))
        (progn
          (setq cur-num (string-to-number (char-to-string c)))
          (if (> cur-num 0)
              (setq rem-fields (- rem-fields cur-num))
            (setq rem-fields (- rem-fields 1))
            )
          ))
      (when (> rem-fields 0)
        (setq rv (concat new-text (number-to-string rem-fields)))
        )
      )
    rv))

(defun fen/update-current-field ()
  (let (
        (p) (olay) (cur) (mod-str)
        )
    
    (when (looking-at "[/}]")
      (backward-char))
    
    (setq olay (car (books/overlays-at-point 'category 'fen-field p)))
    
    (when olay
      (setq cur (buffer-substring-no-properties (overlay-start olay) (overlay-end olay)))
      (setq mod-str (fen/update-fen-field cur))
      (unless (s-equals? mod-str cur)
        (books/do-in-overlay 'fen
                             (books/replace-text-in-range (overlay-start olay) (overlay-end olay) mod-str)
                             (fen/refresh-overlays)
                             (save-buffer))
        ))
    ))

(defun fen/jump-to-field (type)
  (let (
        (fun)
        (field)
        )
    (cl-case type
      ('next (setq fun 'books/next-overlay))
      ('prev (setq fun 'books/prev-overlay))
      )
    (when fun
      (fen/update-current-field)
      (books/do-in-overlay 'fen
                           (when (looking-at "/")
                             (backward-char))
                           (setq field (funcall fun 'category 'fen-field))
                           ))

    (books/do-in-overlay 'fen
                         (remove-overlays (point-min) (point-max) 'category 'fen-edit))
    
    (when field
      (fen/allow-edit field)
      (goto-char (overlay-start field)))
    ))

(defun fen/undo ()
  (interactive)
  (books/do-in-overlay 'fen
                       (undo)
                       (fen/refresh-overlays)
                       (save-buffer))
  )
    
(defun fen/jump-to-next-field ()
  (interactive)
  (fen/jump-to-field 'next))

(defun fen/jump-to-prev-field ()
  (interactive)
  (fen/jump-to-field 'prev))

(defun fen/goto-fen-or-field (type)
  (let ((fen (car (books/overlays-at-point 'category 'fen)))
        (fun)
        )

    (cl-case type
      ('first (setq fun '-first-item))
      ('last (setq fun '-last-item)))

    (if fen
        (fen/goto-field fun)
      (fen/goto-fen fun))
    ))

(defun fen/goto-field (fun)
  (let ((fields) (field))
    (setq fields (books/overlays-in fen 'category 'fen-field))
    (setq field (funcall fun fields))
    (when field
      (goto-char (overlay-start field))
      (books/remove-other-overlays-in 'fen 'fen-edit)
      (fen/allow-edit field)
      )
    ))

(defun fen/goto-fen (fun)
  (let ((fens (books/overlays-in (skg/file-range) 'category 'fen))
        (err "No more fens"))

    (remove-overlays (point-min) (point-max) 'category 'fen-edit)
    (if fens
        (progn
          (goto-char (overlay-start (funcall fun fens)))
          (books/do-in-overlay 'fen-field
                               (fen/allow-edit _overlay))
          )
      (skg/error err))
    ))

(defun fen/next-or-prev (type)
  (let ((olay) (field) (fun))

    (books/do-in-overlay 'category 'fen
                         (remove-overlays (point-min) (point-max) 'category 'fen-edit))
    
    (cl-case type
      ('prev (setq fun 'books/prev-overlay))
      ('next (setq fun 'books/next-overlay))
      )

    (when fun (setq olay (funcall fun 'category 'fen)))
    (when olay
      (goto-char (overlay-start olay))
      (and (setq field (car (books/overlays-at-point 'category 'fen-field))) (fen/allow-edit field))
      )
    ))

(defun fen/toggle-edit-field ()
  (interactive)
  (let ((edit (car (books/overlays-at-point 'category 'fen-edit))))
    (if edit
        (remove-overlays (overlay-start edit) (overlay-end edit) 'category 'fen-edit)
      (progn
        (books/do-in-overlay 'fen-field
                             (fen/allow-edit _overlay))
        (and (setq field (car (books/overlays-at-point 'category 'fen-field))) (goto-char (overlay-start field)))
        ))
    ))

(defun fen/set-defaults-for-field ()
  (interactive)
  (let ((field (car (books/overlays-at-point 'field-num))) (num) (def))
    (when field
      (setq num (overlay-get field 'field-num))
      (setq def (ht-get fen/defaults num))
      (books/do-in-overlay 'fen
                           (books/replace-text-in-range (overlay-start field) (overlay-end field) def)
                           (fen/refresh-overlays)
                           (save-buffer)
                           )
      )
    ))

(defun fen/all-to (type)
  (let ((defs) (nums (number-sequence 1 8)))
    
    (cl-case type
      ('defaults (setq defs (--map (ht-get fen/defaults it) (reverse nums))))
      ('same (setq defs (--map "8" nums))))

    (when defs
      (books/do-in-overlay 'fen
                           (books/replace-text-in-range (point-min) (point-max) (s-join "/" defs))
                           (fen/refresh-overlays)))
    (save-buffer)
    ))

(defun fen/num-before-cursor ()
  (let ((num))
    (save-excursion
      (when (looking-back "[1-8]")
        (setq num (string-to-number (match-string 0)))))
    num))

(defmacro fen/inhibit-modification (&rest body)
  `(let ((inhibit-modification-hooks t))
     ,@body
     ))

(defun fen/last-field ()
  (looking-at-p "}"))

(defun fen/increment-square ()
  (interactive)
  (let ((last) (ins) (nums) (num (fen/num-before-cursor)))

    (when (fen/last-field)
      (setq last t)
      (unless num (setq ins t)))
    
    (unless num
      (setq ins t)
      (setq num 0))

    (and num (< num 8) (setq num (1+ num)))
    (setq nums (number-to-string num))
    
    (if ins
        (insert nums)
      (fen/inhibit-modification (replace-match nums))
      )

    (when last (backward-char))
    (books/do-in-overlay 'fen (fen/refresh-overlays))
    (when last (forward-char))
    ))

(defun fen/decrement-square ()
  (interactive)
  (let ((last) (ins) (nums) (num (fen/num-before-cursor)))

    (when (fen/last-field)
      (setq last t)
      (setq ins t))

    (when num  (setq num (1- num)))

    (if (= num 0)
        (setq nums "")
      (setq nums (number-to-string num)))

    (fen/inhibit-modification (replace-match nums))

    (when last (backward-char))
    (books/do-in-overlay 'fen (fen/refresh-overlays))
    (when last (forward-char))
    ))

(defun fen/toggle-move-color ()
  (interactive)
  (let ((done))
    (skg/case-sensitive
        (save-excursion
          (when (re-search-backward "fen" (line-beginning-position) t)
            (setq done t)
            (replace-match "Fen" t)))
      (unless done
        (save-excursion
          (when (re-search-backward "Fen" (line-beginning-position) t)
            (setq done t)
            (replace-match "fen" t))
          ))
      )
    (when done (save-buffer))
    ))

(key-chord-define fen-minor-mode-keymap "bb" "B")
(key-chord-define fen-minor-mode-keymap "rr" "R")
(key-chord-define fen-minor-mode-keymap "qq" "Q")
(key-chord-define fen-minor-mode-keymap "pp" "P")
(key-chord-define fen-minor-mode-keymap "kk" "K")
(key-chord-define fen-minor-mode-keymap "nn" "N")

(define-key fen-minor-mode-keymap (kbd "e") 'fen/toggle-edit-field)
(define-key fen-minor-mode-keymap (kbd "d") 'fen/set-defaults-for-field)
(define-key fen-minor-mode-keymap (kbd "c") 'fen/clear-current-field)
(define-key fen-minor-mode-keymap (kbd "u") 'fen/undo)
(define-key fen-minor-mode-keymap (kbd "a") '(lambda () (interactive) (fen/all-to 'defaults)))
(define-key fen-minor-mode-keymap (kbd "s-a") '(lambda () (interactive) (fen/all-to 'same)))
(define-key fen-minor-mode-keymap (kbd "<tab>") 'fen/jump-to-next-field)
(define-key fen-minor-mode-keymap (kbd "s-<tab>") 'fen/jump-to-prev-field)
(define-key fen-minor-mode-keymap (kbd "<home>") '(lambda () (interactive) (fen/goto-fen-or-field 'first)))
(define-key fen-minor-mode-keymap (kbd "<end>") '(lambda () (interactive) (fen/goto-fen-or-field 'last)))
(define-key fen-minor-mode-keymap (kbd "<next>") '(lambda () (interactive) (fen/next-or-prev 'next)))
(define-key fen-minor-mode-keymap (kbd "<prior>") '(lambda () (interactive) (fen/next-or-prev 'prev)))
(define-key fen-minor-mode-keymap (kbd ".") 'fen/increment-square)
(define-key fen-minor-mode-keymap (kbd "m") 'fen/decrement-square)
(define-key fen-minor-mode-keymap (kbd "f") 'fen/toggle-move-color)

(eval-after-load 'lush-theme
  '(progn
     (custom-theme-set-faces
      'lush
      '(fen/bg-face ((t (:background "gray20"))))
      '(fen/white-piece-face ((t (:foreground "magenta"))))
      '(fen/black-piece-face ((t (:foreground "cyan"))))
      '(fen/empty-square-face ((t (:foreground "yellow"))))
      '(fen/edit-face ((t (:background "black"))))
      )))

(eval-after-load 'alect-dark-theme
  '(progn
     (custom-theme-set-faces
      'alect-dark
      '(fen/bg-face ((t (:background "gray10"))))
      '(fen/white-piece-face ((t (:foreground "goldenrod"))))
      '(fen/black-piece-face ((t (:foreground "dark goldenrod"))))
      '(fen/empty-square-face ((t (:foreground "rosy brown"))))
      '(fen/edit-face ((t (:background "medium green"))))
      )))

(eval-after-load 'alect-light-alt-theme
  '(progn
     (custom-theme-set-faces
      'alect-light-alt
      '(fen/bg-face ((t (:background "gray90"))))
      '(fen/white-piece-face ((t (:foreground "blue"))))
      '(fen/black-piece-face ((t (:foreground "dark blue"))))
      '(fen/empty-square-face ((t (:foreground "firebrick"))))
      '(fen/edit-face ((t (:background "yellow"))))
      )))


(eval-after-load 'alect-light-theme
  '(progn
     (custom-theme-set-faces
      'alect-light
      '(fen/bg-face ((t (:background "gray90"))))
      '(fen/white-piece-face ((t (:foreground "red"))))
      '(fen/black-piece-face ((t (:foreground "dark red"))))
      '(fen/empty-square-face ((t (:foreground "dark goldenrod"))))
      '(fen/edit-face ((t (:background "light yellow"))))
      )))
