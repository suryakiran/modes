(provide 'books/winnings/soln-mode)
(require 'pcre2el)
(require 'regexp-opt)
(require 'key-chord)


(require 'books/chess (locate-library "chess"))

;; (-map
;;  (lambda (arg) (cm/deftag (car arg) (cdr arg) 'books/winnings/soln-mode))
;;  chess/moves-type-alist)

;; (--map (books/deftag (car it) (cdr it) 'books/winnings/soln-mode) books/text-decor-alist)

(defvar books/winings/soln-mode-hook () "Hooks to run after solution mode is activated")
(defvar books/winnings/soln-mode-map (make-sparse-keymap) "Keymap used in soln mode buffers")
(defvar books/winnings/old-mode-line-format () "Old modeline format")

(defgroup wsm nil
  "Winnings Book Solution Group"
  :prefix "wsm-" :group 'convenience)

(defvar winnings/solution-face 'winnings/solution-face "Face used to display string Solution")
(defface winnings/solution-face
  '((t (:foreground "green" :weight bold)))
  "Face used to display string Solution"
  :group 'wsm)

(defvar winnings/itemize-face 'winnings/itemize-face "Face used to display string Itemize")
(defface winnings/itemize-face
  '((t (:foreground "gold" :weight bold)))
  "Face used to display string Itemize"
  :group 'wsm)

(defvar winnings/item-face 'winnings/item-face "Face used to display string item")
(defface winnings/item-face
  '((t (:foreground "gold" :weight bold)))
  "Face used to display string item"
  :group 'wsm)

(defvar winnings/solution-num-face 'winnings/solution-num-face "Face used to display Solution number")
(defface winnings/solution-num-face
  '((t (:weight bold)))
  "Face used to display Solution number"
  :group 'wsm)

(defun w/sm/join-words ()
  (interactive)
  (let ((text))
    (unless (region-active-p)
      (books/mark-next-word)
      (books/mark-next-word)
      )

    (setq text (buffer-substring-no-properties (region-beginning) (region-end)))
    (setq text (replace-regexp-in-string "[[:space:]]*" "" text))
    (books/replace-text-in-range (region-beginning) (region-end) text)))

(defun w/sm/new-solution ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (insert "\\Solution\n")
  ))

(defun w/sm/cleanup()
  (interactive)
  (let ((text) (end (point)) (start))
    (setq start (re-search-backward "{" (point-min) t))
    
    (setq text (buffer-substring-no-properties start end))

    (skg/case-sensitive
        (setq text (s-replace "l" "1" text))
      (setq text (s-replace " " "" text))
      (setq text (s-replace ".?." "2" text))
      (setq text (s-replace "H" "8" text))
      (setq text (replace-regexp-in-string "J\\.?I" "1?" text t t))
      (setq text (replace-regexp-in-string "8\\([a-h]\\)" "B\\1" text))
      (setq text (replace-regexp-in-string "\\([\\?!+][\\?!+]?\\)" "\\1 " text))
      (setq text (replace-regexp-in-string "\\([0-9]+\\.\\)\\([a-hKQRBN]\\)" "\\1 \\2" text))
      (setq text (replace-regexp-in-string "\\([RN][a-h]x?[a-h][0-9]\\)\\([0-9a-hKQRBN]\\)" "\\1 \\2" text))
      (setq text (replace-regexp-in-string "\\([RN][1-8]x?[a-h][0-9]\\)\\([0-9a-hKQRBN]\\)" "\\1 \\2" text))
      (setq text (replace-regexp-in-string "\\([KQBRN]x?[a-h][0-9]\\)\\([0-9a-hKQRBN]\\)" "\\1 \\2" text))
      (setq text (s-replace "#" "\\#" text))
      (setq text (s-replace " }" "}" text))
      )
    (books/replace-text-in-range start end text)
    ))

(defun w/sm/itemize-square-braces-contents()
  (interactive)
  (let ((start) (end) (text))
    (goto-char (point-min))
    (while (re-search-forward "[[]" (point-max) t)
      (save-excursion
        (setq start (point))
        (backward-char)
        (forward-list)
        (setq end (point))
        (setq text (buffer-substring-no-properties start (1- end)))
        (books/replace-text-in-range
         (1- start) end (format "\n\\begin{Itemize}\n\\item %s\n\\end{Itemize}\n" text)))))
  (save-buffer))

(defun w/sm/itemize-contents-in-parens ()
  (interactive)
  (let ((start) (end) (text))
    (backward-char)
    (save-excursion
      (forward-char)
      (setq start (point))
      (forward-list)
      (setq end (point))
      (setq text (buffer-substring-no-properties (1+ start) (1- end)))
      (books/replace-text-in-range
       start end (format "\n\\begin{Itemize}\n\\item %s\n\\end{Itemize}\n" text))))
  (save-buffer))

(defun w/sm/final-cleanup ()
  (interactive)
  (save-excursion
    ;; (replace-string " = " "\\equal" nil (point-min) (point-max))
    (replace-string "-+" "\\BBetter" nil (point-min) (point-max))
    (replace-string "+-" "\\WBetter" nil (point-min) (point-max))
    (replace-string "±" "\\wBetter" nil (point-min) (point-max))
    (save-buffer)
    ))

(defun w/sm/chess-sign (text)
  (if (region-active-p)
      (books/replace-text-in-range (region-beginning) (region-end) text)
    (books/replace-text-in-range (point) (1+ (point)) text)
    ))

  
(defun w/sm/bbetter ()
  (interactive)
  (w/sm/chess-sign "\\bbetter"))

(defun w/sm/equal ()
  (interactive)
  (w/sm/chess-sign "\\equal"))

(defun w/sm/wbetter ()
  (interactive)
  (w/sm/chess-sign "\\wbetter"))

(defun w/sm/BBetter ()
  (interactive)
  (w/sm/chess-sign "\\BBetter"))

(defun w/sm/WBetter ()
  (interactive)
  (w/sm/chess-sign "\\WBetter"))

(defun w/sm/line-mainline ()
  (interactive)
  (skg/select-line)
  (execute-kbd-macro [?m]))

(defun w/sm/line-variation ()
  (interactive)
  (skg/select-line)
  (execute-kbd-macro [?v]))

(defun w/sm/add-hooks ()
  (add-hook 'chess/post-mainline-hook 'w/sm/cleanup)
  (add-hook 'chess/post-variation-hook 'w/sm/cleanup)
  (add-hook 'chess/post-dirty-hook 'w/sm/cleanup)
  (add-hook 'books/after-itemize-hook 'w/sm/capitalize-itemize)
  )

(defun w/sm/next-solution ()
  (interactive)
  (widen)
  (let ((current (point)) (starts (winnings/solution-starts)) (next-soln-start) (next-soln-end))
    (save-buffer)
    (setq next-soln-start (--find-index (> it current) starts))
    (if next-soln-start
        (progn
          (setq next-soln-end (nth (1+ next-soln-start) starts))
          (unless next-soln-end (setq next-soln-end (point-max)))
          (setq books/winnings/solution-number (1+ books/winnings/solution-number))
          (narrow-to-region (nth next-soln-start starts) next-soln-end)
          )
      (kill-buffer))
    ))

(defun w/sm/modify-mode-line ()
  (add-to-list 'mode-line-format '(:eval (w/sm/solution-number)) t))

(defun w/sm/solution-number ()
  (let ((ret))
    (if (stringp books/winnings/solution-number)
        (setq ret (format "[%s]" books/winnings/solution-number))
      (setq ret (format "[Solution #%d]" books/winnings/solution-number)))
    (propertize ret 'face 'winnings/solution-num-face)))

(defun w/sm/itemize-environment ()
  (interactive)
  (unless (region-active-p)
    (skg/select-line))
  (save-restriction
    (narrow-to-region (region-beginning) (region-end))
    (latex-mode)
    (books/itemize-environment-function)
    )
  (books/winnings/soln-mode)
  (save-buffer)
  (books/winnings/soln-mode))

(defun w/sm/capitalize-itemize ()
  (interactive)
  (replace-string "itemize" "Itemize" nil (point-min) (point-max))
  )

(defun w/sm/cycle-variation ()
  (interactive)
  (books/cycle-words (list "vone" "vtwo" "vthree" "vfour" "vfive" "vall")))

(key-chord-define books/winnings/soln-mode-map "bb" 'w/sm/bbetter)
(key-chord-define books/winnings/soln-mode-map "ww" 'w/sm/wbetter)
(key-chord-define books/winnings/soln-mode-map "mp" 'w/sm/BBetter)
(key-chord-define books/winnings/soln-mode-map "pm" 'w/sm/WBetter)
(key-chord-define books/winnings/soln-mode-map "ee" 'w/sm/equal)
(key-chord-define books/winnings/soln-mode-map "mm" 'w/sm/line-mainline)
(key-chord-define books/winnings/soln-mode-map "vv" 'w/sm/line-variation)

(setq w/sm/keywords (list
                     '("\\\\Solution" . winnings/solution-face)
                     '("Itemize" . winnings/itemize-face)
                     '("\\\\item" . winnings/item-face)
                     ))

(define-derived-mode books/winnings/soln-mode fundamental-mode "Solutions"
  "Major mode for editing Solutions in Winnings book"
  (use-local-map books/winnings/soln-mode-map)
  (set (make-local-variable 'books/winnings/solution-number) "All Solutions")
  (smartparens-mode)
  (key-chord-mode 1)
  (setq font-lock-defaults '(w/sm/keywords))
  (setq books/winnings/old-mode-line-format mode-line-format)
  (w/sm/modify-mode-line)
  (w/sm/add-hooks)
  (run-hooks 'books/winnings/soln-mode-hook)
  )

(define-key books/winnings/soln-mode-map (kbd "<f1>") 'w/sm/join-words)
(define-key books/winnings/soln-mode-map (kbd "<f2>") 'w/sm/next-solution)
(define-key books/winnings/soln-mode-map (kbd "<f4>") 'books/mark-next-word)
(define-key books/winnings/soln-mode-map (kbd "<f5>") 'books/mark-next-char)
(define-key books/winnings/soln-mode-map (kbd "<f6>") 'books/itemize-line)
(define-key books/winnings/soln-mode-map (kbd "<f7>") 'w/sm/cycle-variation)
(define-key books/winnings/soln-mode-map (kbd "<f11>") 'w/sm/itemize-square-braces-contents)
(define-key books/winnings/soln-mode-map (kbd "<f12>") 'w/sm/itemize-contents-in-parens)
(define-key books/winnings/soln-mode-map (kbd "C-<right>") 'books/next-word)
(define-key books/winnings/soln-mode-map (kbd "s-i") 'w/sm/itemize-environment)
;; (define-key books/winnings/soln-mode-map (kbd "C-s-v") 'w/sm/line-variation)

(add-hook 'books/fen-mode-hook
          '(lambda () (progn
                        (yas-minor-mode -1))))

(eval-after-load 'odersky-theme
  '(progn
     (custom-theme-set-faces
      'odersky
      '(winnings/solution-num-face ((t (:weight bold :foreground "maroon" :background "dark gray"))))
      )))

(eval-after-load 'tango-2-theme
  '(progn
     (custom-theme-set-faces
      'tango-2
      '(winnings/solution-num-face ((t (:foreground "yellow" :background "black"))))
      '(winnings/item-face ((t (:foreground "light sea green"))))
      )))
