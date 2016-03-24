(provide 'books/fen)
(require 'pcre2el)
(require 'regexp-opt)
(require 'key-chord)
(books/require 'show-book)

(defun fm/init ()
  (defconst fm/field "[{/][bpqrnkKBPQRN1-8]+")
  (defconst fm/field-sep "/")
  (setq fm/keywords (list
                     '("[BQRNKP]" . fm/white-piece-face)
                     '("[bqrnkp]" . fm/black-piece-face)
                     '("[1-8]" . fm/empty-square-face)
                     ))

  (books/defface fm/white-piece-face "Face used to display white pieces" fen (:foreground "white"))
  (books/defface fm/overlay-face "Face used to highlight field" fen (:background "dark gray"))
  (books/defface fm/black-piece-face "Face used to display black pieces" fen (:foreground "white"))
  (books/defface fm/empty-square-face "Face used to display empty squares" fen (:foreground "yellow"))
  (add-hook 'before-change-functions 'fm/clear-if-required)
  )

(defun fm/key-chord-defines ()
  (key-chord-define books/fen-mode-map "bb" "B")
  (key-chord-define books/fen-mode-map "rr" "R")
  (key-chord-define books/fen-mode-map "qq" "Q")
  (key-chord-define books/fen-mode-map "pp" "P")
  (key-chord-define books/fen-mode-map "kk" "K")
  (key-chord-define books/fen-mode-map "nn" "N")
  )

(defun fm/clear-if-required (start end)
  (and (region-active-p)
       (delete-region (region-beginning) (region-end)))
  )

(books/new-mode "fen" fundamental-mode "Fen"
                (fm/init)
                (key-chord-mode 1)
                (smartparens-mode -1)
                (show-page-mode t)
                (setq font-lock-defaults '(fm/keywords))
                (fm/key-chord-defines)
                )

(defun fm/remove-all-overlays ()
  (interactive)
  (remove-overlays (point-min) (point-max)))

(defun fm/highlight-current-field ()
  )

(defun fm/ends-with-number (text)
  (s-matches? "[[:digit]]$" text))

(defun fm/update-fen-field (text)
  (let (
        (rv text)
        (rem-fields 8)
        (cur-num 0)
        (new-text)
        (len (length text))
        )
    (if (fm/ends-with-number text)
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

(defun fm/update-current-field ()
  (let* (
        (mod-str) (bounds (bounds-of-thing-at-point 'word)) (cur)
        )
    (setq cur (buffer-substring-no-properties (car bounds) (cdr bounds)))
    (setq mod-str (fm/update-fen-field cur))
    (beginning-of-thing 'word)
    (unless (equal cur mod-str)
      (delete-region (car bounds) (cdr bounds))
      (insert mod-str)
      (goto-char (car bounds))
      (save-buffer))
    ))

(defun fm/goto-next-field ()
  (let ((start) (end))
    (if (looking-at "{") (fm/first-field)
      (re-search-forward fm/field-sep (point-max) t))
    (er/mark-word)
    ))

(defun fm/goto-prev-field ()
  (let ((overlay) (start) (end))
    (if (looking-at "{") (fm/last-field)
      (when (re-search-backward fm/field (point-min) t)
        (goto-char (1+ (match-beginning 0)))))
    (er/mark-word)
    ))

(defun fm/next-field ()
  (interactive)
  (fm/update-current-field)
  (fm/goto-next-field)
  )

(defun fm/prev-field ()
  (interactive)
  (fm/update-current-field)
  (fm/goto-prev-field)
  )

(defun fm/clear-current-field ()
  (interactive)
  (er/mark-word)
  (books/replace-text-in-range (region-beginning) (region-end) "8")
  )

(defun fm/set-all-fields-to-defaults ()
  (interactive)
  (delete-region (point-min) (point-max))
  (insert "{8/8/8/8/8/8/8/8}")
  (save-buffer))

(defun fm/show-full-board ()
  (interactive)
  (fm/set-all-fields-to-defaults)
  (fm/first-field)
  (books/replace-text-in-range (point) (1+ (point)) "rnbqkbnr")
  (fm/next-field)
  (books/replace-text-in-range (point) (1+ (point)) "pppppppp")
  (fm/last-field)
  (books/replace-text-in-range (point) (1+ (point)) "RNBQKBNR")
  (save-buffer)
  )

(defun fm/first-field ()
  (interactive)
  (let ((end))
    (goto-char (point-min))
    (when (re-search-forward fm/field (point-max) t)
      (goto-char (1+ (match-beginning 0))))
    (er/mark-word)
    ))

(defun fm/last-field ()
  (interactive)
  (let ((end))
    (goto-char (point-max))
    (when (re-search-backward fm/field (point-min) t)
      (goto-char (1+ (match-beginning 0))))
    (er/mark-word)
    ))

(defun fm/change-case()
  (interactive)
  (let ((lc (s-lowercase? (char-to-string (char-after)))))
    (if lc
        (upcase-region (point) (1+ (point)))
      (downcase-region (point) (1+ (point))))
    (forward-char 1)
    ))

(defun fm/clear-fen-text()
  (interactive)
  (save-excursion
    (when (re-search-backward "\\\\fen\\({[^}]*}\\)" (point-min) t)
      (replace-match "\\1"))
    ))

(defun fm/next-fen ()
  (interactive)
  (let ((start) (end))
    (when (buffer-narrowed-p)
      (goto-char (point-max))
      (widen)
      (fm/clear-fen-text)
      (save-buffer))
    (widen)
    (when (re-search-forward "fen\\( = \\)?{" (point-max) t)
      (setq start (1- (match-end 0))))

    (when (re-search-forward "}" (point-max) t)
      (setq end (match-end 0)))

    (if (and start end)
        (progn
          (narrow-to-region start end)
          (goto-char (point-min)))
      (kill-buffer))
    ))


(eval-after-load 'books/fen
  '(progn
     (define-key books/fen-mode-map (kbd "<tab>") 'fm/next-field)
     (define-key books/fen-mode-map (kbd "<backtab>") 'fm/prev-field)
     (define-key books/fen-mode-map (kbd "c") 'fm/clear-current-field)
     (define-key books/fen-mode-map (kbd "d") 'fm/set-all-fields-to-defaults)
     (define-key books/fen-mode-map (kbd "f") 'fm/show-full-board)
     (define-key books/fen-mode-map (kbd "<home>") 'fm/first-field)
     (define-key books/fen-mode-map (kbd "<end>") 'fm/last-field)
     (define-key books/fen-mode-map (kbd "`") 'fm/change-case)
     (define-key books/fen-mode-map (kbd "<f2>") 'fm/next-fen)
     ))

(add-hook 'books/fen-mode-hook
          '(lambda () (progn
                        (yas-minor-mode -1))))


(eval-after-load 'fogus-theme '(progn
                                 (custom-theme-set-faces
                                  'fogus
                                  '(fm/black-piece-face ((t (:foreground "dodger blue"))))
                                  '(fm/white-piece-face ((t (:foreground "light salmon"))))
                                  '(fm/empty-square-face ((t (:foreground "yellow"))))
                                  )))


(eval-after-load 'tango-2-theme '(progn
                                   (custom-theme-set-faces
                                    'tango-2
                                    '(fm/black-piece-face ((t (:foreground "violet red"))))
                                    '(fm/white-piece-face ((t (:foreground "spring green"))))
                                    '(fm/empty-square-face ((t (:foreground "goldenrod"))))
                                    )))

;; (books/defkeymap books/fen-minor-mode-keymap "Keymap used in books/fen-minor mode")

;; (define-minor-mode books/fen-minor-mode
;;   "Some Documentation"
;;   :lighter " fen"
;;   :keymap books/fen-minor-mode-keymap
;;   (books/fen-minor/startup-or-shutdown)
;;   )

;; (defun books/fen-minor/startup-or-shutdown ()
;;   (if (symbol-value 'books/fen-minor-mode)
;;       (books/fen-minor/startup)
;;     (books/fen-minor/shutdown)
;;     ))

;; (defun books/fen-minor/fontify (start end)
;;   (let ((text (buffer-substring-no-properties start end)) (new-text))
;;     (setq new-text (with-temp-buffer
;;                      (erase-buffer)
;;                      (insert text)
;;                      (delay-mode-hooks (books/fen-mode))
;;                      (font-lock-default-function 'books/fen-mode)
;;                      (font-lock-default-fontify-region (point-min) (point-max) nil)
;;                      (buffer-string)))
;;     (books/replace-text-in-range start end new-text)
;;     ))

;; (defun books/fen-minor/startup ()
;;   (let ((regex "[fF]en{\\([^}]*\\)}"))
;;     (skg/search-file-do regex
;;       (books/fen-minor/fontify (match-beginning 1) (match-end 1))
;;       )
;;     ))

;; (defun books/fen-minor/shutdown ()
;;   )

;; (define-key books/fen-minor-mode-keymap (kbd "<f1>") 'help)
