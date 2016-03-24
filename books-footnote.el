(provide 'books/footnote)

(books/defkeymap books/footnote-mode-keymap "Keymap used in books/footnote mode")

(define-minor-mode books/footnote-mode
  "Some Documentation"
  :lighter " bf"
  :keymap books/footnote-mode-keymap
  (books/footnote/startup-or-shutdown)
  )

(defvar books/footnotes nil "All footnotes in the chapter")
(defvar books/footnote-regex  "\\[\\([0-9]+\\)\\]" "Footnotes regex")

(defun books/footnote/startup-or-shutdown ()
  (if (symbol-value 'books/footnote-mode)
      (books/footnote/startup)
    (books/footnote/shutdown)
    ))

(defun books/footnote/vbtm ()
  (let ((start) (end))
    (widen)
    (skg/search-file-do "\\\\footnote{"
      (setq start (match-beginning 0))
      (backward-char)
      (forward-list)
      (setq end (point))
      (save-restriction
        (narrow-to-region start end)
        (replace-string "\\vbtm{" "\\vbtmfoot{" nil (point-min) (point-max))
        (replace-regexp "\\\\\\(begin\\|\\end\\){Code}" "\\\\\\1{CodeFootnote}" nil (point-min) (point-max))
        (goto-char (point-max)))
      )
    (save-buffer)
    ))

(defun books/footnote/gather-footnotes ()
  (let ((footnotes) (key) (regex (format "^%s" books/footnote-regex)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regex (point-max) t)
        (setq key (match-string-no-properties 1))
        (er/mark-text-paragraph)
        (add-to-list 'footnotes (cons key (buffer-substring-no-properties (region-beginning) (region-end))) t)
        (exchange-point-and-mark)
        ))
    (setq books/footnotes (--map (cons (car it) (s-trim (replace-regexp-in-string regex "" (cdr it)))) footnotes))
    ))

(defun books/footnote/startup ()
  (setq books/foonotes nil)
  (books/footnote/gather-footnotes)
  )

(defun books/footnote/shutdown ()
  (let ((regex (format "^%s" books/footnote-regex)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regex (point-max) t)
        (mark-paragraph)
        (delete-region (region-beginning) (region-end))
        ))
    (save-buffer)
    ))

(defun books/footnote/first ()
  (interactive)
  (goto-char (point-min))
  (books/footnote/next))

(defun books/footnote/next ()
  (interactive)
  (forward-char)
  (when (re-search-forward books/footnote-regex (point-max) t)
    (goto-char (match-beginning 0))))

(defun books/footnote/paste ()
  (interactive)
  (let ((footnote))
    (when (looking-at books/footnote-regex)
      (setq footnote (assoc (match-string-no-properties 1) books/footnotes))
      (when footnote
        (replace-match (format "\\\\footnote{%s}" (cdr footnote)))
        (fill-paragraph)
        )
      )))
    

(define-key books/footnote-mode-keymap (kbd "<f1>") 'books/footnote/paste)
(define-key books/footnote-mode-keymap (kbd "<home>") 'books/footnote/first)
(define-key books/footnote-mode-keymap (kbd "<kp-add>") 'books/footnote/next)
