(books/defkeymap books/single-page-mode-map "Keymap for single page minor mode")
(define-minor-mode books/single-page-mode
  "Minor mode to edit single page of book"
  :lighter " single-page"
  :keymap books/single-page-mode-map)

(defun books/write-back-to-base ()
   (interactive)
  (and base-file base-file-range
       (let ((buf (current-buffer)) (file (buffer-file-name)) (range base-file-range) (text) (pos))
         (goto-char (point-min))
         (when (re-search-forward "Local Variables:" (point-max) t)
           (setq pos (1- (line-beginning-position)))
           (setq text (buffer-substring-no-properties (point-min) pos)))
         (when text
           (skg/open-file base-file)
           (books/replace-text-in-range (car range) (cdr range) text)
           (save-buffer)
           (kill-buffer buf)
           (f-delete file))
         )))

(defun books/show-only-this-page (begin end)
  (interactive "r")
  (when (region-active-p)
    (let* (
           (text (buffer-substring-no-properties (region-beginning) (region-end)))
           (file (buffer-file-name))
           (dir (f-dirname file))
           (base (f-filename file))
           (page-file (f-join dir (format "page-%s" base)))
           )
      (unless (books/if-minor-mode-active 'books/single-page-mode)
        (with-temp-file page-file
          (setq comment-start "%%")
          (insert text)
          (newline 2)
          (add-file-local-variable 'base-file file)
          (add-file-local-variable 'base-file-range (cons begin end))
          (books/add-file-local-eval (books/single-page-mode t))
          (put 'base-file-range 'safe-local-variable 'consp)
          (put 'base-file 'safe-local-variable 'stringp)
          )
        (find-file-existing page-file)
        (skg/revert-buffer))
      )))


(define-key books/single-page-mode-map (kbd "<f5>") 'books/write-back-to-base)
;; (define-key books/single-page-mode-map (kbd "<f1>") 'books/major-mode)

