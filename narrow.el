(books/defkeymap books/narrow-mode-map "Keymap for narrowed buffers")
(define-minor-mode books/narrow-mode
  "Mode to hold keymap for narrowed buffers"
  :lighter " NB"
  :keymap books/narrow-mode-map)

(defun books/show-main-file ()
  (interactive)
  (let ((p (point)))
    (and (buffer-narrowed-p) books/base-buffer
         (progn
           (switch-to-buffer-other-window books/base-buffer)
           (widen)
           (goto-char p)))
    ))


(define-key books/narrow-mode-map (kbd "C-<kp-enter>") 'books/show-main-file)

(provide 'books/narrow)
