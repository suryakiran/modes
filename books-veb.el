(provide 'books/veb)
(require 'thingatpt)

(books/defkeymap books/veb-mode-keymap "Keymap used in books/veb mode")
(books/defkeymap books/veb-mode-overlay-keymap "Keymap used in books/veb mode overlays")
(books/defface books/veb/header-face "Face used to display header in veb mode" veb
               (:foreground "red"))

(books/defface books/veb/search-face "Face used to display search string in veb mode" veb
               (:background "medium turquoise" :foreground "white"))


(defvar books/veb/env "" "One of verb/emph/bold keys")
(defvar books/veb/file "" "File to get strings from")
(defvar books/veb/ignore-envs '() "Ignore creating overlays in these env regions")

(define-minor-mode books/veb-mode
  "Some Documentation"
  :lighter " veb"
  :keymap books/veb-mode-keymap
  (books/veb/startup-or-shutdown)
  )

(defun books/veb/startup-or-shutdown ()
  (if (symbol-value 'books/veb-mode)
      (books/veb/startup)
    (books/veb/shutdown)
    ))

(defun books/veb/refresh ()
  (interactive)
  (books/veb/update-overlays)
  (remove-overlays (point-min) (point))
  )


(defun books/veb/header ()
  (let ((out) (section) (subsection) (start) (end))
    (if books/veb/env
        (progn
          (setq out (format "Type: %s File: %s" books/veb/env
                            (if (f-exists? books/veb/file)
                                (f-filename books/veb/file)
                              "No Such File")))
          )
      (setq out "***** No Type Set *****")
      )

    (save-excursion
      (when (re-search-backward "^\\\\section{" (point-min) t)
        (setq start (match-end 0))
        (backward-char)
        (forward-list)
        (setq end (1- (point)))
        (setq section (buffer-substring-no-properties start end))
        ))
    (when end
      (save-excursion
        (when (re-search-backward "^\\\\subsection{" end t)
          (setq start (match-end 0))
          (backward-char)
          (forward-list)
          (setq end (1- (point)))
          (setq subsection (buffer-substring-no-properties start end)))
        ))

    (when section
      (setq out (format "%s SEC: %s" out section))
      (when subsection
        (setq out (format "%s SUB: %s" out subsection))
        ))
    (propertize out 'face 'books/veb/header-face)
    ))

(defun books/veb/create-overlay (start end)
  (let ((overlay (make-overlay start end (current-buffer) nil t)))
    (overlay-put overlay 'books/veb/search-key t)
    (overlay-put overlay 'face 'books/veb/search-face)
    (overlay-put overlay 'keymap books/veb-mode-overlay-keymap)
    overlay))

(defun books/veb/update-overlays ()
  (let ((overlay) (items) (buf (find-file-noselect books/veb/file)) (srch)
        (ignore-regions) (case-fold-search nil) (case-replace t)
        )
    (remove-overlays (point-min) (point-max) 'books/veb/search-key t)
    (setq srch (with-current-buffer buf
                 (goto-char (point-min))
                 (s-trim (thing-at-point 'line t))))

    (save-excursion
      (goto-char (point-min))
      (while (search-forward srch (point-max) t)
        (books/veb/create-overlay (match-beginning 0) (match-end 0))))

    (when books/veb/ignore-envs
      (setq ignore-regions (-flatten (--map (books/environment-regions it) books/veb/ignore-envs)))
      (--map
       (remove-overlays (car it) (cdr it) 'books/veb/search-key t) ignore-regions))
    ))

(defun books/veb/replace ()
  (interactive)
  (let ((line) (wrap books/veb/env) (overlay (books/veb-overlay t)))
    (when overlay
      (save-excursion
        (and (string-equal "vbtm" books/veb/env)
             (looking-back "\\\\\\(sub\\)*section{[^}]*" (line-beginning-position))
             (setq wrap "texttt")
             ))
      
      (wrap-region-with (format "\\%s{" wrap) "}")
      (save-buffer)
      (with-current-buffer (find-file-noselect books/veb/file)
        (goto-char (point-min))
        (delete-region (line-beginning-position) (line-end-position))
        (flush-lines "^ *$")
        (save-buffer))
      (books/veb/update-overlays)
      (books/veb/next)
      )
    ))

(defun books/veb-overlay (&optional mark)
  (let ((overlays) (ret))
    (setq overlays (--filter (overlay-get it 'books/veb/search-key) (overlays-at (point))))
    (when overlays
      (setq ret (car overlays))
      (when mark
        (goto-char (overlay-end ret))
        (set-mark (overlay-start ret))
        ))
    ret))

(defun books/veb/next ()
  (interactive)
  (let* ((overlays (overlays-in (point-min) (point-max)))
         (overlays (--filter (and (not (invisible-p (overlay-start it))) (overlay-get it 'books/veb/search-key)) overlays))
         (ranges (--annotate (cons (overlay-start it) (overlay-end it)) overlays))
         (overlays (--remove (= (caar it) (cdar it)) ranges))
         (overlays (--sort (< (caar it) (caar other)) overlays))
         (current (books/veb-overlay))
         (next)
         )

    (if current
        (setq next (--first (> (caar it) (overlay-end current)) overlays))
      (setq next (--first (> (caar it) (point)) overlays)))

    (when next
      (goto-char (caar next)))
    ))
    
(defun books/veb/prev ()
  (interactive)
  (let* ((overlays (overlays-in (point-min) (point-max)))
         (overlays (--filter (and (not (invisible-p (overlay-start it))) (overlay-get it 'books/veb/search-key)) overlays))
         (ranges (--annotate (cons (overlay-start it) (overlay-end it)) overlays))
         (overlays (--remove (= (caar it) (cdar it)) ranges))
         (overlays (--sort (> (caar it) (caar other)) overlays))
         (current (books/veb-overlay))
         (next)
         )

    (if current
        (setq next (--first (< (caar it) (overlay-start current)) overlays))
      (setq next (--first (< (caar it) (point)) overlays)))

    (when next
      (goto-char (caar next)))
    ))

(defun books/veb/skip ()
  (interactive)
  (with-current-buffer (find-file-noselect books/veb/file)
    (save-excursion
      (goto-char (point-min))
      (kill-line)
      (goto-char (point-max))
      (newline)
      (yank)
      (flush-lines "^ *$" (point-min) (point-max))
      (save-buffer)
      ))
  (books/veb/update-overlays)
  )

(defun books/veb/startup ()
  (setq books/veb/env nil)
  (setq books/veb/file nil)
  (add-to-list 'header-line-format '(:eval (books/veb/header)) t)
  )

(defun books/veb/shutdown ()
  (setq header-line-format nil)
  (setq books/veb/env nil)
  (setq books/veb/file nil)
  (remove-overlays (point-min) (point-max) 'books/veb/search-key t)
  )

(defun books/veb/key (type)
  (setq books/veb/env (symbol-name type))
  (setq books/veb/file (format "%s-%s" books/veb/env (f-filename (buffer-file-name))))
  (if (f-exists? books/veb/file)
      (books/veb/update-overlays)
    (remove-overlays (point-min) (point-max) 'books/veb/search-key t))
  )

(defun books/veb/vbtm ()
  (interactive)
  (books/veb/key 'vbtm)
  (set-face-attribute 'books/veb/header-face nil
                      :weight 'normal :slant 'normal :family "Consolas")
  )

(defun books/veb/fill-paragraph-and-search ()
  (interactive)
  (set-fill-column 1000000)
  (fill-paragraph)
  (goto-char (line-beginning-position))
  (books/veb/update-overlays)
  )

(defun books/veb/todo ()
  (interactive)
  (save-excursion
    (er/mark-text-paragraph)
    (goto-char (region-beginning))
    (insert "\\todo\n\n")
    (save-buffer)
    ))

(defun books/veb/ignore-environments (&rest envs)
  (setq books/veb/ignore-envs '())
  (--map (add-to-list 'books/veb/ignore-envs it) (-flatten envs))
  )

(defun books/veb/bold ()
  (interactive)
  (books/veb/key 'textbf)
  (set-face-attribute 'books/veb/header-face nil
                      :weight 'bold :slant 'normal :family "Segoe UI")
  )

(defun books/veb/emph ()
  (interactive)
  (books/veb/key 'emph)
  (set-face-attribute 'books/veb/header-face nil
                      :weight 'normal :slant 'italic :family "Segoe UI")
  )

(define-key books/veb-mode-keymap (kbd "C-s-e") 'books/veb/emph)
(define-key books/veb-mode-keymap (kbd "C-s-v") 'books/veb/vbtm)
(define-key books/veb-mode-keymap (kbd "C-s-b") 'books/veb/bold)

(define-key books/veb-mode-keymap (kbd "<kp-6>") 'books/veb/next)
(define-key books/veb-mode-keymap (kbd "<kp-right>") 'books/veb/next)

(define-key books/veb-mode-keymap (kbd "<kp-4>") 'books/veb/prev)
(define-key books/veb-mode-keymap (kbd "<kp-left>") 'books/veb/prev)

(define-key books/veb-mode-keymap (kbd "<kp-enter>") 'books/veb/fill-paragraph-and-search)
(define-key books/veb-mode-keymap (kbd "s-s") 'books/veb/skip)

(define-key books/veb-mode-keymap (kbd "<kp-5>") 'books/veb/todo)
(define-key books/veb-mode-keymap (kbd "<kp-begin>") 'books/veb/todo)
(define-key books/veb-mode-keymap (kbd "s-r") 'books/veb/refresh)

(define-key books/veb-mode-overlay-keymap (kbd "r") 'books/veb/replace)
(define-key books/veb-mode-overlay-keymap (kbd "n") 'books/veb/next)
(define-key books/veb-mode-overlay-keymap (kbd "p") 'books/veb/prev)
(define-key books/veb-mode-overlay-keymap (kbd "s") 'books/veb/skip)
