(provide 'books/show-book)

(defconst book/page-buffer-name "*Book Page*")
(defvar book/page-buffer nil)
(defvar book/current-page nil)
(defvar book/current-book nil)

(defvar book/page-buffer-created nil)
(make-variable-buffer-local 'book/current-page)
(make-variable-buffer-local 'book/current-book)

(books/defkeymap show-page-mode-keymap "Keymap used in show-page mode")
(books/defface book/page-header-face "Face used in header of book page" page
               (:foreground "red"))

(define-minor-mode show-page-mode
  "Some Documentation"
  :lighter " show-page"
  :keymap show-page-mode-keymap
  (show-page/startup-or-shutdown)
  )

(defun show-page/startup-or-shutdown ()
  (if (symbol-value 'show-page-mode)
      (show-page/startup)
    (show-page/shutdown)
    ))

(defun show-page/startup ()
  (if (buffer-live-p book/page-buffer)
      (setq book/page-buffer-created nil)
    (setq book/page-buffer-created t))
  (setq book/page-buffer (get-buffer-create book/page-buffer-name))
  )

(defun show-page/shutdown ()
  (kill-buffer book/page-buffer)
  (setq book/page-buffer-created nil)
  )

;; (defun books/show-page (book page-num type)
;;   (cl-case type
;;     ('pdf (books/show-page/pdf book page-num))
;;     ('djvu (books/show-page/djvu book/page-num))
;;     ))

(defun books/show-page/pdf (book page-num)
  (let* (
         (dir (f-dirname (cdr (assq (books/major-mode) books/main-tex-in-files))))
         (extract-file (f-join dir "extract-one.py"))
         (ret) (err) (out)
         (outbuf "*ExtractOne*") (errbuf "*ExtractErr*")
         )
    (setq ret (shell-command (format "python %s -i %s -p %d" extract-file book page-num) outbuf errbuf))
    (if (= ret 0)
        (progn
          (setq out (s-trim (with-current-buffer outbuf (buffer-string))))
          )
      (progn 
        (setq err (with-current-buffer errbuf (buffer-string)))
        (message (propertize err 'face 'error))
        (error err))
      )
    (when out
      (if book/page-buffer-created
          (progn
            (setq book/page-buffer-created nil)
            (with-current-buffer book/page-buffer
              (insert-file-contents out)
              (image-mode))
            )
        (progn
          (with-current-buffer book/page-buffer
            (image-toggle-display)
            (erase-buffer)
            (insert-file-contents out)
            (image-mode))
          )
        )
      (with-current-buffer book/page-buffer
        (setq book/current-page page-num)
        (setq book/current-book book)
        ;; (eimp-fit-image-to-window nil)
        )
      (display-buffer book/page-buffer)
      (with-current-buffer book/page-buffer
        (setq header-line-format
              (propertize (format "Book: %s, Page: %d" book/current-book book/current-page) 'face book/page-header-face)))
      )
    ))

(defun books/show-page (page-num)
  (let* ((mode (books/major-mode))
         (book (book-file/get-one mode))
         )
    (when book
      (books/show-page/pdf book page-num))
    ))

(defun books/show-page/display (incr)
  (let ((current-page) (current-book) (inp))
    (with-current-buffer book/page-buffer
      (setq current-page book/current-page)
      (setq current-book book/current-book)
      (when (stringp current-page)
        (setq current-page (string-to-number current-page)))
      )
    (if current-page
        (setq current-page (funcall incr current-page))
      (progn
        (setq inp (read-from-minibuffer "Page Number: "))
        (setq current-page (string-to-number inp))
        )
      )
    (books/show-page current-page)
    ))

(defun books/show-page/goto ()
  (interactive)
  (let ((inp) (current-page))
    (setq inp (read-from-minibuffer "Page Number: "))
    (setq current-page (string-to-number inp))
    (books/show-page current-page)
    ))

(defun books/show-page/show ()
  (with-selected-window (next-window)
    (switch-to-buffer book/page-buffer)
    ))

(defun books/show-page/hide ()
  (with-selected-window (get-buffer-window book/page-buffer)
    (skg/open-file emacs-init-file)
    ))

(defun books/show-page/shown? ()
  (get-buffer-window book/page-buffer))

(defun books/show-page/toggle ()
  (interactive)
  (let ((window))
    (if (buffer-live-p book/page-buffer)
        (progn
          (if (books/show-page/shown?)
              (books/show-page/hide)
            (books/show-page/show)
            ))
      (books/show-page/goto))
    ))

(define-key show-page-mode-keymap (kbd "<f5>") '(lambda () (interactive) (books/show-page/display '1+)))
(define-key show-page-mode-keymap (kbd "S-<f5>") '(lambda () (interactive) (books/show-page/display '1-)))
(define-key show-page-mode-keymap (kbd "<f6>") 'books/show-page/goto)
(define-key show-page-mode-keymap (kbd "<f7>") 'books/show-page/toggle)
