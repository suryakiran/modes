(require 'json)
(books/defkeymap books/json-mode-map "Keymap for Books/Json minor mode")
(define-minor-mode books/json-mode
  "Minor mode to create json file of chapters"
  :lighter " books-json"
  :keymap books/json-mode-map
  )

(defvar books/json/target nil "Target Json file to paste pgn info")

(defun books/json/create (items out-file)
  (let (
        (json-key-type 'string)
        ;; (json-array-type 'plist)
        (json-object-type 'hash-table)
        (obj) (objlist '())
        )
  (dolist (item items)
    (setq obj (json-new-object))
    (setq obj (json-add-to-object obj "file" item))
    (setq obj (json-add-to-object obj "pgn" ""))
    (add-to-list 'objlist obj t)
  )
  (with-temp-file out-file
    (insert (json-encode objlist)))
  ))

(defun books/json/paste-pgn (pgn)
  (when pgn
    (let ((buf (current-buffer)))
      (skg/open-file books/json/target t)
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "\"pgn\": \"\"" (point-max) t)
          (backward-char 1)
          (insert pgn)
          (save-buffer)
          (switch-to-buffer-other-window buf)
          )))
    ))

(defun books/json/copy-pgn ()
  (let ((begin) (end) (pgn))
    (save-excursion
      (when (re-search-backward "^1\\." (point-min) t)
        (setq begin (match-beginning 0))
        (if (re-search-forward "^\\[" (point-max) t)
            (setq end (1- (match-beginning 0)))
          (setq end (point-max))
          )))
    (and begin end
         (setq pgn (buffer-substring-no-properties begin end))
         (setq pgn (s-trim (s-replace "\n" " " pgn)))
         (setq pgn (replace-regexp-in-string " [10]\\(/2\\)?-[10]\\(/2\\)?$" "" pgn))
         (setq pgn (s-trim pgn))
         pgn)
    ))

(defun books/json/copy-and-paste-pgn ()
  (interactive)
  (unless books/json/target
    (error "No target json file specified"))
  (books/json/paste-pgn (books/json/copy-pgn)))

(defun books/json/create-files (&optional at-beginning)
  (interactive)
  (unless books/json/target
    (error "No target json file specified"))
  (unless (f-exists? books/json/target)
    (error "JSON file not found"))
  (let ((json-array-type 'list) (file) (pgn)
        (json (json-read-file books/json/target)))
    (-map (lambda (item)
            (setq file (cdr (assq 'file item)))
            (setq pgn  (cdr (assq 'pgn item)))
            (find-file-existing file)
            (if at-beginning (goto-char (point-min)) (goto-char (point-max)))
            (insert (format "\n\\pgn{%s}\n" pgn))
            (save-buffer)
            ) json)
    ))

(define-key books/json-mode-map (kbd "C-<kp-enter>") 'books/json/copy-and-paste-pgn)
