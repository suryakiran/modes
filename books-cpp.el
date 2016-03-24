(books/defkeymap books/cpp-mode-keymap "Keymap used in books/cpp mode")

(defvar books/cpp/chap-regex "\\(app\\|chap\\)-\\([0-9][0-9][0-9]\\|[a-z]\\).tex")

(define-minor-mode books/cpp-mode
  "Some Documentation"
  :lighter " cpp"
  :keymap books/cpp-mode-keymap
  (books/cpp/startup-or-shutdown)
  )

(defvar astyle-cmd "astyle -A4 -FPyej -k1 -W1 --mode=c")


(defun books/cpp/startup-or-shutdown ()
  (if (symbol-value 'books/cpp-mode)
      (books/cpp/startup)
    (books/cpp/shutdown)
    ))

(defun books/cpp/startup ()
  (books/deftag "v" "\\vbtm{" "}" (books/major-mode))
  (books/deftag "u" "\\url{" "}" (books/major-mode))
  )

(defun books/cpp/shutdown ()
  )

(defun books/cpp/get-code ()
  (interactive)
  (let ((num) (code-format "code-%s") (dir (f-dirname (buffer-file-name)))
        (code-file) (begin-code "\\\\begin{Code}") (found) (cur-file (f-filename (buffer-file-name))))
    
    (setq code-file (format code-format (f-filename buffer-file-name)))

    (when (f-exists? code-file)
      (with-current-buffer (find-file-noselect code-file)
        (goto-char (point-min))
        (when (re-search-forward begin-code (point-max) t)
          (setq found t)
          (LaTeX-mark-environment)
          (kill-region (region-beginning) (region-end))
          (save-buffer))
        ))
    (when found
      (yank)
      (save-buffer))
    ))

(defun books/mark-as-code ()
  (interactive)
  (let ((code) (text) (region))
    (if (region-active-p)
        (setq region (cons (region-beginning) (region-end)))
      (setq region (cons (line-beginning-position) (line-end-position))))
    
    (setq text (buffer-substring-no-properties (car region) (cdr region)))

    (when text
      (setq code
            (with-temp-buffer
              (insert text)
              (save-excursion
                (goto-char (point-min))
                (while (re-search-forward "\\(#include [\"<][^\">]*[\">]\\|[{};]\\|template *<[^>]*>\\)" (point-max) t)
                  (newline)))
              (shell-command-on-region (point-min) (point-max) astyle-cmd t t)
              (buffer-string)))
      (books/replace-text-in-range (car region) (cdr region) (format "\\begin{Code}\n%s\n\\end{Code}" code))
      (save-buffer))
    ))

(define-key books/cpp-mode-keymap (kbd "s-c") 'books/mark-as-code)
(define-key books/cpp-mode-keymap (kbd "<f1>") 'books/cpp/get-code)

(defconst books/cc-style
  '(
    (c-offsets-alist . ((innamespace . [0]) (case-label . +) (inline-open . 0)))
    (c-basic-offset . 2)
    ))

(c-add-style "books/cc-style" books/cc-style)

(provide 'books/cpp)
