
(add-hook 'books/chess-line-mode-hook 'books/narrow-mode)
(setq auto-mode-alist 
      (append '(
                ("\\.clm\\'" . books/chess-mainline-mode))
              auto-mode-alist))

(provide 'books/chess-line)
