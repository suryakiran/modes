(require 'pcre2el)
(require 'thingatpt)
(require 'key-chord)
(require 'ht)
(books/require 'evil)
(books/require 'show-book)
(books/require 'overlays)               
(books/require 'books/cpp)

(books/defface books/highlight-env/face "Face used to highight regions" books
               (:foreground "blue" :background "yellow"))

(defun books/get-page-ranges (firsts)
  (let ((lasts (--map (1- it) (-slice firsts 1)))
        (firsts (-butlast firsts))
        )
    (--zip-with (cons it other) firsts lasts)
    ))

(defun books/page-extract-commands (firsts)
  (let ((ranges (books/get-page-ranges firsts)))
    (--map-indexed (format "waf --first=%d --last=%d --out=%d.tex extract" (car it) (cdr it) it-index) ranges)
    ))

(defun books/copy-text-in-parens ()
  (interactive)
  (books/apply-on-paren-region 'copy-region-as-kill))

(defun books/cut-text-in-parens ()
  (interactive)
  (books/apply-on-paren-region 'kill-region)
  )


(defun books/apply-on-latex-command-region (region-func)
  (let ((fun) (beg) (end) (tmp))
    (cond
     ((looking-at "{")
      (progn
        (setq beg (point))
        (setq end (search-forward "}" (point-max) t))
        ))
     ((looking-back "}")
      (progn
        (setq end (point))
        (setq beg (search-backward "{" (point-min) t))
        ))
     (t
      (progn
        (setq beg (search-backward "{" (point-min) t))
        (setq end (search-forward  "}" (point-max) t))
        ))
     )
    (and beg end (apply region-func (list beg end)))
    ))
    
(defun books/apply-on-paren-region(region-func)
  (let ((fun) (beg) (end) (tmp))
    (cond
     ((looking-at "{") (setq fun 'forward-list))
     ((looking-back "}") (setq fun 'backward-list))
     (t (progn
          (re-search-backward "{" (point-min) t)
          (setq fun 'forward-list)))
     )
    (setq beg (point))

    (funcall fun)
    (setq end (point))

    (apply region-func (list beg end))
    ))

(defun books/remove-multiple-empty-lines ()
  (interactive)
  (save-excursion
    (replace-regexp "^ +$" "" nil (point-min) (point-max))
    (goto-char (point-min))
    
    (while (re-search-forward "
\\{3,\\}" (point-max) t)
      (replace-match "

"))
    (save-buffer)
    ))

(defun books/highlight-env-region (env)
  (interactive)
  (let ((ol)(regions (books/environment-regions env)))
    (--map (progn
             (setq ol (make-overlay (car it) (cdr it) (current-buffer) nil t))
             (overlay-put ol 'face 'books/highlight-env/face)
             )
           regions)
    ))
                             

(defun books/remove-multiple-empty-comment-lines ()
  (interactive)
  (goto-char (point-min))
  (execute-kbd-macro [?\M-x ?r ?e ?p ?l ?a ?c ?e ?- ?r ?e ?g ?e ?x ?p return ?\\ ?\( ?% ?\C-q ?\C-j ?\\ ?\) ?\\ ?\{ ?2 ?, ?\\ ?\} return ?% ?\C-q ?\C-j return])
  (save-buffer))

(defun books/final-cleanup (&optional mark-fun)
  (interactive)
  (let ((file))
    (set-fill-column 90)
    (LaTeX-fill-buffer 'full)
    (replace-regexp "- " "" nil (point-min) (point-max))
    (replace-string "ﬁ" "fi" nil (point-min) (point-max))
    (replace-string "ﬂ" "fl" nil (point-min) (point-max))
    (replace-regexp "[[:space:]]+" " " nil (point-min) (point-max))
    (replace-regexp "[[:space:]]*}" "}" nil (point-min) (point-max))
    (books/comment-before-latex-macro "item")
    (books/comment-before-latex-macro "end")
    (books/remove-multiple-empty-comment-lines)
    ))

(defun books/replace-invalid-line-break ()
  (interactive)
  (let ((olay)
        (regex (format "[%s-] *$" books/dash))
        (header-line-format (propertize "Replacing invalid line breaks ..." 'face 'books/header-face)))
    (skg/search-file-do regex
      (setq olay (make-overlay (match-beginning 0) (match-end 0) nil t))
      (overlay-put olay 'face 'isearch)
      (if (y-or-n-p "Replace: ")
          (progn
            (replace-match "")
            (delete-indentation 1)
            (delete-forward-char 1)
            )
        (delete-overlay olay))
      )
    (save-buffer)
    ))

(defun books/search-invalid-line-break ()
  (interactive)
  (let ((header-line-format (propertize "Searching invalid line breaks ..." 'face 'books/header-face)))
    (re-search-forward "-$")
    ))

(defun books/remove-overlays ()
  (interactive)
  (remove-overlays))

(defun books/mark-whole-line()
  (end-of-line)
  (set-mark (line-beginning-position)))

(defun books/mark-chapter-region (macro-name)
  (let ((regex (format "%s *{" macro-name)))
    (goto-char (point-min))
    (when (re-search-forward regex (point-max) t)
      (backward-char 1)
      (forward-list)
      (forward-char 1)
      (set-mark (point))
      (goto-char (point-max))
      (exchange-point-and-mark))
    ))
  
(defun books/change-env-name ()
  (interactive)
  (let ((new-name))
    (LaTeX-mark-environment)
    (setq new-name (read-from-minibuffer "New Environment Name: "))
    (unless (s-blank? (s-trim new-name))
      (save-restriction
        (narrow-to-region (region-beginning) (region-end))
        (goto-char (point-min))
        (books/replace-text-in-range (line-beginning-position) (line-end-position) (format "\\begin{%s}" new-name))
        (goto-char (point-max))
        (books/replace-text-in-range (line-beginning-position) (line-end-position) (format "\\end{%s}" new-name))
        (save-buffer)))
    ))

(defun books/mark-required-buffer-contents ()
  (interactive)
  (cond
   ((or (derived-mode-p 'books/kasparov-mode) (derived-mode-p 'books/anand-mode))
    (books/mark-chapter-region "NewGame"))

   (t (mark-whole-buffer))
   ))

(defun books/comment-before-latex-macro (macro)
  (replace-regexp (format "\\(\\\\%s\\)" macro) "%\n\\1" nil (point-min) (point-max)))

(defun books/insert-dash ()
  (interactive)
  (insert books/dash))

(defun books/mark-section (sec-type)
  (let* (
         (section-name (cdr (assoc sec-type book/section-type)))
         )
    (when section-name
      (skg/operate-on-line 'delete-trailing-whitespace)
      (skg/select-line)
      (wrap-region-with (format "\\%s{" section-name) "}")
      (books/add-new-line-and-comment-it)
      )))

(defun books/push-brace (&optional compress-space)
  (save-excursion
    (when (re-search-forward "}" (point-max) t)
      (and compress-space (looking-at " ") (delete-char 1))
      (transpose-chars 1))
    ))

(defun books/pull-back-closing-brace ()
  (interactive)
  (save-excursion
    (when (re-search-forward "}" (point-max) t)
      (backward-char)
      (transpose-chars 1))
    ))

(defun books/push-back-latex-command ()
  (interactive)
  (let ((start) (end))
    (save-excursion
      (when (re-search-backward "{" (point-min) t)
        (setq end (1+ (point)))
        (when (re-search-backward "\\\\" (point-min) t)
          (setq start (point))
          (transpose-regions start end (1- start) start))))
    ))

(defun books/replace-text-in-range (start end new-text)
  (when (numberp new-text)
    (setq new-text (number-to-string new-text)))
  (delete-region start end)
  (insert new-text))

(defun books/add-new-line-and-comment-it ()
  (interactive)
  (skg/append-new-line)
  (insert "%")
  )

(defun books/cap-case-word ()
  (interactive)
  )

(defun books/small-case-word()
  (interactive)
  )

(defun books/replace-word (new-word)
  (let ((range (bounds-of-thing-at-point 'word)))
    (books/replace-text-in-range (car range) (cdr range) new-word)))

(defun books/insert-hyphen ()
  (interactive)
  (unless (looking-at "—")
    (if (looking-at "-")
        (books/replace-text-in-range (point) (1+ (point)) "—")
      (insert "—"))
    ))

(defun books/end-of-line-comment ()
  (interactive)
  (progn 
    (end-of-line)
    (when (not (or (looking-back "%") (looking-at "%"))) (insert "%"))
    ))

(defun books/replace-text-in-range (start end new-text)
  (progn
    (save-excursion
      (goto-char start)
      (delete-region start end)
      (insert new-text))))

(defun books/narrow-from-current-line ()
  (interactive)
  (skg/operate-on-rest-of-buffer 'narrow-to-region))

(defun books/narrow-to-line ()
  (interactive)
  (skg/operate-till-current-line 'narrow-to-region)
  )

(defun books/operate-on-region (begin end fun)
  (save-restriction
    (narrow-to-region begin end)
    (skg/operate-on-buffer fun)
    ))

(defun books/itemize-buffer ()
  (goto-char (point-min))
  (when (re-search-forward "^ *[^)]*)" (point-max) t)
    (books/itemize-line)
    (while (books/forward-block)
      (books/itemize-line))
    ))
    


         
(defun books/alphabetic-or-hyphen-p (char)
  (let ((s (char-to-string char)))
    (string-match-p "[A-Za-z ]" s)))

(defun books/alphabetic-p (char)
  (let ((s (char-to-string char)))
    (if (string-match-p "[[:alpha:]]" s) (downcase s) nil)))

(defun books/no-accented-string (text)
  (mapconcat 'identity (delq nil (mapcar 'books/alphabetic-p text)) ""))

(defun books/comment-buffer ()
  (interactive)
  (skg/operate-on-buffer 'comment-or-uncomment-region))

(defun books/comment-from-line ()
  (interactive)
  (skg/operate-on-rest-of-buffer 'comment-or-uncomment-region))

(defun books/comment-to-line ()
  (interactive)
  (skg/operate-till-current-line 'comment-or-uncomment-region))

(fset 'books/generic-environment
      [?\C-e])

(fset 'books/itemize-environment
      [?\C-e ?i ?t ?e ?m ?i ?z ?e tab])

(fset 'books/enumerate-environment
      [?\C-e ?e ?n ?u ?m ?e ?r ?a ?t ?e tab])

(fset 'books/algorithm-environment
      [?\C-e ?A ?l ?g ?o ?r ?i ?t ?h ?m tab])

(defun books/itemize-environment-function ()
  (interactive)
  (execute-kbd-macro 'books/itemize-environment)
  (run-hooks 'books/after-itemize-hook))

(defun books/itemize-line ()
  (interactive)
  (save-excursion
    (goto-char (line-beginning-position))
    (insert "\\item ")
    (when (re-search-forward "[^)]*)" (line-end-position) t)
      (replace-match ""))
    (save-buffer)
    ))

(defun books/kill-word-under-cursor ()
  (interactive)
  (let ((r (bounds-of-thing-at-point 'word)))
    (delete-region (car r) (cdr r))))

(defun books/cycle-words (input)
  (let* (
         (cur-word-range (bounds-of-thing-at-point 'word))
         (cur-word (buffer-substring-no-properties (car cur-word-range) (cdr cur-word-range)))
         (index (-elem-index cur-word input))
         (new-list) (rv)
         )
    (when index
      (setq new-list (-split-at (+ 1 index) input))
      (if (cadr new-list) (setq rv (-first-item (cadr new-list)))
        (setq rv (-first-item (car new-list))))
      (books/replace-text-in-range (car cur-word-range) (cdr cur-word-range) rv)
      (goto-char (car cur-word-range))
      )))

(defun books/modify-syntax-entries (chars-as-string classes)
  (let (
        (old-char-syntaxes (mapcar (lambda (x) (char-to-string (char-syntax x)))
                                   (append chars-as-string nil)))
        (new-char-syntaxes (mapcar 'char-to-string (append classes nil)))                
        )
    (-zip-with 'modify-syntax-entry (append chars-as-string nil) new-char-syntaxes)
    old-char-syntaxes))

(defun cycle-chars (input)
  (let* (
         (input-list (append input nil))
         (idx (-elem-index (char-after) input-list))
         (new-idx (+ 1 idx))
         (new-char)
         )
    (if (= new-idx (length input))
        (setq new-char (substring input 0 1))
      (setq new-char (substring input new-idx (+ 1 new-idx))))
    (delete-char 1)
    (insert new-char)
    (backward-char)
    ))

(defun books/create-foot-mark-from-selected-text()
  (interactive)
  (if (region-active-p)
      (progn
        (save-restriction
          (narrow-to-region (region-beginning) (region-end))
          (mark-whole-buffer)
          (wrap-region-with "\\footnote{" "}")
          (kill-region (point-min) (point-max))
          ))))


(defun books/mark-next-word ()
  (interactive)
  (if (not (region-active-p))
      (er/mark-word)
    (progn
      (exchange-point-and-mark)
      (mark-word 1 t)
      ))
  (exchange-point-and-mark))

(defun books/mark-to-end-of-line ()
  (interactive)
  (unless (= (point) (line-end-position))
    (execute-kbd-macro [S-end])
    ))

(fset 'books/open-new-line
      [?\C-o])

(fset 'books/mark-next-char
      [S-right])

(defmacro books/as-word (s &rest body)
  (declare (indent 1) (debug t))
  `(let ((r (make-string (length ,s) ?w)) (old-syntax-entries))
     (setq old-syntax-entries (books/modify-syntax-entries ,s r))
     ,@body
     (books/modify-syntax-entries ,s (mapconcat 'identity old-syntax-entries ""))
     ))

(defun books/newline-and-comment()
  (interactive)
  (books/new-line)
  (insert "%")
  (books/new-line)
  )

(defun books/forward-block (&optional num end)
  (interactive)
  (unless num (setq num 1))
  (unless end (setq end (point-max)))
  (search-forward-regexp "\n[\t\n ]*\n+" end "NOERROR" num)
  )

(defun books/normalize-regions (regions)
  (let ((sorted (--sort (< (car it) (car other)) regions))
        (res) (last)
        )
    (dolist (item sorted)
      (if res
          (progn
            (setq last (-last-item res))
            (if (< (car item) (cdr last))
                (unless (< (cdr item) (cdr last))
                  (delete last res)
                  (add-to-list 'res (cons (car last) (cdr item)) t)
                  )
              (add-to-list 'res item t)
              )
            )
        (progn
          (add-to-list 'res item t)
          )
        ))
    (--remove (= (car it) (cdr it)) res)
    ))

(defun books/other-than-environment-regions (&rest envs)
  (let ((regions))
    (setq regions (--map (books/environment-regions it) envs))
    (books/exclude-regions (books/normalize-regions (-flatten regions)))
    ))
                         

(defun books/exclude-regions (in-regions)
  (let (
        (regions) (start) (min (point-min)) (max (point-max)) (curcons)
        )
    (setq start min)
    (while in-regions
      (setq it (car in-regions))
      (unless (equal (car it) start)
        (setq curcons (cons start (1- (car it))))
        (add-to-list 'regions curcons))

      (setq start (1+ (cdr it)))
      (setq in-regions (cdr in-regions)))
    (when (< start max)
      (add-to-list 'regions (cons start max))
      )
    (--remove (= (car it) (cdr it)) regions)
    ))

(defun books/environment-regions (env)
  (let ((begin (format "begin{%s}" env))
        (end   (format "end{%s}" env))
        (regions) (cur)
        )
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward begin (point-max) t)
        (LaTeX-mark-environment)
        (setq cur (cons (region-beginning) (region-end)))
        (add-to-list 'regions cur)
        (goto-char (cdr cur))
        ))
    (--remove (= (car it) (cdr it)) regions)
    ))

(defun books/next-word ()
  (interactive)
  (forward-word 2)
  (backward-word)
  )

(defun books/hide-envs (&rest envs)
  (let ((regions))
    (setq regions (--map (books/environment-regions it) envs))
    (--map (books/hide-region it) (books/normalize-regions (-flatten regions)))
    ))

(defun books/command-regions (cmd)
  (let ((reg (format "\\\\%s{[^}]*}" cmd)) (regions))
    (skg/search-file-do reg
      (add-to-list 'regions (cons (match-beginning 0) (match-end 0))))
    regions
    ))

(defun books/if-minor-mode-active (mode)
  (symbol-value mode))

(books/defkeymap books/master-book-mode-map "Keymap for books minor mode")
(define-minor-mode books/master-book-mode
  "Mode to write books"
  :lighter " book"
  :keymap books/master-book-mode-map
  )

(defun books/open-main-tex-in-file ()
  (interactive)
  (let ((mode (books/major-mode)) (data))
    (when mode
      (setq data (assq mode books/main-tex-in-files))
      (when data
        (find-file-existing (cdr data))))
    ))

(defun books/open-main-tex-file ()
  (interactive)
  (let ((mode (books/major-mode)) (data))
    (when mode
      (setq data (assq mode books/main-tex-files))
      (when data
        (find-file-existing (cdr data))
        (skg/revert-buffer)))
    ))

(defun books/open-book-lua ()
  (interactive)
  (let ((mode (books/major-mode)) (data))
    (when mode
      (setq data (assq mode books/main-tex-in-files))
      (when data
        (find-file-existing (f-join (f-dirname (cdr data)) "book.lua")))
      )))
    
(defun books/fill-paragraphs-t (col-width)
  (goto-char (point-min))
  (let ((fill-column col-width) (cur (point-min)))
    (while (< cur (point-max))
      (forward-paragraph)
      (mark-paragraph)
      (fill-region (region-beginning) (region-end))
      (setq cur (region-end)))
    (set-fill-column 90)
    (save-buffer)
    ))

(defun books/replace-dashes ()
  (interactive)
  (let ((header-line-format (propertize "Replacing dashes ..." 'face 'books/header-face)))
    (query-replace (format "%s " books/dash) "" nil (point-min) (point-max))
    (query-replace "- " "" nil (point-min) (point-max))
    ))

(defun books/compress-dashes ()
  (let ((header-line-format (propertize "Compressing dashes ..." 'face 'books/header-face)))
    (interactive)
    (query-replace-regexp (format "\\([-%s]\\) +" books/dash) "\\1" nil (point-min) (point-max))
    ))

(defun books/mode-fun-as-str (mode name &rest args)
  (let ((lines '()))
    (format "(defun %s/%s (%s)\n)\n\n" mode name (s-join " " args))
    ))

(defun books/create-book-el (type title mode)
  (let ((file (f-join default-directory "book.el")) (book-type) (scf "books/standard-chapter-format")
        (this "(f-dirname (f-this-file))")
        (mode-symbol (format "books/%s-mode" mode))
        )
    
    (unless (f-exists? file)
      (with-temp-file file
        (erase-buffer)
        (insert (format "(setq dir-obj (make-instance 'Dir :dir %s :pattern %s))\n\n" this scf))
        (cl-case type
          ('chess (progn
                    (insert (format "(defun %s/cleanup-moves-in-parens (start end)\n" mode))
                    (insert "(let (\n(text (buffer-substring-no-properties start end))\n)\n")
                    (insert "))\n\n")
                    (insert (books/mode-fun-as-str mode "cleanup-moves" "type"))
                    (insert (books/mode-fun-as-str mode "replace-chars" "current-char" "point"))
                    (insert (books/mode-fun-as-str mode "hooks"))
                    (goto-char (- (point) 3))
                    (insert (format "(chess/sp-hook '%s '(:variation %s/cleanup-moves :dirty %s/cleanup-moves :mainline %s/cleanup-moves))\n" mode-symbol mode mode mode))
                    (insert (format "(chess/function '%s '(:replace-chars %s/replace-chars))\n" mode-symbol mode))
                    (goto-char (point-max))
                    (insert (format "(chess/book-mode \"%s\" \"%s\" (%s/hooks))\n\n" mode title mode))
                    ))
          )
        (insert (format "(books/chapter-file-format '%s %s %s)\n" mode-symbol scf this))
        (insert "\n")
        (insert (format "(setq %s-files (books/get-files '%s 1 10))\n" mode mode-symbol))
        (insert (format "(provide 'books/%s)\n" mode))
        ))
    (with-current-buffer (find-file-noselect file)
      (skg/revert-buffer)
      (skg/indent-file)
      (save-buffer)
      )
    ))

(defun books/unfill-paragraphs ()
  (interactive)
  (books/fill-paragraphs-t 1000000))

(defun books/fill-paragraphs ()
  (interactive)
  (books/fill-paragraphs-t 90))

(defun books/get-paren-ranges (type start end &optional include-parens)
  (let ((range) (lstart) (lend))
    (save-excursion
      (goto-char start)
      (while (re-search-forward type end t)
        (if include-parens
            (setq lstart (1- (point)))
          (setq lstart (point)))
        (backward-char)
        (forward-list)
        (if include-parens
            (setq lend (point))
          (setq lend (1- (point))))
        (and lstart lend (add-to-list 'range (cons lstart lend) t)))
      )
    range))

(defun books/table-from-region (start end)
  (interactive "r")
  (let ((num-columns) (ampersands))
    (setq num-columns (read-from-minibuffer "Number of Columns: " "2"))
    (setq join-lines (number-sequence 1 (1- (string-to-number num-columns))))
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (flush-lines "^ *$")
      (while (< (point) (point-max))
        (--map (progn
                 (goto-char (line-end-position))
                 (insert " & ")
                 (delete-indentation 1)) join-lines)
        (goto-char (line-end-position))
        (insert "\\\\")
        (goto-char (1+ (line-end-position)))
        ))
    (cons (point-min) (point-max))))

(defun books/paste-env (file-prefix env-name)
  (let ((num) (file-format "%s-%s")
        (dir (f-dirname (buffer-file-name)))
        (other-file)
        (begin-env (format "\\\\begin{%s}" env-name))
        (found)
        (cur-file (f-filename (buffer-file-name))))
    
    (setq other-file (f-join dir (format file-format file-prefix cur-file)))

    (when other-file
      (with-current-buffer (find-file-noselect other-file)
        (goto-char (point-min))
        (when (re-search-forward begin-env (point-max) t)
          (setq found t)
          (LaTeX-mark-environment)
          (kill-region (region-beginning) (region-end))
          (save-buffer))
        ))
    (when found
      (yank)
      (save-buffer))
    ))

(defun books/next-figure ()
  (interactive)
  (let ((num 0) (regex "input{figures") (file (f-no-ext (f-filename (buffer-file-name)))))
    (save-excursion
      (save-restriction
        (narrow-to-region (point-min) (line-beginning-position))
        (goto-char (point-min))
        (while (re-search-forward regex (point-max) t)
          (setq num (1+ num)))
        (setq num (1+ num))
        ))
    (format "\\input{figures/%s-%03d}{\\caption{}}" file num)
    ))

(defun books/split-images ()
  (interactive)
  (let* (
         (file-regex "fig-\\(.*\\)\\.tex")
         (file (f-filename (buffer-file-name))) (dir (f-dirname (buffer-file-name)))
         (chap) (count 0) (image-dir (f-join dir "figures")) (image-file-format "%s-%03d.tex")
         (err)
         )
    (when (string-match file-regex file)
      (setq chap (match-string 1 file))
      (when (and (f-exists? image-dir) (f-file? image-dir))
        (error "images is not a directory")
        (setq err t)
        )
      (unless err
        (f-mkdir image-dir)
        (goto-char (point-min))
        (while (re-search-forward "begin{tikzpicture}" (point-max) t)
          (setq count (1+ count))
          (LaTeX-mark-environment)
          (kill-region (region-beginning) (region-end) t)
          (save-buffer)
          (with-temp-file (f-join image-dir (format image-file-format chap count))
            (goto-char (point-max))
            (yank)
            )
          ))
      )))

(eval-after-load 'books/master-book
  '(progn
     (define-key books/master-book-mode-map (kbd "s-f") 'books/mark-next-word)
     (define-key books/master-book-mode-map (kbd "s-e") 'books/enumerate-environment)
     (define-key books/master-book-mode-map (kbd "s-i") 'books/itemize-environment)
     (define-key books/master-book-mode-map (kbd "s-a") 'books/algorithm-environment)
     (define-key books/master-book-mode-map (kbd "M-n") '(lambda () (interactive) (books/open-chapter 'next)))
     (define-key books/master-book-mode-map (kbd "M-p") '(lambda () (interactive) (books/open-chapter 'prev)))
     )
  )

(defmacro cpp/book-mode (mode-str book &rest body)
  (declare (indent 1) (debug t))
  (let (
        (mode (intern (format "books/%s-mode" mode-str)))
        (hook (intern (format "books/%s-mode-hook" mode-str)))
        )
    (add-hook hook (lambda ()
                     (books/cpp-mode 1)
                     ))
    `(progn
       (books/new-mode ,mode-str latex-mode ,book ,@body
                       (turn-off-wrap-region-mode)))
    
    ))

(defvar book/file/pdf (ht-create 'eq))
(defvar book/file/djvu (ht-create 'eq))

(defun book-file/add (mode type file)
  (cl-case type
    ('pdf (progn
            (ht-set! book/file/pdf mode file)
            ))
    ('djvu (progn
             (ht-set! book/file/djvu mode file)
             ))
    ))

(defun book-file/get (mode type)
  (let ((file))
    (cl-case type
      ('pdf (setq file (ht-get book/file/pdf mode)))
      ('djvu (setq file (ht-get book/file/djvu mode)))
      )
    (when (f-exists? file)
      file)))

(defun book-file/get-one (mode)
  (let ((types '(pdf djvu)) (type))
    (setq type (--first (book-file/get mode it) types))
    (when type (book-file/get mode type))
    ))


(books/defkeymap books/create-paragraphs-mode-keymap "Keymap used in books/create-paragraphs mode")

(define-minor-mode books/create-paragraphs-mode
  "Some Documentation"
  :lighter " bcp"
  :keymap books/create-paragraphs-mode-keymap
  (books/create-paragraphs/startup-or-shutdown)
  )

(defun books/create-paragraphs/startup-or-shutdown ()
  (if (symbol-value 'books/create-paragraphs-mode)
      (books/create-paragraphs/startup)
    (books/create-paragraphs/shutdown)
    ))

(defun books/create-paragraphs/startup ()
  (show-page-mode t)
  )

(defun books/create-paragraphs/shutdown ()
  (show-page-mode -1)
  )

;; (define-key books/create-paragraphs-mode-keymap (kbd "<f1>") 'help)

(provide 'books/master-book)
