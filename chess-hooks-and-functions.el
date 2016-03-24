(require 'ht)

(defvar chess/wrap-functions (ht-create 'eq))
(defvar chess/other-functions (ht-create 'eq))
(defvar chess/pre-move-hooks (ht-create 'equal))
(defvar chess/post-move-keys (ht-create 'equal))

(defvar chess/pre-variation-hook nil "Hook to run before variation wrapper")
(defvar chess/pre-mainline-hook nil "Hook to run before mainline wrapper")
(defvar chess/pre-dirty-hook nil "Hook to run before dirty wrapper")

(defvar chess/post-variation-hook nil "Hook to run after variation wrapper")
(defvar chess/post-mainline-hook nil "Hook to run after mainline wrapper")
(defvar chess/post-dirty-hook nil "Hook to run after dirty wrapper")

(ht-set! chess/pre-move-hooks "m" 'chess/pre-mainline-hook)
(ht-set! chess/pre-move-hooks "v" 'chess/pre-variation-hook)
(ht-set! chess/pre-move-hooks "n" 'chess/pre-mainline-hook)
(ht-set! chess/pre-move-hooks "N" 'chess/pre-mainline-hook)
(ht-set! chess/pre-move-hooks "d" 'chess/pre-dirty-hook)

(ht-set! chess/post-move-keys "m" :mainline)
(ht-set! chess/post-move-keys "v" :variation)
(ht-set! chess/post-move-keys "n" :mainline)
(ht-set! chess/post-move-keys "N" :mainline)
(ht-set! chess/post-move-keys "d" :dirty)

(setq chess/moves-type-alist
      '(("v" . "vone")
        ("m" . "ml")
        ("n" . "ncg")
        ("N" . "ncgb")
        ("d" . "dirty")
        ("h" . "ncgh")
        ("p" . "pgn")
        ("o" . "vonedirty")
        ))

(defun chess/pre-move-function (id action)
  (let (fun (ht-get chess/pre-move-hooks id))
    (when fun (run-hooks fun))
    ))

(defun chess/post-move-function (id action)
  (let* (
         (key (ht-get chess/post-move-keys id))
         (mode (or (books/major-mode) major-mode))
         (fun)
         )
    (and key (setq fun (plist-get (ht-get chess/wrap-functions mode) key)))
    (and fun (fboundp fun) (funcall fun key))
    (save-buffer)
    ))

(defun chess/sp-hook (mode plist)
  (ht-set! chess/wrap-functions mode plist)
  )

(defun chess/function (mode plist)
  (ht-set! chess/other-functions mode plist)
  )


(provide 'books/chess-hooks-and-functions)
