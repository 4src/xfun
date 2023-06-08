(defvar *the* '((bins . 16) (file . "../data/auto93.csv")
                            (go . 'nothing)))

(defun char0(x) (char (symbol-name x) 0))
(defun charn(x) (let ((s (symbol-name x))) (elt s (1- (length s)))))

(defstruct data rows cols)
(defstruct cols names  all x y)
(defun names (&rest lst &aux (cols (make-cols :name lst)))
  (with-slots (all x y klass) cols
    (loop :for at :from 0 :and name in names :do
       (let ((what (if (uppercase-p (elt name 0)) #'make-num #'make-sym))
             (col (funcall what :at at :txt name :w (member (charn
  (let ((cols (make-cols :names lst)))
    (dolist (name names cols)
      (if (uppercase-p (elt name 0))

(defvar *auto93*  
  (data
    :cols  (names "Cyndrs" "Vol" "Hpx" "Lbs-" "Acc+" "Model" "origin" "Mpg+")
    :rows '((8 304.0 193 4732 18.5 70 1 10)
            (8 360 215 4615 14 70 1 10)
            (8 307 200 4376 15 70 1 10)
            (8 318 210 4382 13.5 70 1 10)
            (8 429 208 4633 11 72 1 10)
            (8 400 150 4997 14 73 1 10)
            (8 350 180 3664 11 73 1 10)
            (8 383 180 4955 11.5 71 1 10)
            (8 350 160 4456 13.5 72 1 10)
            (8 429 198 4952 11.5 73 1 10)
            (8 455 225 4951 11 73 1 10)
            (8 400 167 4906 12.5 73 1 10)
            (8 350 180 4499 12.5 73 1 10))))
