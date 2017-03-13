(setf testsudoku 
        #2a((0 9 3 0 0 0 1 4 2)
            (8 5 6 1 4 0 7 0 0)
            (4 2 1 7 0 3 0 6 0)
            (0 3 0 0 1 6 0 8 4)
            (1 0 0 9 3 4 0 2 7)
            (2 6 4 0 8 7 0 0 1)
            (0 0 2 4 0 0 3 0 8)
            (5 7 0 3 0 0 0 0 0)
            (3 4 0 0 0 1 2 7 0)))

(defun sudoku-2a99p (arrsudoku);是否是二维9乘9数组
  (equal '(9 9) (array-dimensions arrsudoku)))

(defun sudoku-inputrecu (lsudoku sr sc);
  (let ((asudoku (car lsudoku))
        (value (aref (car lsudoku) sr sc)))
    (and
      (member value '(0 1 2 3 4 5 6 7 8 9 nil))
      (progn
        (if (eql value 0)
          (setf (aref asudoku sr sc) nil)
          nil)
        (if (eql sc 9)
          (if (eql sr 9)
            'fin
            (sudoku-inputrecu lsudoku (+ sr 1) 0))
          (sudoku-inputrecu lsudoku sr (+ sc 1)))))))

;(defun sudoku-traverse-2a99 (lsudoku key)
;  (let ((asudoku (car lsudoku)))
;    (dotimes (arow 9)
;      (dotimes (acol 9)
;        (apply key (aref arrsudoku arow acol))))))

(defun sudoku-inputprocess (listsudoku)
  (and (consp listsudoku)
       (arrayp (car listsudoku))
       (sudoku-2a99p (car listsudoku))))

(defun sudoku-print2a99 (arrsudoku)
"打印当前二维数组"
  (if (arrayp arrsudoku)
      (if (equal '(9 9) (array-dimensions arrsudoku))
        (progn
          (format t "Print SuDoKu~%")
          (dotimes (arow 9)
            (format t "(")
            (dotimes (acol 9)
              (format t " ~A" 
                (if (aref arrsudoku arow acol)
                  (aref arrsudoku arow acol)
                  'n)))
            (format t " )~%")))
        nil)
      nil))

(defun sudoku-make-SNM2a99 (arrsudoku snum)
  "Single Number Matrix"
  (let ((ret2a (make-array '(9 9))))
    (format t "snum:~A~%" snum)
    (dotimes (i 9)
      (dotimes (j 9)
        (let 
          ((jnum (aref arrsudoku i j))) 
          (setf (aref ret2a i j)
                (cond ((eql 0 jnum) 0)
                      ((eql snum jnum) 'T)
                      (t 'F))))))
    ret2a
    ))

(defun sudoku-mark (arrsudoku numi numj)
  "Mark the 'F in the Matrix"
  (sudoku-fill-row arrsudoku numi)
  (sudoku-fill-col arrsudoku numj)
  ;(sudoku-fill-cell arrsudoku numi numj)
  (setf (aref arrsudoku numi numj) 'T)
  ;(sudoku-print2a99 arrsudoku)
  )

(defun sudoku-fill-row (arrsudoku numi)
  (dotimes (j 9)
    (setf (aref arrsudoku numi j) 'F)))

(defun sudoku-fill-col (arrsudoku numj)
  (dotimes (i 9)
    (setf (aref arrsudoku i numj) 'F)))

(defun sudoku-fill-cell (arrsudoku numi numj))

(defun sudoku-fill-certF (arrMatrix snum)
  "Fill the Single-Number Matrix to fill the certain not numcell"
  (format t "Fill the ~A~%" (+ snum 1))
  (dotimes (i 9)
    (dotimes (j 9)
      (cond ((eql 'T (aref arrMatrix i j)) (sudoku-mark arrMatrix i j))
            (t nil)
            )))
  arrMatrix
  )

(defun sudoku-iterate-nums (arrsudoku)
  "Iterate the 9 Num to create 9 Matrix"
  (sudoku-print2a99 arrsudoku)
  (dotimes (tnum 9)
    (sudoku-print2a99 (sudoku-fill-certF 
                        (sudoku-make-SNM2a99 arrsudoku (+ tnum 1))
                        tnum))
    ;(let ((tempm (sudoku-make-SNM2a99 arrsudoku (+ tnum 1))))
    ;  (sudoku-print2a99 tempm)
    ;  (sudoku-print2a99 (sudoku-fill-certF tempm tnum))
    ;  )
    ))(setf testsudoku 
        #2a((0 9 3 0 0 0 1 4 2)
            (8 5 6 1 4 0 7 0 0)
            (4 2 1 7 0 3 0 6 0)
            (0 3 0 0 1 6 0 8 4)
            (1 0 0 9 3 4 0 2 7)
            (2 6 4 0 8 7 0 0 1)
            (0 0 2 4 0 0 3 0 8)
            (5 7 0 3 0 0 0 0 0)
            (3 4 0 0 0 1 2 7 0)))

(defun sudoku-2a99p (arrsudoku);是否是二维9乘9数组
  (equal '(9 9) (array-dimensions arrsudoku)))

(defun sudoku-inputrecu (lsudoku sr sc)
  (let ((asudoku (car lsudoku))
        (value (aref (car lsudoku) sr sc)))
    (and
      (member value '(0 1 2 3 4 5 6 7 8 9 nil))
      (progn
        (if (eql value 0)
          (setf (aref asudoku sr sc) nil)
          nil)
        (if (eql sc 9)
          (if (eql sr 9)
            'fin
            (sudoku-inputrecu lsudoku (+ sr 1) 0))
          (sudoku-inputrecu lsudoku sr (+ sc 1)))))))

;(defun sudoku-traverse-2a99 (lsudoku key)
;  (let ((asudoku (car lsudoku)))
;    (dotimes (arow 9)
;      (dotimes (acol 9)
;        (apply key (aref arrsudoku arow acol))))))

(defun sudoku-inputprocess (listsudoku)
  (and (consp listsudoku)
       (arrayp (car listsudoku))
       (sudoku-2a99p (car listsudoku))))

(defun sudoku-print2a99 (arrsudoku)
  (if (arrayp arrsudoku)
      (if (equal '(9 9) (array-dimensions arrsudoku))
        (progn
          (format t "Print SuDoKu~%")
          (dotimes (arow 9)
            (format t "(")
            (dotimes (acol 9)
              (format t " ~A" 
                (if (aref arrsudoku arow acol)
                  (aref arrsudoku arow acol)
                  'n)))
            (format t " )~%")))
        nil)
      nil))

(defun sudoku-make-SNM2a99 (arrsudoku snum)
  "Single Number Matrix"
  (let ((ret2a (make-array '(9 9))))
    (format t "snum:~A~%" snum)
    (dotimes (i 9)
      (dotimes (j 9)
        (let 
          ((jnum (aref arrsudoku i j))) 
          (setf (aref ret2a i j)
                (cond ((eql 0 jnum) 0)
                      ((eql snum jnum) 'T)
                      (t 'F))))))
    ret2a
    ))

(defun sudoku-mark (arrsudoku numi numj)
  "Mark the 'F in the Matrix"
  (sudoku-fill-row arrsudoku numi)
  (sudoku-fill-col arrsudoku numj)
  ;(sudoku-fill-cell arrsudoku numi numj)
  (setf (aref arrsudoku numi numj) 'T)
  ;(sudoku-print2a99 arrsudoku)
  )

(defun sudoku-fill-row (arrsudoku numi)
  (dotimes (j 9)
    (setf (aref arrsudoku numi j) 'F)))

(defun sudoku-fill-col (arrsudoku numj)
  (dotimes (i 9)
    (setf (aref arrsudoku i numj) 'F)))

(defun sudoku-fill-cell (arrsudoku numi numj))

(defun sudoku-fill-certF (arrMatrix snum)
  "Fill the Single-Number Matrix to fill the certain not numcell"
  (format t "Fill the ~A~%" snum)
  (dotimes (i 9)
    (dotimes (j 9)
      (cond ((eql 'T (aref arrMatrix i j)) (sudoku-mark arrMatrix i j))
            (t nil)
            )))
  arrMatrix
  )

(defun sudoku-iterate-nums (arrsudoku)
  "Iterate the 9 Num to create 9 Matrix"
  (sudoku-print2a99 arrsudoku)
  (dotimes (tnum 9)
    (sudoku-print2a99 (sudoku-fill-certF 
                        (sudoku-make-SNM2a99 arrsudoku (+ tnum 1))
                        tnum))
    ;(let ((tempm (sudoku-make-SNM2a99 arrsudoku (+ tnum 1))))
    ;  (sudoku-print2a99 tempm)
    ;  (sudoku-print2a99 (sudoku-fill-certF tempm tnum))
    ;  )
    ))
(defun sudoku-test-num (arrsudoku tnum)
  "Test a start"
  (sudoku-print2a99 arrsudoku)
  (sudoku-print2a99 (sudoku-fill-certF
                      (sudoku-make-SNM2a99 arrsudoku tnum )
                      tnum)))

(defun sudoku-test-fill (arrsudoku)
  "Fill the Single Number Matrix"
  (let ((countT (sudoku-count-t-number arrsudoku)))
    countT))

;;返回T的数量
(defun sudoku-count-t-number (arrsudoku)
  "Return Number of T"
  (let ((count0 0))
    (dotimes (i 9)
      (dotimes (j 9)
	(cond ((eql 'T (aref arrsudoku i j)) (setf count0 (+ count0 1)))
	      )))
    count0))

(defun sudoku-check0 (arrsudoku i j)
  "check if the cell can be filled in T"
  0)
