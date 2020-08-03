(defpackage nock
  (:use :cl)
  (:shadowing-import-from :cl :*readtable*)
  (:export :nock :read-nock :tar))

(in-package nock)

(defun nockify (l)
  (cond ((not (consp l)) l)
        ((null l) nil)
        ((null (cdr l)) (nockify (car l)))
        (t (progn
             (cons 
              (nockify (car l))
              (nockify (cdr l)))))))

(defun nlist (&rest p)
  (nockify p))

(defun ntar (&rest p)
  (tar (apply #'nlist p)))

(defun read-separator (stream char)
  (declare (ignore stream char))
  (error "Nock closing brace shouldn't be read alone"))

(defun read-nock-cell (stream)
  (labels ((peek-next-char () (peek-char t stream t nil t))
           (discard-next-char () (progn (read-char stream t nil t) nil))
           (discard-while= (&rest chars) (loop while (find (peek-next-char) chars) do (discard-next-char))))
    (discard-while= #\Tab #\Space #\NewLine #\Return #\LineFeed)
    (unless (char= (peek-next-char) #\[)
      (error "Malformed nock cell, must start with #\\["))
    (discard-next-char)
    (loop
       for nobj = (cond
                    ((progn (discard-while= #\Tab #\Space #\NewLine #\Return #\LineFeed) (char= (peek-next-char) #\[))
                     (read-nock-cell stream))
                    ((progn (discard-while= #\Tab #\Space #\NewLine #\Return #\LineFeed) (char= (peek-next-char) #\]))
                     (discard-next-char))
                    (t (read stream)))
       while nobj
       collect nobj into nobjs
       finally (return `(nockify (list ,@nobjs))))))

(defun read-nock (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((*readtable* (copy-readtable)))
    (set-macro-character #\] #'read-separator)
    (read-nock-cell stream)))

(set-dispatch-macro-character
 #\# #\n #'read-nock)

(defun tis (a b)
  "= is an equivalence operator"
  (unless (and (typep b 'integer) (typep a 'integer))
    (error "tis must be called on atoms"))
  (if (= a b) 0 1))

(defun lus (atom)
  "+ adds 1 to an atom"
  (unless (typep atom 'integer)
    (error "must call lus on atom"))
  (+ atom 1))

(defun wut (noun)
  "? determines type of noun -- atom -> 1, cell -> 0"
  (if (typep noun 'integer) 1 0))

(defun slot (n l)
  "/ is a vector subscript op, but like a tree subscript op"
  (when (< n 1) (error "Cannot view a non-positive slot"))
  (cond ((= 1 n) l)
        ((= 2 n) (car l))
        ((= 3 n) (cdr l))
        ((= (mod n 2) 0) (slot 2 (slot (/ n 2) l)))
        ((= (mod n 2) 1) (slot 3 (slot (truncate (/ n 2)) l)))))

(defun edit (a b c)
  "swaps item at slot a in list c with b"
  (cond ((= a 1) b)
        ((= (mod a 2) 0) (edit (/ a 2) (nlist b (slot (+ a 1) c)) c))
        ((= (mod a 2) 1) (edit (truncate (/ a 2)) (nlist (slot (- a 1) c) b) c))))

(defun tar (l)
  (unless (typep l 'list)
    (error "cannot call tar on l"))
  (let* ((subj (slot 2 l))
         (form (slot 3 l))
         (op (slot 2 form)))
    (if (consp op)
        (nlist (ntar subj op)
               (ntar subj (cdr form)))
        (let* ((a subj)
               (b (slot 3 form)))
          (cond ((= op 0) (slot b a))
                ((= op 1) b)
                ((= op 2) (ntar (ntar a (slot 2 b))
                                (ntar a (slot 3 b))))
                ((= op 3) (wut (ntar a b)))
                ((= op 4) (lus (ntar a b)))
                ((= op 5) (tis (ntar a (slot 2 b))
                               (ntar a (slot 3 b)))) 
                ((= op 6)
                 (let ((b (slot 2 b))
                       (c (slot 6 b))
                       (d (slot 7 b)))
                   (ntar a
                         (ntar (nlist c d)
                               0
                               (ntar a 4 4 b))))) ; tested up to here
                ((= op 7) (ntar (tar (nlist a (slot 2 b)))
                                (slot 3 b)))
                ((= op 8) (ntar (nlist (ntar a (slot 2 b))
                                       a)
                                (slot 3 b)))
                ((= op 9) (ntar (ntar a (slot 3 b)) 2 (nlist 0 1) 0 (slot 2 b)))
                ((= op 10)
                 (let ((b (slot 28 l))
                       (c (slot 29 l))
                       (d (slot 15 l)))
                   (edit b (ntar a c) (ntar a d))))
                ((= op 11) (ntar a (slot 3 b))))))))

#|
(tar #n[42 [8 [1 0] 8 [1 6 [5 [0 7] 4 0 6] [0 6] 9 2 [0 2] [4 0 6] 0 7] 9 2 0 1]])
-> 41
|#
