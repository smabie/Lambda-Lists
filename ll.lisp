;;
;; Lambda Lists
;; This file is public domain, as declared by Sturm S. Mabie
;; 

(defpackage :lambda-lists
  (:use :common-lisp))

(in-package :lambda-lists)

;; Because I couldn't think of a good way to hash the patterns, we just have to
;; organize them by length.
(defvar *dict* (make-array 64 :initial-element nil))

(defmacro init-dict ()
  `(progn
     (setf *dict* (make-array 64 :initial-element nil))
     (mapc #'(lambda (a)
               (add-pattern (first a) (second a) *dict*))
           '(((id ?a) ,#'(lambda (p)
                           (second p)))
             ((if ?a ?b ?c) ,#'(lambda (p)
                                 (if (expand (second p) *dict*)
                                     (third p)
                                     (fourth p))))
             ((?a and ?b) ,#'(lambda (p)
                               (and (expand (first p) *dict*)
                                    (expand (third p) *dict*))))
             ((?a or ?b) ,#'(lambda (p)
                              (or (expand (first p) *dict*)
                                  (expand (third p) *dict*))))
	     ((not ?a) ,#'(lambda (p)
			    (not (expand (second p) *dict*))))
             ((?a * ?b) ,#'(lambda (p)
                             (* (expand (first p) *dict*)
                                (expand (third p) *dict*))))
             ((?a - ?b) ,#'(lambda (p)
                             (- (expand (first p) *dict*)
                                (expand (third p) *dict*))))
             ((?a + ?b) ,#'(lambda (p)
                             (+ (expand (first p) *dict*)
                                (expand (third p) *dict*))))
             ((?a / ?b) ,#'(lambda (p)
                             (/ (expand (first p) *dict*)
                                (expand (third p) *dict*))))
             ((?a = ?b) ,#'(lambda (p)
                             (= (expand (first p) *dict*)
                                (expand (third p) *dict*))))))))

(defun wildcard? (s)
  (char= #\? (char (write-to-string s) 0)))

;; Horrible horrible horrible
(defun match-pattern? (pattern list)
  (catch 'no
    (if (/= (length pattern) (length list))
        (throw 'no nil)
        (mapc #'(lambda (w l)
                  (or (wildcard? w)
                      (if (and (listp w) (listp l))
                          (and (null (match-pattern? w l))
                              (throw 'no nil))
                          (or (equal w l)
                              (throw 'no nil)))))
              pattern list))
    t))

(defun get-pairs (list pattern)
  (remove nil (mapcar #'(lambda (l p)
                          (if (atom l)
                              (if (wildcard? l)
                                  (cons l p)
                                  nil)
                              (get-pairs l p)))
                      pattern list)))

(defun replace-wildcards (alist pattern)
  (mapcar #'(lambda (l)
              (if (atom l)
                  (if (wildcard? l)
                      (cdr (assoc l alist))
                      l)
                  (replace-wildcards alist l)))
           pattern))

(defun expand-prim (list pattern expansion)
  (if (match-pattern? pattern list)
      (replace-wildcards (get-pairs list pattern) expansion)
      list))

(defun add-pattern (pattern expansion dict)
  (push (list pattern expansion) (svref dict (length pattern))))

(defun find-pattern (pattern dict)
  (find pattern (svref dict (length pattern)) 
        :key #'first 
        :test #'(lambda (a b)
                  (match-pattern? b a))))

(defun expand-1 (pattern dict)
  (let ((a (find-pattern pattern dict)))
    (if (functionp (second a))
        (funcall (second a) pattern)
        (expand-prim pattern (first a) (second a)))))

(defun expand (pattern dict)
  (if (atom pattern)
      pattern
      (let ((ex (expand-1 pattern dict)))
        (if (atom ex)
            ex
            (let ((ex2 (expand-1 ex dict)))
              (if (not (equal ex ex2))
                  (expand ex2 dict)
                  (mapcar #'(lambda (a)
                              (if (atom a)
                                  a
                                  (expand a dict))) 
                          ex)))))))

(defun main ()
  (let ((r (read)))
    (if (eql r 'let)
        (let ((p (read)))
          (if (eql (read) '=>)
              (progn
                (add-pattern p (read) *dict*)
                (main))
              (error "Malformed let")))
        (progn
          (format t "~A~%" (expand r *dict*))
          (main)))))
