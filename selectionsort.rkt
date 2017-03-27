#lang racket

(define add-end-of-list
  (lambda (lst a)
    (if (null? lst) (list a) (cons (car lst) (add-end-of-list (cdr lst) a)))))

(define selection-sort
  (lambda (destination source discard)
    (if (null? source)
        (if (null? discard)
            destination
            (selection-sort (cons (car discard) destination) (cdr discard) (list)))
        (if (null? destination)
            (selection-sort (cons (car source) destination) (cdr source) discard)
             (if (> (car source) (car destination))
                 (selection-sort (cons (car source) (cdr destination)) (cdr source) (add-end-of-list discard (car destination)))
                 (selection-sort destination (cdr source) (add-end-of-list discard (car source))))))))

(selection-sort  '() '(2 5 9 1 3 4) '())
