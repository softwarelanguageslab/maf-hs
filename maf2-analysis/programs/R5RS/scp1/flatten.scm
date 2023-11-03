(define (find-last lijst)
  (if (null? lijst)
      (error "find-last -- lijst heeft geen laatste element")
      (let ((next (cdr lijst)))
        (if (null? next)
            lijst
            (find-last next)))))

(define (flatten! lijst)
  (if (null? lijst)
      '()
      (let* ((sublist (car lijst))
             (restlist (flatten! (cdr lijst))))
        (if (null? sublist)
            restlist
            (let ((last (find-last sublist)))
              (set-cdr! last restlist)
              sublist)))))

(define (atom? x) (not (pair? x)))

(define (flatten2! lijst)
  (let ((hulpcel (cons 'dummy lijst)))
    (define (flatten-aux! prev current)
      (cond ((null? current) (cdr hulpcel))
            ((null? (car current))
             (set-cdr! prev (cdr current))
             (flatten-aux! prev (cdr current)))
            ((pair? (car current))
             (set-cdr! prev (flatten2! (car current)))
             (flatten-aux! (find-last prev) (cdr current)))
            ((null? (cdr prev))
             (set-cdr! prev current)
             (flatten-aux! (cdr prev) (cdr current)))
            ((atom? (car current))
             (flatten-aux! (cdr prev) (cdr current)))))
    (flatten-aux! hulpcel lijst)
    (cdr hulpcel)))

(define res (and  (equal? (flatten! '((1 2) (3 4 5) (6) (7 8))) '(1 2 3 4 5 6 7 8))
                  (equal? (flatten! '(() (1 2) (3 4 5) () (6) (7 8))) '(1 2 3 4 5 6 7 8))
                  (equal? (flatten2! '((1 (2 3) 4) 5 6 (7 8))) '(1 2 3 4 5 6 7 8))
                  (equal? (flatten2! '((1 2) (3 4 5) (6) (7 8))) '(1 2 3 4 5 6 7 8))
                  (equal? (flatten2! '(() (1 2) (3 4 5) () (6) (7 8))) '(1 2 3 4 5 6 7 8))
                  (equal? (flatten2! '(1 2 (3 (4 5) 6 (7 8) 9) 10)) '(1 2 3 4 5 6 7 8 9 10))))

res