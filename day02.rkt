#lang racket

(define (read-input in)
  (match (read-line in)
    [(? eof-object?) null]
    [line
      (cons
        (match (regexp-split " " line)
          [(list command parameter)
           (list 
             (string->symbol command)
             (string->number parameter))])
        (read-input in))]))


(let*
  ([input (call-with-input-file "inputs/day02" read-input)])

  (displayln
    (for/fold
      ([hpos  0]
       [depth 0]
      #:result (* hpos depth))
      ([cmd input])
      (match cmd
        [(list 'forward n) (values (+ n hpos) depth)]
        [(list 'down    n) (values hpos (+ depth n))]
        [(list 'up      n) (values hpos (- depth n))])))

  (displayln
    (for/fold
      ([hpos  0]
       [depth 0]
       [aim   0]
      #:result (* hpos depth))
      ([cmd input])
      (match cmd
        [(list 'down    n) (values hpos depth (+ aim n))]
        [(list 'up      n) (values hpos depth (- aim n))]
        [(list 'forward n) (values (+ n hpos) (+ depth (* aim n)) aim)]))))

