#lang racket

(define (read-input in)
  (match (read-line in)
    [(? eof-object?) null]
    [line
      (cons
        (string->number line)
        (read-input in))]))

(let*
  ([input (call-with-input-file "inputs/day01" read-input)]
   [sliding-window
     (for/list
       ([a       input]
        [b (cdr  input)]
        [c (cddr input)])
       (+ a b c))])

  (for ([l (list input sliding-window)])
    (displayln
      (for/sum
        ([a      l]
         [b (cdr l)])
        (if (< a b) 1 0)))))

