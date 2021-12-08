#lang racket

(require racket/string)

(define (board-id square) (match square [(list b l c) (+ (* b 5 5) (* l 5) c)]))
(define (boards-set! vec square val) (vector-set! vec (board-id square) val))
(define (boards-ref  vec square    ) (vector-ref  vec (board-id square)    ))

(define (get-boards in)
  (match (read-line in) ; skip empty line
    [(? eof-object?) null]
    [_ (cons
         (for/list
           ([_ (in-range 5)])
           (map string->number (regexp-split " " (string-normalize-spaces (read-line in)))))
         (get-boards in))]))

(define (check-winner boards square)
  (match square
    [(list b l c)
     (if
       (or
         (for/and 
           ([i (in-range 5)])
           (false? (boards-ref boards (list b i c))))
         (for/and
           ([i (in-range 5)])
           (false? (boards-ref boards (list b l i)))))
       (for/sum
         ([l (in-range 5)])
         (for/sum
           ([c (in-range 5)])
           (match (boards-ref boards (list b l c))
             [#f 0]
             [n n])))
       #f)]))

(define (read-input in)
  (list
    (map string->number (regexp-split "," (read-line in)))
    (get-boards in)))


(match (call-with-input-file "inputs/day04" read-input)
  [(list numbers boards-list)
   (let*
     ([boards-count  (length boards-list)]
      [boards        (list->vector (flatten boards-list))]
      [locations     (make-vector 100 null)]
      [still-playing (make-vector boards-count #t)])

     (for ([b (in-range (sub1 boards-count) -1 -1)])
       (for ([l (in-range 5)])
         (for ([c (in-range 5)])
           (let ([square (boards-ref boards (list b l c))])
             (vector-set! locations square (cons (list b l c) (vector-ref locations square)))))))

     (let 
       ([scores
          (for/fold ; for each draw number
            ([winners null])
            ([n numbers])
            (for/fold ; for each matching square
              ([w winners])
              ([square
                 (filter
                   (λ (s) (vector-ref still-playing (first s)))
                   (vector-ref locations n))])
              (boards-set! boards square #f)
              (match (check-winner boards square)
                [#f w]
                [score (vector-set! still-playing (first square) #f)
                   (cons (* n score) w)])))])

       (displayln (last  scores))
       (displayln (first scores))))])

