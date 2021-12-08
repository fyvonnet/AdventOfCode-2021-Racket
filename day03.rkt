#lang racket

(define (binary->number str [invert false])
  (for/fold
    ([n 0])
    ([c (string->list str)])
    (+ (case c
         [(#\0) (if invert 1 0)]
         [(#\1) (if invert 0 1)])
       (* 2 n))))

(define (read-input in)
  (match (read-line in)
    [(? eof-object?) null]
    [line (cons line (read-input in))]))

(define (rating input compare [bit 0])
  (if (= 1 (length input))
    (binary->number (car input))
    (rating
      (for/fold
        ([zeros null]
         [ones  null]
         #:result (if (compare (length zeros) (length ones)) zeros ones))
        ([word input])
        (case (string-ref word bit)
          [(#\0) (values (cons word zeros) ones)]
          [(#\1) (values zeros (cons word ones))]))
      compare
      (add1 bit))))


(let*
  ([input (call-with-input-file "inputs/day03" read-input)]
   [word-length (string-length (car input))])

  (displayln
    (let ([output-word (make-string word-length)])
      (for ([bit (in-range word-length)])
        (string-set!
          output-word
          bit
          (for/fold
            ([count-zeros 0]
             [count-ones  0]
             #:result (if (> count-zeros count-ones) #\0 #\1))
            ([word input])
            (case (string-ref word bit)
              [(#\0) (values (add1 count-zeros) count-ones)]
              [(#\1) (values count-zeros (add1 count-ones))]))))
      (* (binary->number output-word false)
         (binary->number output-word true ))))

  (displayln
    (* (rating input > )
       (rating input <=))))
