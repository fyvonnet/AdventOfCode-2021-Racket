#lang racket


(define (read-input in)
  (match (read-line in)
    [(? eof-object?) null]
    [line 
      (cons
        (map
          (λ (s) (regexp-split " " s))
          (regexp-split " \\| " line))
        (read-input in))]))

(define letters/bits
  (for/list
    ([i (in-range 7)])
    (list (integer->char (+ i 97)) (expt 2 i))))

(define (string->binary str)
  (for/fold
    ([bin 0])
    ([c (string->list str)])
    (bitwise-ior (second (assoc c letters/bits)) bin)))

;  0000
; 1    2
; 1    2
;  3333
; 4    5
; 4    5
;  6666

(define segments-lists
  (list 
    '(0 1 2   4 5 6)   ;0
    '(    2     5  )   ;1
    '(0   2 3 4   6)   ;2
    '(0   2 3   5 6)   ;3
    '(  1 2 3   5  )   ;4
    '(0 1   3   5 6)   ;5
    '(0 1   3 4 5 6)   ;6
    '(0   2     5  )   ;7
    '(0 1 2 3 4 5 6)   ;8
    '(0 1 2 3   5 6))) ;9

(define (decode-pattern pattern)
  (for/fold
    ([one   0]
     [four  0]
     [seven 0]
     [six-segments '()]
     #:result
     (let
       ([segments (make-vector 7 0)] [eight 127] [six null] [zero-or-nine null] [zero null] [nine null])

       ; one has a segment not in six
       (for ([x six-segments])
         (if (= one (bitwise-and x one))
           (set! zero-or-nine (cons x zero-or-nine))
           (set! six x)))

       ; four has a segment not in zero
       (for ([x zero-or-nine])
         (if (= four (bitwise-and four x))
           (set! nine x)
           (set! zero x)))

       (vector-set! segments 0 (bitwise-xor one seven)) ; segment 0 is in one, not seven
       (vector-set! segments 5 (bitwise-and one six)) ; segment 5 is the only common to one and six
       (vector-set! segments 2 (bitwise-xor one (vector-ref segments 5))) ; segment 2 is the other one in one
       (vector-set! segments 3 (bitwise-and four (bitwise-xor four zero))) ; segment 3 is present in four, not in zero
       (vector-set! segments 1 (bitwise-xor (bitwise-xor one four) (vector-ref segments 3))) ; other segment of the 1-3 part of four
       (vector-set! segments 4 (bitwise-xor eight nine)) ; segment 4 is in eight, not in nine
       (vector-set! segments 6 (for/fold ([lastseg eight]) ([bit (vector->list segments)]) (bitwise-xor lastseg bit))) ; last segment

       ; make signal/digit association list
       (for/list
         ([seglist segments-lists]
          [digit (in-range 10)])
         (list
           (for/fold
             ([bin 0])
             ([s seglist])
             (bitwise-ior bin (vector-ref segments s)))
           digit)))) 

    ([str pattern])
    (case (string-length str)
      [(2)  (values (string->binary str) four seven six-segments)]
      [(3)  (values one four (string->binary str) six-segments)]
      [(4)  (values one (string->binary str) seven six-segments)]
      [(6)  (values one four seven (cons (string->binary str) six-segments))]
      [else (values one four seven six-segments)])))


(let
  ((input (call-with-input-file "inputs/day08" read-input)))
  (displayln
    (for/sum
      ([digits (map cadr input)])
      (for/sum
        ([digit digits])
        (if (member (string-length digit) '(2 3 4 7)) 1 0))))
  (displayln
    (for/sum
      ([entry input])
      (let
        ([decoded-pattern (decode-pattern (first entry))])
        (for/fold
          ([output-value 0])
          ([digit
             (map
               (λ (s) (second (assoc (string->binary s) decoded-pattern)))
               (second entry))])
          (+ (* 10 output-value) digit))))))

;
; exemple: acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab
; 
; 1 is ab (length 2)
; 7 is dab (length 3)
; 
;  dddd
; .    x
; .    x
;  ....
; .    x
; .    x
;  ....
; 
; (x is a or b)
; 
; 4 is eafb (length 4)
; 
;  dddd
; y    x
; y    x
;  yyyy 
; .    x
; .    x
;  ....
; 
; (y is e or f)
; 
; 0, 6 and 9 are in cefabd cdfgeb and cagedb (length 6)
; 6 is cdfgeb (no a)
; a and b are deducted
; 
;  dddd
; y    a
; y    a
;  yyyy 
; .    b
; .    b
;  ....
; 
; 0 and 9 are in cefabd and cagedb
; 0 is cagedb (no f)
; e and f are deducted
; 9 is cefabd
; 
;  dddd
; e    a
; e    a
;  ffff
; .    b
; .    b
;  ....
; 
; g is in 6 and 0, not 9
; 
;  dddd
; e    a
; e    a
;  ffff
; g    b
; g    b
;  ....
; 
; c is deducted
; 
;  dddd
; e    a
; e    a
;  ffff
; g    b
; g    b
;  cccc
;

