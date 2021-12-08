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

(define segments-vector
  (vector
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
  (let*
    ([segments (make-vector  7 0)]
     [signals  (make-vector 10 null)]
     [zero-or-nine null]
     [six-segments
       (for/fold
         ([lst '()])
         ([s pattern])
         (case (string-length s)
           [(2) (vector-set! signals 1 (string->binary s)) lst]
           [(3) (vector-set! signals 7 (string->binary s)) lst]
           [(4) (vector-set! signals 4 (string->binary s)) lst]
           [(6) (cons (string->binary s) lst)]
           [else lst]))])

    ; all segments on
    (vector-set! signals 8 127)

    ; one has a segment not in six
    (for ([x six-segments])
      (if (= (vector-ref signals 1) (bitwise-and x (vector-ref signals 1)))
        (set! zero-or-nine (cons x zero-or-nine))
        (vector-set! signals 6 x)))

    ; four has a segment not in zero
    (for ([x zero-or-nine])
      (if (= (vector-ref signals 4) (bitwise-and (vector-ref signals 4) x))
        (vector-set! signals 9 x)
        (vector-set! signals 0 x)))

    ; segment 0 is in one, not in seven
    (vector-set! segments 0
                 (bitwise-xor
                   (vector-ref signals 1)
                   (vector-ref signals 7)))

    ; segment 5 is the only common to one and six
    (vector-set! segments 5
                 (bitwise-and
                   (vector-ref signals 1)
                   (vector-ref signals 6)))

    ; segment 2 is the other one in one
    (vector-set! segments 2
                 (bitwise-xor
                   (vector-ref signals  1)
                   (vector-ref segments 5)))

    ; segment 3 is present in four, not in zero
    (vector-set! segments 3
                 (bitwise-and
                   (vector-ref signals 4)
                   (bitwise-xor
                     (vector-ref signals 4)
                     (vector-ref signals 0))))

    ; other segment of the 1-3 part of four
    (vector-set! segments 1
                 (bitwise-xor
                   (vector-ref segments 3)
                   (bitwise-xor
                     (vector-ref signals 1)
                     (vector-ref signals 4))))

    ; segment 4 is in eight, not in nine
    (vector-set! segments 4
                 (bitwise-xor 127 (vector-ref signals 9)))

    ; last segment
    (vector-set! segments 6
                 (for/fold
                   ([lastseg 127])
                   ([bit (vector->list segments)])
                   (bitwise-xor lastseg bit)))

    ; complete signals list and make signal/digit association list
    (for/list
      ([sign (vector->list signals)]
       [digit (in-range 10)])
      (list
        (if (null? sign)
          (for/fold
            ([bin 0])
            ([s (vector-ref segments-vector digit)])
            (bitwise-ior bin (vector-ref segments s)))
          sign)
        digit))))


(let ((input (call-with-input-file "inputs/day08" read-input)))

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


; exemple: acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab
; 
; 1 is ab (2 segments)
; 7 is dab (3 segments)
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
; 4 is eafb (4 segments)
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
; 0, 6 and 9 are in cefabd cdfgeb and cagedb (6 segments)
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

