#lang racket
;Copy of program submitted for hackerrank problem at
;https://www.hackerrank.com/challenges/brainf-k-interpreter-fp

;modify-elem : List Integer Function -> List
;Changes element of l at index i by applying f to the element
(define (modify-elem l i f)
  (if (= i 0)
      (cons (f (car l)) (cdr l))
      (cons (car l) (modify-elem (cdr l) (- i 1) f))))

;list-at : List Integer -> ?
;Returns element at index i of l
(define (list-at l i)
  (if (= i 0)
      (car l)
      (list-at (cdr l) (- i 1))))

;string-find : String String -> Integer
;Returns the index of the first instance of sub in s
(define (string-find s sub)
  (define (sf-help s sub i)
    (cond ((> (+ i (string-length sub)) (string-length s)) -1)
          ((string=? sub (substring s i (+ i (string-length sub)))) i)
          (else (sf-help s sub (+ i 1)))))
  (sf-help s sub 0))

;string-at : String Integer -> Char
;Returns character at index i of string s
(define (string-at s i)
  (substring s i (+ i 1)))

;string-contains : String String -> Boolean
;True if sub is a substring of s
(define (string-contains? s sub)
  (> (string-find s sub) -1))

;matching-open-bracket : String Integer -> Integer
;Returns index of the matching open bracket for a given close bracket
(define (matching-open-bracket s b)
  (define (mob-help i sets)
    (define CUR (string-at s i))
    (cond ((string=? CUR "[") (if (= sets 0)
                                  i
                                  (mob-help (- i 1) (- sets 1))))
          ((string=? CUR "]") (mob-help (- i 1) (+ sets 1)))
          (else (mob-help (- i 1) sets))))
  (mob-help (- b 1) 0))

;matching-close-bracket : String Integer -> Integer
;Returns index of the matching close bracket for a given open bracket
(define (matching-close-bracket s a)
  (define (mob-help i sets)
    (define CUR (string-at s i))
    (cond ((string=? CUR "]") (if (= sets 0)
                                  i
                                  (mob-help (+ i 1) (- sets 1))))
          ((string=? CUR "[") (mob-help (+ i 1) (+ sets 1)))
          (else (mob-help (+ i 1) sets))))
  (mob-help (+ a 1) 0))
                                    

;parse-hr-input : String -> '(String String)
;Takes an input string in format specified at:
;  https://www.hackerrank.com/challenges/brainf-k-interpreter-fp
;First line: Input length in characters, then program line count, separated by a space
;Second line: Input to BF program, terminated by $ character
;Rest of lines: BF program
;Returns input string to BF program and string of the BF program itself
(define (parse-hr-input)
  (define (read-lines n)
    (if (= n 0)
        ""
        (string-append (read-line) (read-lines (- n 1)))))
  (define INPUT-LENGTH (read))
  (define PROG-LINECOUNT (read))
  (define TRASH (read-line))
  (define INPUT-LINE (read-line))
  (define INPUT-STR (substring INPUT-LINE 0 (- (string-length INPUT-LINE) 1)))
  (define PROG-STR (read-lines PROG-LINECOUNT))
  (cons (bf-clean PROG-STR) INPUT-STR))

;parse-hr-input : String -> '(String String)
;Takes an input string in format specified at:
;  https://www.hackerrank.com/challenges/brainf-k-interpreter-fp
;Executes BF program specified by HR input with given BF input
(define (exec-hr-input)
  (define PARSED (parse-hr-input))
  (brainfuck (car PARSED) (cdr PARSED)))



;Valid BF operations
(define BF-OPS "><+-.,[]")

;bf-op? : Char -> Boolean
;True if c is a valid brainfuck operation
(define (bf-op? c)
  (string-contains? BF-OPS c))

;bf-clean : String -> String
;Takes a BF program and removes all comments
(define (bf-clean s)
  (cond ((string=? s "") "")
        ((bf-op? (string-at s 0)) (string-append (string-at s 0) 
                                                      (bf-clean (substring s 1))))
        (else (bf-clean (substring s 1)))))

;brainfuck : String String -> String
;Runs BF program with given input, returns empty string on end of program
(define (brainfuck prog in)
  (bf-exec '(0) 0 prog 0 in 0))
       

;bf-exec : List[Number] Number String Number String Number -> Void
; - data: List of numbers representing values contained in memory of program
; - data-ptr: Index of location in memory currently in use by BF
; - prog: String containing program being executed by BF
; - prog-ptr: Index of current instruction being executed by BF
; - bf-input: String of input passed to BF as input to prog
; - op-count: Number of instructions executed since program start
(define (bf-exec data data-ptr prog prog-ptr bf-input op-count)
  (define CUR-OP (if (< prog-ptr (string-length prog))
                     (string-at prog prog-ptr)
                     "END OF PROG"))
  (define CUR-DATA (list-at data data-ptr))
  (cond ((string=? CUR-OP "END OF PROG") (void))
        ((= 100000 op-count) (newline)(display "PROCESS TIME OUT. KILLED!!!") (void))
        ((string=? CUR-OP ">") (if (= data-ptr (- (length data) 1))
                                   (bf-exec (append data '(0)) (+ data-ptr 1)
                                            prog (+ prog-ptr 1)
                                            bf-input (+ op-count 1))
                                   (bf-exec data (+ data-ptr 1)
                                            prog (+ prog-ptr 1)
                                            bf-input (+ op-count 1))))
        ((string=? CUR-OP "<") (bf-exec data (- data-ptr 1)
                                        prog (+ prog-ptr 1)
                                        bf-input (+ op-count 1)))
        ((string=? CUR-OP "+") (bf-exec (modify-elem data data-ptr (lambda(x)(modulo (+ x 1) 256))) 
                                        data-ptr
                                        prog (+ prog-ptr 1)
                                        bf-input (+ op-count 1)))
        ((string=? CUR-OP "-") (bf-exec (modify-elem data data-ptr (lambda(x)(modulo (- x 1) 256))) 
                                        data-ptr
                                        prog (+ prog-ptr 1)
                                        bf-input (+ op-count 1)))
        ((string=? CUR-OP ".") (display (integer->char CUR-DATA))
                               (bf-exec data data-ptr
                                        prog (+ prog-ptr 1)
                                        bf-input (+ op-count 1)))
        ((string=? CUR-OP ",") (bf-exec (modify-elem data data-ptr 
                                                     (lambda(x)(char->integer(string-ref bf-input 0)))) 
                                        data-ptr
                                        prog (+ prog-ptr 1)
                                        (substring bf-input 1) (+ op-count 1)))
        ((string=? CUR-OP "[") (if (= CUR-DATA 0)
                                   (bf-exec data data-ptr
                                            prog (matching-close-bracket prog prog-ptr)
                                            bf-input (+ op-count 1))
                                   (bf-exec data data-ptr
                                            prog (+ prog-ptr 1)
                                            bf-input (+ op-count 1))))
        ((string=? CUR-OP "]") (if (not (= CUR-DATA 0))
                                   (bf-exec data data-ptr
                                            prog (matching-open-bracket prog prog-ptr)
                                            bf-input (+ op-count 1))
                                   (bf-exec data data-ptr
                                            prog (+ prog-ptr 1)
                                            bf-input (+ op-count 1))))))

(exec-hr-input)