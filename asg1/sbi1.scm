#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.3 2016-09-23 18:23:20-07 - - $
;;
;; Matthew Lo, mamlo@ucsc.edu

;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;

;; *stderr*
(define *stderr* (current-error-port))

;; *run-file*
(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

;; die
;; exit and print error
(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

;; usage-exit
;; print usage information then exit
(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

;; evalexpr from examples. evaluates the expressions recursively
(define (evalexpr expr)
 ;(display expr)
 ; (newline)
   (cond ((number? expr) expr)
         ((symbol? expr) (hash-ref *variable-table* expr #f))
         ((pair? expr)   (apply (hash-ref *function-table* (car expr))
                                (map evalexpr (cdr expr))))
         (else #f))
)

;; define the statements
;; dim
;; creates an array given by the variable name and inserts it into the symbol table
;;(define (sbir-dim)
;;)

;; let
;; makes an assignment to a variable. for a variable, store its value into symbol table. for an array, the store message is sent to the vector representing the array
(define (sbir-let arg)
    (let ((result (evalexpr (first (cdr arg)))))
       ;(display result)
        (hash-set! *variable-table* (car arg) result))
)

;; goto
;; control transfers to the statement referred to by Label
(define (sbir-goto program label)
   (display label)
   (display (label-get (car label)))
   (evaluate-line program (label-get (car label)))
)
;;(define (sbir-if)
;;)

(define (sbir-print element)
   (when(not (null? element)) ; when element is not null,
      (display (length element))
      (display element)
      (newline)
      (display (cadr element))
   ; (newline)
      ;(display (caddr element))
     ; (display (string? (cadr element)))
     ; (newline)
     ; (display (number? (cadr element)))
     ;(newline)
   
      (when (and (= (length element) 2) (number? (cadr element))) ; if printing a number
         (display (cadr element)) ; print out the number
         (newline)                ; new line
      )

      (when (and (>= (length element) 2) (string? (cadr element))) ; if printing a string
         (display (cadr element)) ; print the string out
         (when (= (length element) 3) ; but if theres length 3 (an expression)
            (display (evalexpr (caddr element))) ; evaluate the expression and display
      )
      (newline) ; no matter what, new line
      )  
      (when (and (= (length element) 2) (symbol? (cadr element))) ; when printing out a variable
         ((lambda (expr) (display (evalexpr expr))) (cadr element)) ; find value of variable and print
         (newline)
      ))
)

;;(define (sbir-input)
;;)
;; Create the symbol tables

; A function table which holds all the functions
(define *function-table* (make-hash))

(for-each
  (lambda (pair)
          (hash-set! *function-table* (car pair) (cadr pair)))
  `(
      (/       ,(lambda (x y) (/ x  (+ 0.0 y)))) ; if the program is trying to divide by 0, divide by 0.0
      (log10   ,(lambda (x) (/ (log x) (log 10.0))))
      (mod     ,(lambda (x y) (- x (* (div x y) y))))
      (quot    ,(lambda (x y) (truncate (/ x y))))
      (rem     ,(lambda (x y) (- x (* (quot x y) y))))
      (+       ,+) ;(lambda (x y) (+ (+ x 0.0) (+ y 0.0))))
      (-       ,-)
      (*       ,*)
      (^       ,expt)	
      (atan    ,atan)
      (ceil    ,ceiling)
      (exp     ,exp)
      (floor   ,floor)
      (log     ,(lambda (x) (log (if (equal? 0 x) 0.0 x)))) ; if log0, log0.0
      (sqrt    ,sqrt)
   ;;   (dim     ,sbir-dim)
      (print   ,sbir-print)
      (let     ,sbir-let)
      (if      ,(void))
      (goto    ,sbir-goto)
   ;;  (input   ,sbir-input)
  ))

; A lable table that holds the addresses of each line
(define *label-table* (make-hash))

(define (label-get key)
   (hash-ref *label-table* key '()))

; A variable table that holds the value of all variables
(define *variable-table* (make-hash))

(define (variable-get key)
  (hash-ref *variable-table* key))

(for-each
  (lambda (pair)
          (hash-set! *variable-table* (car pair) (cadr pair)))  
  `(
 ;    (/       ,(lambda (x y) (/ x  (+ 0.0 y)))) ; if the program is trying to divide by 0, divide by 0.0
  ;    (log10   ,(lambda (x) (/ (log x) (log 10.0))))
   ;   (mod     ,(lambda (x y) (- x (* (div x y) y))))
    ;  (quot    ,(lambda (x y) (truncate (/ x y))))
    ;  (rem     ,(lambda (x y) (- x (* (quot x y) y))))
    ;  (+       ,+) ;(lambda (x y) (+ (+ x 0.0) (+ y 0.0))))
    ;  (-       ,-)
    ;  (*       ,*)
    ;  (^       ,expt)
    ;  (atan    ,atan)
    ;  (ceil    ,ceiling)
    ;  (exp     ,exp)
    ;  (floor   ,floor)
    ;  (log     ,(lambda (x) (log (if (equal? 0 x) 0.0 x)))) ; if log0, log0.0
    ;  (sqrt    ,sqrt)
      (e       2.718281828459045235360287471352662497757247093)
      (pi      3.141592653589793238462643383279502884197169399)
   ))

;; readlist-from-inputfile
;; read the in-file
(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

;; scan the list returned by (read) and fill the in the hash-label table
(define (scan-program-labels program)
  (map (lambda (line)
     (when (not (null? line)) ; when the line is not null
        (when (= 3 (length line)) ; if the length of the line is 3
           (hash-set! *label-table* (cadr line) (- (car line) 1)))
        (when (and (= 2 (length line)) (not (list? (cadr line))))
             (hash-set! *label-table* (cadr line) (- (car line) 1)))
             )) program)

 ;s;  (when (and (not (null? line)) (>= (length line) 2)      ; if line is not null and >= 2
    ; (not (null? (cadr line))) (symbol? (cadr line)))      ; if label exists
     ;   (hash-set! *label-table* (cadr line) (caddr line))))
 ; program)
)


;; write-program-by-line
(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (map (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n"))

(define (evaluate-line program linenum)
   (scan-program-labels program)

(when (> (length program) linenum)
   (let ((line (list-ref program linenum))) ;let line be the corresponding linenum
      (when (and (>= (length line) 2) (list? (cdr line)))
      (let ((funst (list-ref line (- (length line) 1))))
         (when (>= (length funst) 2)
            (let ((func (car funst))
                  (state (cdr funst)))
           
            (cond ((equal? func 'print) (sbir-print funst)) ; if the function is print, send the whole function + statement over to parse it individually there
                  ((equal? func 'let) (sbir-let state))
                  ((equal? func 'goto) (sbir-goto program state))
                  (else (die '("invalid expression!")))))))))
    (evaluate-line program (+ linenum 1)))     
)
;; sb-interpret
;; goes through the program and interprets each line
;(define (sb-interpret filename program)
   ; scan the program for labels and statements
;   (scan-program-labels program)
;   (map (lambda (line)
;      (when (>= (length line) 2)
;      (let ((funst (list-ref line (- (length line) 1)))) ; take the function + statement
;        (when (>= (length funst) 2)                      ; if the line is >= 2, parse. (skips lines without statements)
          ;(display (car line))         
;           (let ((func (car funst))                      ; get the function
;                 (state (cdr funst)))                   ; get the statement
;         (cond ((equal? func 'print) (sbir-print funst)) ; if the function is print, send the whole function + statement over to parse it individually there
;               ((equal? func 'let) (sbir-let state))
;               (display state)
;               ((equal? func 'goto) (sbir-goto state)) 
;               (else (die '("invalid expression")))))))))
;program)
;)

;; main
(define (main arglist)
    ; Check if number of args is 1
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        ; sbprogfile = filename
        (let* ((sbprogfile (car arglist))
              ; program = List of commands that were read from the inputfile
              (program (readlist-from-inputfile sbprogfile)))
             ; (sb-interpret sbprogfile program)))
              (begin (evaluate-line program 0))))
)

;; begin main
(main (vector->list (current-command-line-arguments)))
