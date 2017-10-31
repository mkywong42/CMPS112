#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.3 2016-09-23 18:23:20-07 - - $
;;
;; Matthew Lo, mamlo@ucsc.edu
;; 1464155
;; CMPS 112
;; Spring 2017
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

  ;(display (evalexpr (cdr expr)))
   (cond ((number? expr) expr)
         ((symbol? expr) (hash-ref *variable-table* expr #f))
         ((pair? expr)   (apply (hash-ref *function-table* (car expr))
                         (map evalexpr (cdr expr))))
         (else #f))
)

;; define the statements
;; dim
;; creates an array given by the variable name 
;; and inserts it into the symbol table
(define (sbir-dim arrayarg)
   (hash-set! *variable-table* (caar arrayarg)
      (make-vector (evalexpr (cadar arrayarg))))
   ;; create a vector (caar arrayarg)  of appropriate size
   ;; need a way to access the values in the array
   ;(display (variable-get (caar arrayarg)))
   (hash-set! *function-table* (caar arrayarg)
      (lambda (x) (vector-ref (hash-ref *variable-table*
      (caar arrayarg)) (- x 1)))) ;; get value of x-1
)

(define (sbir-if program  ugh)
   ;  (display (car ugh))
      (if (evalexpr (car ugh)) ; if the condition is satisfied
      (sbir-goto program (cdr ugh))            ; goto the program 
      (evaluate-line program (+ linenum 1))) ; execute the next line
)

;; let
;; makes an assignment to a variable. for a variable, 
;; store its value into symbol table. 
;; for an array, the store message is 
;; sent to the vector representing the array
(define (sbir-let arg)
    (let ((result (evalexpr (cadr arg))))
      (if (pair? (car arg)); if the car argument is a pair (array) 
         ; then vector-set (a i-1) 
         ; [because the array starts at 0] to the value
         ; (vector-set! a (i-1) result)
         (vector-set! (hash-ref *variable-table* 
         (caar arg)) (- (evalexpr (cadar arg)) 1) result)
     
        ;else just set normally
         
        (hash-set! *variable-table* (car arg) result)))
  
)

;; goto
;; control transfers to the statement referred to by Label
(define (sbir-goto program label)
   (evaluate-line program (label-get (car label)))
)

(define (sbir-print element)
   (when(not (null? element)) ; when element is not null,
    ; for some reason it is hard to use car cdr
    ; to separate it, so I played around with it.
      (when (and (= (length element) 5) (string? (cadr element)))
         (display (cadr element))                                 ; fib(
         (display (evalexpr (caddr element)))                     ; i
         (display (cadddr element))                               ; )=
         (display (evalexpr (first (cddddr element))))            
         (newline)
       )
      
      (when (and (= (length element) 5)
      (symbol? (cadr element))) ; this case is for big-o file
         (display (evalexpr (cadr element)))   ; N1
         (display (caddr element))             ; loops
         (display (evalexpr (cadddr element))) ; i
         (display (first (cddddr element)))    ; times.
         (newline)
      )  

      (when (pair? (cadr element))  ; for printing array values
         (display (evalexpr (cadr element)))
         (newline)
      )  
      
      ; if printing a number
      (when (and (= (length element) 2) (number? (cadr element)))
         (display (cadr element)) ; print out the number
         (newline)                ; new line
      )

      (when (and (or (= (length element) 3) (= (length element) 2)) 
            (string? (cadr element))) ; if printing a string
         (display (cadr element)) ; print the string out
         ; but if theres length 3 (an expression)
         (when (= (length element) 3)
            ; evaluate the expression and display
            (display (evalexpr (caddr element))))
      
      (newline) ; no matter what, new line
      ) 
      (when (and (= (length element) 2) 
             (symbol? (cadr element))) ; when printing out a variable
         ((lambda (expr) (display (evalexpr expr))) 
          (cadr element)) ; find value of variable and print
         (newline)
      )
)
)
(define (sbir-input invar)
   (hash-set! *variable-table* 'inputcount 0)

   (when (not (null? (car invar)))
      (hash-set! *variable-table* (car invar) (void))
    ;(display (hash-ref *variable-table* 'inputcount))
      (let ((obj (read))) ; read in and store in object
         (cond ((eof-object? obj) 
         ; if end of file, set input count to -1
         (hash-set! *variable-table* 'inputcount -1))
         ((number? obj) 
         ; if obj is a number, set it to the input variable
         (hash-set! *variable-table* (car invar) obj)
         ; and add 1 to inputcount
         (hash-set! *variable-table* 'inputcount 
         (+ (hash-ref *variable-table* 'inputcount) 1)))       
         (else (display "enter a number!")))))
)

;; Create the symbol tables

; A function table which holds all the functions
(define *function-table* (make-hash))

(for-each
  (lambda (pair)
          (hash-set! *function-table* (car pair) (cadr pair)))
  `(
      (/       ,(lambda (x y) (/ x  (+ 0.0 y)))) 
      ;always add 0.0 before dividing
      (log10   ,(lambda (x) (/ (log x) (log 10.0))))
      (mod     ,(lambda (x y) (- x (* (div x y) y))))
      (quot    ,(lambda (x y) (truncate (/ x y))))
      (rem     ,(lambda (x y) (- x (* (quot x y) y))))
      (+       ,+) ;(lambda (x y) (+ (+ x 0.0) (+ y 0.0))))
      (-       ,-)
      (*       ,*)
      (>=      ,>=)
      (=       ,=)
      (<=      ,<=)
      (<       ,<)
      (>       ,>)
      (<>      ,(lambda(x y) (not (equal? (+ x 0.0) (+ y 0.0)))))
      (^       ,expt)
      (atan    ,atan)
      (sin     ,sin)
      (cos     ,cos)
      (tan     ,tan)
      (acos    ,acos)
      (asin    ,asin)
      (abs     ,abs)
      (round   ,round)
      (ceil    ,ceiling)
      (exp     ,exp)
      (floor   ,floor)
      (log     ,(lambda (x) (log (if (equal? 0 x) 0.0 x))))
       ; if log0, log0.0
      (sqrt    ,sqrt)
      (dim     ,sbir-dim)
      (print   ,sbir-print)
      (let     ,sbir-let)
      (if      ,(void))
      (goto    ,sbir-goto)
      (input   ,sbir-input)
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
           ; hash-set to the label table
           (hash-set! *label-table* (cadr line) (- (car line) 1)))
; if there is only one element and it isn't a list, then it is a label
        (when (and (= 2 (length line)) (not (list? (cadr line)))) 
; hash-set it to the label table
             (hash-set! *label-table* (cadr line) (- (car line) 1)))
             )) program)
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
;   (display linenum)
;    (newline)
   (scan-program-labels program)

(when (> (length program) linenum)
   (let ((line (list-ref program linenum)))
   ;let line be the corresponding linenum
 
    (when (not (null? (cdr line)))
       (when (and (not (null? (cadr line))) 
                  (>= (length line) 2) 
                  (list? (cdr line)))
      (let ((funst (list-ref line (- (length line) 1))))
         (when (and (not (null? (cadr line))) (>= (length funst) 2))
            (let ((func (car funst))
                  (state (cdr funst)))
                 
            (cond ((equal? func 'print) (sbir-print funst)
                  ; if the function is print, send the whole funst
                   (evaluate-line program (+ linenum 1)))
                                        
                  ((equal? func 'let) (sbir-let state)
                   (evaluate-line program (+ linenum 1)))
                                        
                  ((equal? func 'goto) (sbir-goto program state)) 
                  ; dont evaluate next line if goto

                  ((equal? func 'if) (if (evalexpr (car state)) 
                     ; evaluate to see if the condition is true
                     ; goto the program
                   (sbir-goto program (cdr state))
                     ; else
                   (evaluate-line program (+ linenum 1))))
                                    
                  ((equal? func 'dim) (sbir-dim state)
                   (evaluate-line program (+ linenum 1)))
                  
                  ((equal? func 'input) (sbir-input state)
                   (evaluate-line program (+ linenum 1)))
 
                  (else (evaluate-line program (+ linenum 1))))))
 ; (display (length funst))
 ; (newline)
  (when (= (length funst) 1)
     (evaluate-line program (+ linenum 1)))
                )))
      
  (when (null? (cdr line))
     (evaluate-line program (+ linenum 1)))))
)

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
    (begin (evaluate-line program 0)))) ; start evaluating at line 0
)

;; begin main
(main (vector->list (current-command-line-arguments)))
