#lang plait

(print-only-errors #t)

; <CHECK-TEMPS1>

; 1. Represent temp as type of List of Number, and boolean #t or #f as output
; check-temps1
; 2a. For given list of numbers, we want to find if all measurements are between 5 and 95 degrees
; 2b. List of Number -> Boolean
;
; 4. template
; 5. implementation

(check-temps1 : ((Listof Number) -> Boolean))
(check-helper : (Number -> Boolean))

; Helper function : check-helper
; represent x as number and #true or #false as boolean
; Number -> Boolean
; find out if a given number is between 5 and 95

(define (check-helper x)
  (if (and (>= x 5) (<= x 95))
      #t
      #f))

(define (check-temps1 temps)
  (cond
    [(empty? temps) #t]
    [(= (length temps) (length (filter check-helper temps))) #t]
    [else #f]))

; 3. examples

(test (check-temps1 '()) #t)
(test (check-temps1 '(5 10 15 20 25 95)) #t)
(test (check-temps1 '(46 85 4 90 26)) #f)
(test (check-temps1 '(98 20 25 39)) #f)

; 6. implementation is consistent with example


; <CHECK-TEMPS>

; 1. represent low and high temperatures as numbers, and given list as Listof Numbers, and result as Boolean
; check-temps
; 2a. we want to check if all measurements are between low and high degrees from the given list
; 2b. Listof Numbers Number Number -> Boolean
;
; 4. template
; 5. implementation

; Helper Function : check-between
; represent temp, low, and high as numbers, and true as 1 and false as 0 (number)
; find out if given temperature is between low and high
; Number Number Number -> Number

(check-between : (Number Number Number -> Number))
(check-temps : ((Listof Number) Number Number -> Boolean))

(define (check-between temp low high)
  (if (and (>= temp low) (<= temp high))
      1
      0))

(define (check-temps temps low high)
  (cond
    [(empty? temps) #t]
    [(= (length temps) (length (foldr (lambda (n r) (if (= 1 (check-between n low high))
                                                     (cons n r)
                                                     empty)) empty temps))) #t]
    [else #f]))

; 3. examples

(test (check-temps '() 1 5) #t)
(test (check-temps '(1 3 5 7 9 11) 1 15) #t)
(test (check-temps '(-30 -24 -6) -50 0) #t)
(test (check-temps '(0 49 89 23 12 42 59 23 84 29) 1 87) #f)

; 6. implementation is consistent with examples


; <CONVERT>

; 1. Represent digits as list of numbers, and the result as number
; convert
; 2a. We want to convert the list of number to the number that the first digit is the least significant, and so on.
; 2b. Listof Number -> Number
;     numbers are between 0 and 9
;     if given list is empty, then returns 0

; 4. template
; 5. implementation

(convert : ((Listof Number) -> Number))

(define (convert digits)
  (if (empty? digits)
      0
      (+ (first digits) (convert (map (lambda (x) (* 10 x)) (rest digits))))))
  
; 3. examples

(test (convert '()) 0)
(test (convert '(1 2 3)) 321)
(test (convert '(5 1 3 2 5)) 52315)

; 6. implementation is consistent with example


; <AVERAGE-PRICE>

; 1. Represent prices as list of number and the average price as number
; average-price
; 2a. We want to calculate the average price from the list of prices given
; 2b. Listof Number -> Number
;     prices can't be negative number
;     the length of the given list cannot be 0

; 4. template
; 5. implementation

(average-price : ((Listof Number) -> Number))

(define (average-price prices)
  (/ (foldl + 0 prices) (length prices)))

; 3. examples

(test (average-price '(1 2 3)) 2)
(test (average-price '(3.2 5.4 13.8 9.9)) 8.075)

; 6. implementation is consistent with example


; <CONVERTFC>

; 1. Represent list of fahrenheits as list of number and a list of Celsius measurement as a list of number
; convertFC
; 2a. We want to convert Fahrenheit measurements to a list of Celsius measurements
; 2b. Listof Number -> Listof Number
;     if a given list is empty, result is empty list
;
; 4. template
; 5. implementation

(convertFC : ((Listof Number) -> (Listof Number)))

; Helper function : fah-to-cel
; represent degree as number, and it converts fahrenheit to celsius
; Number -> Number

(fah-to-cel : (Number -> Number))
(define (fah-to-cel degree)
  (* (/ 5 9) (- degree 32.0)))

(define (convertFC fahrenheits)
  (if (empty? fahrenheits)
      '()
      (map fah-to-cel fahrenheits)))

; 3. example

(test (convertFC '()) '())
(test (convertFC '(1 0 75 100)) '(-17.22222222222222 -17.77777777777778 23.88888888888889 37.77777777777778))
(test (convertFC '(-30 32)) '(-34.44444444444444 0.0))

; 6. implementation is consistent with example



; <ELIMINATE_EXP>

; 1. Represent the price ua as number and lop as list of number, and result as list of number
; eliminate-exp
; 2a. we want a list of prices which every price is greater than ua price
; 2b. Number Listof Number -> Listof Number
;     numbers can't be negative
;     eliminates prices not including ua
;
; 4. template
; 5. implementation

(eliminate-exp : (Number (Listof Number) -> (Listof Number)))

(define (eliminate-exp ua lop)
  (cond
    [(empty? lop) '()]
    [else (filter (lambda (x) (if (> x ua)
                                  #f
                                  #t)) lop)]))
; 3. example

(test (eliminate-exp 10 '()) '())
(test (eliminate-exp 50 '(50 79 100 20 30)) '(50 20 30))
(test (eliminate-exp 139 '(19 29 39 100 200 300 400 500 64)) '(19 29 39 100 64))

; 6. implementation is consistent with example



; <COMPOSE-FUNC>

; 1. represent before as a function that takes 'a to 'b, and after as a function that takes 'b to 'c as output
; compose-func
; 2a. we want the composition of functions before and after
; 2b. 'a -> 'b
;
; 4. template
; 5. implementation

(compose-func : (('b -> 'c) ('a -> 'b) -> ('a -> 'c)))

(define (compose-func after before)
  (lambda (x) (after(before x))))

; 3. example
(define (sample s)
  (if (= s 1)
      #t
      #f))

(test ((compose-func add1 sub1) 0) 0)
(test ((compose-func sample add1) 0) #t)

; 6. implementation is consistent with example



; <FLATTEN>

; 1. represent loloa as listof listof 'a type, and output as listof 'a
; flatten
; 2a. we want to produce a list of all the 'as in the elements of loloa
; 2b. (Listof (Listof 'a)) -> Listof 'a
;
; 4. template
; 5. implementation

(flatten : ((Listof (Listof 'a)) -> (Listof 'a)))

(define (flatten loloa)
  (cond
    [(empty? loloa) '()]
    [else (append (first loloa) (flatten (rest loloa)))]))


; 3. example

(test (flatten '()) '())
(test (flatten (list '())) '())
(test (flatten (list (list 1 2) (list 3 4 5) (list 6))) (list 1 2 3 4 5 6))
(test (flatten (list (list "one") (list "two" "three") (list "four" "five" "six"))) (list "one" "two" "three" "four" "five" "six")) 

; 6. implementation is consistent with example



; <FLATTEN-FOLDR>

; 1. represent loloa as listof listof 'a type, and output as listof 'a
; flatten-foldr
; 2a. we want to produce a list of all the 'as in the elements of loloa
; 2b. (Listof (Listof 'a)) -> Listof 'a
;
; 4. template
; 5. implementation

(flatten-foldr : ((Listof (Listof 'a)) -> (Listof 'a)))

(define (flatten-foldr loloa)
  (foldr (lambda (n r) (append n r)) empty loloa))

; 3. example

(test (flatten-foldr '()) '())
(test (flatten-foldr (list '())) '())
(test (flatten-foldr (list (list 1 2) (list 3 4 5) (list 6))) (list 1 2 3 4 5 6))
(test (flatten-foldr (list (list "one") (list "two" "three") (list "four" "five" "six"))) (list "one" "two" "three" "four" "five" "six")) 

; 6. implementation is consistent with example


; <BUCKET>

; 1. represent lon as list of number and output as list of list of number
; bucket
; 2a. we want to find the list of sublists of adjacent equal numbers
; 2b. Listof Number -> Listof (Listof Number)
;
; 4. template
; 5. implementation

(bucket : ((Listof Number) -> (Listof (Listof Number))))

(define (bucket lon)
  (foldr (lambda (n r) (cond
                         [(empty? r) (list (list n))]
                         [(= n (first (first r))) (cons (cons n (first r)) (rest r))]
                         [else (cons (list n) r)])) empty lon))

; 3. example

(test (bucket '(1 1 2 2 2 3 1 1 1 2 3 3)) (list (list 1 1) (list 2 2 2) (list 3) (list 1 1 1) (list 2) (list 3 3)))
(test (bucket '(0 1 1 0 1 2 2 0)) '((0) (1 1) (0) (1) (2 2) (0)))

; 6. implementation is consistent with example


; <TREE-MAP>

; 1. represent f as function from string to string, and pedigree as defined type Pedigree, and output as also Pedigree
; tree-map
; 2a. we want to apply every person's name in given pedigree to the function
; 2b. ((string -> string) Pedigree) -> pedigree
;
; 4. template
; 5. implementation

(tree-map : ((String -> String) Pedigree -> Pedigree))
(define-type Pedigree
  (person [name : String] [birth-year : Number] [eye-color : Symbol] [father : Pedigree] [mother : Pedigree])
  (unknown))

(define father1 (person "father1" 1964 'black (person "father2" 1938 'black (person "father4" 1870 'brown (unknown) (unknown)) (unknown)) (unknown)))
(define mother1 (person "mother1" 1963 'blue (person "father3" 1933 'blue (unknown) (unknown)) (person "mother2" 1930 'orange (unknown) (unknown))))
(define root (person "root person" 1994 'green father1 mother1))


; Helper function : sub-str
; exists to test the function tree-map. Represent x as string and substring as string
; string -> string

(sub-str : (String -> String))
(define (sub-str x)
  (substring x 1 3 ))

(define (tree-map f pedigree)
  (type-case Pedigree pedigree
    [(unknown) (unknown)]
    [(person n b e fa ma) (person (f n) b e (tree-map f fa) (tree-map f ma))]))

; 3. example

(define inoh (person "inoh" 1994 'black (unknown) (unknown)))

(test (tree-map sub-str (unknown)) (unknown))
(test (tree-map sub-str inoh) (person "no" 1994 'black (unknown) (unknown)))

; 6. implementation is consistent with example



; <ADD-LAST-NAME>

; 1. Represent pedigree as defined type Pedigree and last-name as string, and output is defined type Pedigree
; add-last-name
; 2a. We want to append given last name to every person's name in given pedigree
; 2b. Pedigree String -> Pedigree
;     append given last name after a space
;     ex)name : "James", last-name: "Pak" --> "James Pak"
;
; 4. template
; 5. implementation

(add-last-name : (Pedigree String -> Pedigree))

(define (add-last-name pedigree last-name)
  (tree-map (lambda (x) (string-append x (string-append " " last-name)))  pedigree))

; 3. example

(define inchul (person "inchul" 1994 'black (person "sungho" 1964 'black (unknown) (unknown)) (unknown)))

(define rf1 (person "father1 Pak" 1964 'black (person "father2 Pak" 1938 'black (person "father4 Pak" 1870 'brown (unknown) (unknown)) (unknown)) (unknown)))
(define rm1 (person "mother1 Pak" 1963 'blue (person "father3 Pak" 1933 'blue (unknown) (unknown)) (person "mother2 Pak" 1930 'orange (unknown) (unknown))))
(define r (person "root person Pak" 1994 'green rf1 rm1))

(test (add-last-name (unknown) "hello") (unknown))
(test (add-last-name inchul "Lee") (person "inchul Lee" 1994 'black (person "sungho Lee" 1964 'black (unknown) (unknown)) (unknown)))
(test (add-last-name root "Pak") r)

; 6. implementation is consistent with example




















