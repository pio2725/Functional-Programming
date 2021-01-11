#lang plait

(print-only-errors #t)

; <CHECK-TEMPS1>

; 1. Represent temp as type of List of Number, and boolean #t or #f as output
; check-temps1
; 2a. For given list of numbers, we want to find if all measurements are between 5 and 95 degrees
; 2b. List of Number -> Boolean
;     use a helper function to recursively find out the numbers in the given list are between 5 and 95
; 4. template
; 5. implementation

; Helper function check-between
; (Listof Number) Number Number -> Boolean
; recursively checks if the element is between a and b both inclusive

(check-temps1 : ((Listof Number) -> Boolean))
(check-between : ((Listof Number) Number Number -> Boolean))

(define (check-between temps a b)
  (cond
    [(empty? temps) #t]
    [(cons? temps) (and (and (<= (first temps) b) (>= (first temps) a))
                        (check-between (rest temps) a b))]))

(define (check-temps1 temps)
  (cond
    [(empty? temps) #t]
    [(cons? temps) (check-between temps 5 95)]))

; 3. examples

(test (check-temps1 '()) #t)
(test (check-temps1 '(5 10 15 20 25 95)) #t)
(test (check-temps1 '(46 85 4 90 26)) #f)
(test (check-temps1 '(98 20 25 39)) #f)

; 6. implementation is consistent with examples


; <CHECK-TEMPS>

; 1. represent low and high temperatures as numbers, and given list as Listof Numbers, and result as Boolean
; check-temps
; 2a. we want to check if all measurements are between low and high degrees from the given list
; 2b. Listof Numbers Number Number -> Boolean
;     use the helper function check-between from the previous answer
; 4. template
; 5. implementation

(check-temps : ((Listof Number) Number Number -> Boolean))

(define (check-temps temps low high)
  (cond
    [(empty? temps) #t]
    [(cons? temps) (check-between temps low high)]))

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
;     if the last digit given is 0, then it ignores it

; 4. template
; 5. implementation

(convert : ((Listof Number) -> Number))

; Helper function conver-helper
; represent digits as list of numbers given, n-power and sum as numbers, and result is the total sum
; This helper function caculates the total sum by recursively call itself and add sums
; Since the first digit is the least significant, it starts multiplying by 1, 10, 100, etc.
; Listof Number Number Number -> Number

(convert-helper : ((Listof Number) Number Number -> Number))

(define (convert-helper digits n-power sum)
  (if (empty? digits)
      sum
      (convert-helper (rest digits) (* n-power 10) (+ sum (* (first digits) n-power)))))

(define (convert digits)
  (convert-helper digits 1 0))
  
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

; Helper function : average-helper
; represent prices as list of nubmers and total price as number
; average-helper caculates the total price from the list of prices
; Listof Number -> Number, numbers cannot be negative

(average-helper : ((Listof Number) -> Number))

(define (average-helper prices)
  (if (empty? prices)
      0
      (+ (first prices) (average-helper (rest prices)))))

(define (average-price prices)
  (/ (average-helper prices) (length prices)))

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

; 4. template
; 5. implementation

(convertFC : ((Listof Number) -> (Listof Number)))

; Helper function : fah-to-cel
; represent degree as number and converted degree in Celsius as number
; converts given Fahrenheit to Celsius
; Number -> Number

(fah-to-cel : (Number -> Number))
(define (fah-to-cel degree)
  (* (/ 5 9) (- degree 32.0)))

; Helper function : convertFC-helper
; represent fahrenheits as list of number and result as list of number
; convert given fahrenheits measurements to celsius by calculating and using cons recursively
; Listof Number -> Listof Number

(convertFC : ((Listof Number) -> (Listof Number)))
(define (convertFC-helper fahrenheits)
  (if (empty? fahrenheits)
      '()
      (cons (fah-to-cel (first fahrenheits)) (convertFC-helper (rest fahrenheits)))))


(define (convertFC fahrenheits)
  (if (empty? fahrenheits)
      '()
      (convertFC-helper fahrenheits)))

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
    [(> (first lop) ua) (eliminate-exp ua (rest lop))]
    [else (cons (first lop) (eliminate-exp ua (rest lop)))]))

; 3. example

(test (eliminate-exp 10 '()) '())
(test (eliminate-exp 50 '(50 79 100 20 30)) '(50 20 30))
(test (eliminate-exp 139 '(19 29 39 100 200 300 400 500 64)) '(19 29 39 100 64))


; 6. implementation is consistent with example


; <SUFFIXES>

; 1. Represent l as list of certain type, and list of list of certain type as result
; suffixes
; 2a. we want to produce a list of a suffixes of given list
; 2b. Listof 'a -> (Listof (Listof 'a))
;
; 4. template
; 5. implementation

(suffixes : ((Listof 'a) -> (Listof (Listof 'a))))

(define (suffixes l)
  (if (empty? l)
      (list (list))
      (append (list l) (suffixes (rest l)))))

; 3. example

(test (suffixes (list)) (list (list)))
(test (suffixes (list 'a 'b 'c 'd)) (list (list 'a 'b 'c 'd) (list 'b 'c 'd) (list 'c 'd) (list 'd) (list)))
(test (suffixes (list 1 2 3 4 5)) (list (list 1 2 3 4 5) (list 2 3 4 5) (list 3 4 5) (list 4 5) (list 5) (list)))

; 6. implementation is consistent with example


(define-type Pedigree
  (person [name : String] [birth-year : Number] [eye-color : Symbol] [father : Pedigree] [mother : Pedigree])
  (unknown))



; <COUNT-PERSONS>

; 1. represent pedigree as defined type Pedigree, and number of persons as number
; count-persons
; 2a. we want to know the number of known persons in a given pedigree
; 2b. Pedigree -> Number
;
; 4. template
; 5. implementation

(count-persons : (Pedigree -> Number))

; Helper function : count-helper
; represent person as defined type Pedigree, and number of known persons as number
; calcuate the known persons recursively
; Pedigree -> Number

(count-helper : (Pedigree -> Number))
(define (count-helper person)
  (cond
    [(not (unknown? person)) (+ 1 (+ (count-helper (person-father person)) (count-helper (person-mother person))))]
    [else 0]))

(define (count-persons pedigree)
  (type-case Pedigree pedigree
    [(unknown) 0]
    [(person n b e f m) (+ 1 (+ (count-helper f) (count-helper m)))]))

; 3. example

(define father1 (person "father1" 1964 'black (person "father2" 1938 'black (person "father4" 1870 'brown (unknown) (unknown)) (unknown)) (unknown)))
(define mother1 (person "mother1" 1963 'blue (person "father3" 1933 'blue (unknown) (unknown)) (person "mother2" 1930 'orange (unknown) (unknown))))
(define root (person "root person" 1994 'green father1 mother1))

(test (count-persons root) 7)
(test (count-persons (person "sample" 1111 'aa (unknown) (unknown))) 1)
(test (count-persons (unknown)) 0)

; 6. implementation is consistent with example


; <AVERAGE-AGE>

; 1. Represent pedigree as defined type Pedigree, and the average age as number
; average-age
; 2a. we want to calculate the average age of all the known persons in the pedigree
; 2b. Pedigree -> Number
;
; 4. template
; 5. implementation

(average-age : (Pedigree -> Number))

; Helper function : age-helper
; represent person as defined type Pedigree, and recursively add up known persons' age
; Pedigree -> Number

(age-helper : (Pedigree -> Number))
(define (age-helper person)
  (cond
    [(not (unknown? person)) (+ (- 2020 (person-birth-year person)) (+ (age-helper (person-father person)) (age-helper (person-mother person))))]
    [else 0.0]))
  

(define (average-age pedigree)
  (type-case Pedigree pedigree
    [(unknown) 0]
    [(person n b e f m) (/ (age-helper pedigree) (count-persons pedigree))]))

; 3. example

(test (average-age (unknown)) 0)
(test (average-age (person "Tom" 1994 'black (person "Tom's father" 1964 'black (unknown) (unknown)) (unknown))) 41)
(test (average-age root) 78.2857)

; 6. implementation is consistent with example


; <EYE-COLORS>

; 1. represent pedigree as defined type Pedigree, and list of all eye colors as list of symbol
; eye-colors
; 2a. we want to have a list of all eye colors in given pedigree
; 2b. Pedigree -> Listof Symbol
;     eye color can occur more than once in the list
;
; 4. template
; 5. implementation

(eye-colors : (Pedigree -> (Listof Symbol)))

; Helper function : eye-helper
; represent person as defined type Pedigree, and output as list of symbol
; this function recursively append all the eye colors in the given pedigree
; Pedigree -> Listof Symbol

(eye-helper : (Pedigree -> (Listof Symbol)))
(define (eye-helper person)
  (cond
    [(not (unknown? person)) (cons (person-eye-color person) (append (eye-helper (person-father person)) (eye-helper (person-mother person))))]
    [else '()]))
  

(define (eye-colors pedigree)
  (type-case Pedigree pedigree
    [(unknown) '()]
    [(person n b e f m) (cons e (append (eye-helper f) (eye-helper m)))]))

; 3. example

(test (eye-colors (unknown)) '())
(test (eye-colors (person "inoh" 1994 'black (unknown) (unknown))) '(black))
(test (eye-colors root) '(green black black brown blue blue orange))

; 6. implementation is consistent with example









































