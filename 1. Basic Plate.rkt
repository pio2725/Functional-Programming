#lang plait

;(print-only-errors #t)

;<SUM COINS>

;; Design Recipe
;; 1.  Represent types of coins as numbers and total value of coins as number
;; sum-coins
;; 2a. We want to compute the total value of a bag of coins in cents.
;; 2b. Number Number Number Number -> Number
;;     These numbers can't be negative because they represent coins

(sum-coins : (Number Number Number Number -> Number))

;; 4. Take inventory and make template
;; 5. Fill in the template with our implementation

(define (sum-coins pennies nickels dimes quarters)
  (let ([nickel-sum (* nickels 5)]
        [dime-sum (* dimes 10)]
        [quarter-sum (* quarters 15)])
    (+ pennies (+ nickel-sum (+ dime-sum quarter-sum)))))

;; 3. Write functional examples about the usage of your function

(test (sum-coins 4 8 7 7) 219)
(test (sum-coins 13 56 42 27) 1118)

;; 6. Make sure that implementation is consistent with examples


; <AREA-CYLINDER>

;; Design Recipe
;; 1. Represent types of base-radius and height as numbers and the surface area of a cylinder as number
;; area-cylinder
;; 2a. We want to compute the surface area of a cylinder
;; 2b. Number Number -> Number
;;     These can't be negative numbers

(area-cylinder : (Number Number -> Number))

;; 4. Take inventory and make template
;; 5. Fill in the template with our implementation

(define pi 3.1415926535897)
(define (area-cylinder base-radius height)
  (let ([area-rec (* 2 (* pi (* base-radius height)))]
        [area-cir (* 2 (* pi (* base-radius base-radius)))])
    (+ area-rec area-cir)))
  
;; 3. Write functional examples about the usage of your function

(test (area-cylinder 3 5) 150.8)
(test (area-cylinder 7.56 13.24) 988.02)

;; 6. Make sure that implementation is consistent with examples


; <AREA-PIPE 1>

;; Design Recipe
;; 1.  Represent types of inner-radius, wall-thickness, and length as numbers, and the surface area as number
;; area-pipe1
;; 2a. We want to compute the surface area of a pipe, which is an open cylinder
;; 2b. Number Number Number -> Number
;;     These can't be negative numbers
(area-pipe1 : (Number Number Number -> Number))
;; 4. Take inventory and make template
;; 5. Fill in the template with our implementation

(define (area-pipe1 inner-radius wall-thickness length)
  (let ([outer-radius (+ inner-radius wall-thickness)])
    (let ([outer-circle-area (* pi (* outer-radius outer-radius))]
          [inner-circle-area (* pi (* inner-radius inner-radius))]
          [outer-area (* 2 (* pi (* outer-radius length)))]
          [inner-area (* 2 (* pi (* inner-radius length)))])
      (+ inner-area (+ outer-area (* 2 (- outer-circle-area inner-circle-area)))))))
        
;; 3. Write functional examples about the usage of your function
(test (area-pipe1 11.5 1 20) 3166.72539)
(test (area-pipe1 9.5 3 15) 2488.14138)

;; 6. Make sure that implementation is consistent with examples        


; <AREA-PIPE 2>

;; Design Recipe
;; 1.  Represent types of inner-radius, wall-thickness, and length as numbers, and the surface area as number
;; area-pipe2
;; 2a. We want to compute the surface area of a pipe, which is an open cylinder using several helper functions
;; 2b. Number Number Number -> Number
;;     These can't be negative numbers

(area-pipe2 : (Number Number Number -> Number))

;; 4. Take inventory and make template
;; 5. Fill in the template with our implementation

; Helper 1
; Takes radius and length, then calculate the rectangular surface

(define (get-rec-surface radius length)
  (* 2 (* pi (* radius length))))

; Helper 2
; Takes outer radius and inner radius, then calculates the difference of the area of the two circles

(define (get-diff-circle-area outer-radius inner-radius)
  (let ([outer-area (* pi (* outer-radius outer-radius))]
        [inner-area (* pi (* inner-radius inner-radius))])
    (- outer-area inner-area)))

(define (area-pipe2 inner-radius wall-thickness length)
  (let ([outer-radius (+ inner-radius wall-thickness)])
    (+ (get-rec-surface outer-radius length) (+ (get-rec-surface inner-radius length) (* 2 (get-diff-circle-area outer-radius inner-radius))))))


;; 3. Write functional examples about the usage of your function

(test (area-pipe2 11.5 1 20) 3166.72539)
(test (area-pipe2 9.5 3 15) 2488.14138)

;; 6. Make sure that implementation is consistent with examples  


; <TAX>

;; Design Recipe
;; 1.  Represent types of gross pay as number, and tax amount would be as number
;; tax
;; 2a. We want to compute the amount of tax owed from the specified tax rates
;; 2b. Number -> Number
;;     These can't be negative numbers

(tax : (Number -> Number))

;; 4. Take inventory and make template
;; 5. Fill in the template with our implementation

(define (tax gross-pay)
  (cond
    [(<= gross-pay 240) 0]
    [(<= gross-pay 480) (* 0.15 (- gross-pay 240))]
    [else (+ (* 0.15 240) (* 0.28 (- gross-pay 480)))]))

;; 3. Write functional examples about the usage of your function

(test (tax 500) 41.6)
(test (tax 240) 0)
(test (tax 300) 9)

;; 6. Make sure that implementation is consistent with examples 


; <NET-PAY>

;; Design Recipe
;; 1.  Represent types of hours worked and hourly wage as numbers, and the net pay as number
;; net-pay
;; 2a. We want to compute the net pay of an employee, which is the gross pay minus tax
;; 2b. Number Number -> Number
;;     These can't be negative numbers
(net-pay : (Number Number -> Number))

;; 4. Take inventory and make template
;; 5. Fill in the template with our implementation

(define (net-pay hours-worked hourly-wage)
  (let ([gross-pay (* hours-worked hourly-wage)])
    (- gross-pay (tax gross-pay))))

;; 3. Write functional examples about the usage of your function

(test (net-pay 30 10) 291)
(test (net-pay 110 15) 1286.4) 

;; 6. Make sure that implementation is consistent with examples 


; <WHAT-KIND>

;; Design Recipe
;; 1.  Represent types of a, b, and c as numbers, and representing how many solotions an equation has as symbol
;; what-kind
;; 2a. We want to find out if an equation is degenerate, and how many solutions the equation has if not
;; 2b. Number Number Number -> symbol

(what-kind : (Number Number Number -> Symbol))

;; 4. Take inventory and make template
;; 5. Fill in the template with our implementation

(define (what-kind a b c)
  (let ([discriminant (- (* b b) (* 4 (* a c)))])
    (cond
      [(= a 0) 'degenerate]
      [(< discriminant 0) 'none]
      [(= discriminant 0) 'one]
      [else 'two])))
    
;; 3. Write functional examples about the usage of your function

(test (what-kind 0 5 3) 'degenerate)
(test (what-kind 1 2 1) 'one)
(test (what-kind -4 4 8) 'two)
(test (what-kind 1 -3 3) 'none)

;; 6. Make sure that implementation is consistent with examples 


; <TIME-DIFF>

;; Design Recipe
;; 1.  Represent types of t1 and t2 as defined Time, and the difference of time as number
;; time-diff
;; 2a. We want to compute the number of seconds from t1 to t2
;; 2b. Time Time -> Number
;;     Numbers in Time and the difference can't be negative
;;     t2 should be after than t1 in time

(time-diff : (Time Time -> Number))

(define-type Time
  (hms [hours : Number] [minutes : Number] [seconds : Number]))

;; 4. Take inventory and make template
;; 5. Fill in the template with our implementation

(define (time-diff t1 t2)
  (let ([t2-in-seconds (+ (hms-seconds t2) (+ (* (hms-hours t2) 3600) (* (hms-minutes t2) 60)))]
        [t1-in-seconds (+ (hms-seconds t1) (+ (* (hms-hours t1) 3600) (* (hms-minutes t1) 60)))])
    (- t2-in-seconds t1-in-seconds)))
  
;; 3. Write functional examples about the usage of your function

(define sample1 (hms 0 50 0))
(define sample2 (hms 0 55 0))
(test (time-diff sample1 sample2) 300)

(define sample3 (hms 3 21 30))
(define sample4 (hms 5 31 44))
(test (time-diff sample3 sample4) 7814)

;; 6. Make sure that implementation is consistent with examples 


; <AREA>

;; Design Recipe
;; 1.  Represent types of shape as defined type Shape, and the area of shape as number
;; area
;; 2a.  We want to compute the area of given shape
;; 2b.  Shape -> Number

(area : (Shape -> Number))

(define-type Position
  (position [x : Number] [y : Number]))

(define-type Shape
  (circle [center : Position] [radius : Number])
  (square [upper-left : Position] [side-length : Number])
  (rectangle [upper-left : Position] [width : Number] [length : Number]))

;; 4. Take inventory and make template
;; 5. Fill in the template with our implementation

(define (area shape)
  (cond
    [(circle? shape) (* pi (* (circle-radius shape) (circle-radius shape)))]
    [(square? shape) (* (square-side-length shape) (square-side-length shape))]
    [(rectangle? shape) (* (rectangle-width shape) (rectangle-length shape))]))

;; 3. Write functional examples about the usage of your function

(define p1 (position 0 0))

(define c1 (circle p1 10))
(test (area c1) 314.1592)

(define s1 (square p1 10))
(test (area s1) 100)

(define r1 (rectangle p1 3 5))
(test (area r1) 15)

;; 6. Make sure that implementation is consistent with examples 



; <TRANSLATE-SHAPE>

;; Design Recipe
;; 1.  Represent types of shape as defined type Shape and delta as number, and the resulting shape as type Shape
;; translate-shape
;; 2a. We want to move the position by delta in the x direction
;; 2b. Shape Number -> Shape
(translate-shape : (Shape Number -> Shape))

;; 4. Take inventory and make template
;; 5. Fill in the template with our implementation

(define (translate-shape shape delta)
  (cond
    [(circle? shape) (circle (position (+ delta (position-x (circle-center shape))) (position-y (circle-center shape))) (circle-radius shape))]
    [(square? shape) (square (position (+ delta (position-x (square-upper-left shape))) (position-y (square-upper-left shape))) (square-side-length shape))]
    [else         (rectangle (position (+ delta (position-x (rectangle-upper-left shape))) (position-y (rectangle-upper-left shape))) (rectangle-width shape) (rectangle-length shape))]))


;; 3. Write functional examples about the usage of your function

(define pos1 (position 1 3))
(define result1 (circle (position 4 3) 5))
(test (translate-shape (circle pos1 5) 3) result1)

(define pos2 (position -5 7))
(define result2 (square (position 0 7) 10))
(test (translate-shape (square pos2 10) 5) result2)

(define pos3 (position 3 6))
(define result3 (rectangle (position 1 6) 10 10))
(test (translate-shape (rectangle pos3 10 10 ) -2) result3)

;; 6. Make sure that implementation is consistent with examples 


; <IN-SHAPE?>

;; Design Recipe
;; 1.  Represent types of shape as defined type Shape and p as defined type Position, and boolean as a result
;; in-shape?
;; 2a. We want to know if a given point is within the shape (not on the line)
;; 2b. Shape Position -> Boolean

(in-shape? : (Shape Position -> Boolean))

;; 4. Take inventory and make template
;; 5. Fill in the template with our implementation

; Helper 1. Find out if a point is within the given circle (d^2 < r^2 then the point is in the circle)
(define (in-circle? shape p)
  (let ([r-squared (* (circle-radius shape) (circle-radius shape))]
        [xdist-squared (* (- (position-x (circle-center shape)) (position-x p)) (- (position-x (circle-center shape)) (position-x p)))]
        [ydist-squared (* (- (position-y (circle-center shape)) (position-y p)) (- (position-y (circle-center shape)) (position-y p)))])
    (if (< (+ xdist-squared ydist-squared) r-squared)
        #t
        #f)))

; Helper 2. Find out if a point is within the given square
(define (in-square? shape p)
  (let ([oppos (position (+ (square-side-length shape) (position-x (square-upper-left shape))) (- (position-y (square-upper-left shape)) (square-side-length shape)))])
    (cond
      [(and (> (position-x p) (position-x (square-upper-left shape)))
            (< (position-x p) (position-x oppos))
            (> (position-y p) (position-y oppos))
            (< (position-y p) (position-y (square-upper-left shape)))) #t]
      [else #f])))

; Helper 3. Find out if a point is within the given rectangle
(define (in-rectangle? shape p)
  (let ([oppos (position (+ (position-x (rectangle-upper-left shape)) (rectangle-width shape)) (- (position-y (rectangle-upper-left shape)) (rectangle-length shape)))])
    (cond
      [(and (> (position-x p) (position-x (rectangle-upper-left shape)))
            (< (position-x p) (position-x oppos))
            (> (position-y p) (position-y oppos))
            (< (position-y p) (position-y (rectangle-upper-left shape)))) #t]
      [else #f])))
    
   
(define (in-shape? shape p)
  (cond
    [(circle? shape) (in-circle? shape p)]
    [(square? shape) (in-square? shape p)]
    [else (in-rectangle? shape p)]))
    

;; 3. Write functional examples about the usage of your function

(test (in-shape? (circle (position 0 0) 5) (position -3 2)) #t)
(test (in-shape? (square (position -2 2) 4) (position 3 0)) #f)
(test (in-shape? (rectangle (position -1 1) 3 5) (position -0.5 2)) #f)

;; 6. Make sure that implementation is consistent with examples



