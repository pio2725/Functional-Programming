#lang plait

(require (typed-in "backend.rkt" [read-number : ((Listof String) -> Number)]))
(require (typed-in "backend.rkt" [display-result : ((Listof String) -> 'a)]))
(require (typed-in "backend.rkt" [read-user-input : (String -> String)]))


; typing invalid input will not allow to retype

(define-type Item
  (item [question : String] [response : String]))

(define q1 "Q1. Which Star Wars trilogy is the best?  ")
(define q2 "Q2. I'm amazed (and delighted!) at your choice. What about it bothers you the least?  ")
(define q3 "Q3. Who arc do you find more satisfying?  ")
(define q4 "Q4. What was the root cause of Anakin's downfall?  ")
(define q5 "Q5. Is Luke defined more by his hopefulness or his loyalty?  ")
(define q6 "Q6. What in-universe explanation for Luke's loss of hope in the sequel trilogy is most plausible?  ")
(define q7 "Q7. To whom did Luke owe the most loyalty?  ")

(define (question2 result)
  (begin
    ;(display "I'm amazed (and delighted!) at your choice. What about it bothers you the least?\n 1.the words the actors say\n 2.how they say them\n 3.something else\n")
    ;(read-number "I'm amazed (and delighted!) at your choice. What about it bothers you the least?\n 1.the words the actors say\n 2.how they say them\n 3.something else\n")
    (let ([input (read-number (list "Q2. I'm amazed (and delighted!) at your choice. What about it bothers you the least?" "1. the words the actors say" "2. how they say them" "3. something else"))])
      [cond
        [(= input 1) (let ([result (cons (string-append q2 "Answer -> the words the actors say ") result)]) (question3 result))];(set-box! a2 "the words the actors say")]
        [(= input 2) (let ([result (cons (string-append q2 "Answer -> how they say them ") result)]) (question3 result))];(set-box! a2 "how they say them")]
        [(= input 3) (let ([result (cons (string-append q2 "Answer -> something else ") result)]) (question3 result))]
        [else (error 'survey "Invalid input. Choose between 1-3")]])))


                                  
(define (question3 result)
  (begin
    ;(display "Who arc do you find more satisfying?\n 1.Anakin Skywalker\n 2.Luke Skywalker\n")
    (let ([input (read-number (list "Q3. Who arc do you find more satisfying?" "1. Anakin Skywalker" "2. Luke Skywalker"))])
      ;(if (s-exp-number? input)
      [cond
        [(= input 1) (let ([result (cons (string-append q3 "Answer -> Anakin Skywalker ") result)]) (question4 result))];(begin (set-box! a3 "Anakin Skywalker") (question4))]
        [(= input 2) (let ([result (cons (string-append q3 "Answer -> Luke Skywalker ") result)]) (question5 result))];(begin (set-box! a3 "Luke Skywalker") (question5))]
        [else (error 'survey "Invalid input. Choose between 1-2")]])))
            
            

(define (question4 result)
  (begin
    ;(display "What was the root cause of Anakin's downfall?\n 1.his abilities\n 2.his pride in his abilities\n 3.his relationship with Padme\n 4.his attachment to temporary things\n 5.others\n")
    (let ([input (read-number (list "Q4. What was the root cause of Anakin's downfall?" "1. his abilities" "2. his pride in his abilities" "3. his relationship with Padme" "4. his attachment to temporary things" "5. others"))])
      ;(if (s-exp-number? input)
            (cond
            [(= input 1) (let ([result (cons (string-append q4 "Answer -> his abilities ") result)]) (ending result))]
            [(= input 2) (let ([result (cons (string-append q4 "Answer -> his pride in his abilities ") result)]) (ending result))]
            [(= input 3) (let ([result (cons (string-append q4 "Answer -> his relationship with Padme ") result)]) (ending result))]
            [(= input 4) (let ([result (cons (string-append q4 "Answer -> his attachment to temporary things ") result)]) (ending result))]
            [(= input 5) (begin
                                           ;(display "5. other(quoted string) : ")
                                           (let ([user-input (read-user-input "5. other: ")])
                                             ;(if (s-exp-string? user-input)
                                                 (let ([result (cons (string-append q4 (string-append "Answer -> " user-input)) result)]) (ending result))))]
            [else (error 'survey "Invalid input. Choose between 1-5")]))))

; Luke Skywalker from Question3
(define (question5 result)
  (begin
    ;(display "Is Luke defined more by his hopefulness or his loyalty?\n 1.his hopefulness\n 2.his loyalty\n")
    (let ([input (read-number (list "Q5. Is Luke defined more by his hopefulness or his loyalty?" "1. his hopefulness" "2. his loyalty"))])
      ;(if (s-exp-number? input)
      [cond
        [(= input 1) (let ([result (cons (string-append q5 "Answer -> his hopefulness ") result)]) (question6 result))];(begin (set-box! a5 "his hopefulness") (question6))]
        [(= input 2) (let ([result (cons (string-append q5 "Answer -> his loyalty ") result)]) (question7 result))];(begin (set-box! a5 "his loyalty") (question7))]
        [else (error 'survey "Invalid input. Choose between 1-2")]])))

; from his hopefulness from question5
(define (question6 result)
  (begin
    ;(display "What in-universe explanation for Luke's loss of hope in the sequel trilogy is most plausible?\n 1.the pressure of building a new Jedi Order\n 2.unresolved fears about the Dark Side\n 3.disillusionment with the history of the Jedi Order\n 4.new knowledge about midichlorians\n 5.other\n")
    (let ([input (read-number (list "Q6. What in-universe explanation for Luke's loss of hope in the sequel trilogy is most plausible?" "1. the pressure of building a new Jedi Order" "2. unresolved fears about the Dark Side" "3. disillusionment with the history of the Jedi Order" "4. new knowledge about midichlorians" "5. other"))])
      ;(if (s-exp-number? input)
            [cond
              [(= input 1) (let ([result (cons (string-append q6 "Answer -> the pressure of building a new Jedi Order ") result)]) (ending result))];(set-box! a6 "the pressure of building a new Jedi Order")]
              [(= input 2) (let ([result (cons (string-append q6 "Answer -> unresolved fears about the Dark Side ") result)]) (ending result))];(set-box! a6 "unresolved fears about the Dark Side")]
              [(= input 3) (let ([result (cons (string-append q6 "Answer -> disillusionment with the history of the Jedi Order ") result)]) (ending result))];(set-box! a6 "disillusionment with the history of the Jedi Order")]
              [(= input 4) (let ([result (cons (string-append q6 "Answer -> new knowledge about midichlorians ") result)]) (ending result))];(set-box! a6 "new knowledge about midichlorians")]
              [(= input 5) (begin
                                             ;(display "5.other(quoted string): "))]
                             (let ([user-input (read-user-input "5. other: ")])
                               (let ([result (cons (string-append q6 (string-append "Answer ->" user-input)) result)]) (ending result))))]
              [else (error 'survey "Invalid input. Choose between 1-5")]])))
                                             ;(let ([user-input (read)])
                                               ;(if (s-exp-string? user-input)
                                                   ;(let ([result (cons (item q6 (s-exp->string user-input)) result)]) (ending result));(set-box! a6 (s-exp->string user-input))
                                                   ;(error 'survey "Invalid input(not a quoted string)"))))])]])))
              

(define (question7 result)
  (begin
    ;(display "To whom did Luke owe the most loyalty?\n 1.his father\n 2.Obi Wan\n 3.Han and Leia\n 4.Yoda\n 5.other\n")
    (let ([input (read-number (list "Q7. To whom did Luke owe the most loyalty?" "1. his father" "2. Obi Wan" "3. Han and Leia" "4. Yoda" "5. other"))])
      ;(if (s-exp-number? input)
          [cond
            [(= input 1) (let ([result (cons (string-append q7 "Answer -> his father ") result)]) (ending result))];(set-box! a7 "his father")]
            [(= input 2) (let ([result (cons (string-append q7 "Answer -> Obi Wan ") result)]) (ending result))];(set-box! a7 "Obi Wan")]
            [(= input 3) (let ([result (cons (string-append q7 "Answer -> Han and Leia ") result)]) (ending result))];(set-box! a7 "Han and Leia")]
            [(= input 4) (let ([result (cons (string-append q7 "Answer -> Yoda ") result)]) (ending result))];(set-box! a7 "Yoda")]
            [(= input 5) (begin
                                           ;(display "5.other(quoted string): ")
                                           (let ([user-input (read-user-input "5. other: ")])
                                             ;(if (s-exp-string? user-input)
                                                 (let ([result (cons (string-append q7 (string-append "Answer -> " user-input)) result)]) (ending result));(set-box! a7 (s-exp->string user-input))
                                                 ))]
            [else (error 'survey "Invalid input. Choose between 1-5")]])))

;(define (f x)
  ;(display (string-append (to-string x) "\n")))
  ;(type-case Item x
   ; [(item q r) (display (string-append (string-append (to-string q) "\n") (string-append (to-string r) "\n")))]))
  
;(for-each : (('a -> Void) (Listof 'a) -> Void))
;(define (for-each f xs)
 ; (type-case (Listof 'a) xs
  ;  [empty (void)]
   ; [(cons x xs)
    ; (begin (f x) (for-each f xs))]))


(define (ending result)
  (reverse result))
  ;(begin
    ;(display "End of Survey. Printing results...\n")
    ;(for-each f (reverse result)))
    ;(display-result result)))
  ;result)    
    

; Start of the survey
(define (start-survey)
  (begin
    ;(display "Which Star Wars trilogy is the best?\n 1.The Original\n 2.The Prequel\n 3.The Sequel\n")
    ;(read-number "Which Star Wars trilogy is the best?\n 1.The Original\n 2.The Prequel\n 3.The Sequel\n")
    (let ([input (read-number (list "Q1. Which Star Wars trilogy is the best?" "1. The Original" "2. The Prequel" "3. The Sequel"))])
      [cond
        [(= input 2) (let ([result (cons (string-append q1 "Answer -> The Prequel ") empty)]) (question2 result))];(begin (set-box! a1 "The Prequel") (question2))]
        [(= input 1) (let ([result (cons (string-append q1 "Answer -> The Original ") empty)]) (question3 result))];(begin (set-box! a1 "The Original") (question3))]
        [(= input 3) (let ([result (cons (string-append q1 "Answer -> The Sequel ") empty)]) (question3 result))];(begin (set-box! a1 "The Sequel") (question3))]
        [else (error 'survey "Invalid input. Choose between 1-3")]])))


          
          
           






