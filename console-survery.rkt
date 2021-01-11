#lang plait

; typing invalid input will not allow to retype

(define-type Item
  (item [question : String] [response : String]))

(define q1 "Which Star Wars trilogy is the best?")
(define q2 "I'm amazed (and delighted!) at your choice. What about it bothers you the least?")
(define q3 "Who arc do you find more satisfying?")
(define q4 "What was the root cause of Anakin's downfall?")
(define q5 "Is Luke defined more by his hopefulness or his loyalty?")
(define q6 "What in-universe explanation for Luke's loss of hope in the sequel trilogy is most plausible?")
(define q7 "To whom did Luke owe the most loyalty?")

(define (question2 result)
  (begin
    (display "I'm amazed (and delighted!) at your choice. What about it bothers you the least?\n 1.the words the actors say\n 2.how they say them\n 3.something else\n")
    (let ([input (read)])
      (if (s-exp-number? input)
            [cond
              [(= (s-exp->number input) 1) (let ([result (cons (item q2 "the words the actors say") result)]) (question3 result))];(set-box! a2 "the words the actors say")]
              [(= (s-exp->number input) 2) (let ([result (cons (item q2 "how they say them") result)]) (question3 result))];(set-box! a2 "how they say them")]
              [(= (s-exp->number input) 3) (let ([result (cons (item q2 "something else") result)]) (question3 result))]
              [else (error 'survey "Invalid input. Choose between 1-3")]]
          (error 'survey "Invalid input(not a number)")))))


                                  
(define (question3 result)
  (begin
    (display "Who arc do you find more satisfying?\n 1.Anakin Skywalker\n 2.Luke Skywalker\n")
    (let ([input (read)])
      (if (s-exp-number? input)
          [cond
            [(= (s-exp->number input) 1) (let ([result (cons (item q3 "Anakin Skywalker") result)]) (question4 result))];(begin (set-box! a3 "Anakin Skywalker") (question4))]
            [(= (s-exp->number input) 2) (let ([result (cons (item q3 "Luke Skywalker") result)]) (question5 result))];(begin (set-box! a3 "Luke Skywalker") (question5))]
            [else (error 'survey "Invalid input. Choose between 1-2")]]
          (error 'survey "Invalid input(not a number)")))))
            
            

(define (question4 result)
  (begin
    (display "What was the root cause of Anakin's downfall?\n 1.his abilities\n 2.his pride in his abilities\n 3.his relationship with Padme\n 4.his attachment to temporary things\n 5.others\n")
    (let ([input (read)])
      (if (s-exp-number? input)
            (cond
            [(= (s-exp->number input) 1) (let ([result (cons (item q4 "his abilities") result)]) (ending result))];(set-box! a4 "his abilities")]
            [(= (s-exp->number input) 2) (let ([result (cons (item q4 "his pride in his abilities") result)]) (ending result))];(set-box! a4 "his pride in his abilities")]
            [(= (s-exp->number input) 3) (let ([result (cons (item q4 "his relationship with Padme") result)]) (ending result))];(set-box! a4 "his relationship with Padme")]
            [(= (s-exp->number input) 4) (let ([result (cons (item q4 "his attachment to temporary things") result)]) (ending result))];(set-box! a4 "his attachment to temporary things")]
            [(= (s-exp->number input) 5) (begin
                                           (display "5. other(quoted string) : ")
                                           (let ([user-input (read)])
                                             (if (s-exp-string? user-input)
                                                 (let ([result (cons (item q4 (s-exp->string user-input)) result)]) (ending result));(set-box! a4 (s-exp->string user-input))
                                                 (error 'survey "Invalid input(not a quoted string)"))))]
            [else (error 'survey "Invalid input. Choose between 1-5")])
          (error 'survey "Invalid input(not a number)")))))

; Luke Skywalker from Question3
(define (question5 result)
  (begin
    (display "Is Luke defined more by his hopefulness or his loyalty?\n 1.his hopefulness\n 2.his loyalty\n")
    (let ([input (read)])
      (if (s-exp-number? input)
          [cond
            [(= (s-exp->number input) 1) (let ([result (cons (item q5 "his hopefulness") result)]) (question6 result))];(begin (set-box! a5 "his hopefulness") (question6))]
            [(= (s-exp->number input) 2) (let ([result (cons (item q5 "his loyalty") result)]) (question7 result))];(begin (set-box! a5 "his loyalty") (question7))]
            [else (error 'survey "Invalid input. Choose between 1-2")]]
          (error 'survey "Invalid input(not a number)")))))

; from his hopefulness from question5
(define (question6 result)
  (begin
    (display "What in-universe explanation for Luke's loss of hope in the sequel trilogy is most plausible?\n 1.the pressure of building a new Jedi Order\n 2.unresolved fears about the Dark Side\n 3.disillusionment with the history of the Jedi Order\n 4.new knowledge about midichlorians\n 5.other\n")
    (let ([input (read)])
      (if (s-exp-number? input)
            [cond
              [(= (s-exp->number input) 1) (let ([result (cons (item q6 "the pressure of building a new Jedi Order") result)]) (ending result))];(set-box! a6 "the pressure of building a new Jedi Order")]
              [(= (s-exp->number input) 2) (let ([result (cons (item q6 "unresolved fears about the Dark Side") result)]) (ending result))];(set-box! a6 "unresolved fears about the Dark Side")]
              [(= (s-exp->number input) 3) (let ([result (cons (item q6 "disillusionment with the history of the Jedi Order") result)]) (ending result))];(set-box! a6 "disillusionment with the history of the Jedi Order")]
              [(= (s-exp->number input) 4) (let ([result (cons (item q6 "new knowledge about midichlorians") result)]) (ending result))];(set-box! a6 "new knowledge about midichlorians")]
              [(= (s-exp->number input) 5) (begin
                                             (display "5.other(quoted string): ")
                                             (let ([user-input (read)])
                                               (if (s-exp-string? user-input)
                                                   (let ([result (cons (item q6 (s-exp->string user-input)) result)]) (ending result));(set-box! a6 (s-exp->string user-input))
                                                   (error 'survey "Invalid input(not a quoted string)"))))]
              [else (error 'survey "Invalid input. Choose between 1-5")]]
          (error 'survey "Invalid input(not a number)")))))
              

(define (question7 result)
  (begin
    (display "To whom did Luke owe the most loyalty?\n 1.his father\n 2.Obi Wan\n 3.Han and Leia\n 4.Yoda\n 5.other\n")
    (let ([input (read)])
      (if (s-exp-number? input)
          [cond
            [(= (s-exp->number input) 1) (let ([result (cons (item q7 "his father") result)]) (ending result))];(set-box! a7 "his father")]
            [(= (s-exp->number input) 2) (let ([result (cons (item q7 "Obi Wan") result)]) (ending result))];(set-box! a7 "Obi Wan")]
            [(= (s-exp->number input) 3) (let ([result (cons (item q7 "Han and Leia") result)]) (ending result))];(set-box! a7 "Han and Leia")]
            [(= (s-exp->number input) 4) (let ([result (cons (item q7 "Yoda") result)]) (ending result))];(set-box! a7 "Yoda")]
            [(= (s-exp->number input) 5) (begin
                                           (display "5.other(quoted string): ")
                                           (let ([user-input (read)])
                                             (if (s-exp-string? user-input)
                                                 (let ([result (cons (item q7 (s-exp->string user-input)) result)]) (ending result));(set-box! a7 (s-exp->string user-input))
                                                 (error 'survey "Invalid input(not a quoted string)"))))]
            [else (error 'survey "Invalid input. Choose between 1-5")]]
        (error 'survey "Invalid input(not a number)")))))

(define (f x)
  ;(display (string-append (to-string x) "\n")))
  (type-case Item x
    [(item q r) (display (string-append (string-append (to-string q) "\n") (string-append (to-string r) "\n")))]))
  
(for-each : (('a -> Void) (Listof 'a) -> Void))
(define (for-each f xs)
  (type-case (Listof 'a) xs
    [empty (void)]
    [(cons x xs)
     (begin (f x) (for-each f xs))]))


(define (ending result)
  (begin
    (display "End of Survey. Printing results...\n")
    (for-each f (reverse result))))
    
      
    

; Start of the survey
(define (start-survey)
  (begin
    (display "Which Star Wars trilogy is the best?\n 1.The Original\n 2.The Prequel\n 3.The Sequel\n")
    (let ([input (read)])
      (if (s-exp-number? input)
          [cond
            [(= (s-exp->number input) 2) (let ([result (cons (item q1 "The Prequel") empty)]) (question2 result))];(begin (set-box! a1 "The Prequel") (question2))]
            [(= (s-exp->number input) 1) (let ([result (cons (item q1 "The Original") empty)]) (question3 result))];(begin (set-box! a1 "The Original") (question3))]
            [(= (s-exp->number input) 3) (let ([result (cons (item q1 "The Sequel") empty)]) (question3 result))];(begin (set-box! a1 "The Sequel") (question3))]
            [else (error 'survey "Invalid input. Choose between 1-3")]]
          (error 'survey "Invalid input(not a number)")))))


          
          
           






