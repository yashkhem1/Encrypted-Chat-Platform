#lang racket
(require math/matrix)
(require "Hill_Encryption") ; This imports an encrypter built to encrypt messages when they are transferred
(define-syntax lc
  (syntax-rules (: <- @)
    [(lc expr : var <- drawn-from) (map (lambda (var) expr) drawn-from)]
    [(lc expr : @ guard) (if guard (list expr) `())]
    [(lc expr : @ guard  qualifier ...) 
     (append* (lc (lc expr : qualifier ...) : @ guard))]
    [(lc expr : var <- drawn-from  qualifier ...) 
     (append* (lc (lc expr :  qualifier ... ) : var <- drawn-from))]))

;;;;;;;;;;;;;;;;;;NOTE : ALL MESSAGES HAVE TO BE IN CAPITALS AND NUMERALS ARE NOT ALLOWED (THIS IS DUE TO RESTRICTED ENCRYPTION OF ENCRYPTER);;;;;;;;;;;;;;;;;;;

(define (key-finder l a)
  (cond [(null? l) #f]
        [(equal? (caar l) a) (cdar l)]
        [else (key-finder (cdr l) a)]))   ; This finds the key that is shared between two accounts while they chat

(define (add-name a1)
  (lambda (x) (hash-set! x a1 null)))   ; This adds new chat to the account 

(define (add-chat a1 a msg)
  (lambda (x) (hash-set! x a1 (append (hash-ref x a1) (list (cons a msg)))))) ; This extends the ongoing chat between two accounts

(define sys%   ; This class defines a server or the center of the chat station which controls the flow of messages
  (class object%
    (super-new)
    (define admin 'ATY) 
    (define keys (list->vector (lc (list->matrix 2 2 (append (list a) (list b) (list c) (list d))) : 
      a <- (range 1 10)  b <- (range 1 10)  c <- (range 1 10)
      d <- (range 1 10) @(= (- (* a d) (* b c)) 1))))                                             ; -> This is a set of 28 random keys (here keys are in form
                                                                                                      ; matrix as required by hill cipher.

    ; Uncomment the part below and the comment the above part to have a list of about 1 million keys (though it takes a minute to generate these many keys)
    ;(define keys (list->vector (lc (list->matrix 3 3 (append (list a1) (list a2) (list a3) (list a4) (list a5)
     ;                                          (list a6) (list a7) (list a8) (list a9))) : 
     ; a1 <- (range 1 10) a2 <- (range 1 10) a3 <- (range 1 10)
      ;a4 <- (range 1 10) a5 <- (range 1 10) a6 <- (range 1 10)
     ; a7 <- (range 1 10) a8 <- (range 1 10) a9 <- (range 1 10)
     ; @(= (- (+ (* a1 a5 a9) (* a2 a6 a7) (* a3 a4 a8)) (+ (* a1 a6 a8) (* a2 a4 a9) (* a3 a5 a7)))  1)))        ; -> This is a set of around 1M random keys

    (define account-log (make-hash null))
    (define account-status-log (make-hash null))
    
    (define/public (create-chat-session u1 u2 sysad)     ; This creates a chat session between two accounts and can be created only by the administrator
      (if (not(equal? sysad admin)) (error "You are not authorised to create a chat session between these two accounts")
      (cond [(not(hash-has-key? account-log u2)) (error "No account with username" u1)]
            [else 
             (define acc1 (hash-ref account-log u1))
             (define acc2 (hash-ref account-log u2))
             (let* [(session-key (vector-ref keys (random 28)))]
               (begin (send acc1 allow-messages acc2 session-key)
                      (send acc2 allow-messages acc1 session-key)
                      (send acc1 change-chat-info (add-name u2) admin)
                      (send acc2 change-chat-info (add-name u1) admin)))])))

    (define/public (sign-up username password)  ; This helps a user to sign up to the Chat Machine
      (if (hash-has-key? account-log username) (error "An account with username" username "already exists")
      (begin (hash-set! account-log 
                 username
                 (new account% [username username] [password password])) 
             (hash-set! account-status-log username #t))))
    
    (define/public (reciever acc en-msg sender)  ; This is a reciever that recieves message from the sender in encrypted form (and also displays the encrypted form)
      (display en-msg)
      (send acc recieve-message en-msg sender))


    (define/public (login username password)  ; This helps to login a person to the Chat Machine
      (if (hash-has-key? account-status-log username)
          (if (equal? password (get-field password (hash-ref account-log username)))
              (if (equal? (hash-ref account-status-log username) #t)
                  (error "Account is already logged in")
                  (hash-set! account-status-log username #t))
              (error "Wrong password for the account"))
          (error "No account with username" username)))

    (define/public (logout username)  ; This helps to logout a person from the Chat Machine
      (if (hash-has-key? account-status-log username)
          (if (equal? (hash-ref account-status-log username) #f)
              (display (error "Account is not logged in"))
              (hash-set! account-status-log username #f))
          (error "No account with username" username)))

    (define (online username)  ; This tells whether a person is online on the Chat Machine or not
      (if (equal? (hash-ref account-status-log username) #t) #t #f))

    (define/public (send-message a1 a2 msg sysad)  ; This is an initiator that initiates the account class to send a message
      (if (not(equal? sysad admin)) (error "You are not authorised to perform this function")
      (cond [(not(hash-has-key? account-log a1)) (error "No account with username" a1)]
            [(not(hash-has-key? account-log a2)) (error "No account with username" a2)]
            [else (define acc1 (hash-ref account-log a1))
                  (define acc2 (hash-ref account-log a2))
                  (if (online a1)
                      (send acc1 send-message msg acc2)
                      (error "You must login to message"))])))
    
    (define/public (recieve-message a1 a2 msg sysad)  ; This is an initiator that initiates the account class to recieve message 
      (cond [(not(equal? sysad admin)) (error "You are not authorised to perform this function")]
            [else (define acc1 (hash-ref account-log a1))
                  (define acc2 (hash-ref account-log a2))
                  (send acc1 recieve-message msg acc2)]))

    (define/public (check-inbox a1 sysad)  ;  This helps checking inbox of the person
      (cond [(not(equal? sysad admin)) (error "Illegal attempt to check inbox")]
            [else (define acc (hash-ref account-log a1))
                  (send acc my-inbox)]))

    (define/public (check-chat a1 sysad)   ;  This helps checking chat-box of the person
      (cond [(not(equal? sysad admin)) (error "Illegal attempt to check chat box")]
            [else (define acc (hash-ref account-log a1))
                  (send acc my-chat-box)]))))

(define account%    ; This is a class that is never directly called on terminal but is always called with function for sys% class
  (class object%
    (super-new)
    (init-field username)
    (init-field password)
    (define chat-log null)
    (define chat-info (make-hash null))
    (define admin 'ATY)
    (define inbox null)
    (define/public (allow-messages acc key)
      (set! chat-log (append chat-log (list (cons acc key)))))
    (define/public (check-chat-info)
      chat-info)
    (define/public (change-chat-info change sysad)  ; a higher order function that updates the chat box of the person
      (if (equal? sysad admin) (begin (change chat-info) (void)) (error "You are not authorised to access or change this chat info")))
      
    (define/public (send-message msg acc)   ; sends message from one account to another
      (let* [(system-key (key-finder chat-log acc))]
        (if (false? system-key) (error "Please create a chat session with" (get-field username acc))
            (begin (change-chat-info (add-chat (get-field username acc) (get-field username this) msg) admin)
                   (send my-system reciever acc (encrypt-by-hill-cipher msg system-key) this)))))

    (define/public (recieve-message en-msg acc) ; recieves message from another account
      (let* [(system-key (key-finder chat-log acc))]
        (if (false? system-key) (error "Please create a chat session with this account" (get-field username acc))
            (let* [(msg (decrypt-by-hill-cipher en-msg system-key))]
            (begin (change-chat-info (add-chat (get-field username acc) (get-field username acc) msg) admin)
                   (set! inbox (append (list (cons (get-field username acc) (decrypt-by-hill-cipher en-msg system-key))) inbox)))))))

    (define/public (my-inbox)
      inbox)
    (define/public (my-chat-box)
      chat-info)))
    
(define my-system (new sys%))  ; This is my server which creates a new chat machine every time the code is run (compiled) so the code needs to be compiled once
                               ; and other people can work on this chat machine 