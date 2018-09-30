#lang racket

(provide (all-defined-out))

(require math/matrix)

;;;;;;;;;;;;;;;;;;;; HILL CIPHER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (code m msg) 
  (define iterator1 -1)
  (define iterator2 0)
  (define my-hash #hash())
  (define coder (make-hash (map (λ (x) (cons x (begin (set! iterator1 (+ iterator1 1)) iterator1)))
                                (string->list " ABCDEFGHIJKLMNOPQRSTUVWXYZ")))) ;;;; codes letters to numbers i.e. A to 1, B to 2, ......
  (define decoder (make-hash (append (hash->list (make-hash (map (λ (x) (cons (begin (set! iterator2 (+ iterator2 1)) (remainder iterator2 26)) x))
                                  (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZ")))) (list (cons 0 #\space)))))
                                                                                ;;;; decodes numbers back to their corresponding letters
  (define (encrypter m)
    (cond [(null? m) null]
          [else (append (list (hash-ref coder (car m))) (encrypter (cdr m)))]))

  (define (decrypter m)
    (cond [(null? m) null]
          [else (append (list (hash-ref decoder (modulo (car m) 26))) (decrypter (cdr m)))]))

  (if (equal? m 'encrypt) (encrypter (string->list msg))
      (decrypter msg)))

(define (msg_matrix_creator msg key_size)  ;;;;;;;;;;;;;;;;;;;;;;;;;; converts message to matrix
  (define basic_code (code 'encrypt msg))
  (let* [(codel (length basic_code))
         (c-factor (- key_size (remainder codel key_size)))]
    (list->matrix (/ (+ codel c-factor) key_size) key_size (append basic_code (make-list c-factor 0))))) 

(define (encrypt-by-hill-cipher msg key)
  (define msg_matrix (matrix-transpose (msg_matrix_creator msg (square-matrix-size key)))) ;;;;; HILL CIPHER : ENCRYPTER
  (define encrypted_msg (matrix->list (matrix-transpose (matrix* key msg_matrix))))
  (list->string (remove-until-letter (reverse (code 'decrypt encrypted_msg)))))

(define (remove-until-letter l)
  (cond [(not(equal? (car l) #\space)) (reverse l)]
        [else (remove-until-letter (cdr l))]))

(define (decrypt-by-hill-cipher encrypted_msg key)
  (define decrypter_key (matrix-inverse key))
  (define encrypted_matrix (matrix-transpose (msg_matrix_creator encrypted_msg (square-matrix-size key)))) ;;;;; HILL CIPHER : DECRYPTER
  (define decrypted_msg (matrix->list (matrix-transpose (matrix*   decrypter_key encrypted_matrix))))
  (list->string (remove-until-letter (reverse (code 'decrypt decrypted_msg)))))
