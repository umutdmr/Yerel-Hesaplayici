; umut demir
; 2019400219
; compiling: yes
; complete: yes



#lang racket

(provide (all-defined-out))

; 10 points
(define := (lambda (var value) (list var value)))

; 10 points
(define -- (lambda args (list 'let args)))

; 10 points
(define @ (lambda (bindings expr) 

    (if (null? bindings) expr
        (cons (car bindings) (@ (cdr bindings) expr)))))

; 20 points
(define split_at_delim (lambda (delim args) 
    (foldr (lambda (split current)
        (if (eqv? split delim)
            (cons '() current)
            (cons (cons split (car current)) (cdr current))))
        (list '()) args)))

; 30 points
(define parse_expr (lambda (expr) 

    (define (merge a b)
            (cond ((null? a) b)
                ((null? b) a)
                (else (cons (car a) (merge (cdr a) b)))
        ))
   
    (define eval_binding (lambda (binding)


         

        (define eval_last (lambda (last)
            (if (not (eqv? empty last))
                
                (eval(merge (list ':=) (merge (list(car last)) (cddr last))))

                empty
            )
            
        ))

        (define (remove-last lst)
            (if (null? (cdr lst))
            '()
            (cons (car lst) (remove-last (cdr lst)))))
             
        (if (null? (cdr binding)) 
            (eval_binding (car binding))

            (list 'let (map eval_last (remove-last(split_at_delim '-- binding))))
        ) 
        
    ))
    
    (if (list? expr)

        (if(null? (cdr (split_at_delim '+ expr)))
            (if(null? (cdr (split_at_delim '* expr)))
                (if (null? (cdr (split_at_delim '@ expr)))
                    (if (null? (cdr expr)) 
                        (parse_expr (car expr))
                        expr
                    )
                    (merge (eval_binding (list (car (split_at_delim '@ expr)))) (list(parse_expr (cdr (split_at_delim '@ expr)))))
                )
                (cons '* (map parse_expr (split_at_delim '* expr)))
            ) 
            (cons '+ (map parse_expr (split_at_delim '+ expr)))   
        )
            

        expr
    )

))

; 20 points
(define eval_expr (lambda (expr) 
    (eval (parse_expr expr))))

