(define temp '((name: addtwo) (args: x) (body: (+ x 2)))) 
;helper functon which is pretty much steamroller again. 
(define (helper x) ;define function
  (cond ((null? x) '()) ;check if list is null
        ((pair? x) ;check for pairs
         (append (helper (car x)) (helper (cdr x)))) ;recursive calls 
        (else (list x))))
;fn-maker
(define (fn-maker fn-spec)
  (cond
    [(null? fn-spec) '()] ;check if list is null
    [else (eval (list 'define (helper(list (cadar fn-spec) ;if list is not null 
                                                (if (> (length (cdr (cadr fn-spec))) 2)
                                    (cdadr fn-spec)
                                   (car (cdr (cadr fn-spec))))))
           (cadar (cddr fn-spec))))])
  )
                                                
                                          