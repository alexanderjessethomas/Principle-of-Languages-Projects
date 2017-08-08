(define x '(1 2 3 4 5 6 7 8 9 10))
(define y '(((a) b) (c d (d (f g) h)) i))
(define z ' (a f c d f g e g h e g e e))
(define a '(1 3 5 7 9))
(define b '(2 4 6 8 10))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Steamroller function 
(define (steamroller x) ;define function
  (cond ((null? x) '()) ;check if list is null
        ((pair? x) ;check for pairs
         (append (steamroller (car x)) (steamroller (cdr x)))) ;recursive calls 
        (else (list x))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;ndelete
(define (ndelete lst x) 
   (define (helper lst x c) 
     (if (null? lst) 
       '() 
       (if (= x 1) 
         (helper (cdr lst) c c) 
         (cons (car lst) (helper (cdr lst) (- x 1) c))))) 
   (helper lst x x)) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;deep memeber

(define deep-member?
  (lambda (x lst)
    (or (equal? x lst)
        (and (pair? lst)
             (or (deep-member? x (car lst))
                 (deep-member? x (cdr lst)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;mymergesort
(define mymergesort
  (lambda (alist)
    (if (null? (cdr alist)) alist
        (let ((splits (splitter alist)))
          (merge (mymergesort (car splits)) (mymergesort (cadr splits)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;splitter
(define splitter
  (lambda (lst)
    (if (null? lst)
        (list '() '())
        (if (null? (cdr lst))
            (list lst '())
            (let ((rest (splitter (cdr (cdr lst)))))
              (list (cons (car lst) (car rest))
                    (cons (car (cdr lst)) (car (cdr rest)))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;merge
(define merge (lambda (list1 list2)
                (if (null? list1)
                    list2
                    (if (null? list2)
                        list1
                        (if (< (car list1) (car list2))
                            (cons (car list1) (merge (cdr list1) list2))
                            (cons (car list2) (merge (cdr list2) list1)))))))
