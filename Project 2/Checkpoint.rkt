;;Dr. D's load-file
;; load-file is a simple function that loads the definitions from a file
;; It recursives calls itself, reading one line at a time; on the recursive return, it cons'es
;; all of the lines together in a giant list, which it returns to caller.

;; The "port" parameter is a read port that you pass it.  The easiest way to get one is to 
;; use the open-input-file built-in function.  So:  (open-input-file filename) returns you a port to that file.

( define load-file
   ( lambda ( port )
      ( let ( ( nextrec ( read port ) ) )
         ( cond
            ( ( eof-object? nextrec ) '() ) ;; If I've read off the end, return empty list
            ( else
              ( let* ( ( nascent-db ( load-file port ) ) ) ;; Recursive call to finish reading file
                 ;; Now add the line read at this level to growing list
                 ( cons nextrec nascent-db ) ) ) ) ) ) )

;; This new function takes in a class that has already been written.
;; The "." between class and parameters allows the function to take in as many parameters
;; Then it takes in the parameters for the class
;; Apply takes all of this and makes it work. 
(define new (lambda (class . parameters)
              (apply class parameters)))

;; Takes in a class definition, which is in a list
;; Then it changes it into a list that is valid scheme
;; This scheme code is for a constructor for the object that is defined in the definitions
(define pClass
  (lambda (cDefinitions)
    (let ([cName (second cDefinitions)]
          [constArgs (cdr (fourth cDefinitions))]
          [ivars (cdr (fifth cDefinitions))]
          [mDefinitions (cdr (sixth cDefinitions))])
      `(define ,cName
         (lambda ,constArgs
           (letrec (,@ivars
                    [instance (lambda paramlist
                                      (let ([msg (first paramlist)]
                                            [parameters (cdr paramlist)])
                                        (cond
                                          ,@(map pMethod mDefinitions)
                                          [(display "Error. The method was not found.")])))]
                    [*this* instance])
             instance))))))

;;Take in a function definition which is a list
;;It then changes the function defintion into valid scheme code that is a cond
(define pMethod
  (lambda (methodDefinitions)
    (let ([mName   (car methodDefinitions)]
          [mParameters (second methodDefinitions)]
          [mBody   (third methodDefinitions)])
      (if (null? mParameters)
          `[(eq? msg ' ,mName) ((lambda ,mParameters ,mBody))]
          `[(eq? msg ' ,mName) (apply(lambda ,mParameters mBody) parameters)]))))

;;This loads the classes from the text file.
;;This function will also alert the user to which files are laoded. 
(define load-classes
  (lambda (filename)
    (let* ([classDefinitions (load-file (open-input-file filename))]  ;; List of class definitions as read in from "filename"
           [constructorDefinitions (map pClass classDefinitions)]
           [names (map second constructorDefinitions)])      ;; List of return valuse from calling pClass once on each item in classDefinitions
      (map eval constructorDefinitions) (display "Classes that were loaded:") (print `(,@names)))))