;; Name: Alex Thomas
;; Course: CS396
;; Assignemnt: OOPs World: a basic OOP interpreter
;; Due Date: 4/13/2016



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
;; The list, however it also happens to be valid scheme code
;; This scheme code will define an object when it is evaluated.
;;At this point it has no been used for inheritance 
(define pClass
  (lambda (classDefintions)
    (let ([cName (second classDefintions)]
          [pName (if (null? (cdr (third classDefintions)))
                           #f
                           (second (third classDefintions)))]
          [constArgs (cdr (fourth classDefintions))]
          [ivars (cdr (fifth classDefintions))]
          [mDefinitions (cdr (sixth classDefintions))])
      `(,cName ,pName
        (define ,cName
          (lambda ,constArgs
            (let ,ivars
              (letrec ([objλ (λ parameterList
                                        (let ([message (car parameterList)]
                                              [parameters (cdr parameterList)])
                                          (cond
                                            ,@(map pMethod mDefinitions)
                                            ,(if (not (eq? pName #f))
                                               '[#t (apply *super* parameterList)]
                                               '[#t (display "The method was not found. Where is it?")]))))]
                       [*this* objλ]) objλ))))))))


;;Take in a function definition which is a list
;;It then changes the function defintion into valid scheme code.
;;This is injected into the constructor definition built by pClass
(define pMethod
  (λ (methodDefintions)
    (let ([mName   (car methodDefintions)]
          [mParameters (second methodDefintions)]
          [mBody   (third methodDefintions)])
     (quasiquote[(and (eq? message ',mName) (eq? ,(length mParameters) (length parameters)))
       ,(if (null? mParameters)
            `((lambda ,mParameters ,mBody))
           `(apply (λ ,mParameters ,mBody) parameters))]))))

;; Fetches the constructor definitions of the passed in constructor-records ancestors. Note that the
;; constructor definition of the passed constructor record is included in the result. Further note
;; that the resulting list is in the order of "original->parent->grandparent->...->eldest".
(define getConstructorDefintions
  (lambda (rConstructor rConstructorDirectory)
    (let ([constructDefintions (third rConstructor)]
          [pName (second rConstructor)])
      (if pName
          (let ([rConstructorParent (assoc pName rConstructorDirectory)])
            (cons constructDefintions (getConstructorDefintions rConstructorParent rConstructorDirectory)))
          (list constructDefintions)))))

;; Gets the ivars from the passes in rConstructor and its parents if it has any.
(define getIvars
  (λ (rConstructor  rConstructorDirectory)
    (let ([neededConstructorDefintions (getConstructorDefintions  rConstructor rConstructorDirectory)]
          [getter (lambda (constructDefintions) (second (third (third constructDefintions))))])
      (apply append (map getter neededConstructorDefintions)))))

;; Gets the object lambdas from the passed in rConstructor and its parents it if has any.
(define getObjectlambdas
  (λ (rConstructor rConstructorDirectory)
    (let ([neededConstructorDefintions (getConstructorDefintions rConstructor  rConstructorDirectory)]
          [get (lambda (constructDefintions)
                           (second (car (second (third (third (third constructDefintions)))))))])
      (map get neededConstructorDefintions))))

;; Recursively puts in parent object lambdas into the child's object lambda
;; This function takes in the onject lambdas.
;; Then it checks to see if the child has any parents.
;; If it doesnt we do not enter the recursive state of this method
;; If the child does the method combines the object lambdas
;; This is done until the base case is hit. 
(define combineLambdas
  (λ (objLambdas)
    (if (null? (cdr objLambdas))
      ;; This is where the base case takes place. 
      (let ([objLambdasF (car objLambdas)])
        `(letrec ([objLambda ,objLambdasF]
                  [*this* objLambda])
           objLambda))
      ;; This is the start of the recursive case
      (let ([cObjectLambda (car objLambdas)]
            [pObjectLambda
              (combineLambdas (cdr objLambdas))])
        `(letrec ([objLambda ,cObjectLambda]
                  [*this* objLambda]
                  [*super* ,pObjectLambda])
           objLambda)))))

;; This function is meant to handle when the child has the same ivar name as the parent
;; This what we are going to call overloading
;; What this function is meant to do is take all the ivar names from the child object
;; Then when it loads the the ivar names from the parent object that are not already loaded form the child object
;; for example if child as an ivar named weight. and Parent has an ivar names weight. the parent one is not loaded. 
(define joinIvars
  (lambda (rConstructor rConstructorDirectory)
  ))  ;; unfortunately I could not get this function to work properly and I knew hard coding the solution to work for the animals inheritance file would
      ;; solve it either. 

;; This takes in a rConstructor.
;; Then it puts the parts that are nessecary into the constructDefintions.
;; This is where the inheritance takes place in the program.
(define inheritance
  (lambda (rConstructor rConstructorDirectory)
    (let* ([pName (second rConstructor)]
           [constructDefintions (third rConstructor)]
           [cName (second constructDefintions)]
           [constructArgs (second (third constructDefintions))]
           [neededIvars (getIvars rConstructor rConstructorDirectory)]
           [ojbectLambas (getObjectlambdas rConstructor rConstructorDirectory)]
           [objLambdasF (combineLambdas ojbectLambas)])
      `(define ,cName
         (λ ,constructArgs
           (let ,neededIvars
             ,objLambdasF))))))

;;This loads the classes from the text file.
;;This function will also alert the user to which files are loaded. 
(define load-classes
  (lambda (fileName)
    (let* ([classDefintions (load-file (open-input-file fileName))]
           [rConstructorDirectory (map pClass classDefintions)]
           [conStructorDefinitions (map (λ (rConstructor) (inheritance rConstructor rConstructorDirectory)) rConstructorDirectory)])
      (map eval conStructorDefinitions)
      (map (λ (rConstructor)
             (display (format "Class loaded: ~a" (cadr rConstructor)))
             (newline))
          conStructorDefinitions) (display " "))))
      


(load-classes "/Users/Alexj/Downloads/inherit.txt")