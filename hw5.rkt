;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function ;; nameopt (optional name), formal (arguments) 
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; mupl value is
;;   a mupl integer constant,
;;   a mupl closure,
;;   a mupl aunit,
;;   or a mupl pair of mupl values

(struct closure (env fun) #:transparent) 

;; Problem 1

;; CHANGE (put your solutions here)

(define (racketlist->mupllist xs)
  (if (empty? xs)
      (aunit)
  (apair (car xs) (racketlist->mupllist (cdr xs)))))

(define (mupllist->racketlist xs)
  (if (equal? (apair-e2 xs) (aunit))
      (apair-e1 xs)
  (list (apair-e1 xs) (mupllist->racketlist (apair-e2 xs)))))

;; Problem 2


;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(fun? e) (closure env e)]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(int? e) e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to subexpression which e1 or e2 is non-int")))]
        [(closure? e) e]
        [(mlet? e)
         (let ([val (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (append env (list (cons (mlet-var e) val)))))]
        [(call? e)
         (let ([cls (eval-under-env (call-funexp e) env)])
           (if (closure? cls)
             (eval-under-env (fun-body (closure-fun cls)) (append (closure-env cls) (list (cons (fun-formal (closure-fun cls)) (eval-under-env (call-actual e) env)) (cons (fun-nameopt (closure-fun cls)) cls))))    
           (error "call-funexp is not a fun")))]
        [(apair? e)
         (apair (eval-under-env (apair-e1 e) env) (eval-under-env (apair-e2 e) env))]
        [(fst? e)
         (let ([r (eval-under-env (fst-e e) env)])
           (if (apair? r)
               (apair-e1 r)
               (error "MUPL fst applied to non-pair")))]
        [(snd? e)
         (let ([r (eval-under-env (snd-e e) env)])
           (if (apair? r)
               (apair-e2 r)
               (error "MUPL snd applied to non-pair")))]
        [(isaunit? e)
         (let ([r (eval-under-env (isaunit-e e) env)])
           (if (equal? (aunit) r)
               (int 1)
               (int 0)))]
        [(aunit? e) e]
        [#t (error (format "bad MUPL expression: ~v" e))]))


;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3)
  )

(define (mlet* lst e2)
  (if (empty? lst)
      e2
      (mlet (car (car lst)) (cdr (car lst)) (mlet* (cdr lst) e2))))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
  (ifgreater (var "_x") (var "_y")
    e4
    (ifgreater (add (var "_y") (var "_y")) (add (var "_x") (var "_x")) e4 e3))))

;; Problem 4

(define mupl-map
  (fun #f "f"
    (fun "map" "lst"
      (ifgreater (isaunit (var "lst")) (int 0) (aunit) (apair (call (var "f") (fst (var "lst"))) (call (var "map") (snd (var "lst"))))))))
                

(define mupl-mapAddN 
  (mlet "map" mupl-map
      (fun #f "i"
        (fun #f "lst"
          (call (call (var "map") (fun #f "elem" (add (var "i") (var "elem")))) (var "lst"))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) (aunit)))
