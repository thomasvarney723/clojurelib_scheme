(define (map fun coll)
    (if (list? coll)
        (if (null? coll)
            coll
            (cons (fun (car coll)) (map fun (cdr coll))))
        "Error: not a list"))
        
(define (filter pred coll)
    (if (list? coll)
        (if (null? coll)
            coll
            (if (pred (car coll))
                (cons (car coll) (filter pred (cdr coll)))
                (filter pred (cdr coll))))
        "Error: not a list"))
        
(define (reduce fun init coll)
    (if (list? coll)
        (if (null? coll)
            init
            (reduce fun (fun init (car coll)) (cdr coll)))
        "Error: not a list"))
        
(define (range limit) ;; not working
    (if (= limit 1)
        limit
        (cons limit (range (dec limit)))))
        
(define (length expr)
    (if (list? expr)
        (if (null? expr)
            0
            (+ 1 (length (cdr expr))))
        "Error: not a list"))

(define (zero? numb)
    (if (number? numb)
        (if (= numb 0)
            #t
            #f)
        "Error: not a number"))

(define (inc numb)
    (+ numb 1))
    
(define (dec numb)
    (- numb 1))

(define (biggest x y)
    (if (> x y)
        x
        y))
        
(define (smallest x y)
    (if (< x y)
        x
        y))

(define (max coll)
    (reduce biggest 0 coll))
    
(define (min coll)
    (reduce smallest 99999999999 coll))
