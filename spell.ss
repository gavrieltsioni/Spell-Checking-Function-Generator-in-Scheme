;;BY GAVRIEL TSIONI (150005575)
;; -----------------------------------------------------
;; HELPER FUNCTIONS

;;Test function which will check if all words in a dictionary appear as true
(define check-all-words
  (lambda (checker dictionary)
    (cond ((null? dictionary) (display "done"))
          ((eq? #f (checker (car dictionary))) (display "found error") (check-all-words checker (cdr dictionary)))
          (else (check-all-words checker (cdr dictionary)))
          )))

;;This function uses hash to call a hashfunction on every word in the dict
(define bitvector-create
  (lambda (hashfunctionlist dict)
    (cond
      ((eq? (cdr hashfunctionlist) '()) (hash (car hashfunctionlist) dict '()))
      (else (hash (car hashfunctionlist) dict (bitvector-create (cdr hashfunctionlist) dict)))
    )
  )
)

;;This function will hash every word in a dictionary and add the result to the ilist
(define hash
  (lambda (hashfunc dict ilist)
    (cond
      ((eq? (cdr dict) '()) (append ilist (list (hashfunc (car dict)))))
      (else (cons (hashfunc (car dict)) (hash hashfunc (cdr dict) ilist)))
      )
))

;;This function is used by reduce in checkword. It compares two elements together and returns t if they are equal
(define comp
  (lambda (x y)
    (cond
      ((or (eq? y #t) (eq? x #t)) #t)
      ((= x y) #t)
      ((not (eq? x y)) y)
      )
    )
  )    

;;This function uses reduce to see if a given word is in a given bitvector when using given hash functions.
(define checkword
  (lambda (hashfunctionlist word bitvector)
    (cond
      ((null? (cdr hashfunctionlist))
         (cond
           ((eq? (reduce comp bitvector ((car hashfunctionlist) word)) #t) #t)
           (else #f)
           )
       )
      (else
         (and (reduce comp bitvector ((car hashfunctionlist) word)) (checkword (cdr hashfunctionlist) word bitvector))
       )
      )
    )
  )

;; -----------------------------------------------------
;; KEY FUNCTION

(define key
  (lambda (w)
    (cond
      ((eq? w '()) 5387)
      ((eq? (cdr w) '()) (+ (* 31 5387) (ctv (car w))))
      (else (+ (* 31 (key (cdr w))) (ctv (car w))))
     )
))

;; -----------------------------------------------------
;; EXAMPLE KEY VALUES
;;   (key '(h e l l o))     = 154238504134
;;   (key '(w a y))         = 160507203 
;;   (key '(r a i n b o w)) = 148230379423562

;; -----------------------------------------------------
;; HASH FUNCTION GENERATORS

(define gen-hash-division-method
  (lambda (size) ;; range of values: 0..size-1
     (lambda (word)
       (modulo (key word) size)
      )
))

(define gen-hash-multiplication-method
  (lambda (size) ;; range of values: 0..size-1
    (lambda (word)
      (truncate (* size (- (* (key word) A) (truncate (* (key word) A)))))
    )
))

;; -----------------------------------------------------
;; EXAMPLE HASH FUNCTIONS AND HASH FUNCTION LISTS

(define hash-1 (gen-hash-division-method 70111))
(define hash-2 (gen-hash-division-method 89997))
(define hash-3 (gen-hash-multiplication-method 700224))
(define hash-4 (gen-hash-multiplication-method 900))

(define hashfl-1 (list hash-1 hash-2 hash-3 hash-4))
(define hashfl-2 (list hash-1 hash-3))
(define hashfl-3 (list hash-2 hash-3))

;; -----------------------------------------------------
;; SPELL CHECKER GENERATOR

;;Creates a bitvector and then generates the spell check function, using checkword to actually check the given word.
(define gen-checker
  (lambda (hashfunctionlist dict)
    (define bitvector (bitvector-create hashfunctionlist dict))
    (lambda (word)
      (checkword hashfunctionlist word bitvector)
    )
))

;; -----------------------------------------------------
;; EXAMPLE SPELL CHECKERS

(define checker-1 (gen-checker hashfl-1 dictionary))
(define checker-2 (gen-checker hashfl-2 dictionary))
(define checker-3 (gen-checker hashfl-3 dictionary))

;; EXAMPLE APPLICATIONS OF A SPELL CHECKER
;;
;;  (checker-1 '(a r g g g g)) ==> #f
;;  (checker-2 '(h e l l o)) ==> #t

