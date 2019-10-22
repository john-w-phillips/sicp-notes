;; statements
(define all-statements '(((betty 3) . #t) ((kitty 2) . #f) ((ethel 1) . #f) ((joan 2) . #t) ((joan 3) . #f) ((ethel 5) . #t) ((kitty 2) . #f) ((mary 4) . #t) ((betty 1) . #f) ((mary 4) . #t)))
;; small statement set
(define small '(((betty 3) . #t) ((kitty 2) . #f)))
;; medium statement set
(define med '(((betty 3) . #t) ((kitty 2) . #f) ((ethel 1) . #f)))
;; medium large statement set
(define med-large '(((betty 3) . #t) ((kitty 2) . #f) ((ethel 1) . #f) ((joan 2) . #t)))
;; medium large without Joan T
(define no-multiple-joan '(((betty 3) . #t) ((kitty 2) . #f) ((ethel 1) . #f) ((joan 3) . #f)))
;; medium large with ethel 5 T
(define medium-large-ethel '(((betty 3) . #t) ((kitty 2) . #f) ((ethel 1) . #f)  ((joan 3) . #f) ((ethel 5) . #t)))
;; medium large with only a single statement per person
(define single-per-person '(((betty 3) . #t) ((kitty 2) . #f) ((ethel 1) . #f) ((joan 2) . #t)  ((kitty 2) . #f) ((mary 4) . #t)  ((mary 4) . #t)))
;; medium large with a single statement _per place_.
(define single-per-place '(((betty 3) . #t)  ((kitty 2) . #f) ((joan 2) . #f)))

(define (test-require placings)
  (display placings)
  (display "\n")
  (require (eq? (list-ref placings 0) 'kitty))
  (require (eq? (list-ref placings 1) 'joan)))

(let ((placings2 (list (amb 'ethel 'joan 'kitty 'mary)
		       (amb 'ethel 'joan 'kitty 'mary))))
  (test-require placings2)
  placings2)

(let ((statements (amb 'ethel 'joan 'kitty 'mary)))
  (display statements)
  (display " ")
  (require (eq? statements 'mary))
  statements)
(let ((statements single-per-place)
      (placings2 placings))
  (require-placings-accurate placings2 statements)
  placings2)
  
