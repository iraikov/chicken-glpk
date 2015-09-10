;;
;; Example from section 1.3 in the GLPK reference manual:
;;
;; maximize
;;   z = (10 * x1) + (6 * x2) + (4 * x3)
;;
;; subject to
;;
;;    x1 + x2 + x3 <= 100
;;    (10 * x1) + (4 * x2) + (5 * x3) <= 600
;;    ( 2 * x1) + (2 * x2) + (6 * x3) <= 300
;;
;; where
;;
;;    x1 >= 0, x2 >= 0, x3 >= 0

(require-extension srfi-1 srfi-4 glpk)
(import srfi-1 srfi-4 glpk)


;; Auxiliary variables (rows)
;;
;;  p = x1 + x2 + x3
;;  q = (10 * x1) + (4 * x2) + (5 * x3)
;;  r = ( 2 * x1) + (2 * x2) + (6 * x3)
;;
;;  -inf < p <= 100
;;  -inf < q <= 600
;;  -inf < r <= 300

(define pbounds `((upper-bound 100) (upper-bound 600) (upper-bound 300)))

;; Structural variables (columns)
;;
;;  0 <= x1 < +inf
;;  0 <= x2 < +inf
;;  0 <= x3 < +inf

(define xbounds  `((lower-bound 0) (lower-bound 0) (lower-bound 0)))

;; Objective coefficients: 10, 6, 4

(define objcoefs (list 10 6 4))

;; Constraints matrix (in row-major order)
;; 
;;   1  1   1
;;  10  4   5
;;   2  2   6

(define constraints (f64vector 1 1 1 10 4 5 2 2 6))

;; Create the problem definition & run the solver
(let ((lpp (lpx:make-problem 'maximize pbounds xbounds objcoefs constraints)))
  (lpx:scale-problem lpp)
  (lpx:use_presolver lpp #t)
  (let ((status (lpx:simplex lpp)))
    (assert (= status 200))
    (assert (< (abs (- (lpx:get-objective-value lpp) 733.33333333333333)) 1e-12))
    (assert (every (lambda (x y) (< (abs (- x y)) 1e-12))
		   (f64vector->list (lpx:get-column-primals lpp))
		   '(33.333333333333333 66.66666666666667 0.0)))
    ))
