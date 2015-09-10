;;
;; Two Mines Linear programming example from
;;
;; http://people.brunel.ac.uk/~mastjjb/jeb/or/basicor.html#twomines
;; 


;; Two Mines Company

;; The Two Mines Company owns two different mines that produce an ore
;; which, after being crushed, is graded into three classes: high,
;; medium and low-grade. The company has contracted to provide a
;; smelting plant with 12 tons of high-grade, 8 tons of medium-grade
;; and 24 tons of low-grade ore per week. The two mines have different
;; operating characteristics as detailed below.
;; 
;; Mine    Cost per day ($'000)    Production (tons/day)                
;;                                High    Medium    Low
;; X       180                     6       3         4
;; Y       160                     1       1         6
;;
;;                                 Production (tons/week)
;;                                High    Medium    Low
;; Contract                       12       8        24
;;
;; How many days per week should each mine be operated to fulfill the
;; smelting plant contract?
;;

(require-extension srfi-4)
(require-extension glpk)

;; (1) Unknown variables
;;
;;      x = number of days per week mine X is operated
;;      y = number of days per week mine Y is operated
;;
;; (2) Constraints
;;
;;
;;    * ore production constraints - balance the amount produced with
;;      the quantity required under the smelting plant contract
;;
;;      High    6x + 1y >= 12
;;      Medium  3x + 1y >= 8
;;      Low     4x + 6y >= 24
;;
;; (3) Objective
;;
;;     The objective is to minimise cost which is given by 180x + 160y.
;;
;;     minimise  180x + 160y
;;     subject to
;;         6x + y >= 12
;;         3x + y >= 8
;;         4x + 6y >= 24
;;         x <= 5
;;         y <= 5
;;         x,y >= 0
;;
;; (4) Auxiliary variables (rows)
;;
;;  p = 6x + y
;;  q = 3x + y
;;  r = 4x + 6y
;;
;;  12 <= p < +inf
;;   8 <= q < +inf
;;  24 <= r < +inf

(define pbounds `((lower-bound 12) (lower-bound 8) (lower-bound 24)))

;; (5) Structural variables (columns)
;;
;;  0 <= x <= 5
;;  0 <= y <= 5

(define xbounds  `((double-bounded 0 5) (double-bounded 0 5)))

;; (6) Objective coefficients: 180, 160

(define objcoefs (list 180 160))


;; Constraints matrix (in row-major order)
;; 
;;   6  1   
;;   3  1   
;;   4  6   

(define constraints (f64vector 6 1 3 1 4 6))

;; Create the problem definition & run the solver
(let ((lpp (make-problem 'minimize pbounds xbounds objcoefs constraints)))
  (scale-problem lpp)
  (use_presolver lpp #t)
  (let ((status (simplex lpp)))
    (print "solution status = " status)
    (print "objective value = " (get-objective-value lpp))
    (print "primals = " (get-column-primals lpp))))
