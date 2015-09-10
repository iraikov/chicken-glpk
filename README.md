
GNU Linear Programming Kit (GLPK).

GLPK (http://www.gnu.org/software/glpk/) is a package for solving
linear programming and mixed integer programming problems.

The Chicken GLPK library provides a Scheme interface to a large subset
of the GLPK procedures for problem setup and solving.  Below is a list
of procedures that are included in this egg, along with brief
descriptions. This egg has been tested with GLPK version 4.53.

## Problem constructors and predicates

<procedure>empty-problem:: () -> LPX</procedure>

This procedure creates a new problem that has no rows or columns.

<procedure>make-problem:: DIR * PBOUNDS * XBOUNDS * OBJCOEFS * CONSTRAINTS * [ORDER] -> LPX</procedure>

This procedure creates a new problem with the specified parameters. 

* Argument {{DIR}} specifies the optimization direction flag. It can be one of {{'maximize}} or  {{'minimize}}. 
* Argument {{PBOUNDS}} is a list that specifies the type and bounds for each row of the problem object. Each element of this list can take one of the following forms: 

<table class="symbol-table"><tr><td>'(unbounded)</td><td>Free (unbounded) variable, {{-Inf <= x <= +Inf}}</td></tr>
<tr><td>'(lower-bound LB)</td><td>Variable with lower bound, {{LB <= x <= +Inf}}</td></tr>
<tr><td>'(upper-bound UB)</td><td>Variable with upper bound, {{-Inf <= x <= UB}}</td></tr>
<tr><td>'(double-bounded LB UB)</td><td>Double-bounded variable, {{LB <= x <= UB}}</td></tr>
<tr><td>'(fixed LB UB)</td><td>Fixed variable, {{LB = x = UB}}</td></tr>
</table>

* Argument {{XBOUNDS}} is a list that specifies the type and bounds for each column (structural variable) of the problem object. Each element of this list can take one of the forms described for parameter {{PBOUNDS}}. 
* Argument {{OBJCOEFS}} is a list that specifies the objective coefficients for each column (structural variable). This list must be of the same length as {{XBOUNDS}}. 
* Argument {{OBJCOEFS}} is a list that specifies the objective coefficients for each column (structural variable). 
* Argument {{CONSTRAINTS}} is an SRFI-4 {{f64vector}} that represents the problem's constraint matrix (in row-major or column-major order). 
* Optional argument {{ORDER}} specifies the element order of the constraints matrix. It can be one of {{'row-major}} or {{'column-major}}. 


<procedure>lpx?:: OBJECT -> BOOL</procedure>

Returns true if the given object was created by {{empty-problem}} or {{make-problem}}, false otherwise. 



## Problem accessors and modifiers


<procedure>set-problem-name:: LPX * NAME -> LPX</procedure>
Sets problem name. 


<procedure>get-problem-name:: LPX -> NAME</procedure>
Returns the name of the given problem. 


<procedure>set-direction:: LPX * DIR -> LPX</procedure>
Specifies the optimization direction flag, which can be one of {{'maximize}} or  {{'minimize}}. 


<procedure>get-direction:: LPX -> DIR</procedure>
Returns the optimization direction for the given problem. 


<procedure>set-class:: LPX * CLASS -> LPX</procedure>
Sets problem class (linear programming or mixed-integer programming. Argument {{CLASS}} can be one of {{'lp}} or {{'mip}}. 


<procedure>get-class:: LPX -> CLASS</procedure>
Returns the problem class. 


<procedure>add-rows:: LPX * N -> LPX</procedure>
This procedure adds {{N}} rows (constraints) to the given problem. Each new row is initially unbounded and has an empty list of constraint coefficients. 


<procedure>add-columns:: LPX * N -> LPX</procedure>
This procedure adds {{N}} columns (structural variables) to the given problem. 


<procedure>set-row-name:: LPX * I * NAME -> LPX</procedure>
Sets the name of row {{I}}.


<procedure>set-column-name:: LPX * J * NAME -> LPX</procedure>
Sets the name of column {{J}}.


<procedure>get-row-name:: LPX * I -> NAME</procedure>
Returns the name of row {{I}}.


<procedure>get-column-name:: LPX * J -> NAME</procedure>
Returns the name of column {{J}}.


<procedure>get-num-rows:: LPX -> N</procedure>
Returns the current number of rows in the given problem. 


<procedure>get-num-columns:: LPX -> N</procedure>
Returns the current number of columns in the given problem. 


<procedure>set-row-bounds:: LPX * I * BOUNDS -> LPX</procedure>
Sets bounds for row {{I}} in the given problem. Argument {{BOUNDS}} specifies the type and bounds for the specified row. It can take one of the following forms: 

<table class="symbol-table"><tr><td>'(unbounded)</td><td>Free (unbounded) variable, {{-Inf <= x <= +Inf}}</td></tr>
<tr><td>'(lower-bound LB)</td><td>Variable with lower bound, {{LB <= x <= +Inf}}</td></tr>
<tr><td>'(upper-bound UB)</td><td>Variable with upper bound, {{-Inf <= x <= UB}}</td></tr>
<tr><td>'(double-bounded LB UB)</td><td>Double-bounded variable, {{LB <= x <= UB}}</td></tr>
<tr><td>'(fixed LB UB)</td><td>Fixed variable, {{LB = x = UB}}</td></tr>
</table>



<procedure>set-column-bounds:: LPX * J * BOUNDS -> LPX</procedure>
Sets bounds for column {{J}} in the given problem. Argument {{BOUNDS}} specifies the type and bounds for the specified column. It can take one of the following forms: 

<table class="symbol-table"><tr><td>'(unbounded)</td><td>Free (unbounded) variable, {{-Inf <= x <= +Inf}}</td></tr>
<tr><td>'(lower-bound LB)</td><td>Variable with lower bound, {{LB <= x <= +Inf}}</td></tr>
<tr><td>'(upper-bound UB)</td><td>Variable with upper bound, {{-Inf <= x <= UB}}</td></tr>
<tr><td>'(double-bounded LB UB)</td><td>Double-bounded variable, {{LB <= x <= UB}}</td></tr>
<tr><td>'(fixed LB UB)</td><td>Fixed variable, {{LB = x = UB}}</td></tr>
</table>



<procedure>set-objective-coefficient:: LPX * J * COEF -> LPX</procedure>
Sets the objective coefficient at column {{J}} (structural variable). 


<procedure>set-column-kind:: LPX * J * KIND -> LPX</procedure>
Sets the kind of column {{J}} (structural variable). Argument {{KIND}} can be one of the following: 

<table class="symbol-table"><tr><td>'iv</td><td>integer variable</td></tr>
<tr><td>'cv</td><td>continuous variable</td></tr>
</table>



<procedure>load-constraint-matrix:: LPX * F64VECTOR * NROWS * NCOLS [* ORDER] -> LPX</procedure>
Loads the constraint matrix for the given problem. The constraints matrix is represented as an SRFI-4 {{f64vector}} (in row-major or column-major order). Optional argument {{ORDER}} specifies the element order of the constraints matrix. It can be one of {{'row-major}} or {{'column-major}}. 


<procedure>get-column-primals:: LPX -> F64VECTOR</procedure>
Returns the primal values of all structural variables (columns). 


<procedure>get-objective-value:: LPX -> NUMBER</procedure>
Returns the current value of the objective function. 


## Problem control parameters


The procedures in this section retrieve or set control parameters of GLPK problem object. If a procedure is invoked only with a problem object as an argument, it will return the value of its respective control parameter. If it is invoked with an additional argument, that argument is used to set a new value for the control parameter. 

<procedure>message_level:: LPX [ * (none | error | normal | full)] -> LPX | VALUE</procedure>
Level of messages output by solver routines.


<procedure>scaling:: LPX [ * (none | equilibration | geometric-mean | geometric-mean+equilibration)] -> LPX | VALUE</procedure>
Scaling option.


<procedure>use_dual_simplex:: LPX [ * BOOL] -> LPX | VALUE</procedure>
Dual simplex option.


<procedure>pricing:: LPX [ * (textbook | steepest-edge)] -> LPX | VALUE</procedure>
Pricing option (for both primal and dual simplex).


<procedure>solution_rounding:: LPX [ * BOOL] -> LPX | VALUE</procedure>
Solution rounding option.


<procedure>iteration_limit:: LPX [ * INT] -> LPX | VALUE</procedure>
Simplex iteration limit.


<procedure>iteration_count:: LPX [ * INT] -> LPX | VALUE</procedure>
Simplex iteration count.


<procedure>branching_heuristic:: LPX [ * (first | last | driebeck+tomlin)] -> LPX | VALUE</procedure>
Branching heuristic option (for MIP only).


<procedure>backtracking_heuristic:: LPX [ * (dfs | bfs | best-projection | best-local-bound)] -> LPX | VALUE</procedure>
Backtracking heuristic option (for MIP only).


<procedure>use_presolver:: LPX [ * BOOL] -> LPX | VALUE</procedure>
Use the LP presolver.


<procedure>relaxation:: LPX [ * REAL] -> LPX | VALUE</procedure>
Relaxation parameter used in the ratio test.


<procedure>time_limit:: LPX [ * REAL] -> LPX | VALUE</procedure>
Searching time limit, in seconds.


## Scaling & solver procedures


<procedure>scale-problem:: LPX -> LPX</procedure>
This procedure performs scaling of of the constraints matrix in order to improve its numerical properties. 


<procedure>simplex:: LPX -> STATUS</procedure>
This procedure solves the given LP problem using the simplex method.  It can return one of the following status codes: 

<table class="symbol-table"><tr><td>LPX_E_OK</td><td>the LP problem has been successfully solved</td></tr>
<tr><td>LPX_E_BADB</td><td>Unable to start
the search, because the initial basis specified in the problem object
is invalid--the number of basic (auxiliary and structural) variables
is not the same as the number of rows in the problem object. </td></tr>
<tr><td>LPX_E_SING</td><td>Unable to start
the search, because the basis matrix corresponding to the initial
basis is singular within the working precision. </td></tr>
<tr><td>LPX_E_COND</td><td>Unable to start
the search, because the basis matrix corresponding to the initial
basis is ill-conditioned, i.e. its condition number is too large. </td></tr>
<tr><td>LPX_E_BOUND</td><td>Unable to start
the search, because some double-bounded (auxiliary or structural)
variables have incorrect bounds. </td></tr>
<tr><td>LPX_E_FAIL</td><td>The search was
prematurely terminated due to the solver failure. </td></tr>
<tr><td>LPX_E_OBJLL</td><td>The search was
prematurely terminated, because the objective function being maximized
has reached its lower limit and continues decreasing (the dual simplex
only). </td></tr>
<tr><td>LPX_E_OBJUL</td><td>The search was
prematurely terminated, because the objective function being minimized
has reached its upper limit and continues increasing (the dual simplex
only). </td></tr>
<tr><td>LPX_E_ITLIM</td><td>The search was
prematurely terminated, because the simplex iteration limit has been
exceeded. </td></tr>
<tr><td>LPX_E_TMLIM</td><td>The search was
prematurely terminated, because the time limit has been exceeded. </td></tr>
<tr><td>LPX_E_NOPFS</td><td>The LP problem
instance has no primal feasible solution (only if the LP presolver is used). </td></tr>
<tr><td>LPX_E_NODFS</td><td>The LP problem
instance has no dual feasible solution (only if the LP presolver is used).</td></tr>
</table>



<procedure>integer:: LPX -> STATUS</procedure>
Solves an MIP problem using the branch-and-bound method. 


## Examples

```scheme
 ;;
 ;; Two Mines Linear programming example from
 ;;
 ;; http://people.brunel.ac.uk/~mastjjb/jeb/or/basicor.html#twomines
 ;; 
 
 ;; Two Mines Company
 ;;
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
```

## License

>
> Copyright 2008-2015 Ivan Raikov
> 
> This program is free software: you can redistribute it and/or modify
> it under the terms of the GNU General Public License as published by
> the Free Software Foundation, either version 3 of the License, or (at
> your option) any later version.
> 
> This program is distributed in the hope that it will be useful, but
> WITHOUT ANY WARRANTY; without even the implied warranty of
> MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
> General Public License for more details.
> 
> A full copy of the GPL license can be found at
> <http://www.gnu.org/licenses/>.
>
