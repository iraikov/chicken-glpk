
;;
;; Chicken interface to the GLPK API.
;;
;; Copyright 2008-2015 Ivan Raikov
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; A full copy of the GPL license can be found at
;; <http://www.gnu.org/licenses/>.
;;

(module glpk

 (lpx? lpx:empty-problem lpx:make-problem 
       lpx:set-problem-name lpx:get-problem-name
       lpx:set-direction lpx:get-direction lpx:set-class lpx:get-class 
       lpx:add-rows lpx:add-columns lpx:set-row-name lpx:set-column-name
       lpx:get-column-name lpx:get-row-name lpx:get-num-rows lpx:get-num-columns
       lpx:set-row-bounds lpx:set-column-bounds
       lpx:set-objective-coefficient
       lpx:set-column-kind lpx:load-constraint-matrix
       lpx:get-column-primals lpx:get-objective-value

       lpx:message_level lpx:scaling lpx:use_dual_simplex
       lpx:pricing lpx:solution_rounding lpx:iteration_limit
       lpx:iteration_count lpx:branching_heuristic 
       lpx:backtracking_heuristic lpx:use_presolver 
       lpx:relaxation lpx:time_limit
       
       LPX_E_OK LPX_E_EMPTY LPX_E_BADB LPX_E_INFEAS
       LPX_E_FAULT LPX_E_OBJLL LPX_E_OBJUL LPX_E_ITLIM 
       LPX_E_TMLIM LPX_E_NOFEAS LPX_E_INSTAB LPX_E_SING 
       LPX_E_NOCONV LPX_E_NOPFS LPX_E_NODFS
       
       lpx:scale-problem lpx:simplex lpx:integer
       )
		   
 (import scheme chicken foreign srfi-1 srfi-4 )

#>

#include <glpk.h>
#include <chicken-glpk.h>

#define ERR_INVALID_LPX        1
#define ERR_INVALID_LPX_DIR    2
#define ERR_INVALID_LPX_CLASS  3
#define ERR_NEG_IND            4
#define ERR_INVALID_COL_KIND   5
#define ERR_INVALID_MAT_ORD    6


static C_word LPX_p(C_word obj) 
{
  if (C_immediatep(obj)) {
    return C_SCHEME_FALSE;
  } else if (C_block_header(obj) == LPX_TAG) 
  {
    return C_SCHEME_TRUE;
  } else {
    return C_SCHEME_FALSE;
  }
}


static void chicken_Panic (C_char *) C_noret;
static void chicken_Panic (C_char *msg)
{
  C_word *a = C_alloc (C_SIZEOF_STRING (strlen (msg)));
  C_word scmmsg = C_string2 (&a, msg);
  C_halt (scmmsg);
  exit (5); /* should never get here */
}

static void chicken_ThrowException(C_word value) C_noret;
static void chicken_ThrowException(C_word value)
{
  char *aborthook = C_text("\003sysabort");

  C_word *a = C_alloc(C_SIZEOF_STRING(strlen(aborthook)));
  C_word abort = C_intern2(&a, aborthook);

  abort = C_block_item(abort, 0);
  if (C_immediatep(abort))
    chicken_Panic(C_text("`##sys#abort' is not defined"));

#if defined(C_BINARY_VERSION) && (C_BINARY_VERSION >= 8)
  C_word rval[3] = { abort, C_SCHEME_UNDEFINED, value };
  C_do_apply(3, rval);
#else
  C_save(value);
  C_do_apply(1, abort, C_SCHEME_UNDEFINED);
#endif
}

void chicken_LPX_exception (int code, int msglen, const char *msg) 
{
  C_word *a;
  C_word scmmsg;
  C_word list;

  a = C_alloc (C_SIZEOF_STRING (msglen) + C_SIZEOF_LIST(2));
  scmmsg = C_string2 (&a, (char *) msg);
  list = C_list(&a, 2, C_fix(code), scmmsg);
  chicken_ThrowException(list);
}


static C_word check_LPX (C_word obj) 
{
  if (C_immediatep(obj)) 
  {
    chicken_LPX_exception (ERR_INVALID_LPX, 18, "invalid LPX object");
  } else if (C_block_header(obj) == LPX_TAG) 
  {
    return C_SCHEME_UNDEFINED;
  } else {
    chicken_LPX_exception (ERR_INVALID_LPX, 18, "invalid LPX object");
  }
}

<#

(define lpx? (foreign-lambda scheme-object "LPX_p" scheme-object))

(define lpx:empty-problem 
    (foreign-primitive scheme-object ()
#<<END
   C_word result;
   chicken_LPX_t newlpx;
   LPX* lpx;
   
   lpx = lpx_create_prob();

   newlpx.tag = LPX_TAG;
   newlpx.lpx_data = lpx;
   result = (C_word)&newlpx;

   C_return (result);
END
))

;; integer-valued control parameters
(define K_message_level          (foreign-value "LPX_K_MSGLEV" int))
(define K_scaling                (foreign-value "LPX_K_SCALE"  int))
(define K_use_dual_simplex       (foreign-value "LPX_K_DUAL"   int))
(define K_pricing                (foreign-value "LPX_K_PRICE"  int))
(define K_solution_rounding      (foreign-value "LPX_K_ROUND"  int))
(define K_iteration_limit        (foreign-value "LPX_K_ITLIM"  int))
(define K_iteration_count        (foreign-value "LPX_K_ITCNT"  int))
(define K_branching_heuristic    (foreign-value "LPX_K_BRANCH" int))
(define K_backtracking_heuristic (foreign-value "LPX_K_BTRACK" int))
(define K_use_presolver          (foreign-value "LPX_K_PRESOL" int))
;; real-valued control parameters
(define K_relaxation (foreign-value "LPX_K_RELAX" int))
(define K_time_limit (foreign-value "LPX_K_TMLIM" int))

(define lpx_set_int_parameter
    (foreign-lambda* scheme-object ((scheme-object lpx)
				    (int kindex)
				    (int val))
#<<END
     LPX* lp;

     check_LPX(lpx);

     lp = LPX_val(lpx);

     lpx_set_int_parm (lp, kindex, val);

     C_return(lpx);
END
))     

(define lpx_get_int_parameter
    (foreign-lambda* int ((scheme-object lpx)
			      (int kindex))
#<<END
     LPX* lp; int result;

     check_LPX(lpx);

     lp = LPX_val(lpx);

     result = lpx_get_int_parm (lp, kindex);

     C_return(result);
END
))     

(define lpx_set_real_parameter
    (foreign-lambda* scheme-object ((scheme-object lpx)
				    (int kindex)
				    (double val))
#<<END
     LPX* lp;

     check_LPX(lpx);

     lp = LPX_val(lpx);

     lpx_set_real_parm (lp, kindex, val);

     C_return(lpx);
END
))     

(define lpx_get_real_parameter
    (foreign-lambda* double ((scheme-object lpx)
			     (int kindex))
#<<END
     LPX* lp; double result;

     check_LPX(lpx);

     lp = LPX_val(lpx);

     result = lpx_get_real_parm (lp, kindex);

     C_return(result);
END
))     

(define (make-lpx-parameter kindex typ )
  (if (list? typ)
      (lambda (lpx . rest) 
	(if (null? rest) 
	    (let ((val (lpx_get_int_parameter lpx kindex)))
	      (cadr (find (lambda (p) (eq? (car p) val)) typ)))
	    (let* ((val (car rest))
		   (to-val (car (find (lambda (p) (eq? (cadr p) val)) typ))))
	      (lpx_set_int_parameter lpx kindex to-val))))
      (case typ
	((bool boolean)   
	 (lambda (lpx . rest) 
	   (if (null? rest) 
	       (let ((val (lpx_get_int_parameter lpx kindex)))  (= val 1))
	       (let* ((val (car rest)))
		 (if val (lpx_set_int_parameter lpx kindex 1)
		     (lpx_set_int_parameter lpx kindex 0))))))
	((int integer)
	 (lambda (lpx . rest) 
	   (if (null? rest) 
	       (lpx_get_int_parameter lpx kindex)
	       (let* ((val (car rest)))
		 (lpx_set_int_parameter lpx kindex val)))))
	((real)
	 (lambda (lpx . rest) 
	   (if (null? rest) 
	       (lpx_get_real_parameter lpx kindex)
	       (let* ((val (car rest)))
		 (lpx_set_real_parameter lpx kindex val)))))
	(else (error 'make-lpx-parameter "unknown type" typ)))))
	 

;; integer-valued control parameters
(define lpx:message_level
  (make-lpx-parameter K_message_level `((0 none) (1 error) (2 normal) (3 full))))
(define lpx:scaling                
  (make-lpx-parameter K_scaling `((0 none) (1 equilibration) (2 geometric-mean) (3 geometric-mean+equilibration))))
(define lpx:use_dual_simplex       
  (make-lpx-parameter K_use_dual_simplex `bool))
(define lpx:pricing                
  (make-lpx-parameter K_pricing `((0 textbook) (1 steepest-edge))))
(define lpx:solution_rounding      
  (make-lpx-parameter K_solution_rounding `bool))
(define lpx:iteration_limit        
  (make-lpx-parameter K_iteration_limit `int))
(define lpx:iteration_count        
  (make-lpx-parameter K_iteration_count `int))
(define lpx:branching_heuristic    
  (make-lpx-parameter K_branching_heuristic `((0 first) (1 last) (2 driebeck+tomlin))))
(define lpx:backtracking_heuristic 
  (make-lpx-parameter K_backtracking_heuristic `((0 dfs) (1 bfs) (2 best-projection) (3 best-local-bound))))
(define lpx:use_presolver          
  (make-lpx-parameter K_use_presolver `bool))
;; real-valued control parameters
(define lpx:relaxation (make-lpx-parameter K_relaxation `real))
(define lpx:time_limit (make-lpx-parameter K_time_limit `real))
  

(define lpx:set-problem-name 
    (foreign-lambda* scheme-object ((scheme-object lpx)
				    (c-string name))
#<<END
     LPX* lp;

     check_LPX(lpx);

     lp = LPX_val(lpx);

     lpx_set_prob_name (lp, name);

     C_return(lpx);
END
))     


(define lpx:get-problem-name 
    (foreign-lambda* c-string ((scheme-object lpx))
#<<END
     LPX* lp; char *result;

     check_LPX(lpx);

     lp = LPX_val(lpx);

     result = lpx_get_prob_name (lp);

     C_return(result);
END
))     


(define lpx_set_dir 
    (foreign-lambda* scheme-object ((scheme-object lpx)
				    (int dir))
#<<END
     LPX* lp;

     check_LPX(lpx);
     if (!((dir == LPX_MIN) || (dir == LPX_MAX)))
     {
       chicken_LPX_exception (ERR_INVALID_LPX_DIR, 21, "invalid LPX direction");
     }

     lp = LPX_val(lpx);

     lpx_set_obj_dir (lp, dir);

     C_return(lpx);
END
))     

(define LPX_MIN   (foreign-value "LPX_MIN" int) )
(define LPX_MAX   (foreign-value "LPX_MAX" int) )


(define (lpx:set-direction lpx dir)
  (case dir
    ((min minimize)   (lpx_set_dir lpx LPX_MIN))
    ((max maximize)   (lpx_set_dir lpx LPX_MAX))
    (else   (error 'lpx:set-direction "invalid direction" dir))))
    

(define lpx_get_dir 
    (foreign-lambda* int ((scheme-object lpx))
#<<END
     LPX* lp; int dir;

     check_LPX(lpx);

     lp = LPX_val(lpx);

     dir = lpx_get_obj_dir (lp);

     C_return(dir);
END
))     


(define (lpx:get-direction lpx)
  (let ((dir (lpx_get_dir lpx)))
    (cond
     ((= LPX_MIN dir)  'minimize)
     ((= LPX_MAX dir)  'maximize)
     (else   (error 'lpx:get-direction "unknown direction" dir)))))

(define lpx_set_class 
    (foreign-lambda* scheme-object ((scheme-object lpx)
				    (int clss))
#<<END
     LPX* lp;

     check_LPX(lpx);
     if (!((clss == LPX_LP) || (clss == LPX_MIP)))
     {
       chicken_LPX_exception (ERR_INVALID_LPX_CLASS, 17, "invalid LPX class");
     }

     lp = LPX_val(lpx);

     lpx_set_class (lp, clss);

     C_return(lpx);
END
))     

(define LPX_LP   (foreign-value "LPX_LP" int) )
(define LPX_MIP  (foreign-value "LPX_MIP" int) )


(define (lpx:set-class lpx clss)
  (case clss
    ((lp)   (lpx_set_class lpx LPX_LP))
    ((mip)  (lpx_set_class lpx LPX_MIP))
    (else   (error 'lpx:set-class "invalid class" clss))))
    
(define lpx_get_class 
    (foreign-lambda* int ((scheme-object lpx))
#<<END
     LPX* lp; int clss;

     check_LPX(lpx);

     lp = LPX_val(lpx);

     clss = lpx_get_class (lp);

     C_return(clss);
END
))     

(define (lpx:get-class lpx)
  (let ((clss (lpx_get_class lpx)))
    (cond
     ((= LPX_LP clss)  'lp)
     ((= LPX_MIP clss) 'mip)
     (else   (error 'lpx:get-class "unknown class" clss)))))

(define lpx:add-rows 
    (foreign-lambda* scheme-object ((scheme-object lpx)
				    (int nrows))
#<<END
     LPX* lp;

     check_LPX(lpx);
     if (!(nrows > 0))
     {
       chicken_LPX_exception (ERR_NEG_IND, 22, "nrows must be positive");
     }

     lp = LPX_val(lpx);

     lpx_add_rows (lp, nrows);

     C_return(lpx);
END
))     


(define lpx:get-num-rows 
    (foreign-lambda* int ((scheme-object lpx))
#<<END
     LPX* lp; int result;

     check_LPX(lpx);

     lp = LPX_val(lpx);

     result = lpx_get_num_rows (lp);

     C_return(result);
END
))     



(define lpx:add-columns 
    (foreign-lambda* scheme-object ((scheme-object lpx)
				    (int ncols))
#<<END
     LPX* lp;

     check_LPX(lpx);
     if (!(ncols > 0))
     {
       chicken_LPX_exception (ERR_NEG_IND, 22, "ncols must be positive");
     }

     lp = LPX_val(lpx);

     lpx_add_cols (lp, ncols);

     C_return(lpx);
END
))     


(define lpx:get-num-columns 
    (foreign-lambda* int ((scheme-object lpx))
#<<END
     LPX* lp; int result;

     check_LPX(lpx);

     lp = LPX_val(lpx);

     result = lpx_get_num_cols (lp);

     C_return(result);
END
))     


(define lpx:set-row-name 
    (foreign-lambda* scheme-object ((scheme-object lpx)
				    (int i)
				    (c-string name))
#<<END
     LPX* lp;

     check_LPX(lpx);
     if (!(i >= 0))
     {
       chicken_LPX_exception (ERR_NEG_IND, 34, "row index must be zero or positive");
     }

     lp = LPX_val(lpx);

     lpx_set_row_name (lp, i, name);

     C_return(lpx);
END
))     


(define lpx:set-column-name 
    (foreign-lambda* scheme-object ((scheme-object lpx)
				    (int j)
				    (c-string name))
#<<END
     LPX* lp;

     check_LPX(lpx);

     if (!(j >= 0))
     {
       chicken_LPX_exception (ERR_NEG_IND, 37, "column index must be zero or positive");
     }

     lp = LPX_val(lpx);

     lpx_set_col_name (lp, j, name);

     C_return(lpx);
END
))     

(define lpx:get-row-name 
    (foreign-lambda* c-string ((scheme-object lpx)
			       (int i))
#<<END
     LPX* lp; char *result;

     check_LPX(lpx);
     if (!(i >= 0))
     {
       chicken_LPX_exception (ERR_NEG_IND, 34, "row index must be zero or positive");
     }

     lp = LPX_val(lpx);

     result = lpx_get_row_name (lp, i);

     C_return(result);
END
))     


(define lpx:get-column-name 
  (foreign-lambda* c-string ((scheme-object lpx)
			     (int i))
#<<END
     LPX* lp; char *result;

     check_LPX(lpx);
     if (!(i >= 0))
     {
       chicken_LPX_exception (ERR_NEG_IND, 37, "column index must be zero or positive");
     }

     lp = LPX_val(lpx);

     result = lpx_get_col_name (lp, i);

     C_return(result);
END
))     


(define LPX_LO   (foreign-value "LPX_LO" int))
(define LPX_UP   (foreign-value "LPX_UP" int))
(define LPX_DB   (foreign-value "LPX_DB" int))
(define LPX_FX   (foreign-value "LPX_FX" int))
(define LPX_FR   (foreign-value "LPX_FR" int))


(define lpx_set_row_bounds 
    (foreign-lambda* scheme-object ((scheme-object lpx)
				    (int i)
				    (int typx)
				    (double lb) (double ub)
				    )
#<<END
     LPX* lp;

     check_LPX(lpx);
     if (!(i >= 0))
     {
       chicken_LPX_exception (ERR_NEG_IND, 34, "row index must be zero or positive");
     }

     lp = LPX_val(lpx);

     lpx_set_row_bnds (lp, i, typx, lb, ub);

     C_return(lpx);
END
))     


(define lpx_set_col_bounds 
    (foreign-lambda* scheme-object ((scheme-object lpx)
				    (int j)
				    (int typx)
				    (double lb) (double ub)
				    )
#<<END
     LPX* lp;

     check_LPX(lpx);
     if (!(j >= 0))
     {
       chicken_LPX_exception (ERR_NEG_IND, 37, "column index must be zero or positive");
     }

     lp = LPX_val(lpx);

     lpx_set_col_bnds (lp, j, typx, lb, ub);

     C_return(lpx);
END
))     

(define (lpx_set_bounds label f_set_bounds)
  (lambda (lpx i typx . rest)
    (let-optionals rest ((b1 #f) (b2 #f))
      (case typx 
	((free unbounded)  (f_set_bounds lpx i LPX_FR 0 0))
	((lower-bound lower lo lb)  
	 (if (integer? b1)
	     (f_set_bounds lpx i LPX_LO b1 0)
	     (error label "lower bound argument must be an integer")))
	((upper-bound upper up ub)  
	 (if (integer? b1)
	     (f_set_bounds lpx i LPX_UP 0 b1)
	     (error label "upper bound argument must be an integer")))
	((double-bounded double db)  
	 (if (and (integer? b1) (integer? b2))
	     (f_set_bounds lpx i LPX_DB b1 b2)
	     (error label "lower and upper bound arguments must be integers")))
	((fixed fx)  
	 (if (and (integer? b1) (integer? b2))
	     (f_set_bounds lpx i LPX_FX b1 b2)
	     (error label "lower and upper bound arguments must be integers")))
	(else (error label "invalid bound type" typx))))))
      
(define lpx:set-row-bounds     (lpx_set_bounds 'lpx:set-row-bounds lpx_set_row_bounds))
(define lpx:set-column-bounds  (lpx_set_bounds 'lpx:set-column-bounds lpx_set_col_bounds))

(define lpx_get_column_primals
    (foreign-lambda* void ((scheme-object lpx)
			   (int n)
			   (f64vector v)
			   )
#<<END
     LPX* lp;  int j; 

     check_LPX(lpx);

     lp = LPX_val(lpx);

     for (j = 1; j <= n; j++)
       v[j-1] = lpx_get_col_prim (lp, j);

     C_return(C_SCHEME_UNDEFINED);
END
))     


(define lpx:get-objective-value
    (foreign-lambda* double ((scheme-object lpx)
			   )
#<<END
     LPX* lp;  double result; 

     check_LPX(lpx);

     lp = LPX_val(lpx);

     result = lpx_get_obj_val (lp);

     C_return(result);
END
))     
      
(define (lpx:get-column-primals lpx)
  (let* ((n (lpx:get-num-columns lpx))
	 (v (make-f64vector n 0)))
    (lpx_get_column_primals lpx n v)
    v))


(define lpx:set-objective-coefficient 
    (foreign-lambda* scheme-object ((scheme-object lpx)
				    (int j)
				    (double coef) 
				    )
#<<END
     LPX* lp;

     check_LPX(lpx);
     if (!(j >= 0))
     {
       chicken_LPX_exception (ERR_NEG_IND, 37, "column index must be zero or positive");
     }

     lp = LPX_val(lpx);

     lpx_set_obj_coef (lp, j, coef);

     C_return(lpx);
END
))     



(define LPX_CV   (foreign-value "LPX_CV" int))
(define LPX_IV   (foreign-value "LPX_IV" int))

(define lpx_set_column_kind 
    (foreign-lambda* scheme-object ((scheme-object lpx)
				    (int j)
				    (int kind) 
				    )
#<<END
     LPX* lp;

     check_LPX(lpx);
     if (!(j >= 0))
     {
       chicken_LPX_exception (ERR_NEG_IND, 37, "column index must be zero or positive");
     }
     if (!((kind == LPX_CV) || (kind == LPX_IV)))
     {
       chicken_LPX_exception (ERR_INVALID_COL_KIND, 23, "invalid LPX column kind");
     }

     lp = LPX_val(lpx);

     lpx_set_col_kind (lp, j, kind);

     C_return(lpx);
END
))     

(define (lpx:set-column-kind lpx j kind)
  (case kind
    ((integer int iv) (lpx_set_column_kind lpx j LPX_IV))
    ((continuous cont cv) (lpx_set_column_kind lpx j LPX_CV))
    (else (error 'lpx:set-column-kind "invalid column kind" kind))))


(define lpx_load_constraint_matrix 
    (foreign-lambda* scheme-object ((scheme-object lpx)
				    (int nrows)
				    (int ncols)
				    (char order)
				    (f64vector m)
				    (s32vector ia)
				    (s32vector ja)
				    (f64vector ar)
				    )
#<<END
     int i,j,k;
     double x;
     LPX* lp;

     check_LPX(lpx);
     k = 1;

     // row-major
     if ((order == 'r') || (order == 'R'))
     {
       for (i = 0; i < nrows; i++)
       {
	  for (j = 0; j < ncols; j++)
	  {
	       x = m[(i*ncols)+j];
	       if (x != 0)
	       {
		    ia[k]  = i+1;
		    ja[k]  = j+1;
		    ar[k]  = x;
		    k++;
	       }
	  }
       }
     // column-major
     } else if ((order == 'c') || (order == 'C'))
       {
        for (j = 0; j < ncols; j++)
        {
          for (i = 0; i < nrows; i++)
          {
	       x = m[(j*nrows)+i];
	       if (x != 0)
	       {
		    ia[k]  = i+1;
		    ja[k]  = j+1;
		    ar[k]  = x;
		    k++;
	       }
	  }
        }
       } else 
       {
         chicken_LPX_exception (ERR_INVALID_MAT_ORD, 35, "invalid LPX constraint matrix order");
       }
    
       lp = LPX_val(lpx);
       lpx_load_matrix(lp, k-1, ia, ja, ar);


     C_return(lpx);
END
))     

(define (lpx:load-constraint-matrix lpx m nrows ncols . rest)
  (let-optionals rest ((order 'row-major))
    (if (not (positive? nrows)) (error 'lpx:load-constraint-matrix "nrows must be positive"))
    (if (not (positive? ncols)) (error 'lpx:load-constraint-matrix "ncols must be positive"))
    (let ((ia (make-s32vector nrows 0))
	  (ja (make-s32vector ncols 0))
	  (ar (make-f64vector (* ncols nrows) 0))
	  (oc (case order
		((row row-major)  #\r)
		((col column col-major column-major)  #\c)
		(else (error 'lpx:load-constraint-matrix "invalid constraint matrix order")))))
      (lpx_load_constraint_matrix lpx nrows ncols oc m ia ja ar))))


(define LPX_E_OK      (foreign-value "LPX_E_OK" int))        ;;   /* success */
(define LPX_E_EMPTY   (foreign-value "LPX_E_EMPTY" int))     ;;   /* empty problem */
(define LPX_E_BADB    (foreign-value "LPX_E_BADB" int))      ;;   /* invalid initial basis */
(define LPX_E_INFEAS  (foreign-value "LPX_E_INFEAS" int))    ;;   /* infeasible initial solution */
(define LPX_E_FAULT   (foreign-value "LPX_E_FAULT" int))     ;;   /* unable to start the search */
(define LPX_E_OBJLL   (foreign-value "LPX_E_OBJLL" int))     ;;   /* objective lower limit reached */
(define LPX_E_OBJUL   (foreign-value "LPX_E_OBJUL" int))     ;;   /* objective upper limit reached */
(define LPX_E_ITLIM   (foreign-value "LPX_E_ITLIM" int))     ;;   /* iterations limit exhausted */
(define LPX_E_TMLIM   (foreign-value "LPX_E_TMLIM" int))     ;;   /* time limit exhausted */
(define LPX_E_NOFEAS  (foreign-value "LPX_E_NOFEAS" int))    ;;   /* no feasible solution */
(define LPX_E_INSTAB  (foreign-value "LPX_E_INSTAB" int))    ;;   /* numerical instability */
(define LPX_E_SING    (foreign-value "LPX_E_SING" int))      ;;   /* problems with basis matrix */
(define LPX_E_NOCONV  (foreign-value "LPX_E_NOCONV" int))    ;;   /* no convergence (interior) */
(define LPX_E_NOPFS   (foreign-value "LPX_E_NOPFS" int))     ;;   /* no primal feas. sol. (LP presolver) */
(define LPX_E_NODFS   (foreign-value "LPX_E_NODFS" int))     ;;   /* no dual feas. sol. (LP presolver) */


(define lpx:simplex 
    (foreign-lambda* int ((scheme-object lpx)) 
				    
#<<END
     int status;
     LPX* lp;

     check_LPX(lpx);

     lp = LPX_val(lpx);

     status = lpx_simplex (lp);

     C_return(status);
END
))     


(define lpx:integer 
    (foreign-lambda* int ((scheme-object lpx)) 
				    
#<<END
     int status;
     LPX* lp;

     check_LPX(lpx);

     lp = LPX_val(lpx);

     status = lpx_integer (lp);

     C_return(status);
END
))     


(define lpx:scale-problem 
    (foreign-lambda* scheme-object ((scheme-object lpx)) 
				    
#<<END
     LPX* lp;

     check_LPX(lpx);

     lp = LPX_val(lpx);

     lpx_scale_prob (lp);

     C_return(lpx);
END
))     


(define (lpx:make-problem dir pbounds xbounds objcoefs constraints . rest)
  (let-optionals rest ((m-order 'row-major))
   (let* ((lpx    (lpx:empty-problem))
	  (lpx    (lpx:set-direction lpx dir ))
	  (nrows  (length pbounds ))
	  (ncols  (length xbounds ))
	  (lpx    (lpx:add-rows lpx nrows ))
	  (lpx    (lpx:add-columns lpx ncols )))
     (fold (lambda (pi i)
	     (apply lpx:set-row-bounds (cons* lpx i pi))
	     (+ 1 i))
	   1 pbounds)
     (fold (lambda (xi ci i)
	     (lpx:set-objective-coefficient lpx i ci)
	     (apply lpx:set-column-bounds (cons* lpx i xi))
	     (+ 1 i))
	   1 xbounds objcoefs)
     (let ((lpx (lpx:load-constraint-matrix lpx constraints nrows ncols m-order)))
       lpx))))

)
