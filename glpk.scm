
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

 (glp? empty-problem make-problem 
       set-problem-name get-problem-name
       set-direction get-direction  
       add-rows add-columns set-row-name set-column-name
       get-column-name get-row-name get-num-rows get-num-columns
       set-row-bounds set-column-bounds
       set-objective-coefficient
       set-column-kind load-constraint-matrix
       get-column-primals get-objective-value

       GLP_EBADB GLP_ESING GLP_ECOND GLP_EBOUND GLP_EFAIL GLP_EOBJLL
       GLP_EOBJUL GLP_EITLIM GLP_ETMLIM GLP_ENOPFS GLP_ENODFS GLP_EROOT
       GLP_ESTOP GLP_EMIPGAP GLP_ENOFEAS GLP_ENOCVG GLP_EINSTAB GLP_EDATA
       GLP_ERANGE 

       GLP_SF_GM GLP_SF_EQ GLP_SF_2N GLP_SF_SKIP

       scale-problem simplex integer )

 (import scheme chicken foreign srfi-1 srfi-4 )

#>

#include <glpk.h>
#include <chicken-glpk.h>

#define ERR_INVALID_GLP        1
#define ERR_INVALID_GLP_DIR    2
#define ERR_INVALID_GLP_CLASS  3
#define ERR_NEG_IND            4
#define ERR_INVALID_COL_KIND   5
#define ERR_INVALID_MAT_ORD    6


static C_word GLP_p(C_word obj) 
{
  if (C_immediatep(obj)) {
    return C_SCHEME_FALSE;
  } else if (C_block_header(obj) == GLP_TAG) 
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

void chicken_GLP_exception (int code, int msglen, const char *msg) 
{
  C_word *a;
  C_word scmmsg;
  C_word list;

  a = C_alloc (C_SIZEOF_STRING (msglen) + C_SIZEOF_LIST(2));
  scmmsg = C_string2 (&a, (char *) msg);
  list = C_list(&a, 2, C_fix(code), scmmsg);
  chicken_ThrowException(list);
}


static C_word check_GLP (C_word obj) 
{
  if (C_immediatep(obj)) 
  {
    chicken_GLP_exception (ERR_INVALID_GLP, 18, "invalid GLP object");
  } else if (C_block_header(obj) == GLP_TAG) 
  {
    return C_SCHEME_UNDEFINED;
  } else {
    chicken_GLP_exception (ERR_INVALID_GLP, 18, "invalid GLP object");
  }
}

<#

(define glp? (foreign-lambda scheme-object "GLP_p" scheme-object))

(define empty-problem 
    (foreign-primitive scheme-object ()
#<<END
   C_word result;
   chicken_GLP_t newglp;
   glp_prob* glp;
   
   glp = glp_create_prob();

   newglp.tag = GLP_TAG;
   newglp.glp_data = glp;
   result = (C_word)&newglp;

   C_return (result);
END
))

(define set-problem-name 
    (foreign-lambda* scheme-object ((scheme-object glpval)
				    (c-string name))
#<<END
     glp_prob* glp;

     check_GLP(glpval);

     glp = GLP_val(glpval);

     glp_set_prob_name (glp, name);

     C_return(glp);
END
))     


(define get-problem-name 
    (foreign-lambda* c-string ((scheme-object glpval))
#<<END
     glp_prob* glp; char *result;

     check_GLP(glpval);

     glp = GLP_val(glpval);

     result = glp_get_prob_name (glp);

     C_return(result);
END
))     


(define glp_set_dir 
    (foreign-lambda* scheme-object ((scheme-object glpval)
				    (int dir))
#<<END
     glp_prob* glp;

     check_GLP(glpval);
     if (!((dir == GLP_MIN) || (dir == GLP_MAX)))
     {
       chicken_GLP_exception (ERR_INVALID_GLP_DIR, 21, "invalid GLP direction");
     }

     glp = GLP_val(glpval);

     glp_set_obj_dir (glp, dir);

     C_return(glp);
END
))     

(define GLP_MIN   (foreign-value "GLP_MIN" int) )
(define GLP_MAX   (foreign-value "GLP_MAX" int) )


(define (set-direction glp dir)
  (case dir
    ((min minimize)   (glp_set_dir glp GLP_MIN))
    ((max maximize)   (glp_set_dir glp GLP_MAX))
    (else   (error 'set-direction "invalid direction" dir))))
    

(define glp_get_dir 
    (foreign-lambda* int ((scheme-object glpval))
#<<END
     glp_prob* glp; int dir;

     check_GLP(glpval);

     glp = GLP_val(glpval);

     dir = glp_get_obj_dir (glp);

     C_return(dir);
END
))     


(define (get-direction glp)
  (let ((dir (glp_get_dir glp)))
    (cond
     ((= GLP_MIN dir)  'minimize)
     ((= GLP_MAX dir)  'maximize)
     (else   (error 'get-direction "unknown direction" dir)))))



(define add-rows 
    (foreign-lambda* scheme-object ((scheme-object glpval)
				    (int nrows))
#<<END
     glp_prob* glp;

     check_GLP(glpval);
     if (!(nrows > 0))
     {
       chicken_GLP_exception (ERR_NEG_IND, 22, "nrows must be positive");
     }

     glp = GLP_val(glpval);

     glp_add_rows (glp, nrows);

     C_return(glp);
END
))     

(define get-num-rows 
    (foreign-lambda* int ((scheme-object glpval))
#<<END
     glp_prob* glp; int result;

     check_GLP(glpval);

     glp = GLP_val(glpval);

     result = glp_get_num_rows (glp);

     C_return(result);
END
))     



(define add-columns 
    (foreign-lambda* scheme-object ((scheme-object glpval)
				    (int ncols))
#<<END
     glp_prob* glp;

     check_GLP(glpval);
     if (!(ncols > 0))
     {
       chicken_GLP_exception (ERR_NEG_IND, 22, "ncols must be positive");
     }

     glp = GLP_val(glpval);

     glp_add_cols (glp, ncols);

     C_return(glp);
END
))     


(define get-num-columns 
    (foreign-lambda* int ((scheme-object glpval))
#<<END
     glp_prob* glp; int result;

     check_GLP(glpval);

     glp = GLP_val(glpval);

     result = glp_get_num_cols (glp);

     C_return(result);
END
))     


(define set-row-name 
    (foreign-lambda* scheme-object ((scheme-object glpval)
				    (int i)
				    (c-string name))
#<<END
     glp_prob* glp;

     check_GLP(glpval);
     if (!(i >= 0))
     {
       chicken_GLP_exception (ERR_NEG_IND, 34, "row index must be zero or positive");
     }

     glp = GLP_val(glpval);

     glp_set_row_name (glp, i, name);

     C_return(glp);
END
))     


(define set-column-name 
    (foreign-lambda* scheme-object ((scheme-object glpval)
				    (int j)
				    (c-string name))
#<<END
     glp_prob* glp;

     check_GLP(glpval);

     if (!(j >= 0))
     {
       chicken_GLP_exception (ERR_NEG_IND, 37, "column index must be zero or positive");
     }

     glp = GLP_val(glpval);

     glp_set_col_name (glp, j, name);

     C_return(glp);
END
))     

(define get-row-name 
    (foreign-lambda* c-string ((scheme-object glpval)
			       (int i))
#<<END
     glp_prob* glp; char *result;

     check_GLP(glpval);
     if (!(i >= 0))
     {
       chicken_GLP_exception (ERR_NEG_IND, 34, "row index must be zero or positive");
     }

     glp = GLP_val(glpval);

     result = glp_get_row_name (glp, i);

     C_return(result);
END
))     


(define get-column-name 
  (foreign-lambda* c-string ((scheme-object glpval)
			     (int i))
#<<END
     glp_prob* glp; char *result;

     check_GLP(glpval);
     if (!(i >= 0))
     {
       chicken_GLP_exception (ERR_NEG_IND, 37, "column index must be zero or positive");
     }

     glp = GLP_val(glpval);

     result = glp_get_col_name (glp, i);

     C_return(result);
END
))     


(define GLP_LO   (foreign-value "GLP_LO" int))
(define GLP_UP   (foreign-value "GLP_UP" int))
(define GLP_DB   (foreign-value "GLP_DB" int))
(define GLP_FX   (foreign-value "GLP_FX" int))
(define GLP_FR   (foreign-value "GLP_FR" int))


(define glp_set_row_bounds 
    (foreign-lambda* scheme-object ((scheme-object glpval)
				    (int i)
				    (int typx)
				    (double lb) (double ub)
				    )
#<<END
     glp_prob* glp;

     check_GLP(glpval);
     if (!(i >= 0))
     {
       chicken_GLP_exception (ERR_NEG_IND, 34, "row index must be zero or positive");
     }

     glp = GLP_val(glpval);

     glp_set_row_bnds (glp, i, typx, lb, ub);

     C_return(glp);
END
))     


(define glp_set_col_bounds 
    (foreign-lambda* scheme-object ((scheme-object glpval)
				    (int j)
				    (int typx)
				    (double lb) (double ub)
				    )
#<<END
     glp_prob* glp;

     check_GLP(glpval);
     if (!(j >= 0))
     {
       chicken_GLP_exception (ERR_NEG_IND, 37, "column index must be zero or positive");
     }

     glp = GLP_val(glpval);

     glp_set_col_bnds (glp, j, typx, lb, ub);

     C_return(glp);
END
))     


(define (glp_set_bounds label f_set_bounds)
  (lambda (glp i typx . rest)
    (let-optionals rest ((b1 #f) (b2 #f))
      (case typx 
	((free unbounded)  (f_set_bounds glp i GLP_FR 0 0))
	((lower-bound lower lo lb)  
	 (if (integer? b1)
	     (f_set_bounds glp i GLP_LO b1 0)
	     (error label "lower bound argument must be an integer")))
	((upper-bound upper up ub)  
	 (if (integer? b1)
	     (f_set_bounds glp i GLP_UP 0 b1)
	     (error label "upper bound argument must be an integer")))
	((double-bounded double db)  
	 (if (and (integer? b1) (integer? b2))
	     (f_set_bounds glp i GLP_DB b1 b2)
	     (error label "lower and upper bound arguments must be integers")))
	((fixed fx)  
	 (if (and (integer? b1) (integer? b2))
	     (f_set_bounds glp i GLP_FX b1 b2)
	     (error label "lower and upper bound arguments must be integers")))
	(else (error label "invalid bound type" typx))))))
      
(define set-row-bounds     (glp_set_bounds 'set-row-bounds glp_set_row_bounds))
(define set-column-bounds  (glp_set_bounds 'set-column-bounds glp_set_col_bounds))

(define glp_get_column_primals
    (foreign-lambda* void ((scheme-object glpval)
			   (int n)
			   (f64vector v)
			   )
#<<END
     glp_prob* glp;  int j; 

     check_GLP(glpval);

     glp = GLP_val(glpval);

     for (j = 1; j <= n; j++)
       v[j-1] = glp_get_col_prim (glp, j);

     C_return(C_SCHEME_UNDEFINED);
END
))     


(define get-objective-value
    (foreign-lambda* double ((scheme-object glpval))
#<<END
     glp_prob* glp;  double result; 

     check_GLP(glpval);

     glp = GLP_val(glpval);

     result = glp_get_obj_val (glp);

     C_return(result);
END
))     
      
(define (get-column-primals glp)
  (let* ((n (get-num-columns glp))
	 (v (make-f64vector n 0)))
    (glp_get_column_primals glp n v)
    v))


(define set-objective-coefficient 
    (foreign-lambda* scheme-object ((scheme-object glpval)
				    (int j)
				    (double coef))
#<<END
     glp_prob* glp;

     check_GLP(glpval);
     if (!(j >= 0))
     {
       chicken_GLP_exception (ERR_NEG_IND, 37, "column index must be zero or positive");
     }

     glp = GLP_val(glpval);

     glp_set_obj_coef (glp, j, coef);

     C_return(glp);
END
))     



(define GLP_CV   (foreign-value "GLP_CV" int))
(define GLP_IV   (foreign-value "GLP_IV" int))

(define glp_set_column_kind 
    (foreign-lambda* scheme-object ((scheme-object glpval)
				    (int j)
				    (int kind) 
				    )
#<<END
     glp_prob* glp;

     check_GLP(glpval);
     if (!(j >= 0))
     {
       chicken_GLP_exception (ERR_NEG_IND, 37, "column index must be zero or positive");
     }
     if (!((kind == GLP_CV) || (kind == GLP_IV)))
     {
       chicken_GLP_exception (ERR_INVALID_COL_KIND, 23, "invalid GLP column kind");
     }

     glp = GLP_val(glpval);

     glp_set_col_kind (glp, j, kind);

     C_return(glp);
END
))     

(define (set-column-kind glp j kind)
  (case kind
    ((integer int iv) (glp_set_column_kind glp j GLP_IV))
    ((continuous cont cv) (glp_set_column_kind glp j GLP_CV))
    (else (error 'set-column-kind "invalid column kind" kind))))


(define glp_load_constraint_matrix 
    (foreign-lambda* scheme-object ((scheme-object glpval)
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
     glp_prob* glp;

     check_GLP(glpval);
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
         chicken_GLP_exception (ERR_INVALID_MAT_ORD, 35, "invalid GLP constraint matrix order");
       }
    
       glp = GLP_val(glpval);
       glp_load_matrix(glp, k-1, ia, ja, ar);


     C_return(glp);
END
))     


(define (load-constraint-matrix glp m nrows ncols . rest)
  (let-optionals rest ((order 'row-major))
    (if (not (positive? nrows)) 
        (error 'load-constraint-matrix "nrows must be positive"))
    (if (not (positive? ncols)) 
        (error 'load-constraint-matrix "ncols must be positive"))
    (let ((ia (make-s32vector nrows 0))
	  (ja (make-s32vector ncols 0))
	  (ar (make-f64vector (* ncols nrows) 0))
	  (oc (case order
		((row row-major)  #\r)
		((col column col-major column-major)  #\c)
		(else (error 'load-constraint-matrix "invalid constraint matrix order")))))
      (glp_load_constraint_matrix glp nrows ncols oc m ia ja ar))))



(define GLP_EBADB (foreign-value "GLP_EBADB" int))
(define GLP_ESING (foreign-value "GLP_ESING" int))
(define GLP_ECOND (foreign-value "GLP_ECOND" int))
(define GLP_EBOUND (foreign-value "GLP_EBOUND" int))
(define GLP_EFAIL (foreign-value "GLP_EFAIL" int))
(define GLP_EOBJLL (foreign-value "GLP_EOBJLL" int))
(define GLP_EOBJUL (foreign-value "GLP_EOBJUL" int))
(define GLP_EITLIM (foreign-value "GLP_EITLIM" int))
(define GLP_ETMLIM (foreign-value "GLP_ETMLIM" int))
(define GLP_ENOPFS (foreign-value "GLP_ENOPFS" int))
(define GLP_ENODFS (foreign-value "GLP_ENODFS" int))
(define GLP_EROOT (foreign-value "GLP_EROOT" int))
(define GLP_ESTOP (foreign-value "GLP_ESTOP" int))
(define GLP_EMIPGAP (foreign-value "GLP_EMIPGAP" int))
(define GLP_ENOFEAS (foreign-value "GLP_ENOFEAS" int))
(define GLP_ENOCVG (foreign-value "GLP_ENOCVG" int))
(define GLP_EINSTAB (foreign-value "GLP_EINSTAB" int))
(define GLP_EDATA (foreign-value "GLP_EDATA" int))
(define GLP_ERANGE (foreign-value "GLP_ERANGE" int))



(define simplex 
    (foreign-lambda* int ((scheme-object glpval)
                          (c-pointer parm))
				    
#<<END
     int status;
     glp_prob* glp;

     check_GLP(glpval);

     glp = GLP_val(glpval);


     status = glp_simplex (glp, parm);

     C_return(status);
END
))     


(define integer 
    (foreign-lambda* int ((scheme-object glpval)) 
				    
#<<END
     int status;
     glp_prob* glp;

     check_GLP(glpval);

     glp = GLP_val(glpval);

     status = glp_integer (glp);

     C_return(status);
END
))     


(define GLP_SF_GM (foreign-value "GLP_SF_GM" int)) ;; perform geometric mean scaling;
(define GLP_SF_EQ (foreign-value "GLP_SF_EQ" int)) ;; perform equilibration scaling;
(define GLP_SF_2N (foreign-value "GLP_SF_2N" int)) ;; round scale factors to nearest power of two;
(define GLP_SF_SKIP (foreign-value "GLP_SF_SKIP" int)) ;; skip scaling, if the problem is well scaled.


(define scale-problem 
    (foreign-lambda* scheme-object ((scheme-object glpval) (int flags))
				    
#<<END
     glp_prob* glp;

     check_GLP(glpval);

     glp = GLP_val(glpval);

     glp_scale_prob (glp, flags);

     C_return(glp);
END
))     


(define (make-problem dir pbounds xbounds objcoefs constraints . rest)
  (let-optionals rest ((m-order 'row-major))
   (let* ((glp    (empty-problem))
	  (glp    (set-direction glp dir ))
	  (nrows  (length pbounds ))
	  (ncols  (length xbounds ))
	  (glp    (add-rows glp nrows ))
	  (glp    (add-columns glp ncols )))
     (fold (lambda (pi i)
	     (apply set-row-bounds (cons* glp i pi))
	     (+ 1 i))
	   1 pbounds)
     (fold (lambda (xi ci i)
	     (set-objective-coefficient glp i ci)
	     (apply set-column-bounds (cons* glp i xi))
	     (+ 1 i))
	   1 xbounds objcoefs)
     (let ((glp (load-constraint-matrix glp constraints nrows ncols m-order)))
       glp))))

)
