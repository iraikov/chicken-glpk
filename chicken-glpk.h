
/* 

;;
;; Chicken interface to the GLPK API.
;;
;; Copyright 2008-2015 Ivan Raikov.
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

*/


#include <glpk.h>
#include <chicken.h>

typedef struct chicken_GLP_struct {
     C_header tag;
     void *glp_data;
} chicken_GLP_t;

static const C_header GLP_TAG = 
     ((sizeof(chicken_GLP_t) - sizeof(C_header)) / sizeof(C_word)) | C_POINTER_TYPE;

#define GLP_val(x) (C_c_pointer_nn(x))
