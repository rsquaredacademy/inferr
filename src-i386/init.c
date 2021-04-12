#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _inferr_gvar(SEXP, SEXP);
extern SEXP _inferr_nsignC(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_inferr_gvar",   (DL_FUNC) &_inferr_gvar,   2},
    {"_inferr_nsignC", (DL_FUNC) &_inferr_nsignC, 1},
    {NULL, NULL, 0}
};

void R_init_inferr(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}