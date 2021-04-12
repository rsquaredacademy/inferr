#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double nsignC(NumericVector x) {
    
    int n = x.size();
    int count = 1;
    
    for(int i = 1; i < n; i++) {
        int l = i - 1;
        if(x[i] != x[l]) {
            count++;
        }
    }
    
    return count;
}




