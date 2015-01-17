#include <Rcpp.h>

using namespace Rcpp;


#include<vector>
#include<algorithm>

// [[Rcpp::export]]

	NumericVector rcpp_groupsum(IntegerVector cat_vec, NumericVector val_vec) {
		// init
		IntegerVector ids_vec=na_omit(sort_unique(cat_vec));
		IntegerVector levels_vec=match(cat_vec,ids_vec)-1;
		NumericVector ret_vec(ids_vec.size());

		// main
		for (int i=0; i<cat_vec.size(); ++i) {
			if (!IntegerVector::is_na(cat_vec[i]) && !NumericVector::is_na(val_vec[i])) {
				ret_vec[levels_vec[i]]+=val_vec[i];
			}
		}
		
		// exports
		ret_vec.attr("ids")=ids_vec;
		return(ret_vec);
	}

