#include <Rcpp.h>
// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;


#include<vector>
#include<algorithm>

// [[Rcpp::export]]

	NumericVector rcpp_groupcombine(std::vector<NumericVector> group_sums) {
		// init
		std::vector<int> all_ids_vec;
		all_ids_vec.reserve(group_sums[0].size() * group_sums.size() * 10);
		std::vector<double> all_vals_vec;
		all_vals_vec.reserve(group_sums[0].size() * group_sums.size() * 10);
		
		// preliminary processing
		IntegerVector int_tmp;
		for (std::size_t i=0; i!=group_sums.size(); ++i) {
			int_tmp=group_sums[i].attr("ids");
			for (auto j=int_tmp.begin(); j!=int_tmp.end(); ++j)
				all_ids_vec.push_back(*j);
			for (auto j=group_sums[i].begin(); j!=group_sums[i].end(); ++j)
				all_vals_vec.push_back(*j);
		}
		int_tmp=wrap(all_ids_vec);
		
		// main processing
		IntegerVector ids_vec=wrap(na_omit(sort_unique(int_tmp)));
		IntegerVector levels_vec=match(int_tmp,ids_vec)-1;
		NumericVector ret_vec(ids_vec.size());
		
		for (std::size_t i=0; i!=all_vals_vec.size(); ++i) {
			ret_vec[levels_vec[i]]+=all_vals_vec[i];
		}

		// exports
		ret_vec.attr("ids") = ids_vec;
		return(ret_vec);
	}

