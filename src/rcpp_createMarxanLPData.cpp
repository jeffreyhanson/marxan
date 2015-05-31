#include <Rcpp.h>
// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;

      #include <vector>
      #include <algorithm>
      #include <Rcpp.h>
      

// [[Rcpp::export]]

	Rcpp::List rcpp_createMarxanLPData(
		std::vector<std::size_t>& pu_id, 
		std::vector<double>& pu_cost, 
		std::vector<std::size_t>& pu_status, 
		std::vector<std::size_t>& species_id, 
		std::vector<double>& species_spf,
		std::vector<std::size_t>& species_target, 
		std::vector<std::size_t>& puvspecies_pu, 
		std::vector<std::size_t>& puvspecies_species, 
		std::vector<double>& puvspecies_amount, 
		std::vector<std::size_t>& bound_id1, 
		std::vector<std::size_t>& bound_id2,
		std::vector<double>& bound_amount,
		double BLM=0.0,
		double MISSLEVEL=1.0;
		std::size_t n_preallocate=1000000, 
		std::string problem="ILP"		
	) {
		//// initialization
		/// declare variables and preallocate memory
		// main variables
		std::vector<std::size_t> model_A_row;
		std::vector<std::size_t> model_A_col;
		std::vector<double> model_A_val;
		std::vector<double> model_rhs;
		std::vector<double> model_obj;
		std::vector<std::string> model_sense;
		std::vector<std::string> model_vtypes;
		
		model_A_row.reserve(n_preallocate);
		model_A_col.reserve(n_preallocate);
		model_A_row.reserve(n_preallocate);
		model_rhs.reserve(n_preallocate);
		model_obj.reserve(n_preallocate);
		model_sense.reserve(n_preallocate);
		model_vtypes.reserve(n_preallocate);
		
		// temporary variables
		std::size_t n_constraints=-1;
		std::vector<std::size_t> sortedPuIndices;
		std::vector<std::size_t> softSpp;
		std::vector<std::size_t> firmSpp;
		std::vector<std::size_t> edge;
		std::vector<std::size_t> notEdge;
		std::vector<std::size_t> puFree;
		std::vector<std::size_t> puLockedIn;
		std::vector<std::size_t> puLockedOut;
		
		softSpp.reserve(species_id.size());
		firmSpp.reserve(species_id.size());
		edge.reserve(bound_id1.size());
		notEdge.reserve(bound_id1.size());
		puFree.reserve(pu_id.size());
		puLockedIn.reserve(pu_id.size());
		puLockedOut.reserve(pu_id.size());
		
		//// preliminary processing
		// memoisation
		pu_id_std_sorted.resize(pu_id.size());
		std::iota(pu_id_std_sorted.begin(), pu_id_std_sorted.end(), 0);
		std::sort(pu_id_std_sorted.beign(), pu_id_std_sorted.end() [&](auto i, auto j){return(pu_status[i] < pu_status[j]);});
		
		bound_id1_std=match(pu_id, bound_id1);
		bound_id2_std=match(pu_id, bound_id2);
		
		puvspecies_pu_std=match(pu_id, puvspecies_pu);
		puvspecies_species_std=match(species_id, puvspecies_species);
		
		// extract indices
		for (std::size_t i=0; i<species_id.size(); ++i) {
			if (std::isfinite(species_spf[i])) {
				firmSpp.push_back(i);
			} else {
				softSpp.push_back(i);
			}
		}
		for (std::size_t i=0; i<bound_id1.size(); ++i) {
			if (bound_id1[i]==bound_id2[i]) {
				edge.push_back(i);
			} else {
				notEdge.push_back(i);
			}
		}
		
		//// main processing
		// status 0/1 pu costs in objective function vector
		std::size_t i=0;
		for (; (i<pu_id_std_sorted.size() & pu_status[pu_id_std_sorted[i]]<2); ++i) {
			model_obj.push_back(pu_cost[pu_id_std_sorted[i]]);
		}
		
		// pu decision variables and constraints
		if ((BLM-0.0)>0.00001) {
			/// status 2-3 pu costs and constraints 
			for (; (i<pu_id_std_sorted.size() & pu_status[pu_id_std_sorted[i]]==2); ++i) {
				/// status  2 pus
				// costs in objective function vector
				model_obj.push_back(pu_cost[pu_id_std_sorted[i]]);
				// locked in constraints
				++n_constraints;
				model_A_row.push_back(n_constraints);
				model_A_col.push_back(i);
				model_A_val.push_back(1);
				model_rhs.push_back(1);
				model_sense.push_back("=");
			}
			for (; (i<pu_id_std_sorted.size() & pu_status[pu_id_std_sorted[i]]==3); ++i) {
				/// status 3 pus
				// costs out objective function vector
				model_obj.push_back(pu_cost[pu_id_std_sorted[i]]);
				// locked out constraints
				++n_constraints;
				model_A_row.push_back(n_constraints);
				model_A_col.push_back(i);
				model_A_val.push_back(1);
				model_rhs.push_back(0);
				model_sense.push_back("=");
			}
			/// boundary length penalties and constraints
			for (std::size_t i=0; i<bound_id1_std.size(); ++i) {
				// boundary length penalty
				model_obj.push_back(bound_boundary[i]);
				
				// constraint 1
				++n_constraints;
				model_A_row.push_back(n_constraints);
				model_A_col.push_back(bound_id1_std[i]);
				model_A_val.push_back(-1);
				
				model_A_row.push_back(n_constraints);
				model_A_col.push_back(i);
				model_A_val.push_back(1);
				
				model_rhs.push_back(0);
				model_sense.push_back("<=");				
				
				// constraint 2
				++n_constraints;
				model_A_row.push_back(n_constraints);
				model_A_col.push_back(bound_id2_std[i]);
				model_A_val.push_back(1);

				model_A_row.push_back(n_constraints);
				model_A_col.push_back(i);
				model_A_val.push_back(1);
				
				model_rhs.push_back(0);
				model_sense.push_back("<=");				
			}
		}

		// species decision variables
		++n_constraints;		
		for (std::size_t i=0; i<puvspecies_pu_std.size(); ++i) {		
			model_A_row.push_back(n_constraints+pu_id_std_sorted[puvspecies_species_std[i]]);
			model_A_col.push_back(puvspecies_pu[puvspecies_pu_std[i]]);
			model_A_val.push_back(puvspecies_amount[i]);
		}
		
		for (std::size_t i=0; i<species_id.size(); ++i) {
			if (std::isfinite(species_spf[i])) {
			
			} else {
				model_sense.push_back(">=");
				model_rhs.push_back(species_spf[i]);			
			}
		}
		for (i
		
		}
		
		for (std::size_t i=0; i<softSpp.size(); ++i) {
			model_sense.push_back(">=");
			model_rhs.push_back(0);
		}
		
		// firm species decision variables and constraints
		for (std::size_t i=0; i<firmSpp.size(); ++i) {
			// objective vector
			model_obj.push_back(species_spf[firmSpp[i]]);
			
			// constraints
			++n_constraints;
			for (std::size_t j=0; j<
		
		}
		
		// soft species constraints

		
		
		//// exports
 		return;
	}

