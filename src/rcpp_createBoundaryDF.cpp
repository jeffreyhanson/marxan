#include <Rcpp.h>
// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;


      #include <vector>
      #include <string>
      #include <algorithm>
      #include <unordered_map>
      #include <iomanip>
      #include <iostream>
      #include <Rcpp.h>

      template<int P>
      inline double Pow(double x) {
	return (Pow<P-1>(x) * x);
      }

      template<>
      inline double Pow<1>(double x) {
	return (x);
      }

      template<>
      inline double Pow<0>(double x) {
	return (1.0);
      }

      double distance(double x0, double y0, double x1, double y1) {
	return(sqrt(std::abs(Pow<2>(x0-x1)) + std::abs(Pow<2>(y0-y1))));
      }
      
      template<typename T>
      inline std::string num2str(T number, int precision=10)
      {
	std::ostringstream ss;
	ss << std::setprecision(precision) << number;
	return(ss.str());
      }
      
      template<typename T>
      void remove_duplicates(std::vector<T> &v) {
	v.shrink_to_fit();
	sort(v.begin(), v.end());
	v.erase(unique(v.begin(), v.end()), v.end());
	v.shrink_to_fit();
      }
            
      class LINE
      {
	public:
	  // declare constructor
	  LINE(){};
	  LINE(int pid, int pos0, int pos1, double x0, double y0, double x1, double y1, int tol) 
	    : _pid(pid), _pos0(pos0), _pos1(pos1), _x0(x0), _y0(y0), _x1(x1), _y1(y1) {
	      if (_x0 > _x1 || _y0 > _y1) {
		_key = num2str<double>(_x0,tol) + "," + num2str<double>(_y0,tol) + ";" +num2str<double>(_x1,tol) + "," + num2str<double>(_y1,tol);
	      } else {
		_key = num2str<double>(_x1,tol) + "," + num2str<double>(_y1,tol) + ";" +num2str<double>(_x0,tol) + "," + num2str<double>(_y0,tol);
	      }
	  };
	  // declare deconstructor
	  ~LINE(){};

	  // declare methods
	  inline const std::string getLID() const {
	    return(_key + ";" + num2str<int>(_pid) + ";" +num2str<int>(_pos0) + "," + num2str<int>(_pos1));
	  }
	  
	  inline const double getLength() const {
	    return(distance(_x0,_y0,_x1,_y1));
	  }
	  
	  // declare fields
	  int _pid;
	  int _pos0;
	  int _pos1;
	  double _x0;
	  double _y0;
	  double _x1;
	  double _y1;
	  std::string _key;
      };
      
      class PUPAIR
      {
	public:
	// declare constructor
	PUPAIR(){};
	PUPAIR(int pid0, int pid1, double boundary_length)
	  : _pid0(pid0), _pid1(pid1), _boundary_length(boundary_length) {
	    if (_pid0 > _pid1) {
	      _key = num2str<int>(pid0) + ";" + num2str<int>(pid1);
	    } else {
	      _key = num2str<int>(pid1) + ";" + num2str<int>(pid0);
	    }
	};
	
	// declare deconstructor
	~PUPAIR(){};
	
	// declare methods
	
	// declare fields
	int _pid0;
	int _pid1;
	double _boundary_length;
	std::string _key;
      };
      
      

// [[Rcpp::export]]

	Rcpp::List rcpp_createBoundaryDF(Rcpp::DataFrame df , double tolerance=0.001, double lengthFactor=1.0, double edgeFactor=1.0) {
		//// initialization
		/// declare variables and preallocate memory
		std::vector<int> PID = df["PID"];
		std::vector<double> X = df["X"];
		std::vector<double> Y = df["Y"];


		// calculation vars
		int tol=(1.0/tolerance);
		std::vector<int> pos_VINT(PID.size());
		std::iota(pos_VINT.begin(), pos_VINT.end(), 1);
		std::vector<std::string> line_key_VSTR;
		line_key_VSTR.reserve(PID.size()*10);
 		std::unordered_multimap<std::string, LINE> line_UMMAP;
 		line_UMMAP.reserve(PID.size()*10);
		std::vector<std::string> pupair_key_VSTR;
		pupair_key_VSTR.reserve(PID.size()*10);
 		std::unordered_multimap<std::string, PUPAIR> pupair_UMMAP;
 		pupair_UMMAP.reserve(PID.size()*10);
		
		// export vars
		std::vector<int> puid0_VINT;
		std::vector<int> puid1_VINT;
		std::vector<double> length_VDBL;
		std::vector<std::string> warning_VSTR;
		warning_VSTR.reserve(PID.size()*10);
		
		//// preliminary processing
		// generate lines
		int currPIdFirstElement=0;
		LINE currLine;
		for (std::size_t i=1; i!=PID.size(); ++i) {
		  if (PID[i]==PID[currPIdFirstElement]) {
		    currLine=LINE(PID[i], pos_VINT[i], pos_VINT[i-1], X[i], Y[i], X[i-1], Y[i-1], tol);
		    line_UMMAP.insert(std::pair<std::string, LINE>(currLine._key, currLine)); 
		    line_key_VSTR.push_back(currLine._key);
		  } else {
		    currPIdFirstElement=i;
		  }
		}
		
		// free memory
		PID.clear();
		PID.shrink_to_fit();
		X.clear();
		X.shrink_to_fit();
		Y.clear();
		Y.shrink_to_fit();
		pos_VINT.clear();
		pos_VINT.shrink_to_fit();
		
		// obtain unique line keys
		remove_duplicates(line_key_VSTR);

		//// main processing
		/// construct lines
		{
		  // declare local vars
		  int currPID_INT;
		  double currLEN_DBL;
		  PUPAIR currPUPAIR;
		  std::unordered_multimap<std::string,LINE>::iterator it;
 		  std::pair<std::unordered_multimap<std::string,LINE>::iterator,std::unordered_multimap<std::string,LINE>::iterator> range;
		  
		  // main loop
		  for (auto i=line_key_VSTR.cbegin(); i!=line_key_VSTR.cend(); ++i) {
		    // init
		    range=line_UMMAP.equal_range(*i);
		    
		    // store line data
		    it=range.first;
		    currPID_INT=(it->second)._pid;
		    currLEN_DBL=(it->second).getLength();
		    ++it;
		    if (it == range.second) {
		      // store same pid if no duplicate lines
		      currPUPAIR=PUPAIR(currPID_INT,currPID_INT,currLEN_DBL);
		      pupair_key_VSTR.push_back(currPUPAIR._key);
		      pupair_UMMAP.insert(std::pair<std::string, PUPAIR>(currPUPAIR._key,currPUPAIR));
		    } else {
		      // store second pid at least one duplicate lines
		      currPUPAIR=PUPAIR(currPID_INT,(it->second)._pid,currLEN_DBL);
		      pupair_key_VSTR.push_back(currPUPAIR._key);
		      pupair_UMMAP.insert(std::pair<std::string, PUPAIR>(currPUPAIR._key,currPUPAIR));
		      
		      // check to see if more than 2 spatially identical lines
		      ++it;
		      if (it != range.second) {
			it=range.first;
			for (; it!=range.second; ++it) {
			    warning_VSTR.push_back((it->second).getLID());
			}
		      }
		    }
		  }
		}
		
		// free memory
		line_key_VSTR.clear();
		line_key_VSTR.shrink_to_fit();
		
		/// construct pairs
		{
		  // obtain unique pair keys
		  remove_duplicates(pupair_key_VSTR);

		  // allocate memory
		  puid0_VINT.resize(pupair_key_VSTR.size());
		  puid1_VINT.resize(pupair_key_VSTR.size());
		  length_VDBL.resize(pupair_key_VSTR.size());
		  
		  // declare local vars
 		  std::pair<std::unordered_multimap<std::string,PUPAIR>::iterator,std::unordered_multimap<std::string,PUPAIR>::iterator> range;
		  
		  // main loop
		  for (std::size_t i=0; i<pupair_key_VSTR.size(); ++i) {
		    // init
		    range=pupair_UMMAP.equal_range(pupair_key_VSTR[i]);
		    
		    // store pu data
		    puid0_VINT[i]=(range.first->second)._pid0;
		    puid1_VINT[i]=(range.first->second)._pid1;
		    for (auto it=range.first; it!=range.second; ++it) {
		      length_VDBL[i]+=(it->second)._boundary_length;
		    }
		    
		    // apply boundary length factors
		    length_VDBL[i]*=lengthFactor;
		    if (puid0_VINT[i]==puid1_VINT[i]) {
		      length_VDBL[i]*=edgeFactor;
		    }
		  }
		}
		
		//// exports
 		return(
 		  Rcpp::List::create(
		    Rcpp::Named("bldf") = Rcpp::DataFrame::create(Named("id1")=puid0_VINT, Named("id2")=puid1_VINT, Named("boundary")=length_VDBL),
 		    Rcpp::Named("warnings")=warning_VSTR
 		  )
 		);
	}

