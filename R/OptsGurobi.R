#' @include RcppExports.R marxan-internal.R misc.R Opts.R
NULL

#' OptsGurobi: An S4 class to store input parameters for Gurobi
#'
#' This class is used to store input parameters for Gurobi.
#'
#' @slot BarlterLimit "integer" Limits the number of barrier iterations performed. Defaults to \code{Inf}.
#' @slot CutOff "numeric" Indicates that you aren't interested in solutions whose objective values are worse than the specified value. Defaults to \code{-Inf}.
#' @slot IterationLimit "numeric" Limits the number of simplex iterations performed. Defaults to \code{Inf}.
#' @slot NodeLimit "numeric" Limits the number of MIP nodes explored. Defaults to \code{Inf}.
#' @slot SolutionLimit "integer" Limits the number of feasible MIP solutions found. Defaults to \code{Inf}.
#' @slot TimeLimit "numeric" Limits the total time expended (in seconds). Defaults to \code{Inf}.
#' @slot BarConvTol "numeric" The barrier solver terminates when the relative difference between the primal and the duel objective values is less than the specified tolerance. Defaults to \code{1e-8}.
#' @slot BarQCPConvTol "numeric" When solving a QCP model, the solver terminates when the relative difference between the primal and dual objective is less than the specified tolerance. Defaults to \code{1e-6}.
#' @slot FeasibilityTol "numeric" All constraints must be satisfied to this tolerance. Defaults to \code{1e-6}.
#' @slot IntFeasTol "numeric" An integrality restriction on a variable is considered satisfied when the variable's value is less than this value from the nearest integer value. Defaults to \code{1e-5}.
#' @slot MarkowitzTol "numeric" The Markowitz tolerance is used to limit numerical error in the simplex algorithm. Defaults to \code{0.0078125}.
#' @slot MIPGap "numeric" The MIP solver will terminate (with an optimal result) when the relative gap between the lower and upper objective bound is less than this value times the upper bound. Defaults to \code{1e-4}.
#' @slot MIPGapAbs "numeric" The MIP solver will terminate (with an optimal result) when the absolute gap between the lower and upper objective bound is less than this value. Defaults to \code{1e-10}.
#' @slot OptimalityTol "numeric" Reduced costs must all be smaller than this value in the improving direction in order for a model to be declared optimal. Defaults to \code{1e-6}.
#' @slot PSDTol "numeric" Sets a limit on the amount of diagonal perturbation that the optimizer is allowed to perform on a Q matrix in order to correct minor PSD violations. Defaults to \code{1e-6}.
#' @slot InfUnbdInfo "integer" Determines whether simplex (and crossover) will compute additional information when a model is determined to be feasible or unbounded. Defaults to \code{0L}.
#' @slot NormAdjust "integer" Chooses from among multiple pricing variants. Defaults to \code{-1L}.
#' @slot ObjScale "numeric" Divides the model objective by the specified value to avoid numerical errors that may result from very large objective coefficients. Defaults to \code{0.0}.
#' @slot PerturbValue "numeric" Magnitude of the simplex perturbation. Defaults to \code{0.0002}.
#' @slot Quad "integer" Enables or disables quad precision computation in simplex. Defaults to \code{-1L}.
#' @slot ScaleFlag "integer" Enables or disables model scaling. Defaults to \code{1L}.
#' @slot Sifting "integer" Controls sifting within dual simplex. Defaults to \code{-1L}.
#' @slot SiftMethod "integer" LP method used to solve sifting sub-problems. Defaults to \code{-1L}.
#' @slot SimplexPricing "integer" Determines the simplex variable pricing strategy. Defaults to \code{-1L}.
#' @slot BarCorrectors "integer" Limits the number of central corrections performed in each barrier iteration. Defaults to \code{-1L}.
#' @slot BarHomogenous "integer" Determines whether to use the homogeneous barrier algorithm. Defaults to \code{-1L}.
#' @slot BarOrder "integer" Choose the barrier sparse matrix fill-reducing algorithm. Defaults to \code{-1L}.
#' @slot Crossover "integer" Determines the crossover strategy used to transform the interior solution produced by barrier into basic solution. Defaults to \code{-1L}.
#' @slot CrossoverBasis "integer" Determines the initial basis construction strategy for crossover. Defaults to \code{0L}.
#' @slot QCPDual "integer" Determines whether dual variable values are computed for QCP models. Defaults to \code{0L}.
#' @slot BranchDir "integer" Determines which child node is explored first in the branch-and-cut search. Defaults to \code{0L}.
#' @slot ConcurrentMIP "integer" This parameter enables the concurrent MIP solver. Defaults to \code{1L}.
#' @slot ConcurrentMIPJobs "integer" Enables distributed concurrent MIP. Defaults to \code{0L}.
#' @slot Disconnected "integer" A MIP model can sometimes be made up of multiple, completely independently sub-models. This parameter controls how aggressively this structure is exploited. Defaults to \code{-1L}.
#' @slot Heuristics "numeric" Determines the amount of time spent in MIP heuristics. Defaults to \code{0.05}.
#' @slot ImproveStartGap "numeric" The MIP solver can change parameter settings in the middle of the search to order to adopt to strategy that gives up on moving the best bound and instead devotes all of its efforts towards finding better feasible solutions. This parameter specifies an optimality gap at which the MIP solver switches strategies. Defaults to \code{0}.
#' @slot ImproveStartNodes "numeric" The MIP solver can change parameter settings in the middle of the search to adopt a strategy that gives up on moving the best bound and instead devotes all of its efforts towards finding better feasible solutions. This parameter specifies the node count at which the MIP solver switches strategies. Defaults to \code{Inf}.
#' @slot ImproveStartTime "numeric" The MIP solver can change parameter settings in the middle of the search in order to adopt a strategy that gives up on moving the best bound and instead devotes all of its efforts towards finding better feasible solutions. This parameter specifies the time when the MIP solver switches to strategies. Defaults to \code{Inf}.
#' @slot MinRelNodes "integer" Number of nodes to explore in the minimum relaxation heuristic. Defaults to \code{-1L}.
#' @slot MIPFocus "integer" This parameter modifies the high-level solution strategy. Defaults to \code{0L}.
#' @slot MIQCPMethod "integer" Method used to solve MIQCP models. Defaults to \code{-1}.
#' @slot NodefileStart "numeric" When the amount of memory used to store nodes (in GB) exceeds this parameter, nodes are written to disk. Defaults to \code{Inf}.
#' @slot NodeMethod "integer" Algorithm used for MIP node relaxations. Defaults to \code{1L}.
#' @slot PumpPasses "integer" Number of passes of the feasibility pump heuristic. Defaults to \code{0L}.
#' @slot RINS "integer" Frequency of the RINS heuristic. Defaults to \code{-1L}.
#' @slot SolutionNumber "integer" When querying attribute \code{Xn} to retrieve an alternate MIP solution, this parameter determines which alternate solution is retrieved. Defaults to \code{0L}.
#' @slot SubMIPNodes "integer" Limits the number of nodes explored by the RINS heuristic. Defaults to \code{500L}.
#' @slot Symmetry "integer" MIP symmetry detection. Defaults to \code{-1L}.
#' @slot VarBranch "integer" Branch variable selection strategy. Defaults to \code{-1L}.
#' @slot ZeroObjNodes "integer" Number of nodes to explore in the zero objective heuristic. Defaults to \code{0L}.
#' @slot Cuts "integer" Global cut aggressiveness setting. Defaults to \code{-1L}.
#' @slot CliqueCuts "integer" Clique cut generation. Defaults to \code{-1L}.
#' @slot CoverCuts "integer" Cover cut generation. Defaults to \code{-1L}.
#' @slot FlowCoverCuts "integer" Flow cover cut generation. Defaults to \code{-1L}.
#' @slot FlowPathCuts "integer" Flow path cut generation. Defaults to \code{-1L}.
#' @slot GUBCoverCuts "integer" GUB cover cut generation. Defaults to \code{-1L}.
#' @slot ImpliedCuts "integer" Implied bound cut generation. Defaults to \code{-1L}.
#' @slot MIPSepCuts "integer" MIP separation cut generation. Defaults to \code{-1L}.
#' @slot MIRCuts "integer" Mixed Integer Rounding (MIR) cut generation. Defaults to \code{-1L}.
#' @slot ModKCuts "integer" Mod-k generation. Defaults to \code{-1L}.
#' @slot NetworkCuts "integer" Network cut generation. Defaults to \code{-1L}.
#' @slot SubMIPCuts "integer" Sub-MIP cut generation. Defaults to \code{-1L}.
#' @slot ZeroHalfCuts "integer" Zero-half cut generation. Defaults to \code{-1L}.
#' @slot CutAggPasses "integer" A non-negative indicates the maximum number of constraint aggregation passes performed during cut generation. Overrides the \code{Cuts} parameter. Defaults to \code{-1L}.
#' @slot CutPasses "integer" A non-negative value indicates the maximum number of cutting place passes performed during root cut generation. Defaults to \code{-1L}.
#' @slot GomoryPasses "integer" A non-negative value indicates the maximum number of Gomory cut passes performed. Defaults to \code{-1L}.
#' @slot AggFill "integer" Amount of fill allowed during presolve aggregation. Defaults to \code{-1L}.
#' @slot Aggregate "integer" Aggregation in presolve. Defaults to \code{-1L}.
#' @slot DisplayInterval "integer" Frequency at which log lines are printed (in seconds). Defaults to \code{5L}.
#' @slot DualReductions "integer" Dual reductions are performed in presolve. Defaults to \code{1L}.
#' @slot FeasRelaxBigM "numeric" It is sometimes necessary to introduce a big-M value when relaxing a constraint. This parameter determines the default magnitude of this value. Defaults to \code{1e6}.
#' @slot IISMethod "integer" IIS method to use. Defaults to \code{-1L}.
#' @slot Method "integer" Algorithm used to solve continuous models or the root node of a MIP model. Defaults to \code{-1L}.
#' @slot NumericFocus "integer" Degree to which the code attempts to detect and manage numerical issues. Defaults to \code{0L}.
#' @slot PreCrush "integer" Translate contraints on the original model to equivalent constraints on the presolved model. Defaults to \code{0L}.
#' @slot PreDepRow "integer" The presolve row reduction, which eliminates linearly dependent constraints from the constraint matrix. Defaults to \code{-1L}.
#' @slot PreDual "integer" Controls whether presolve forms the dual of a continuous model. Defaults to \code{-1L}.
#' @slot PrePasses "integer" Limits the number of passes performed by presolve. Defaults to \code{-1L}.
#' @slot PreQLinearize "integer" Controls presolve Q matrix linearization. Defaults to \code{-1L}.
#' @slot Presolve "integer" The presolve level. Defaults to \code{-1L}.
#' @slot PreSOS1BigM "numeric" The automatic reformulation of SOS1 constraints into binary form. Defaults to \code{-1}.
#' @slot PreSOS2BigM "numeric" The automatic reformulation of SOS2 constraints into binary form. Defaults to \code{0}.
#' @slot PreSparsify "integer" The presolve sparsify reduction. Defaults to \code{-1L}. 
#' @seealso \code{\url{http://www.gurobi.com/documentation/5.6/reference-manual/parameters}}
#' @export
setClass("OptsGurobi",
	contains="Opts"
	representation(
		BarlterLimit="integer",
		CutOff="numeric",
		IterationLimit="numeric",
		NodeLimit="numeric",
		SolutionLimit="integer",
		TimeLimit="numeric",
		BarConvTol="numeric",
		BarQCPConvTol="numeric",
		FeasibilityTol="numeric",
		IntFeasTol="numeric",
		MarkowitzTol="numeric",
		MIPGap="numeric",
		MIPGapAbs="numeric",
		OptimalityTol="numeric",
		PSDTol="numeric",
		InfUnbdInfo="integer",
		NormAdjust="integer",
		ObjScale="numeric",
		PerturbValue="numeric",
		Quad="integer",
		ScaleFlag="integer",
		Sifting="integer",
		SiftMethod="integer",
		SimplexPricing="integer",
		BarCorrectors="integer",
		BarHomogenous="integer",
		BarOrder="integer",
		Crossover="integer",
		CrossoverBasis="integer",
		QCPDual="integer",
		BranchDir="integer",
		ConcurrentMIP="integer",
		ConcurrentMIPJobs="integer",
		Disconnected="integer",
		Heuristics="numeric",
		ImproveStartGap="numeric",
		ImproveStartNodes="numeric",
		ImproveStartTime="numeric",
		MinRelNodes="integer",
		MIPFocus="integer",
		MIQCPMethod="integer",
		NodefileStart="numeric",
		NodeMethod="integer",
		PumpPasses="integer",
		RINS="integer",
		SolutionNumber="integer",
		SubMIPNodes="integer",
		Symmetry="integer",
		VarBranch="integer",
		ZeroObjNodes="integer",
		Cuts="integer",
		CliqueCuts="integer",
		CoverCuts="integer",
		FlowCoverCuts="integer",
		FlowPathCuts="integer",
		GUBCoverCuts="integer",
		ImpliedCuts="integer",
		MIPSepCuts="integer",
		MIRCuts="integer",
		ModKCuts="integer",
		NetworkCuts="integer",
		SubMIPCuts="integer",
		ZeroHalfCuts="integer",
		CutAggPasses="integer",
		CutPasses="integer",
		GomoryPasses="integer",
		AggFill="integer",
		Aggregate="integer",
		DisplayInterval="integer",
		DualReductions="integer",
		FeasRelaxBigM="numeric",
		IISMethod="integer",
		Method="integer",
		NumericFocus="integer",
		PreCrush="integer",
		PreDepRow="integer",
		PreDual="integer",
		PrePasses="integer",
		PreQLinearize="integer",
		Presolve="integer",
		PreSOS1BigM="numeric",
		PreSOS2BigM="numeric",
		PreSparsify="integer"
	),
	prototype=list(
		BarlterLimit=Inf,
		CutOff=-Inf,
		IterationLimit=Inf,
		NodeLimit=Inf,
		SolutionLimit=Inf,
		TimeLimit=Inf,
		BarConvTol=1e-8,
		BarQCPConvTol=1e-6,
		FeasibilityTol=1e-6,
		IntFeasTol=1e-5,
		MarkowitzTol=0.0078125,
		MIPGap=1e-4,
		MIPGapAbs=1e-10,
		OptimalityTol=1e-6,
		PSDTol=1e-6,
		InfUnbdInfo=0L,
		NormAdjust=-1L,
		ObjScale=0.0,
		PerturbValue=0.0002,
		Quad=-1L,
		ScaleFlag=1L,
		Sifting=-1L,
		SiftMethod=-1L,
		SimplexPricing=-1L,
		BarCorrectors=-1L,
		BarHomogenous=-1L,
		BarOrder=-1L,
		Crossover=-1L,
		CrossoverBasis=0L,
		QCPDual=0L,
		BranchDir=0L,
		ConcurrentMIP=1L,
		ConcurrentMIPJobs=0L,
		Disconnected=-1L,
		Heuristics=0.05,
		ImproveStartGap=0,
		ImproveStartNodes=Inf,
		ImproveStartTime=Inf,
		MinRelNodes=-1L,
		MIPFocus=0L,
		MIQCPMethod=-1,
		NodefileStart=Inf,
		NodeMethod=1L,
		PumpPasses=0L,
		RINS=-1L,
		SolutionNumber=0L,
		SubMIPNodes=500L,
		Symmetry=-1L,
		VarBranch=-1L,
		ZeroObjNodes=0L,
		Cuts=-1L,
		CliqueCuts=-1L,
		CoverCuts=-1L,
		FlowCoverCuts=-1L,
		FlowPathCuts=-1L,
		GUBCoverCuts=-1L,
		ImpliedCuts=-1L,
		MIPSepCuts=-1L,
		MIRCuts=-1L,
		ModKCuts=-1L,
		NetworkCuts=-1L,
		SubMIPCuts=-1L,
		ZeroHalfCuts=-1L,
		CutAggPasses=-1L,
		CutPasses=-1L,
		GomoryPasses=-1L,
		AggFill=-1L,
		Aggregate=-1L,
		DisplayInterval=5L,
		DualReductions=1L,
		FeasRelaxBigM=1e6,
		IISMethod=-1L,
		Method=-1L,
		NumericFocus=0L,
		PreCrush=0L,
		PreDepRow=-1L,
		PreDual=-1L,
		PrePasses=-1L,
		PreQLinearize=-1L,
		Presolve=-1L,
		PreSOS1BigM=-1,
		PreSOS2BigM=0,
		PreSparsify=-1L
	),
	validity=function(object) {
		# check for NA or non-finite values
		for (i in slotNames(object))
			if (!is.finite(slot(object, i)) & i!="break.at.value")
				stop('argument to ',i,'is NA or non-finite')
		# bounds checks for parameters
		if (object@BarlterLimit<0L) stop('argument to BarlterLimit must be greater than or equal to 0L')
		if (object@IterationLimit<0) stop('argument to IterationLimit must be greater than or equal to 0')
		if (object@NodeLimit<0) stop('argument to NodeLimit must be greater than or equal to 0')
		if (object@SolutionLimit<1L) stop('argument to SolutionLimit must be greater than or equal to 1L')
		if (object@TimeLimit<0) stop('argument to TimeLimit must be greater than or equal to 0')
		if (object@BarConvTol<0 || object@BarConvTol>1) stop('argument to BarConvTol must be between 0 and 1')
		if (object@BarQCPConvTol<0 || object@BarQCPConvTol>1) stop('argument to BarQCPConvTol must be between 0 and 1')
		if (object@FeasibilityTol<1e-9 || object@FeasibilityTol>1e-2) stop('argument to FeasibilityTol must be between 1e-9 and 1e-2')
		if (object@IntFeasTol<1e-9 || object@IntFeasTol>1e-1) stop('argument to IntFeasTol must be between 1e-9 and 1e-1')
		if (object@MarkowitzTol<1e-4 || object@MarkowitzTol>0.999) stop('argument to MarkowitzTol must be between 1e-4 and 0.999')
		if (object@MIPGap<0) stop('argument to MIPGap must be between greater than or equal to 0')
		if (object@MIPGapAbs<0) stop('argument to MIPGapAbs must be between greater than or equal to 0')
		if (object@OptimalityTol<1e-9 || object@OptimalityTol>1e-2) stop('argument to OptimalityTol must be between 1e-9 and 1e-2')
		if (object@PSDTol<0) stop('argument to PSDTol must be greater than or equal to 0')
		if (object@InfUnbdInfo<0L || object@InfUnbdInfo>1L) stop('argument to InfUnbdInfo must be 0L or 1L')
		if (object@NormAdjust< -1L || object@NormAdjust>3L) stop('argument to NormAdjust must be between -1L and 3L')
		if (object@ObjScale< -1) stop('argument to ObjScale must be greater than or equal to -1')
		if (object@PerturbValue<0 || object@PerturbValue>0.01) stop('argument to PerturbValue must be between 0 and 0.01')
		if (object@Quad< -1L || object@Quad>1L) stop('argument to Quad must be between -1L and 1L')
		if (object@ScaleFlag<0L || object@ScaleFlag>1L) stop('argument to ScaleFlag must be 0L or 1L')
		if (object@Sifting< -1L || object@Sifting>2L) stop('argument to Sifting must be between -1L and 2L')
		if (object@SiftMethod< -1L || object@SiftMethod>2L) stop('argument to SiftMethod must be between -1L and 2L')
		if (object@SimplexPricing< -1L || object@SimplexPricing>3L) stop('argument to SimplexPricing must be between -1L and 3L')
		if (object@BarCorrectors< -1L) stop('argument to BarCorrectors must be greater than or equal to -1L')
		if (object@BarHomogeneous< -1L || object@BarHomogeneous>1L) stop('argument to BarHomogeneous must be between -1L and 1L')
		if (object@BarOrder< -1L || object@BarOrder>1L) stop('argument to BarOrder must be between -1L and 1L')
		if (object@Crossover< -1L || object@Crossover>4L) stop('argument to Crossover must be between -1L and 4L')
		if (object@CrossoverBasis<0L || object@CrossoverBasis>1L) stop('argument to CrossoverBasis must be 0L or 1L')
		if (object@QCPDual<0L || object@QCPDual>1L) stop('argument to QCPDual must be 0L or 1L')
		if (object@BranchDir< -1L || object@BranchDir>1L) stop('argument to BranchDir must be between -1L and 1L')
		if (object@ConcurrentMIP<1L || object@ConcurrentMIP>2e+09L) stop('argument to ConcurrentMIP must be between 1L and 2e+09L')
		if (object@ConcurrentMIPJobs<0L || object@ConcurrentMIPJobs>2e+09L) stop('argument to ConcurrentMIPJobs must be between 0L and 2e+09L')
		if (object@Disconnected< -1L || object@Disconnected>2L) stop('argument to Disconnected must be between -1L and 2L')
		if (object@Heuristics<0 || object@Heuristics>1) stop('argument to Heuristics must be between 0 and 1')
		if (object@ImproveStartGap<0) stop('argument to ImproveStartGap must be greater than or equal to 0')
		if (object@ImproveStartNodes<0) stop('argument to ImproveStartNodes must be greater than or equal to 0')
		if (object@ImproveStartTime<0) stop('argument to ImproveStartTime must be greater than or equal to 0')
		if (object@MinRelNodes< -1L) stop('argument to MinRelNodes must be greater than or equal to -1L')
		if (object@MIPFocus<0L || object@MIPFocus>3L) stop('argument to MIPFocus must be between 0L and 3L')
		if (object@MIQCPMethod< -1L || object@MIQCPMethod>1L) stop('argument to MIPFocus must be between -1L and 1L')
		if (object@NodefileStart<0) stop('argument to NodefileStart must be greater than or equal to 0')
		if (object@NodeMethod< 0L || object@NodeMethod>2L) stop('argument to NodeMethod must be between 0L and 2L')
		if (object@PumpPasses<0L) stop('argument to PumpPasses must be greater than or equal to 0L')
		if (object@RINS< -1L) stop('argument to RINS must be greater than or equal to -1L')
		if (object@SolutionNumber< 0L) stop('argument to SolutionNumber must be greater than or equal to 0L')
		if (object@SubMIPNodes< 0L) stop('argument to SubMIPNodes must be greater than or equal to 0L')
		if (object@Symmetry< -1L || object@Symmetry>2L) stop('argument to Symmetry must between -1L and 2L')
		if (object@VarBranch< -1L || object@VarBranch>3L) stop('argument to VarBranch must between -1L and 3L')
		if (object@ZeroObjNodes<0L) stop('argument to ZeroObjNodes must be greater than or equal to 0L')
		if (object@Cuts< -1L || object@Cuts>3L) stop('argument to Cuts must be between -1L and 3L')
		if (object@CliqueCuts< -1L || object@CliqueCuts>2L) stop('argument to CliqueCuts must be between -1L and 2L')
		if (object@CoverCuts< -1L || object@CoverCuts>2L) stop('argument to CoverCuts must be between -1L and 2L')
		if (object@FlowCoverCuts< -1L || object@FlowCoverCuts>2L) stop('argument to FlowCoverCuts must be between -1L and 2L')
		if (object@FlowPathCuts< -1L || object@FlowPathCuts>2L) stop('argument to FlowPathCuts must be between -1L and 2L')
		if (object@GUBCoverCuts< -1L || object@GUBCoverCuts>2L) stop('argument to GUBCoverCuts must be between -1L and 2L')
		if (object@ImpliedCuts< -1L || object@ImpliedCuts>2L) stop('argument to ImpliedCuts must be between -1L and 2L')
		if (object@MIPSepCuts< -1L || object@MIPSepCuts>2L) stop('argument to MIPSepCuts must be between -1L and 2L')
		if (object@MIRCuts< -1L || object@MIRCuts>2L) stop('argument to MIRCuts must be between -1L and 2L')
		if (object@ModKCuts< -1L || object@ModKCuts>2L) stop('argument to ModKCuts must be between -1L and 2L')
		if (object@NetworkCuts< -1L || object@NetworkCuts>2L) stop('argument to NetworkCuts must be between -1L and 2L')
		if (object@SubMIPCuts< -1L || object@SubMIPCuts>2L) stop('argument to SubMIPCuts must be between -1L and 2L')
		if (object@ZeroHalfCuts< -1L || object@ZeroHalfCuts>2L) stop('argument to ZeroHalfCuts must be between -1L and 2L')
		if (object@CutAggPasses< -1L || object@CutAggPasses>2L) stop('argument to CutAggPasses must be between -1L and 2L')
		if (object@CutPasses< -1L) stop('argument to CutPasses must be greater than or equal to -1L')
		if (object@GomoryPasses< -1L) stop('argument to GomoryPasses must be greater than or equal to -1L')
		if (object@AggFill< -1L) stop('argument to AggFill must be greater than or equal to -1L')
		if (object@Aggregate< -0L || object@Aggregate>1L) stop('argument to Aggregate must be 0L or 1L')
		if (object@DisplayInterval< 1L) stop('argument to DisplayInterval must be greater than or equal to 1L')
		if (object@DualReductions< -0L || object@DualReductions>1L) stop('argument to DualReductions must be 0L or 1L')
		if (object@FeasRelaxBigM<0) stop('argument to FeasRelaxBigM must be greater than or equal to 0')
		if (object@IISMethod< -1L || object@IISMethod>1L) stop('argument to IISMethod must be between -1L and 1L')
		if (object@Method< -1L || object@Method>4L) stop('argument to Method must be between -1L and 4L')
		if (object@NumericFocus<0L || object@NumericFocus>3L) stop('argument to NumericFocus must be between 0L and 3L')
		if (object@PreCrush<0L || object@PreCrush>1L) stop('argument to PreCrush must be 0L or 1L')
		if (object@PreDepRow< -1L || object@PreDepRow>1L) stop('argument to PreDepRow must be between -1L and 1L')
		if (object@PreDual< -1L || object@PreDual>2L) stop('argument to PreDual must be between -1L and 2L')
		if (object@PrePasses< -1L) stop('argument to PrePasses must be greater than or equal to -1L')
		if (object@PreQLinearize< -1L || object@PreQLinearize>1L) stop('argument to PreQLinearize must be between -1L and 1L')
		if (object@Presolve< -1L || object@Presolve>2L) stop('argument to Presolve must be between -1L and 2L')
		if (object@PreSOS1BigM< -1 || object@PreSOS1BigM>1e10) stop('argument to PreSOS1BigM must be between -1 and 1e10')
		if (object@PreSOS2BigM<0 || object@PreSOS2BigM>1e10) stop('argument to PreSOS2BigM must be between 0 and 1e10')
		if (object@PreSparsify< -1L || object@PreSOS2BigM>1L) stop('argument to PreSparsify must be between -1L and 1L')
		return(TRUE)
	}
)

#' Create "OptsGurobi" object
#'
#' This function creates a new "OptsGurobi" object.
#'
#' @param ... arguments to set slots in a "OptsGurobi" object.
#' @param ignore.extra "logical" Should extra arguments be ignored? Defaults to \code{FALSE}.
#' @details
#' The slots of class "OptsGurobi" are shown below for reference.
#' \tabular{cccl}{
#' \strong{Name} \tab \strong{Class} \tab \strong{Default} \tab \strong{Description}\cr
#' BarlterLimit \tab "integer" \tab Inf \tab limits the number of barrier iterations performed \cr
#' CutOff \tab "numeric" \tab -Inf \tab indicates that you aren't interested in solutions whose objective values are worse than the specified value \cr
#' IterationLimit \tab "numeric" \tab Inf \tab limits the number of simplex iterations performed \cr
#' NodeLimit \tab "numeric" \tab Inf \tab limits the number of MIP nodes explored \cr
#' SolutionLimit \tab "integer" \tab Inf \tab limits the number of feasible MIP solutions found \cr
#' TimeLimit \tab "numeric" \tab Inf \tab limits the total time expended (in seconds) \cr
#' BarConvTol \tab "numeric" \tab 1e-8 \tab the barrier solver terminates when the relative difference between the primal and the duel objective values is less than the specified tolerance \cr
#' BarQCPConvTol \tab "numeric" \tab 1e-6 \tab when solving a QCP model, the solver terminates when the relative difference between the primal and dual objective is less than the specified tolerance \cr
#' FeasibilityTol \tab "numeric" \tab 1e-6 \tab all constraints must be satisfied to this tolerance \cr
#' IntFeasTol \tab "numeric" \tab 1e-5 \tab an integrality restriction on a variable is considered satisfied when the variable's value is less than this value from the nearest integer value \cr
#' MarkowitzTol \tab "numeric" \tab 0.0078125 \tab the Markowitz tolerance is used to limit numerical error in the simplex algorithm \cr
#' MIPGap \tab "numeric" \tab 1e-4 \tab the MIP solver will terminate (with an optimal result) when the relative gap between the lower and upper objective bound is less than this value times the upper bound \cr
#' MIPGapAbs \tab "numeric" \tab 1e-10 \tab the MIP solver will terminate (with an optimal result) when the absolute gap between the lower and upper objective bound is less than this value \cr
#' OptimalityTol \tab "numeric" \tab 1e-6 \tab reduced costs must all be smaller than this value in the improving direction in order for a model to be declared optimal \cr
#' PSDTol \tab "numeric" \tab 1e-6 \tab sets a limit on the amount of diagonal perturbation that the optimizer is allowed to perform on a Q matrix in order to correct minor psd violations \cr
#' InfUnbdInfo \tab "integer" \tab 0L \tab determines whether simplex (and crossover) will compute additional information when a model is determined to be feasible or unbounded \cr
#' NormAdjust \tab "integer" \tab -1L \tab chooses from among multiple pricing variants \cr
#' ObjScale \tab "numeric" \tab 0.0 \tab divides the model objective by the specified value to avoid numerical errors that may result from very large objective coefficients \cr
#' PerturbValue \tab "numeric" \tab 0.0002 \tab magnitude of the simplex perturbation \cr
#' Quad \tab "integer" \tab -1L \tab enables or disables quad precision computation in simplex \cr
#' ScaleFlag \tab "integer" \tab 1L \tab enables or disables model scaling \cr
#' Sifting \tab "integer" \tab -1L \tab controls sifting within dual simplex \cr
#' SiftMethod \tab "integer" \tab -1L \tab LP method used to solve sifting sub-problems \cr
#' SimplexPricing \tab "integer" \tab -1L \tab determines the simplex variable pricing strategy \cr
#' BarCorrectors \tab "integer" \tab -1L \tab limits the number of central corrections performed in each barrier iteration \cr
#' BarHomogenous \tab "integer" \tab -1L \tab determines whether to use the homogeneous barrier algorithm \cr
#' BarOrder \tab "integer" \tab -1L \tab choose the barrier sparse matrix fill-reducing algorithm \cr
#' Crossover \tab "integer" \tab -1L \tab determines the crossover strategy used to transform the interior solution produced by barrier into basic solution \cr
#' CrossoverBasis \tab "integer" \tab 0L \tab determines the initial basis construction strategy for crossover \cr
#' QCPDual \tab "integer" \tab 0L \tab determines whether dual variable values are computed for QCP models \cr
#' BranchDir \tab "integer" \tab 0L \tab determines which child node is explored first in the branch-and-cut search \cr
#' ConcurrentMIP \tab "integer" \tab 1L \tab this parameter enables the concurrent MIP solver \cr
#' ConcurrentMIPJobs \tab "integer" \tab 0L \tab enables distributed concurrent MIP \cr
#' Disconnected \tab "integer" \tab -1L \tab a MIP model can sometimes be made up of multiple, completely independently sub-models. this parameter controls how aggressively this structure is exploited \cr
#' Heuristics \tab "numeric" \tab 0.05 \tab determines the amount of time spent in MIP heuristics \cr
#' ImproveStartGap \tab "numeric" \tab 0 \tab the MIP solver can change parameter settings in the middle of the search to order to adopt to strategy that gives up on moving the best bound and instead devotes all of its efforts towards finding better feasible solutions. this parameter specifies an optimality gap at which the MIP solver switches strategies \cr
#' ImproveStartNodes \tab "numeric" \tab Inf \tab the MIP solver can change parameter settings in the middle of the search to adopt a strategy that gives up on moving the best bound and instead devotes all of its efforts towards finding better feasible solutions. this parameter specifies the node count at which the MIP solver switches strategies \cr
#' ImproveStartTime \tab "numeric" \tab Inf \tab the MIP solver can change parameter settings in the middle of the search in order to adopt a strategy that gives up on moving the best bound and instead devotes all of its efforts towards finding better feasible solutions. this parameter specifies the time when the MIP solver switches to strategies \cr
#' MinRelNodes \tab "integer" \tab -1L \tab number of nodes to explore in the minimum relaxation heuristic \cr
#' MIPFocus \tab "integer" \tab 0L \tab this parameter modifies the high-level solution strategy \cr
#' MIQCPMethod \tab "integer" \tab -1 \tab method used to solve MIQCP models \cr
#' NodefileStart \tab "numeric" \tab Inf \tab when the amount of memory used to store nodes (in gb) exceeds this parameter, nodes are written to disk \cr
#' NodeMethod \tab "integer" \tab 1L \tab algorithm used for MIP node relaxations \cr
#' PumpPasses \tab "integer" \tab 0L \tab number of passes of the feasibility pump heuristic \cr
#' RINS \tab "integer" \tab -1L \tab frequency of the RINS heuristic \cr
#' SolutionNumber \tab "integer" \tab 0L \tab when querying attribute \code{xn} to retrieve an alternate MIP solution, this parameter determines which alternate solution is retrieved \cr
#' SubMIPNodes \tab "integer" \tab 500L \tab limits the number of nodes explored by the RINS heuristic \cr
#' Symmetry \tab "integer" \tab -1L \tab MIP symmetry detection \cr
#' VarBranch \tab "integer" \tab -1L \tab branch variable selection strategy \cr
#' ZeroObjNodes \tab "integer" \tab 0L \tab number of nodes to explore in the zero objective heuristic \cr
#' Cuts \tab "integer" \tab -1L \tab global cut aggressiveness setting \cr
#' CliqueCuts \tab "integer" \tab -1L \tab clique cut generation \cr
#' CoverCuts \tab "integer" \tab -1L \tab cover cut generation \cr
#' FlowCoverCuts \tab "integer" \tab -1L \tab flow cover cut generation \cr
#' FlowPathCuts \tab "integer" \tab -1L \tab flow path cut generation \cr
#' GUBCoverCuts \tab "integer" \tab -1L \tab GUB cover cut generation \cr
#' ImpliedCuts \tab "integer" \tab -1L \tab implied bound cut generation \cr
#' MIPSepCuts \tab "integer" \tab -1L \tab MIP separation cut generation \cr
#' MIRCuts \tab "integer" \tab -1L \tab mixed integer rounding (mir) cut generation \cr
#' ModKCuts \tab "integer" \tab -1L \tab MOD-K generation \cr
#' NetworkCuts \tab "integer" \tab -1L \tab network cut generation \cr
#' SubMIPCuts \tab "integer" \tab -1L \tab sub-MIP cut generation \cr
#' ZeroHalfCuts \tab "integer" \tab -1L \tab zero-half cut generation \cr
#' CutAggPasses \tab "integer" \tab -1L \tab a non-negative indicates the maximum number of constraint aggregation passes performed during cut generation. overrides the \code{cuts} parameter \cr
#' CutPasses \tab "integer" \tab -1L \tab a non-negative value indicates the maximum number of cutting place passes performed during root cut generation \cr
#' GomoryPasses \tab "integer" \tab -1L \tab a non-negative value indicates the maximum number of Gomory cut passes performed \cr
#' AggFill \tab "integer" \tab -1L \tab amount of fill allowed during presolve aggregation \cr
#' Aggregate \tab "integer" \tab -1L \tab aggregation in presolve \cr
#' DisplayInterval \tab "integer" \tab 5L \tab frequency at which log lines are printed (in seconds) \cr
#' DualReductions \tab "integer" \tab 1L \tab dual reductions are performed in presolve \cr
#' FeasRelaxBigM \tab "numeric" \tab 1e6 \tab it is sometimes necessary to introduce a big-m value when relaxing a constraint. this parameter determines the default magnitude of this value \cr
#' IISMethod \tab "integer" \tab -1L \tab IIS method to use \cr
#' Method \tab "integer" \tab -1L \tab algorithm used to solve continuous models or the root node of a MIP model \cr
#' NumericFocus \tab "integer" \tab 0L \tab degree to which the code attempts to detect and manage numerical issues \cr
#' PreCrush \tab "integer" \tab 0L \tab translate contraints on the original model to equivalent constraints on the presolved model \cr
#' PreDepRow \tab "integer" \tab -1L \tab the presolve row reduction, which eliminates linearly dependent constraints from the constraint matrix \cr
#' PreDual \tab "integer" \tab -1L \tab controls whether presolve forms the dual of a continuous model \cr
#' PrePasses \tab "integer" \tab -1L \tab limits the number of passes performed by presolve \cr
#' PreQLinearize \tab "integer" \tab -1L \tab controls presolve q matrix linearization \cr
#' Presolve \tab "integer" \tab -1L \tab the presolve level \cr
#' PreSOS1BigM \tab "numeric" \tab -1 \tab the automatic reformulation of SOS1 constraints into binary form \cr
#' PreSOS2BigM \tab "numeric" \tab 0 \tab the automatic reformulation of SOS2 constraints into binary form \cr
#' PreSparsify \tab "integer" \tab -1L \tab the presolve sparsify reduction \cr
#' }
#' The slots for the 'Opts' super-class are also shown below for reference.
#' \tabular{cccl}{
#' \strong{Name} \tab \strong{Class} \tab \strong{Default} \tab \strong{Description}\cr
#' BLM \tab "numeric" \tab 100 \tab boundary length modifier \cr
#' PROP \tab "numeric" \tab 0 \tab proportion of planning units in initial reserve system \cr
#' COSTTHRESH \tab "numeric" \tab 0 \tab cost threshold \cr
#' MISSLEVEL \tab "numeric" \tab 1 \tab amount of target below which it is counted as 'missing' \cr
#' NCORES \tab "integer" \tab 1L \tab number of cores to use for processing \cr
#' VERBOSITY \tab "integer" \tab 1L \tab amount of output displayed on the program screen \cr
#' }
#' @return "OptsGurobi" object
#' @seealso \code{\link{OptsGurobi-class}}.
#' @export
#' @examples
#' x<-OptsGurobi(NCORES=4, PreSparsify=1)
OptsGurobi<-function(..., ignore.extra=FALSE) {
	return(
		constructOpts(
			'OptsGurobi',
			as.list(substitute(list(...)))[c(-1L)],
			ignore.extra
		)
	)
}

#' @export
print.OptsGurobi<-function(x, header=TRUE) {
	if (header)
		cat("OptsGurobi object.\n")
}

#' @export
setMethod(
	'show',
	'OptsGurobi',
	function(object)
		print.OptsGurobi(object)
)



