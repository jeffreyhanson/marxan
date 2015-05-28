// combined headers for Marxan

/*    Marxan and this unit was coded by Ian Ball
    and written by Ian Ball and Hugh Possingham

    ian_bal@antdiv.gov.au
    hpossing@zen.uq.edu.au

      Modified by Matthew Watts  4 Nov 2005

        m.watts@uq.edu.au
    */


/* SPEX.H start */
#define DebugFree(x) /*Blank)*/
#ifndef mainheaderfile
#define mainheaderfile

    int *bestyet;
    double delta;

    // type definitions for Marxan sparse matrix optimisations data structures
    typedef struct binsearch
    {
        int name;
        int index;
    } typebinsearch;

    typebinsearch *PULookup;
    typebinsearch *SPLookup;

    typedef struct spu
    {
        double amount;
        double prob;
        int clump;
        int spindex;
    } typepu;

    typepu *SM;

    //typedef struct smprob {
    //  double prob;
    //    int spindex;
    //} typesmprob;

    //typesmprob *probSM;

    typedef struct spusporder
    {
        double amount;
        int puindex;
    } typepusporder;

    typepusporder *SMsporder;

    // type definitions for original Ian Ball Marxan data structures

    typedef struct spustuff
    {
        int id;
        int status;
        double xloc,yloc;
        double cost;
        double prob;
        int richness,offset,probrichness,proboffset;
    } typespu;

    typespu *pu;

    typedef struct scost
    {
        double total;
        int pus;
        double connection;
        int missing;
        double penalty;
        double cost;
        double threshpen;
        double shortfall;
        double probability1D;
        double probability2D;
    } typecost;

    typecost reserve,change,debugcost;

    typedef struct sspecies
    {
        int name;
        int type;
        char *sname;
        double target;
        double prop;
        int targetocc;
        double spf;
        double penalty;
        double amount;
        double expected1D, expected2D, variance1D, variance2D;
        int occurrence;
        double sepdistance;
        int sepnum;
        int separation;
        int clumps;
        double target2;  // Only clumping species need this
        struct sclumps *head;  // needed for clumping species
        int richness,offset;
        double Zscore1D, Zscore2D;
        double probability1D, probability2D;
        double ptarget1d, ptarget2d;
    }typesp;

    typesp *spec;

    /* Connectivity Structure. Fixed connectivity number. Should replace with link list! */
    struct sneighbour
    {
        int nbr;
        double cost;
        struct sneighbour *next;
        int connectionorigon;
    } *debugnbr;

    typedef struct sconnections
    {
        struct sneighbour *first;
        double fixedcost;
        int nbrno;
    }typeconnection;

    typeconnection *connections;

    struct sclumppu
    {
      int puid;
      struct sclumppu *next;
    }; /* PU in clump node for clump structure */

    struct sclumps
    {
      int clumpid;
      double amount;
      int occs;
      struct sclumppu *head;
      struct sclumps *next;
    }; /* Clump nodes for species Clump Structure */


/*** Annealing Control ****/

    typedef struct sanneal
    {
        long int Titns;
        long int iterations;
        long int Tlen;
        double Tinit;    /* Initial Temperature */
        double Tcool;    /* Cooling Factor */
        double temp; /* Current Temperature */
        double tempold;
        int type;    /* Type of annealing. 0 = none, 1 = fixed, 2 = adaptive */
        double sigma; /*Used in adaptive annealing */
        double sum; /* Used in adaptive annealing */
        double sum2; /* used in adaptive annealing */
    } typeanneal; /* Annealing Control handler */

    typeanneal anneal;

    /* Useful general link list structure */

    /* General Species information structure */
    typedef struct sgenspec
    {
        int type;
        int targetocc;
        double target;
        double target2;
        int sepnum;
        double sepdistance;
        double prop;
        double spf;
    } typegenspec;

    /* Input File Name Structure */
    typedef struct sfname
    {
        char *inputdir;
        char *outputdir;
        char *specname;
        char *puname;
        char *puvsprname;
        //char *puvspprobname;
        char *matrixspordername;
        char *connectionname;
        char *blockdefname;
        char *bestfieldname;
        char *connectionfilesname;
        char *rbinarypath;
        int savebest;
        int saverun;
        int savesum;
        int savesen;
        int savespecies;
        int savesumsoln;
        int savelog;
        int savesnapsteps;
        int savesnapchanges;
        int savesnapfrequency;
        int savepenalty;
        int savetotalareas;
        int savesolutionsmatrix;
        int solutionsmatrixheaders;
        int saveannealingtrace;
        int annealingtracerows;
        int saveitimptrace;
        int itimptracerows;
        int saverichness;
        int savespeciesdata;
        int rexecutescript;
        int rimagetype;
        int rimagewidth;
        int rimageheight;
        int rimagefontsize;
        int rclustercount;
    } typefname;

    typefname fnames;

    typedef struct srunoptions
    {
        int ThermalAnnealingOn;
        int QuantumAnnealingOn;
        int HeuristicOn;
        int ItImpOn;
    } typerunopts;

    typerunopts runoptions;

/* Useful link definition */

    struct slink
    {
        int id;
        struct slink *next;
    }; /* slink struct */

    typedef struct iimp
    {
        double randomfloat;
        int puindex;
    } typeiimp;




/* Protytpe function Headers */
int Marxan(char sInputFileName[]);
void AddReserve(int puno,struct spustuff pu[],int R[]);
void SetBlockDefs(int gspno,int spno,int puno,struct sgenspec gspec[], struct sspecies spec[],struct spustuff PU[], struct spu SM[]);
void SetDefs(int spno, struct sspecies spec[]);
void SetRunOptions(int runopts, struct srunoptions *runoptions);
int CalcPenalties(int puno,int spno,struct spustuff pu[],struct sspecies spec[],
                  struct sconnections connections[],struct spu SM[],int PUtemp[],int aggexist,double cm,int clumptype);
int CalcPenaltiesOptimise(int puno,int spno,struct spustuff pu[],struct sspecies spec[],
                          struct sconnections connections[],struct spu SM[],struct spusporder SMsp[],
                          int PUtemp[],int aggexist,double cm,int clumptype);


/**** Valuing a reserve ******/

double ConnectionCost1(int ipu,struct spustuff pu[],struct sconnections connections[],double cm);
double cost(int ipu,struct spustuff pu[],struct sconnections connections[],double cm);
double ConnectionCost2(int ipu,struct sconnections connections[],int R[],int imode,int imode2,double cm);
void ComputeConnectivityIndices(double *rConnectivityTotal,double *rConnectivityIn,
                                double *rConnectivityEdge,double *rConnectivityOut,
                                int puno,int *R,typeconnection connections[]);
double ChangePen(int ipu,int puno,struct sspecies spec[], struct spustuff pu[],
                 struct spu SM[],int R[], struct sconnections connections[],
                 int imode,int clumptype,double *rShortfall);
void ReserveCost(int puno,int spno,int R[],struct spustuff pu[],
        struct sconnections connections[],struct spu SM[],
        double cm, struct sspecies spec[],int aggexist,struct scost *reserve,int clumptype);
void InitReserve(int puno,double prop, int R[]);
void SpeciesAmounts(int spno,int puno,struct sspecies spec[],struct spustuff pu[],
                    struct spu SM[],int R[],int clumptype);

/*** Checking and instigating Changes ****/

double ThresholdPenalty(double tpf1,double tpf2,double timeprop);
void CheckChange(int iIteration,int ipu,int spno,int puno,struct spustuff pu[],struct sconnections connections[],
                 struct sspecies spec[],struct spu SM[],int *R,double cm,int imode,
                 struct scost *change, struct scost *reserve,double costthresh,double tpf1, double tpf2,
                 double timeprop,int clumptype);
void CheckQuantumChange(int spno,int puno,struct spustuff pu[],struct sconnections connections[],
                        struct sspecies spec[],struct spu SM[],int *R,double cm,
                        struct scost *change, struct scost *reserve,double costthresh,double tpf1,
                        double tpf2,double timeprop,int clumptype,int iFluctuationCount,int *PUChosen);
double NewPenalty(int ipu,int isp,struct sspecies spec[],struct spustuff pu[], struct spu SM[],int imode);
int GoodChange(struct scost change,double temp);
int GoodQuantumChange(struct scost change,double rProbAcceptance);
void DoChange(int ipu,int puno,int R[],struct scost *reserve,struct scost change,
              struct spustuff pu[],struct spu SM[],struct sspecies spec[],struct sconnections connections[],int imode,int clumptype);
void DoQuantumChange(int puno,int R[],struct scost *reserve,struct scost change,
                     struct spustuff pu[],struct spu SM[],struct sspecies spec[],struct sconnections connections[],
                     int clumptype,int iFluctuationCount,int *PUChosen);

/**** Post Processing Headers ********/

int CountMissing(int spno,struct sspecies spec[],double misslevel,double *shortfall,double *rMinimumProportionMet);
void PrintResVal (int puno, int spno,int R[],struct scost reserve,
     struct sspecies spec[],double misslevel);
void ChangeCost(struct scost *cost,double changemult);

void TimePassed(void);
void PauseProg(void);
void PauseExit(void);


/********* Prototype Headers For Clumping Utilities ********/
/* moved to clumping.h */

/********** Prototype Headers for Heuristics ***********/
/* moved to heuristic.h */

/************ Prototype Headers for Annealing specific functions ***/
/* moved to annealing.h */

/************* Prototype Headers for Separation specific functions ****/

/* Now moved to separation.h */


/************** Debugging Routines ****************/
/* moved to debug.h */

#endif
/* SPEX.H END */
/* ************************************************************************** */
/* ANNEALING.H START */
#ifndef annealingheader
#define annealingheader


void ConnollyInit(int puno,int spno,struct spustuff pu[],typeconnection connections[],typesp spec[],
                  struct spu SM[],double cm, struct sanneal *anneal,int aggexist,
                  int R[],double prop,int clumptype,int verbose,int irun);
void AdaptiveInit(int puno,int spno,double prop,int *R,struct spustuff pu[],struct sconnections connections[],
                  struct spu SM[],double cm,struct sspecies spec[],int aggexist,struct sanneal *anneal,int clumptype);
void AdaptiveDec(struct sanneal *anneal);
void ThermalAnnealing(int spno, int puno, struct sconnections connections[],int R[], double cm,
                      typesp *spec, struct spustuff pu[], struct spu SM[], struct scost *change, struct scost *reserve,
                      long int repeats,int irun,char *savename,int verbose,double misslevel,
                      int aggexist,double costthresh, double tpf1, double tpf2,int clumptype);
void QuantumAnnealing(int spno, int puno, struct sconnections connections[],int R[], double cm,
                      typesp *spec, struct spustuff pu[], struct spu SM[], struct scost *change, struct scost *reserve,
                      long int repeats,int irun,char *savename,int verbose,double misslevel,
                      int aggexist,double costthresh, double tpf1, double tpf2,int clumptype);
#endif

/* ANNEALING.H END */
/* ************************************************************************** */
/* CLUMPING.H START */
#ifndef clumpingheader
#define clumpinheader



void ClearClump(int isp,struct sclumps *target,struct spustuff pu[],
                struct spu SM[]);
int ClumpCut(int isp,struct spustuff pu[],
        struct sspecies spec[],struct sclumps *clump,
        struct sclumppu *clumppu,struct sconnections connections[],struct spu SM[],
        double *totalamount,int *totalocc,
        int *iseparation, int imode,int clumptype);
void SetSpeciesClumps(int puno,int R[],struct sspecies spec[],struct spustuff pu[],
                      struct spu SM[],struct sconnections connections[],int clumptype);
void SpeciesAmounts4(int isp,struct sspecies spec[],int clumptype);
void ClearClumps(int spno,struct sspecies spec[],struct spustuff pu[], struct spu SM[]);
struct sclumps *AddNewClump(int isp,int ipu,struct sspecies spec[],struct spustuff pu[], struct spu SM[]);
void AddNewPU(int ipu,int isp,struct sconnections connections[],struct sspecies spec[],struct spustuff pu[],
              struct spu SM[], int clumptype);
void RemPu(int ipu, int isp,struct sconnections connections[], struct sspecies spec[],struct spustuff pu[],
           struct spu SM[],int clumptype);
int RemClumpCheck(struct sclumps *pclump,struct spustuff pu[]);

int CalcPenaltyType4(int isp,int puno, struct spu SM[],struct sconnections connections[],
    struct sspecies spec[],struct spustuff pu[],double cm,int clumptype);
double PartialPen4(int isp,double amount, struct sspecies spec[],int clumptype);
double ValueAdd(int isp,int ipu,int puno, int R[],struct sconnections connections[],struct spustuff pu[],
                struct spu SM[],struct sspecies spec[],int clumptype);
double ValueRem(int ipu,int isp,struct sspecies spec[],struct sconnections connections[],
                struct spustuff pu[],struct spu SM[],int clumptype);
double NewPenalty4(int ipu,int isp,int puno,struct sspecies spec[],struct spustuff pu[],struct spu SM[],
             int R[],struct sconnections connections[],int imode,int clumptype);


#endif

/* CLUMPING.H END */
/* ************************************************************************** */
/* FILEIN.H START */

#ifndef fileinheader
#define fileinheader


struct snlink{char *name; struct snlink *next;};


/* Parmtypes */
#define NOTYPE 0
#define INTEGER 1
#define LONGINT 2
#define REAL 3
#define DOUBLE 4
#define STRING 5

/* Prototype headers */

struct snlink *GetVarName(char **varlist,int numvars,char *sVarName,
    struct snlink *head,char *fname);
int CheckVarName(char **varlist, int numvars, char *sVarName);

int NameToPUID(int puno,int name, struct spustuff pu[]);
int NameToSPID(int spno,int name,typesp spec[]);
void rdsvar(FILE *infile, char varname[], void *address, int parmtype, int crit,int present);

void SetOptions(double *cm,double *prop,struct sanneal *anneal,
                int *iseed,
                long int *repeats,char savename[],struct sfname *fname,char filename[],
                int *runopts,double *misslevel,int *heurotype,int *verbose,int *clumptype,
                int *itimptype,
                double *costthresh,double *tpf1,double *tpf2);

int ReadPUCosts(int puno,struct spustuff pu[],struct binsearch PULookup[],int verbose,char indir[]);
int ReadPUFile(int puno,struct spustuff pu[],struct binsearch PULookup[],int verbose,char indir[]);
int ReadPUXYfile(int puno,struct spustuff pu[],struct binsearch PULookup[],char indir[]);
int ReadPUData(int *puno,struct spustuff *pu[],struct sfname fnames);
int ReadSpeciesData(int *spno,struct sspecies *spec[],struct sfname fnames);
int ReadGenSpeciesData(int *gspno,struct sgenspec *gspec[],struct sfname fnames);
int DumpAsymmetricConnectivityFile(int puno,struct sconnections connections[],struct spustuff pu[],struct sfname fnames);
int ReadConnections(int puno,struct sconnections connections[],int verbose,struct spustuff pu[],
                    struct binsearch PULookup[],struct sfname fnames);
int PrepareWeightedConnectivityFile(struct sfname fnames);

void ReadPUVSPFile22(int puno,int spno,struct spu SM[],int verbose,struct spustuff pu[],
    typesp spec[],struct sfname fnames);
void ReadPUVSPFileTable(FILE *infile, int puno,int spno,struct spu SM[],struct spustuff pu[],
    typesp spec[]);

/* new functions added by Matt for Marxan optimization */
void LoadSparseMatrix(int *iSMSize, struct spu *SM[], int puno, int spno, struct spustuff PU[],
                      struct binsearch PULookup[],struct binsearch SPLookup[],
                      struct sfname fnames);
void DumpSparseMatrix(int iSMno,int puno, struct spustuff PU[], struct sspecies spec[], struct spu SM[],struct sfname fnames);
void LoadSparseMatrix_sporder(int *iSMSize, struct spusporder *SM[], int puno, int spno,
                              struct binsearch PULookup[],struct binsearch SPLookup[],// typesp spec[],
                              struct sfname fnames);
void DumpPU_richness_offset(int puno, struct spustuff PU[],struct sfname fnames);
void DumpBinarySearchArrays(char *sName,struct sfname fnames, int puno, int spno, struct binsearch PULookup[],
                            struct binsearch SPLookup[]);

void PrepareBinarySearchArrays(int puno, int spno, struct spustuff PU[], typesp spec[],
                               struct binsearch *PULookup[], struct binsearch *SPLookup[]);
void TestFastNameToPUID(int puno, struct binsearch PULookup[], struct spustuff PU[], struct sfname fnames);
int FastNameToPUID(int puno,int name, struct binsearch PULookup[]);
void TestFastNameToSPID(int spno, struct binsearch SPLookup[], typesp spec[], struct sfname fnames);
int FastNameToSPID(int spno,int name, struct binsearch SPLookup[]);
int rtnIdxSpecAtPu(struct spustuff PU[], struct spu SM[], int iPUIndex, int iSpecIndex);
double rtnAmountSpecAtPu(struct spustuff PU[], struct spu SM[], int iPUIndex, int iSpecIndex);
int rtnClumpSpecAtPu(struct spustuff PU[], struct spu SM[], int iPUIndex, int iSpecIndex);
void setClumpSpecAtPu(struct spustuff PU[], struct spu SM[], int iPUIndex, int iSpecIndex, int iSetClump);
void TestrtnAmountSpecAtPu(int puno, int spno, struct spustuff pu[], struct sspecies spec[], struct spu SM[],
                           struct sfname fnames);
void StartDebugTraceFile(void);
void AppendDebugTraceFile(char sMess[],...);

#endif

/* FILEIN.H END */
/* ************************************************************************** */
/* FILEOUT.H START */

#ifndef spexioheader
#define spexioheader
void SaveSeed(int iseed);
void OutputSummary(int puno,int spno,int R[],struct sspecies spec[],struct scost reserve,
                   int itn,char savename[],double misslevel,int imode);
void OutputScenario(int puno,int spno,double prop,double cm,
    struct sanneal anneal, int seedinit,long int repeats,int clumptype,
    int runopts,int heurotype,double costthresh, double tpf1, double tpf2,
    char savename[]);
void OutputSolution(int puno,int R[],struct spustuff pu[],char savename[],int imode,struct sfname fnames);
void OutputSpecies(int spno,struct sspecies spec[],char savename[],int imode,double misslevel);
void OutputSumSoln(int puno,int sumsoln[],struct spustuff pu[],char savename[],int imode);

#endif

/* FILEOUT.H END */
/* ************************************************************************** */
/* HEURISTIC.H START */

#ifndef heuristicheader
#define heuristicheader


double GreedyPen(int ipu, int puno, int spno, typesp spec[],int R[],struct spustuff pu[],
                 struct spu SM[],int clumptype);
double GreedyScore(int ipu,int puno,int spno, typesp *spec,struct spu SM[],struct sconnections connections[],
                   int R[],struct spustuff pu[],double cm,int clumptype);
void SetRareness(int puno, int spno, double Rare[],int R[],struct spustuff pu[],struct spu SM[]);
double RareScore(int isp,int ipu,int puno,typesp spec[],struct spu SM[], int R[],
    struct sconnections connections[], struct spustuff pu[], double cm,int clumptype);
double MaxRareScore(int ipu,int puno,struct sspecies spec[],struct spu SM[],
    int R[],struct sconnections connections[],struct spustuff pu[],double cm,double Rare[],int clumptype);
double BestRareScore(int ipu,int puno,struct sspecies spec[],struct spu SM[],
    int R[],struct sconnections connections[],struct spustuff pu[],double cm,double Rare[],int clumptype);
double AveRareScore(int ipu,int puno,struct sspecies spec[],struct spu SM[],
    int R[],struct sconnections connections[],struct spustuff pu[],double cm,double Rare[],int clumptype);
double SumRareScore(int ipu,int puno,struct sspecies spec[],struct spu SM[],
    int R[],struct sconnections connections[],struct spustuff pu[],double cm,double Rare[],int clumptype);
void SetAbundance(int puno,double Rare[],struct spustuff pu[],struct spu SM[]);
double Irreplaceability(int ipu,int isp, double Rare[],struct spustuff pu[],struct spu SM[],typesp *spec);
double ProdIrr(int ipu,double Rare[],struct spustuff pu[],struct spu SM[],typesp *spec);
double SumIrr(int ipu,double Rare[],struct spustuff pu[],struct spu SM[],typesp *spec);
void Heuristics(int spno,int puno,struct spustuff pu[],struct sconnections connections[],
        int R[], double cm,typesp *spec,struct spu SM[], struct scost *reserve,
        double costthresh, double tpf1,double tpf2, int imode,int clumptype);

#endif

/* HEURISTIC.H END */
/* ************************************************************************** */
/* ITIMP.H START */

#ifndef itimpheader
#define itimpheader

struct slink* ItImpDiscard(int ichoice, struct slink *list, struct slink **discard);
struct slink* ItImpUndiscard(struct slink *list, struct slink **discard);
int FindSwap( struct slink **list,double targetval,int itestchoice,int puuntried,
             int puno,int spno,struct spustuff pu[], struct sconnections connections[],
             struct sspecies spec[],struct spu SM[],
             int R[], double cm, struct scost *reserve, struct scost *change,
             double costthresh, double tpf1, double tpf2, int clumptype);
void IterativeImprovement(int puno,int spno,struct spustuff pu[], struct sconnections connections[],
                           struct sspecies spec[],struct spu SM[],int R[], double cm,
                           struct scost *reserve,struct scost *change,double costthresh,double tpf1, double tpf2,
                           int clumptype,int itimptype);
void IterativeImprovementOptimise(int puno,int spno,struct spustuff pu[], struct sconnections connections[],
                                   struct sspecies spec[],struct spu SM[],int R[], double cm,
                                   struct scost *reserve,struct scost *change,double costthresh,double tpf1, double tpf2,
                                   int clumptype,int irun,char *savename);

#endif

/* ITIMP.H END */
/* ************************************************************************** */
/* RANDOM.H START */

#ifndef randomheader
#define randomheader

float rand1(void);
void InitRandSeed(int iSeed);
int RandNum (int num);


#endif

/* RANDOM.H END */
/* ************************************************************************** */
/* SCREENOUT.H START */

#ifndef outputheader
#define outputheader
/* Prototype Headers */

void ShowStartupScreen(void);        /* Displays splash screen info */
void ShowShutdownScreen(void);        /* displays program termination info */
void ShowPauseExit(void);            /* Press any key to terminate */
void ShowPauseProg(void);            /* Press any key to continue */

void SetVerbosity(int verb);

void ShowErrorMessage(char sMess[],...);  /* Displays message then terminates */
void ShowWarningMessage(char sMess[],...);/* Displays message without terminating */

void ShowProg(char sMess[],...);
void ShowGenProg(char sMess[],...);
void ShowGenProgInfo(char sMess[],...);
void ShowDetProg(char sMess[],...);

void ShowTimePassed(void);

void SetLogFile(int my_savelog, char* my_savelogname);

#endif

/* SCREENOUT.H END */
/* ************************************************************************** */
/* SEPERATION.H START */

#ifndef sepheader
#define sepheader

/* Structure Definitions */

typedef struct sseplist{
    int size;
    struct slink *head,*tail;
} typeseplist;


/* Function Prototypes */

double SepPenalty(int ival);
double SepPenalty2(int ival, int itarget);
int ValidPU(int ipu,int isp,struct sclumps *newno,struct sspecies spec[],struct spustuff pu[],
            struct spu SM[],int imode);
int CheckDistance(int i, int j,struct spustuff pu[],double squaretarget);
int CountSeparation(int isp,struct sclumps *newno,
    struct spustuff pu[],struct spu SM[],typesp spec[],int imode);
struct slink *makelist(int isp,int inpu, int puno,int R[],struct sclumps *newno,struct sspecies spec[],
                       struct spustuff pu[],struct spu SM[],int imode);
int SepDealList(struct slink *head, typeseplist *Dist,struct spustuff *pu,
        struct sspecies spec[],int first,int sepnum,double targetdist,int isp);
int CountSeparation2(int isp,int ipu,struct sclumps *newno,int puno,int R[],
    struct spustuff pu[],struct spu SM[],typesp spec[],int imode);

    /* Debug files */

void CheckDist(struct sseplist *Dist,int sepnum);


#endif

/* SEPERATION.H END */
/* ************************************************************************** */

void CalcTotalAreas(int puno,int spno,struct spustuff pu[],struct sspecies spec[],struct spu SM[]);

void DumpR(int iMessage,char sMessage[],int puno,int R[],struct spustuff pu[],struct sfname fnames);

void Dump_specrichoff(int spno,struct sspecies spec[],struct sfname fnames);
void Dump_SparseMatrix_sporder(int iSMSize, struct spusporder SM[],struct spustuff pu[],struct sfname fnames);
void ApplySpecProp(int spno,typesp spec[],int puno,struct spustuff pu[],struct spu SM[]);

void DumpProbData(int puno,struct spustuff pu[],struct sfname fnames);

double probZUT(double z);
double probZLT(double z);

void ComputeP_AllPUsSelected_1D(char savename[],int puno,int spno,struct spustuff pu[],struct spu SM[],struct sspecies spec[]);
void ComputeP_AllPUsSelected_2D(char savename[],int puno,int spno,struct spustuff pu[],struct spu SM[],struct sspecies spec[]);
double ChangeProbability1D(int iIteration, int ipu, int spno,int puno,struct sspecies spec[],struct spustuff pu[],struct spu SM[],int imode);
double ChangeProbability2D(int iIteration, int ipu, int spno,int puno,struct sspecies spec[],struct spustuff pu[],struct spu SM[],int imode);
double Probability(int ipu, int spno,int puno,struct sspecies spec[],struct spustuff pu[],struct spu SM[]);
void ReturnProbabilityAmounts1D(double ExpectedAmount1D[],double VarianceInExpectedAmount1D[],int ipu,
                                int puno,struct spustuff pu[],struct spu SM[]);
void ReturnProbabilityAmounts2D(double ExpectedAmount2D[],double VarianceInExpectedAmount2D[],int ipu,
                                int puno,struct spustuff pu[],struct spu SM[]);
double ComputeProbability1D(double ExpectedAmount1D[],double VarianceInExpectedAmount1D[],
                            int spno,struct sspecies spec[]);
double ComputeProbability2D(double ExpectedAmount2D[],double VarianceInExpectedAmount2D[],
                            int spno,struct sspecies spec[]);
void StartDebugFile(char sFileName[],char sHeader[],struct sfname fnames);
void AppendDebugFile(char sFileName[],char sLine[],struct sfname fnames);
void OutputPenalty(int spno,struct sspecies spec[],char savename[],int iOutputType);
void OutputPenaltyPlanningUnits(int puno,struct spustuff pu[],int Rtemp[],char savename[],int iOutputType);
void OutputRichness(int puno,struct spustuff pu[],char savename[],int iOutputType);
void OutputSpeciesData(int spno,struct sspecies spec[],char savename[]);

// functions for cluster analysis in R
void InitSolutionsMatrix(int puno,struct spustuff pu[],char savename_ism[],int iOutputType,int iIncludeHeaders);
void AppendSolutionsMatrix(int iRun,int puno,int R[],char savename[],int iOutputType,int iIncludeHeaders);
void WriteRScripts(int iSolutionCount, char solutionsfilename[],char savename[], struct sfname fnames);
void ExecuteRScript(struct sfname fnames);

//void Load2DProbData(int *iSMSize, struct smprob *probSM[], int puno, int spno, struct spustuff pu[],
//                    struct binsearch PULookup[],struct binsearch SPLookup[],
//                    struct sfname fnames);
//void Dump2DProbData(int iSMSize, struct smprob probSM[], int puno,struct spustuff pu[],struct sspecies spec[],struct sfname fnames);
void WriteSlaveSyncFileRun(int iSyncRun);
void SlaveExit(void);
void OutputTotalAreas(int puno,int spno,struct spustuff pu[],struct sspecies spec[],struct spu SM[],char savename[],int iOutputType);
void CopyFile(char sInputFile[],char sOutputFile[]);

