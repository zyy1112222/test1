* Encoding: UTF-8.

DATASET ACTIVATE DataSet1.
USE ALL.
COMPUTE filter_$=(include = 1).
VARIABLE LABELS filter_$ 'include = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

* Generalized Linear Models.
GENLIN punishment (REFERENCE=FIRST) BY condition (ORDER=ASCENDING)
  /MODEL condition INTERCEPT=YES
 DISTRIBUTION=BINOMIAL LINK=LOGIT
  /CRITERIA METHOD=FISHER(1) SCALE=1 COVB=MODEL MAXITERATIONS=100 MAXSTEPHALVING=5 
    PCONVERGE=1E-006(ABSOLUTE) SINGULAR=1E-012 ANALYSISTYPE=3(WALD) CILEVEL=95 CITYPE=WALD 
    LIKELIHOOD=FULL
  /EMMEANS TABLES=condition SCALE=ORIGINAL
  /MISSING CLASSMISSING=EXCLUDE
  /PRINT CPS DESCRIPTIVES MODELINFO FIT SUMMARY SOLUTION.

FILTER OFF.
USE ALL.
EXECUTE.

USE ALL.
COMPUTE filter_$=(include = 1 AND (condition = 1 OR condition = 2)).
VARIABLE LABELS filter_$ 'include = 1 AND (condition = 1 OR condition = 2) (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.



USE ALL.
COMPUTE filter_$=(include = 1 AND (condition = 1 OR condition = 3)).
VARIABLE LABELS filter_$ 'include = 1 AND (condition = 1 OR condition = 3) (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

USE ALL.
COMPUTE filter_$=(include = 1 AND (condition = 2 OR condition = 3)).
VARIABLE LABELS filter_$ 'include = 1 AND (condition = 2 OR condition = 3) (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

* Generalized Linear Models.
GENLIN punishment (REFERENCE=FIRST) BY condition study (ORDER=ASCENDING)
  /MODEL condition study condition*study INTERCEPT=YES
 DISTRIBUTION=BINOMIAL LINK=LOGIT
  /CRITERIA METHOD=FISHER(1) SCALE=1 COVB=MODEL MAXITERATIONS=100 MAXSTEPHALVING=5 
    PCONVERGE=1E-006(ABSOLUTE) SINGULAR=1E-012 ANALYSISTYPE=3(WALD) CILEVEL=95 CITYPE=WALD 
    LIKELIHOOD=FULL
  /EMMEANS TABLES=condition SCALE=ORIGINAL
  /MISSING CLASSMISSING=EXCLUDE
  /PRINT CPS DESCRIPTIVES MODELINFO FIT SUMMARY SOLUTION.

SORT CASES  BY condition.
SPLIT FILE SEPARATE BY condition.

* Generalized Linear Models.
GENLIN punishment (REFERENCE=FIRST) BY study (ORDER=ASCENDING)
  /MODEL study INTERCEPT=YES
 DISTRIBUTION=BINOMIAL LINK=LOGIT
  /CRITERIA METHOD=FISHER(1) SCALE=1 COVB=MODEL MAXITERATIONS=100 MAXSTEPHALVING=5 
    PCONVERGE=1E-006(ABSOLUTE) SINGULAR=1E-012 ANALYSISTYPE=3(WALD) CILEVEL=95 CITYPE=WALD 
    LIKELIHOOD=FULL
  /EMMEANS TABLES=study SCALE=ORIGINAL
  /MISSING CLASSMISSING=EXCLUDE
  /PRINT CPS DESCRIPTIVES MODELINFO FIT SUMMARY SOLUTION.

SPLIT FILE OFF.

* Generalized Linear Models.
GENLIN punishment (REFERENCE=FIRST) BY condition (ORDER=ASCENDING) WITH agecontinuous
  /MODEL condition agecontinuous condition*agecontinuous INTERCEPT=YES
 DISTRIBUTION=BINOMIAL LINK=LOGIT
  /CRITERIA METHOD=FISHER(1) SCALE=1 COVB=MODEL MAXITERATIONS=100 MAXSTEPHALVING=5 
    PCONVERGE=1E-006(ABSOLUTE) SINGULAR=1E-012 ANALYSISTYPE=3(WALD) CILEVEL=95 CITYPE=WALD 
    LIKELIHOOD=FULL
  /EMMEANS TABLES=condition SCALE=ORIGINAL
  /MISSING CLASSMISSING=EXCLUDE
  /PRINT CPS DESCRIPTIVES MODELINFO FIT SUMMARY SOLUTION.



DATASET ACTIVATE DataSet1.
USE ALL.
COMPUTE filter_$=(include = 1 AND (agecategorical = 5 OR agecategorical = 6 OR agecategorical = 7) 
    ).
VARIABLE LABELS filter_$ 'include = 1 AND (agecategorical = 5 OR agecategorical = 6 OR '+
    'agecategorical = 7)  (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

USE ALL.
COMPUTE filter_$=(include = 1 AND (agecategorical = 5 OR agecategorical = 6 OR agecategorical = 7) 
    AND (condition = 1 OR condition = 2)).
VARIABLE LABELS filter_$ 'include = 1 AND (agecategorical = 5 OR agecategorical = 6 OR '+
    'agecategorical = 7) AND (condition = 1 OR condition = 2) (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.
