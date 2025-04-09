* Encoding: UTF-8.


USE ALL.
COMPUTE filter_$=(include = 1).
VARIABLE LABELS filter_$ 'include = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

***main analyses***

* Generalized Estimating Equations.
GENLIN punishyesno (REFERENCE=FIRST) BY condition_recodedintuitive (ORDER=ASCENDING)
  /MODEL condition_recodedintuitive INTERCEPT=YES
 DISTRIBUTION=BINOMIAL LINK=LOGIT
  /CRITERIA METHOD=FISHER(1) SCALE=1 MAXITERATIONS=100 MAXSTEPHALVING=5 PCONVERGE=1E-006(ABSOLUTE) 
    SINGULAR=1E-012 ANALYSISTYPE=3(WALD) CILEVEL=95 LIKELIHOOD=FULL
  /EMMEANS TABLES=condition_recodedintuitive SCALE=ORIGINAL
  /REPEATED SUBJECT=participantnumber SORT=YES CORRTYPE=INDEPENDENT ADJUSTCORR=YES COVB=ROBUST 
    MAXITERATIONS=100 PCONVERGE=1e-006(ABSOLUTE) UPDATECORR=1
  /MISSING CLASSMISSING=EXCLUDE
  /PRINT CPS DESCRIPTIVES MODELINFO FIT SUMMARY SOLUTION.


** comm v non comm** 

USE ALL.
COMPUTE filter_$=(include = 1 AND (condition_recodedintuitive = 1 OR condition_recodedintuitive = 
    2)).
VARIABLE LABELS filter_$ 'include = 1 AND (condition_recodedintuitive = 1 OR '+
    'condition_recodedintuitive = 2) (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

**noncomm vs. control**

USE ALL.
COMPUTE filter_$=(include = 1 AND (condition_recodedintuitive = 2 OR condition_recodedintuitive = 
    3)).
VARIABLE LABELS filter_$ 'include = 1 AND (condition_recodedintuitive = 2 OR '+
    'condition_recodedintuitive = 3) (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.


**comm v control**

USE ALL.
COMPUTE filter_$=(include = 1 AND (condition_recodedintuitive = 1 OR condition_recodedintuitive = 
    3)).
VARIABLE LABELS filter_$ 'include = 1 AND (condition_recodedintuitive = 1 OR '+
    'condition_recodedintuitive = 3) (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.


**select all**

USE ALL.
COMPUTE filter_$=(include = 1 ).
VARIABLE LABELS filter_$ 'include = 1  (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

**controlling for age**

* Generalized Linear Models.
GENLIN punishyesno (REFERENCE=FIRST) BY condition (ORDER=ASCENDING) WITH preciseage
  /MODEL condition preciseage condition*preciseage INTERCEPT=YES
 DISTRIBUTION=BINOMIAL LINK=LOGIT
  /CRITERIA METHOD=FISHER(1) SCALE=1 COVB=MODEL MAXITERATIONS=100 MAXSTEPHALVING=5 
    PCONVERGE=1E-006(ABSOLUTE) SINGULAR=1E-012 ANALYSISTYPE=3(WALD) CILEVEL=95 CITYPE=WALD 
    LIKELIHOOD=FULL
  /EMMEANS TABLES=condition SCALE=ORIGINAL
  /MISSING CLASSMISSING=EXCLUDE
  /PRINT CPS DESCRIPTIVES MODELINFO FIT SUMMARY SOLUTION.


**including all participants**


FILTER OFF.
USE ALL.
EXECUTE.

**comm vs noncomm**

USE ALL.
COMPUTE filter_$=(condition_recodedintuitive = 1 OR condition_recodedintuitive = 2).
VARIABLE LABELS filter_$ 'condition_recodedintuitive = 1 OR condition_recodedintuitive = 2 '+
    '(FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

**comm vs control**

USE ALL.
COMPUTE filter_$=(condition_recodedintuitive = 1 OR condition_recodedintuitive = 3).
VARIABLE LABELS filter_$ 'condition_recodedintuitive = 1 OR condition_recodedintuitive = 3 '+
    '(FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

**non-comm vs control**

USE ALL.
COMPUTE filter_$=(condition_recodedintuitive = 2 OR condition_recodedintuitive = 3).
VARIABLE LABELS filter_$ 'condition_recodedintuitive = 2 OR condition_recodedintuitive = 3 '+
    '(FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.


***supplemental materials***

USE ALL.
COMPUTE filter_$=(include = 0).
VARIABLE LABELS filter_$ 'include = 0 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.


DATASET ACTIVATE DataSet3.
FREQUENCIES VARIABLES=age
  /ORDER=ANALYSIS.



USE ALL.
COMPUTE filter_$=(include = 0 AND (age = 4 OR age = 5 OR age = 6 OR age =7)).
VARIABLE LABELS filter_$ 'include = 0 AND (age = 4 OR age = 5 OR age = 6 OR age =7) (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

CROSSTABS
  /TABLES=condition_recodedintuitive BY age
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT
  /COUNT ROUND CELL.

FILTER OFF.
USE ALL.
EXECUTE.

CROSSTABS
  /TABLES=understandsboxes BY age
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT
  /COUNT ROUND CELL.

CROSSTABS
  /TABLES=understandsknowledge BY age
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT
  /COUNT ROUND CELL.

CROSSTABS
  /TABLES=understandslessonlearning BY age
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT
  /COUNT ROUND CELL.

CROSSTABS
  /TABLES=understandssadness BY age
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT
  /COUNT ROUND CELL.


**costly validation measures"**



USE ALL.
COMPUTE filter_$=(include = 1).
VARIABLE LABELS filter_$ 'include = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

FREQUENCIES VARIABLES=likeplayingonipad wanttoplayonipadmore
  /STATISTICS=STDDEV MEAN MEDIAN
  /ORDER=ANALYSIS.

ONEWAY likeplayingonipad wanttoplayonipadmore BY condition_recodedintuitive
  /STATISTICS DESCRIPTIVES 
  /MISSING ANALYSIS.



RECODE likeplayingonipad wanttoplayonipadmore (1=0) (ELSE=1) INTO likeplayingonipad_exclusion 
    wanttoplayonipad_exclusion.
EXECUTE.

if(likeplayingonipad_exclusion = 0 OR wanttoplayonipad_exclusion = 0) ipadpreference_exclusion = 0.
if(likeplayingonipad_exclusion = 1 AND wanttoplayonipad_exclusion = 1) ipadpreference_exclusion = 1. 
execute. 



USE ALL.
COMPUTE filter_$=(include = 1 AND ipadpreference_exclusion = 1).
VARIABLE LABELS filter_$ 'include = 1 AND ipadpreference_exclusion = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.


* Generalized Estimating Equations.
GENLIN punishyesno (REFERENCE=FIRST) BY condition_recodedintuitive (ORDER=ASCENDING)
  /MODEL condition_recodedintuitive INTERCEPT=YES
 DISTRIBUTION=BINOMIAL LINK=LOGIT
  /CRITERIA METHOD=FISHER(1) SCALE=1 MAXITERATIONS=100 MAXSTEPHALVING=5 PCONVERGE=1E-006(ABSOLUTE) 
    SINGULAR=1E-012 ANALYSISTYPE=3(WALD) CILEVEL=95 LIKELIHOOD=FULL
  /EMMEANS TABLES=condition_recodedintuitive SCALE=ORIGINAL
  /REPEATED SUBJECT=participantnumber SORT=YES CORRTYPE=INDEPENDENT ADJUSTCORR=YES COVB=ROBUST 
    MAXITERATIONS=100 PCONVERGE=1e-006(ABSOLUTE) UPDATECORR=1
  /MISSING CLASSMISSING=EXCLUDE
  /PRINT CPS DESCRIPTIVES MODELINFO FIT SUMMARY SOLUTION.




FREQUENCIES VARIABLES=ipadpreference_exclusion
  /ORDER=ANALYSIS.

**measure validation item**



USE ALL.
COMPUTE filter_$=(include = 1).
VARIABLE LABELS filter_$ 'include = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

UNIANOVA meanness BY condition_recodedintuitive
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL) 
  /EMMEANS=TABLES(condition_recodedintuitive) COMPARE ADJ(LSD)
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=condition_recodedintuitive.



**emotion validation items**

UNIANOVA happiness BY condition_recodedintuitive
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL) 
  /EMMEANS=TABLES(condition_recodedintuitive) COMPARE ADJ(LSD)
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=condition_recodedintuitive.

UNIANOVA sadness BY condition_recodedintuitive
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL) 
  /EMMEANS=TABLES(condition_recodedintuitive) COMPARE ADJ(LSD)
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=condition_recodedintuitive.

UNIANOVA anger BY condition_recodedintuitive
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL) 
  /EMMEANS=TABLES(condition_recodedintuitive) COMPARE ADJ(LSD)
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=condition_recodedintuitive.


*projected sadness measure*


UNIANOVA projectedsadness BY condition_recodedintuitive
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL) 
  /EMMEANS=TABLES(condition_recodedintuitive) COMPARE ADJ(LSD)
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=condition_recodedintuitive.

SORT CASES  BY condition_recodedintuitive.
SPLIT FILE SEPARATE BY condition_recodedintuitive.


FREQUENCIES VARIABLES=projectedsadness
  /STATISTICS=STDDEV MEAN MEDIAN
  /ORDER=ANALYSIS.


SPLIT FILE OFF.


**continuous punishment**



USE ALL.
COMPUTE filter_$=(include = 1).
VARIABLE LABELS filter_$ 'include = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

UNIANOVA punishcontinuous BY condition_recodedintuitive
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL) 
  /EMMEANS=TABLES(condition_recodedintuitive) COMPARE ADJ(LSD)
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=condition_recodedintuitive.

**age analyses**

UNIANOVA punishcontinuous BY condition_recodedintuitive WITH preciseage
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL) WITH(preciseage=MEAN) 
  /EMMEANS=TABLES(condition_recodedintuitive) WITH(preciseage=MEAN) COMPARE ADJ(LSD)
  /PRINT=ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=condition_recodedintuitive preciseage condition_recodedintuitive*preciseage.


**video frequencies**



CROSSTABS
  /TABLES=gender BY video
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT
  /COUNT ROUND CELL.


USE ALL.
COMPUTE filter_$=(include = 1 AND (condition_recodedintuitive = 1 OR condition_recodedintuitive = 
    2)).
VARIABLE LABELS filter_$ 'include = 1 AND (condition_recodedintuitive = 1 OR '+
    'condition_recodedintuitive = 2) (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.


* Generalized Linear Models.
GENLIN punishyesno (REFERENCE=FIRST) BY video gender condition_recodedintuitive (ORDER=ASCENDING)
  /MODEL video gender condition_recodedintuitive video*gender video*condition_recodedintuitive 
    gender*condition_recodedintuitive video*gender*condition_recodedintuitive INTERCEPT=YES
 DISTRIBUTION=BINOMIAL LINK=LOGIT
  /CRITERIA METHOD=FISHER(1) SCALE=1 COVB=MODEL MAXITERATIONS=100 MAXSTEPHALVING=5 
    PCONVERGE=1E-006(ABSOLUTE) SINGULAR=1E-012 ANALYSISTYPE=3(WALD) CILEVEL=95 CITYPE=WALD 
    LIKELIHOOD=FULL
  /EMMEANS TABLES=video SCALE=ORIGINAL
  /EMMEANS TABLES=gender SCALE=ORIGINAL
  /EMMEANS TABLES=condition_recodedintuitive SCALE=ORIGINAL
  /EMMEANS TABLES=video*gender SCALE=ORIGINAL
  /EMMEANS TABLES=video*condition_recodedintuitive SCALE=ORIGINAL
  /EMMEANS TABLES=gender*condition_recodedintuitive SCALE=ORIGINAL
  /EMMEANS TABLES=video*gender*condition_recodedintuitive SCALE=ORIGINAL
  /MISSING CLASSMISSING=EXCLUDE
  /PRINT CPS DESCRIPTIVES MODELINFO FIT SUMMARY SOLUTION.

* Generalized Linear Models.
GENLIN punishyesno (REFERENCE=FIRST) BY video gender (ORDER=ASCENDING)
  /MODEL video gender video*gender INTERCEPT=YES
 DISTRIBUTION=BINOMIAL LINK=LOGIT
  /CRITERIA METHOD=FISHER(1) SCALE=1 COVB=MODEL MAXITERATIONS=100 MAXSTEPHALVING=5 
    PCONVERGE=1E-006(ABSOLUTE) SINGULAR=1E-012 ANALYSISTYPE=3(WALD) CILEVEL=95 CITYPE=WALD 
    LIKELIHOOD=FULL
  /EMMEANS TABLES=video SCALE=ORIGINAL
  /EMMEANS TABLES=gender SCALE=ORIGINAL
  /EMMEANS TABLES=video*gender SCALE=ORIGINAL
  /MISSING CLASSMISSING=EXCLUDE
  /PRINT CPS DESCRIPTIVES MODELINFO FIT SUMMARY SOLUTION.

* Generalized Linear Models.
GENLIN punishyesno (REFERENCE=FIRST) BY video (ORDER=ASCENDING)
  /MODEL video INTERCEPT=YES
 DISTRIBUTION=BINOMIAL LINK=LOGIT
  /CRITERIA METHOD=FISHER(1) SCALE=1 COVB=MODEL MAXITERATIONS=100 MAXSTEPHALVING=5 
    PCONVERGE=1E-006(ABSOLUTE) SINGULAR=1E-012 ANALYSISTYPE=3(WALD) CILEVEL=95 CITYPE=WALD 
    LIKELIHOOD=FULL
  /EMMEANS TABLES=video SCALE=ORIGINAL
  /MISSING CLASSMISSING=EXCLUDE
  /PRINT CPS DESCRIPTIVES MODELINFO FIT SUMMARY SOLUTION.

***age table ***


SORT CASES  BY condition_recodedintuitive.
SPLIT FILE SEPARATE BY condition_recodedintuitive.


CROSSTABS
  /TABLES=age BY punishyesno
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT ROW COLUMN TOTAL 
  /COUNT ROUND CELL.


**excluding kids who didn't like playing on the ipad***

USE ALL.
COMPUTE filter_$=(include = 1 AND likeplayingonipad_exclusion = 1).
VARIABLE LABELS filter_$ 'include = 1 AND likeplayingonipad_exclusion = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.
