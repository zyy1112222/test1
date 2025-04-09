* Encoding: UTF-8.

USE ALL.
COMPUTE filter_$=(include = 1).
VARIABLE LABELS filter_$ 'include = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

* Generalized Linear Models.
GENLIN boxselection (REFERENCE=FIRST) BY condition (ORDER=ASCENDING)
  /MODEL condition INTERCEPT=YES
 DISTRIBUTION=BINOMIAL LINK=LOGIT
  /CRITERIA METHOD=FISHER(1) SCALE=1 COVB=MODEL MAXITERATIONS=100 MAXSTEPHALVING=5 
    PCONVERGE=1E-006(ABSOLUTE) SINGULAR=1E-012 ANALYSISTYPE=3(WALD) CILEVEL=95 CITYPE=WALD 
    LIKELIHOOD=FULL
  /EMMEANS TABLES=condition SCALE=ORIGINAL
  /MISSING CLASSMISSING=EXCLUDE
  /PRINT CPS DESCRIPTIVES MODELINFO FIT SUMMARY SOLUTION.

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
GENLIN boxselection (REFERENCE=FIRST) BY condition (ORDER=ASCENDING) WITH agecontinuous
  /MODEL condition agecontinuous condition*agecontinuous INTERCEPT=YES
 DISTRIBUTION=BINOMIAL LINK=LOGIT
  /CRITERIA METHOD=FISHER(1) SCALE=1 COVB=MODEL MAXITERATIONS=100 MAXSTEPHALVING=5 
    PCONVERGE=1E-006(ABSOLUTE) SINGULAR=1E-012 ANALYSISTYPE=3(WALD) CILEVEL=95 CITYPE=WALD 
    LIKELIHOOD=FULL
  /EMMEANS TABLES=condition SCALE=ORIGINAL
  /EMMEANS TABLES=condition SCALE=ORIGINAL
  /MISSING CLASSMISSING=EXCLUDE
  /PRINT CPS DESCRIPTIVES MODELINFO FIT SUMMARY SOLUTION.

SORT CASES  BY condition.
SPLIT FILE SEPARATE BY condition.

CROSSTABS
  /TABLES=agecategorical BY boxselection
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT
  /COUNT ROUND CELL.



SPLIT FILE OFF.

USE ALL.
COMPUTE filter_$=(include = 1 AND (condition = 1 OR condition = 2)).
VARIABLE LABELS filter_$ 'include = 1 AND (condition = 1 OR condition = 2) (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.


* Generalized Linear Models.
GENLIN boxselection (REFERENCE=FIRST) BY condition (ORDER=ASCENDING) WITH agecontinuous
  /MODEL condition agecontinuous condition*agecontinuous INTERCEPT=YES
 DISTRIBUTION=BINOMIAL LINK=LOGIT
  /CRITERIA METHOD=FISHER(1) SCALE=1 COVB=MODEL MAXITERATIONS=100 MAXSTEPHALVING=5 
    PCONVERGE=1E-006(ABSOLUTE) SINGULAR=1E-012 ANALYSISTYPE=3(WALD) CILEVEL=95 CITYPE=WALD 
    LIKELIHOOD=FULL
  /EMMEANS TABLES=condition SCALE=ORIGINAL
  /EMMEANS TABLES=condition SCALE=ORIGINAL
  /MISSING CLASSMISSING=EXCLUDE
  /PRINT CPS DESCRIPTIVES MODELINFO FIT SUMMARY SOLUTION.


* Generalized Linear Models.
GENLIN recidivism (REFERENCE=FIRST) BY condition boxselection (ORDER=ASCENDING)
  /MODEL condition boxselection condition*boxselection INTERCEPT=YES
 DISTRIBUTION=BINOMIAL LINK=LOGIT
  /CRITERIA METHOD=FISHER(1) SCALE=1 COVB=MODEL MAXITERATIONS=100 MAXSTEPHALVING=5 
    PCONVERGE=1E-006(ABSOLUTE) SINGULAR=1E-012 ANALYSISTYPE=3(WALD) CILEVEL=95 CITYPE=WALD 
    LIKELIHOOD=FULL
  /EMMEANS TABLES=condition SCALE=ORIGINAL
  /EMMEANS TABLES=condition SCALE=ORIGINAL
  /EMMEANS TABLES=condition SCALE=ORIGINAL
  /EMMEANS TABLES=boxselection SCALE=ORIGINAL
  /EMMEANS TABLES=condition*boxselection SCALE=ORIGINAL
  /MISSING CLASSMISSING=EXCLUDE
  /PRINT CPS DESCRIPTIVES MODELINFO FIT SUMMARY SOLUTION.

SORT CASES  BY condition.
SPLIT FILE SEPARATE BY condition.

* Generalized Linear Models.
GENLIN recidivism (REFERENCE=FIRST) BY boxselection (ORDER=ASCENDING)
  /MODEL boxselection INTERCEPT=YES
 DISTRIBUTION=BINOMIAL LINK=LOGIT
  /CRITERIA METHOD=FISHER(1) SCALE=1 COVB=MODEL MAXITERATIONS=100 MAXSTEPHALVING=5 
    PCONVERGE=1E-006(ABSOLUTE) SINGULAR=1E-012 ANALYSISTYPE=3(WALD) CILEVEL=95 CITYPE=WALD 
    LIKELIHOOD=FULL
  /EMMEANS TABLES=boxselection SCALE=ORIGINAL
  /MISSING CLASSMISSING=EXCLUDE
  /PRINT CPS DESCRIPTIVES MODELINFO FIT SUMMARY SOLUTION.

SPLIT FILE OFF.


* Generalized Linear Models.
GENLIN recidivism (REFERENCE=FIRST) BY boxselection condition (ORDER=ASCENDING) WITH agecontinuous
  /MODEL boxselection condition agecontinuous boxselection*condition boxselection*agecontinuous 
    condition*agecontinuous boxselection*condition*agecontinuous INTERCEPT=YES
 DISTRIBUTION=BINOMIAL LINK=LOGIT
  /CRITERIA METHOD=FISHER(1) SCALE=1 COVB=MODEL MAXITERATIONS=100 MAXSTEPHALVING=5 
    PCONVERGE=1E-006(ABSOLUTE) SINGULAR=1E-012 ANALYSISTYPE=3(WALD) CILEVEL=95 CITYPE=WALD 
    LIKELIHOOD=FULL
  /EMMEANS TABLES=boxselection SCALE=ORIGINAL
  /EMMEANS TABLES=boxselection SCALE=ORIGINAL
  /EMMEANS TABLES=condition SCALE=ORIGINAL
  /EMMEANS TABLES=boxselection*condition SCALE=ORIGINAL
  /MISSING CLASSMISSING=EXCLUDE
  /PRINT CPS DESCRIPTIVES MODELINFO FIT SUMMARY SOLUTION.

**supplemental materials**


USE ALL.
COMPUTE filter_$=(include = 0).
VARIABLE LABELS filter_$ 'include = 0 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

SORT CASES  BY condition.
SPLIT FILE SEPARATE BY condition.



FREQUENCIES VARIABLES=agecategorical
  /ORDER=ANALYSIS.

SPLIT FILE OFF.

FILTER OFF.
USE ALL.
EXECUTE.

CROSSTABS
  /TABLES=agecategorical BY boxcomprehensioncollapsed
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT
  /COUNT ROUND CELL.

CROSSTABS
  /TABLES=agecategorical BY knowledgecheck
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT
  /COUNT ROUND CELL.

CROSSTABS
  /TABLES=agecategorical BY lessonlearncheck
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT
  /COUNT ROUND CELL.

CROSSTABS
  /TABLES=agecategorical BY sadnesscheck
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT
  /COUNT ROUND CELL.


USE ALL.
COMPUTE filter_$=(include = 1).
VARIABLE LABELS filter_$ 'include = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.


CORRELATIONS
  /VARIABLES=agecontinuous likeplayingonipad playonipadmore boxselection
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.

DESCRIPTIVES VARIABLES=likeplayingonipad playonipadmore
  /STATISTICS=MEAN STDDEV MIN MAX.

FREQUENCIES VARIABLES=likeplayingonipad playonipadmore
  /ORDER=ANALYSIS.

UNIANOVA likeplayingonipad BY condition
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL) 
  /EMMEANS=TABLES(condition) COMPARE ADJ(LSD)
  /CRITERIA=ALPHA(0.05)
  /DESIGN=condition.

UNIANOVA playonipadmore BY condition
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /CRITERIA=ALPHA(0.05)
  /DESIGN=condition.

RECODE likeplayingonipad playonipadmore (0=0) (ELSE=1) INTO likeplayingonipad_forexclusions 
    playonipadmore_forexclusions.
EXECUTE.

if(likeplayingonipad_forexclusions = 0 OR playonipadmore_forexclusions = 0) ipadpreference_forexclusions = 0.
if(likeplayingonipad_forexclusions = 1 AND playonipadmore_forexclusions = 1) ipadpreference_forexclusions = 1.
execute.



FREQUENCIES VARIABLES=ipadpreference_forexclusions
  /ORDER=ANALYSIS.

USE ALL.
COMPUTE filter_$=(include = 1 AND ipadpreference_forexclusions = 1).
VARIABLE LABELS filter_$ 'include = 1 AND ipadpreference_forexclusions = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.


* Generalized Linear Models.
GENLIN boxselection (REFERENCE=FIRST) BY condition (ORDER=ASCENDING)
  /MODEL condition INTERCEPT=YES
 DISTRIBUTION=BINOMIAL LINK=LOGIT
  /CRITERIA METHOD=FISHER(1) SCALE=1 COVB=MODEL MAXITERATIONS=100 MAXSTEPHALVING=5 
    PCONVERGE=1E-006(ABSOLUTE) SINGULAR=1E-012 ANALYSISTYPE=3(WALD) CILEVEL=95 CITYPE=WALD 
    LIKELIHOOD=FULL
  /EMMEANS TABLES=condition SCALE=ORIGINAL
  /MISSING CLASSMISSING=EXCLUDE
  /PRINT CPS DESCRIPTIVES MODELINFO FIT SUMMARY SOLUTION.


USE ALL.
COMPUTE filter_$=(include = 1).
VARIABLE LABELS filter_$ 'include = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.


UNIANOVA sadness BY condition
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=condition.


USE ALL.
COMPUTE filter_$=(include = 1 AND (condition = 1 OR condition = 2)).
VARIABLE LABELS filter_$ 'include = 1 AND (condition = 1 OR condition = 2) (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

UNIANOVA happy_atdecision_recoded BY boxselection
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL) 
  /EMMEANS=TABLES(boxselection) COMPARE ADJ(LSD)
  /PRINT ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=boxselection.

UNIANOVA excited_atdecision_recoded BY boxselection
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=boxselection.

UNIANOVA sad_atdecision_recoded BY boxselection
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=boxselection.

UNIANOVA upset_atdecision_recoded BY boxselection
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=boxselection.

UNIANOVA happy_atdecision_recoded BY boxselection condition
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL) 
  /EMMEANS=TABLES(boxselection) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(condition) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(boxselection*condition) 
  /PRINT ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=boxselection condition boxselection*condition.


UNIANOVA happy_atdecision_recoded BY boxselection condition
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=boxselection condition boxselection*condition.

UNIANOVA excited_atdecision_recoded BY boxselection condition
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=boxselection condition boxselection*condition.

UNIANOVA sad_atdecision_recoded BY boxselection condition
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=boxselection condition boxselection*condition.

UNIANOVA upset_atdecision_recoded BY boxselection condition
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=boxselection condition boxselection*condition.

CORRELATIONS
  /VARIABLES=boxselection happy_atdecision_recoded excited_atdecision_recoded 
    sad_atdecision_recoded upset_atdecision_recoded
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.



USE ALL.
COMPUTE filter_$=(include = 1).
VARIABLE LABELS filter_$ 'include = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

UNIANOVA angry_atperson_recoded BY condition
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL) 
  /EMMEANS=TABLES(condition) COMPARE ADJ(LSD)
  /PRINT ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=condition.

UNIANOVA sad_atperson_recoded BY condition
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=condition.

UNIANOVA happy_atperson_recoded BY condition
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=condition.

UNIANOVA surprised_atperson_recoded BY condition
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=condition.




USE ALL.
COMPUTE filter_$=(include = 1 AND (condition = 1 OR condition = 2)).
VARIABLE LABELS filter_$ 'include = 1 AND (condition = 1 OR condition = 2) (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

UNIANOVA angry_atperson_recoded BY boxselection
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL) 
  /EMMEANS=TABLES(boxselection) COMPARE ADJ(LSD)
  /PRINT ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=boxselection.

UNIANOVA sad_atperson_recoded BY boxselection
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=boxselection.

UNIANOVA happy_atperson_recoded BY boxselection
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=boxselection.

UNIANOVA surprised_atperson_recoded BY boxselection
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=boxselection.

UNIANOVA angry_atperson_recoded BY boxselection condition
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL) 
  /EMMEANS=TABLES(boxselection) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(condition) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(boxselection*condition) 
  /PRINT ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=boxselection condition boxselection*condition.

UNIANOVA sad_atperson_recoded BY boxselection condition
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=boxselection condition boxselection*condition.

UNIANOVA happy_atperson_recoded BY boxselection condition
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=boxselection condition boxselection*condition.

UNIANOVA surprised_atperson_recoded BY boxselection condition
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=boxselection condition boxselection*condition.



USE ALL.
COMPUTE filter_$=(include = 1).
VARIABLE LABELS filter_$ 'include = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.


UNIANOVA evaluation_atperson BY condition
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /PRINT ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=condition.


USE ALL.
COMPUTE filter_$=(include = 1 AND (condition = 1 OR condition = 2)).
VARIABLE LABELS filter_$ 'include = 1 AND (condition = 1 OR condition = 2) (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

UNIANOVA deservingness BY boxselection
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL) 
  /EMMEANS=TABLES(boxselection) COMPARE ADJ(LSD)
  /PRINT ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=boxselection.

UNIANOVA deservingness BY boxselection condition
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /EMMEANS=TABLES(OVERALL) 
  /EMMEANS=TABLES(boxselection) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(condition) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(boxselection*condition) 
  /PRINT ETASQ DESCRIPTIVE
  /CRITERIA=ALPHA(.05)
  /DESIGN=boxselection condition boxselection*condition.


**beliefs***

USE ALL.
COMPUTE filter_$=(include = 1 AND (condition = 1 OR condition = 2)).
VARIABLE LABELS filter_$ 'include = 1 AND (condition = 1 OR condition = 2) (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.


* Generalized Linear Models.
GENLIN boxselection (REFERENCE=FIRST) BY condition (ORDER=ASCENDING) WITH punishmentitem_deserve
  /MODEL condition punishmentitem_deserve condition*punishmentitem_deserve INTERCEPT=YES
 DISTRIBUTION=BINOMIAL LINK=LOGIT
  /CRITERIA METHOD=FISHER(1) SCALE=1 COVB=MODEL MAXITERATIONS=100 MAXSTEPHALVING=5 
    PCONVERGE=1E-006(ABSOLUTE) SINGULAR=1E-012 ANALYSISTYPE=3(WALD) CILEVEL=95 CITYPE=WALD 
    LIKELIHOOD=FULL
  /EMMEANS TABLES=condition SCALE=ORIGINAL
  /MISSING CLASSMISSING=EXCLUDE
  /PRINT CPS DESCRIPTIVES MODELINFO FIT SUMMARY SOLUTION.

* Generalized Linear Models.
GENLIN boxselection (REFERENCE=FIRST) BY condition (ORDER=ASCENDING) WITH punishmentitem_rightthing
  /MODEL condition punishmentitem_rightthing condition*punishmentitem_rightthing INTERCEPT=YES
 DISTRIBUTION=BINOMIAL LINK=LOGIT
  /CRITERIA METHOD=FISHER(1) SCALE=1 COVB=MODEL MAXITERATIONS=100 MAXSTEPHALVING=5 
    PCONVERGE=1E-006(ABSOLUTE) SINGULAR=1E-012 ANALYSISTYPE=3(WALD) CILEVEL=95 CITYPE=WALD 
    LIKELIHOOD=FULL
  /EMMEANS TABLES=condition SCALE=ORIGINAL
  /MISSING CLASSMISSING=EXCLUDE
  /PRINT CPS DESCRIPTIVES MODELINFO FIT SUMMARY SOLUTION.

* Generalized Linear Models.
GENLIN boxselection (REFERENCE=FIRST) BY condition (ORDER=ASCENDING) WITH punishmentitem_lesson
  /MODEL condition punishmentitem_lesson condition*punishmentitem_lesson INTERCEPT=YES
 DISTRIBUTION=BINOMIAL LINK=LOGIT
  /CRITERIA METHOD=FISHER(1) SCALE=1 COVB=MODEL MAXITERATIONS=100 MAXSTEPHALVING=5 
    PCONVERGE=1E-006(ABSOLUTE) SINGULAR=1E-012 ANALYSISTYPE=3(WALD) CILEVEL=95 CITYPE=WALD 
    LIKELIHOOD=FULL
  /EMMEANS TABLES=condition SCALE=ORIGINAL
  /MISSING CLASSMISSING=EXCLUDE
  /PRINT CPS DESCRIPTIVES MODELINFO FIT SUMMARY SOLUTION.

* Generalized Linear Models.
GENLIN boxselection (REFERENCE=FIRST) BY condition (ORDER=ASCENDING) WITH 
    punishmentitem_changebehavior
  /MODEL condition punishmentitem_changebehavior condition*punishmentitem_changebehavior 
    INTERCEPT=YES
 DISTRIBUTION=BINOMIAL LINK=LOGIT
  /CRITERIA METHOD=FISHER(1) SCALE=1 COVB=MODEL MAXITERATIONS=100 MAXSTEPHALVING=5 
    PCONVERGE=1E-006(ABSOLUTE) SINGULAR=1E-012 ANALYSISTYPE=3(WALD) CILEVEL=95 CITYPE=WALD 
    LIKELIHOOD=FULL
  /EMMEANS TABLES=condition SCALE=ORIGINAL
  /MISSING CLASSMISSING=EXCLUDE
  /PRINT CPS DESCRIPTIVES MODELINFO FIT SUMMARY SOLUTION.

* Generalized Linear Models.
GENLIN boxselection (REFERENCE=FIRST) BY condition (ORDER=ASCENDING) WITH agecontinuous 
    punishmentitem_deserve
  /MODEL condition agecontinuous condition*agecontinuous punishmentitem_deserve 
    condition*agecontinuous condition*punishmentitem_deserve agecontinuous*punishmentitem_deserve 
    condition*agecontinuous*punishmentitem_deserve INTERCEPT=YES
 DISTRIBUTION=BINOMIAL LINK=LOGIT
  /CRITERIA METHOD=FISHER(1) SCALE=1 COVB=MODEL MAXITERATIONS=100 MAXSTEPHALVING=5 
    PCONVERGE=1E-006(ABSOLUTE) SINGULAR=1E-012 ANALYSISTYPE=3(WALD) CILEVEL=95 CITYPE=WALD 
    LIKELIHOOD=FULL
  /EMMEANS TABLES=condition SCALE=ORIGINAL
  /MISSING CLASSMISSING=EXCLUDE
  /PRINT CPS DESCRIPTIVES MODELINFO FIT SUMMARY SOLUTION.

* Generalized Linear Models.
GENLIN boxselection (REFERENCE=FIRST) BY condition (ORDER=ASCENDING) WITH agecontinuous 
    punishmentitem_rightthing
  /MODEL condition agecontinuous condition*agecontinuous condition*agecontinuous 
    punishmentitem_rightthing condition*agecontinuous condition*punishmentitem_rightthing 
    agecontinuous*punishmentitem_rightthing condition*agecontinuous*punishmentitem_rightthing 
    INTERCEPT=YES
 DISTRIBUTION=BINOMIAL LINK=LOGIT
  /CRITERIA METHOD=FISHER(1) SCALE=1 COVB=MODEL MAXITERATIONS=100 MAXSTEPHALVING=5 
    PCONVERGE=1E-006(ABSOLUTE) SINGULAR=1E-012 ANALYSISTYPE=3(WALD) CILEVEL=95 CITYPE=WALD 
    LIKELIHOOD=FULL
  /EMMEANS TABLES=condition SCALE=ORIGINAL
  /MISSING CLASSMISSING=EXCLUDE
  /PRINT CPS DESCRIPTIVES MODELINFO FIT SUMMARY SOLUTION.

* Generalized Linear Models.
GENLIN boxselection (REFERENCE=FIRST) BY condition (ORDER=ASCENDING) WITH agecontinuous 
    punishmentitem_lesson
  /MODEL condition agecontinuous condition*agecontinuous condition*agecontinuous 
    condition*agecontinuous punishmentitem_lesson condition*agecontinuous 
    condition*punishmentitem_lesson agecontinuous*punishmentitem_lesson 
    condition*agecontinuous*punishmentitem_lesson INTERCEPT=YES
 DISTRIBUTION=BINOMIAL LINK=LOGIT
  /CRITERIA METHOD=FISHER(1) SCALE=1 COVB=MODEL MAXITERATIONS=100 MAXSTEPHALVING=5 
    PCONVERGE=1E-006(ABSOLUTE) SINGULAR=1E-012 ANALYSISTYPE=3(WALD) CILEVEL=95 CITYPE=WALD 
    LIKELIHOOD=FULL
  /EMMEANS TABLES=condition SCALE=ORIGINAL
  /MISSING CLASSMISSING=EXCLUDE
  /PRINT CPS DESCRIPTIVES MODELINFO FIT SUMMARY SOLUTION.

* Generalized Linear Models.
GENLIN boxselection (REFERENCE=FIRST) BY condition (ORDER=ASCENDING) WITH agecontinuous 
    punishmentitem_changebehavior
  /MODEL condition agecontinuous condition*agecontinuous condition*agecontinuous 
    condition*agecontinuous condition*agecontinuous punishmentitem_changebehavior 
    condition*agecontinuous condition*punishmentitem_changebehavior 
    agecontinuous*punishmentitem_changebehavior condition*agecontinuous*punishmentitem_changebehavior 
    INTERCEPT=YES
 DISTRIBUTION=BINOMIAL LINK=LOGIT
  /CRITERIA METHOD=FISHER(1) SCALE=1 COVB=MODEL MAXITERATIONS=100 MAXSTEPHALVING=5 
    PCONVERGE=1E-006(ABSOLUTE) SINGULAR=1E-012 ANALYSISTYPE=3(WALD) CILEVEL=95 CITYPE=WALD 
    LIKELIHOOD=FULL
  /EMMEANS TABLES=condition SCALE=ORIGINAL
  /MISSING CLASSMISSING=EXCLUDE
  /PRINT CPS DESCRIPTIVES MODELINFO FIT SUMMARY SOLUTION.


**stimuli effects**


USE ALL.
COMPUTE filter_$=(include = 1).
VARIABLE LABELS filter_$ 'include = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

DATASET ACTIVATE DataSet1.
SORT CASES  BY gender_recoded.
SPLIT FILE SEPARATE BY gender_recoded.



CROSSTABS
  /TABLES=stimgender BY stimcharacter
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT
  /COUNT ROUND CELL.


SPLIT FILE OFF.

USE ALL.
COMPUTE filter_$=(include = 1 AND (condition = 1 OR condition = 2)).
VARIABLE LABELS filter_$ 'include = 1 AND (condition = 1 OR condition = 2) (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.


* Generalized Linear Models.
GENLIN boxselection (REFERENCE=FIRST) BY stimgender (ORDER=ASCENDING)
  /MODEL stimgender INTERCEPT=YES
 DISTRIBUTION=BINOMIAL LINK=LOGIT
  /CRITERIA METHOD=FISHER(1) SCALE=1 COVB=MODEL MAXITERATIONS=100 MAXSTEPHALVING=5 
    PCONVERGE=1E-006(ABSOLUTE) SINGULAR=1E-012 ANALYSISTYPE=3(WALD) CILEVEL=95 CITYPE=WALD 
    LIKELIHOOD=FULL
  /EMMEANS TABLES=stimgender SCALE=ORIGINAL
  /MISSING CLASSMISSING=EXCLUDE
  /PRINT CPS DESCRIPTIVES MODELINFO FIT SUMMARY SOLUTION.

* Generalized Linear Models.
GENLIN boxselection (REFERENCE=FIRST) BY stimgender gender_recoded (ORDER=ASCENDING)
  /MODEL stimgender gender_recoded stimgender*gender_recoded INTERCEPT=YES
 DISTRIBUTION=BINOMIAL LINK=LOGIT
  /CRITERIA METHOD=FISHER(1) SCALE=1 COVB=MODEL MAXITERATIONS=100 MAXSTEPHALVING=5 
    PCONVERGE=1E-006(ABSOLUTE) SINGULAR=1E-012 ANALYSISTYPE=3(WALD) CILEVEL=95 CITYPE=WALD 
    LIKELIHOOD=FULL
  /EMMEANS TABLES=stimgender SCALE=ORIGINAL
  /EMMEANS TABLES=stimgender SCALE=ORIGINAL
  /EMMEANS TABLES=gender_recoded SCALE=ORIGINAL
  /EMMEANS TABLES=stimgender*gender_recoded SCALE=ORIGINAL
  /MISSING CLASSMISSING=EXCLUDE
  /PRINT CPS DESCRIPTIVES MODELINFO FIT SUMMARY SOLUTION.

***exploratory age effects***


USE ALL.
COMPUTE filter_$=(include = 1).
VARIABLE LABELS filter_$ 'include = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

SORT CASES  BY condition.
SPLIT FILE SEPARATE BY condition.

CROSSTABS
  /TABLES=agecategorical BY boxselection
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT
  /COUNT ROUND CELL.

***excluding kids who didn't like playing on the iPad*

USE ALL.
COMPUTE filter_$=(include = 1 AND likeplayingonipad_forexclusions = 1).
VARIABLE LABELS filter_$ 'include = 1 AND likeplayingonipad_forexclusions = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.
