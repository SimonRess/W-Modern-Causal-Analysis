use "A:\Lehre\2016_SSFerien_Kausalanalyse_GESIS\Uebung_Matching\matching_DID.dta", clear
log using  "A:\Lehre\2016_SSFerien_Kausalanalyse_GESIS\Uebung_Matching\matching_DID.log", replace


*******************************************
* global defining control variables + deleting missings
*******************************************
# d ;
global X "age female east nongerman casmin1c casmin2a casmin2bc casmin3ab expft expue01 firm20_199 firm200_1999 firm1999over publicservice";
# d cr

**** delete missings
egen varmiss=rowmiss(D $X)
drop if varmiss>0


****************************** Selection on observables: Propensity score matching **********************************

*******************************************
* Ex 1.1) NN(1) matching with replacement
*******************************************
psmatch2 D $X, outcome(Y03) logit common neighbor(1) ate 
pstest $X, both
psgraph, bin(50)


*******************************************
* Ex 1.2) other matching algorithms
*******************************************
psmatch2 D $X, outcome(Y03) logit common neighbor(5) quietly 
pstest $X, both

psmatch2 D $X, outcome(Y03) logit common neighbor(10) quietly 
pstest $X, both

psmatch2 D $X, outcome(Y03) logit common kernel(normal) quietly 
pstest $X, both

*******************************************
* Ex 1.3) other outcomes
*******************************************
psmatch2 D $X, outcome(Y03) logit common neighbor(10) quietly
psmatch2 D $X, outcome(Y04) logit common neighbor(10) quietly
psmatch2 D $X, outcome(Y05) logit common neighbor(10) quietly



****************************** Causal analysis using longitudinal data: Difference-in-difference propensity score matching **********************************

**************************************************************
* Ex 2.1) Diff-in-Diff
**************************************************************
gen Ydiff0301=Y03-Y01

sum Ydiff0301 Y03 Y01 if D==0 & Y03~=. & Y01~=.
sum Ydiff0301 Y03 Y01 if D==1 & Y03~=. & Y01~=.

**************************************************************
* Ex 2.2) NN(10) matching with replacement combined with Diff-in-Diff
**************************************************************
psmatch2 D $X, outcome(Ydiff0301) common neighbor(10) quietly

*** alternative
psmatch2 D $X, outcome(Y03 Y01) common neighbor(10)


log close



































