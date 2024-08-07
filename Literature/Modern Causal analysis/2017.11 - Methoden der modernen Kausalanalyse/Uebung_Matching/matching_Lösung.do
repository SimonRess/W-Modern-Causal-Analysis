use "A:\Lehre\2015_WS_Workshop Kausalanalyse Kassel\Uebung_Matching\matching.dta", clear
log using "A:\Lehre\2015_WS_Workshop Kausalanalyse Kassel\Uebung_Matching\matching.log", replace


*********************************** Datenaufbereitung ***********************************************

*************************
*** Treatment-Variable
*************************
gen D=inlist(casmin,8,9)
replace D=. if casmin<=0
tab casmin D,m

lab var D "treatment: college"
label define Dlab 0 "no college" 1 "college" 
label value D Dlab



*************************
*** Outcome-Variable
*************************
gen Y=grossincome/(workinghours*4.35)
replace Y=. if grossincome<=0 | workinghours<=0
label var Y "gross hourly wage"
sum Y, detail


****************************
* Analysestichprobe
****************************
keep if lfs==11 				/* nur Erwerbstaetige */
keep if (age>=20 & age<=60) 		/* Alter 20 bis 60 */



*************************************************************
* globale Definition der Kontrollvariablen + Missings loeschen
*************************************************************
# d ;
global X "age female german feduc2 feduc3 feduc4 feduc5 dummytwoparents";
# d cr

**** Missings loeschen
egen varmiss=rowmiss(Y D $X)
drop if varmiss>0


*******************************
* randomize sort order of data
*******************************
set seed 12345 
gen random = uniform() 
sort random




****************************** Selection on observables: Propensity score matching **********************************

*******************************************
* Aufgabe 1) PS estimation
*******************************************
logit D $X


********************************************
* Aufgabe 2) NN(1) matching with replacement
********************************************
psmatch2 D $X, outcome(Y) logit common neighbor(1) ate 

*** by hand
bysort D: sum Y
sum Y _Y if D==1 & _support==1


********************************************
* Aufgabe 3) Balancing tests
********************************************
*** balancing test for covariate "age"
pstest age, both 
pstest age, both graph

*** by hand
bysort D: sum age, detail
di (45.17789-39.53637)/(0.5*(88.12353+129.9279))^0.5
sum age if D==0 & _support==1 [weight=_weight], detail
di (45.33842-45.17789)/(0.5*(86.08441+88.12353))^0.5

*** balancing test for all other covariates 
pstest $X, both rubin 

pstest $X, both rubin graph
pstest $X, both rubin scatter


********************************************
* Aufgabe 4) Analysis PS distribution
********************************************
*** balancing test propensity score
pstest _pscore, both

*** graph
psgraph, bin(50)

*** individual propensity score difference to matched control
sum _pdif, detail

*** examples
list D $X _pscore _pdif _n1 _nn if _id==1
list D $X _pscore _pdif _n1 _nn if _id==1876


*******************************************
* Aufgabe 5) other matching algorithms
*******************************************
psmatch2 D $X, outcome(Y) common neighbor(10)
pstest $X, both
psmatch2 D $X, outcome(Y) common kernel
pstest $X, both


*********************************************
* Aufgabe 6) bootstrapping of standard errors
*********************************************
psmatch2 D $X, outcome(Y) common  neighbor(1) quietly
bs "psmatch2 D $X, outcome(Y) common  neighbor(1)" "r(att)", reps(10)


*************************************************
* Aufgabe 7) conditional average treatment effect
*************************************************
gen indTT=Y-_Y
sum indTT if D==1 & female==0 & _support==1
sum indTT if D==1 & female==1 & _support==1
reg indTT female if D==1 & _support==1


*** stratified
psmatch2 D $X if female==0, outcome(Y) common  neighbor(1) quietly
psmatch2 D $X if female==1, outcome(Y) common  neighbor(1) quietly


*******************************************
* Aufgabe 8) quantile treatment effect
*******************************************
sum indTT if D==1, detail


log close







