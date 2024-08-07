use "A:\Lehre\2017 ECPR Winter School\Exercise_IV\IV.dta", clear

*********************************** Data preparation ***********************************************

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
* Sample definition
****************************
keep if lfs==11 				/* only employed people */
keep if (age>=20 & age<=60) 		/* age 20 to 60 */



*************************************************************************
* global definition of control variables + listwise deletion of missings
*************************************************************************
# d ;
global X "age female german feduc2 feduc3 feduc4 feduc5";
# d cr

**** listwise delition of missings
egen varmiss=rowmiss(Y D $X)
drop if varmiss>0



*******************************************
* Ex. 1) 
*******************************************

*** IV: rural socialisation
ivregress 2sls Y (D=IVruralsoc), first

* by hand: 
reg D IVruralsoc
predict D_hat
reg Y D_hat /* Attention: wrong standard errors */



*******************************************
* Ex. 2) 
*******************************************

*** IV: argument with father
ivregress 2sls Y (D=IVfatherargue), first



*******************************************
* Ex. 3) 
*******************************************

*** IV: raised up with two parents
ivregress 2sls Y (D=IVtwoparents), first

 
*** IV: raised up with two parents + additional control variables to block the backdoor path
ivregress 2sls Y feduc2-feduc5 (D=IVtwoparents feduc2-feduc5), first 







