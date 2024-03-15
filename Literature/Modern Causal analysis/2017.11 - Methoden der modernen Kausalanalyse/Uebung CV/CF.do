use "D:\Kausalanalyse\Uebung CV\CF.dta", clear

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
global X "age female german feduc2 feduc3 feduc4 feduc5"
global IV "IVruralsoc"


**** listwise delition of missings
egen varmiss=rowmiss(Y D $X $IV)
drop if varmiss>0




******************************************************************************************
* NO observed heterogeneity in causal effect, NO unobserved heterogeneity in causal effect
******************************************************************************************

*** Heckman treatreg (neu: etregress)
etregress Y $X, treat(D = $X $IV) twostep vce(bootstrap, rep(200)) // wenn es in zwei Schritten schätzt (twostep) muss man bootstrapen (bootstrap), ansonsten nicht
* ATE ist 18,54 Euro (wenn alle Annahmen stimmen)

* rho: Ist Korrelationskoeffiziet, gib Korrelation zwischen den unbeobachtbaren Variablen an
	* Einfluss der Variablen die ins Bildungssystem selektieren und das Einkommen beeinflussen
	* Wenn negativ, unterschätzt der OLS-Schätzer den wahren Effekt (weil was positiv auf Bildung wirkt, wirkt negativ auf Einkommen)
	* Wenn positiv, überschätzt der OLZ-Schätzer den wahren Effekt (weil was positiv auf Bildung wirkt, wirkt positiv auf Einkommen)
	
* lambda: wenn insignifikant spielt Selektion on unobservables keine Rolle und man kann zu OLS Schätzer zurückkehren

* sigma: Sigma aus der Formel

gen ATE1=_b[D]
gen ATT1=ATE1
gen ATNT1=ATE1


*** by hand
probit D $X $IV
predict hat_D, xb
gen lam0=(1-D)*(-normalden(-hat_D)/normal(-hat_D)) // 1. Korrekturterm
gen lam1=D*(normalden(-hat_D)/(1-normal(-hat_D)))  // 2. Korrekturterm
gen lambda=lam0+lam1
reg Y D $X lambda
gen ATE1b=_b[D]
gen ATT1b=ATE1b
gen ATNT1b=ATE1b

sum AT*
drop ATE1b ATT1b ATNT1b


********************************************************************************************
* WITH observed heterogeneity in causal effect, NO unobserved heterogeneity in causal effect
********************************************************************************************

local X "age female german feduc2 feduc3 feduc4 feduc5"

foreach var of varlist `X' {
			gen D`var'=D*`var'
			local DX `DX' D`var'
		}


*** Heckman treatreg 
etregress Y $X `DX', treat(D = $X $IV) twostep vce(bootstrap, rep(10)) coefleg

#d ;
predictnl ATE2=_b[D]+_b[Dage]*age+_b[Dfemale]*female+_b[Dgerman]*german+_b[Dfeduc2]*Dfeduc2+_b[Dfeduc3]*Dfeduc3
+_b[Dfeduc4]*Dfeduc4+_b[Dfeduc5]*Dfeduc5;
predictnl ATT2=_b[D]+_b[Dage]*age+_b[Dfemale]*female+_b[Dgerman]*german+_b[Dfeduc2]*Dfeduc2+_b[Dfeduc3]*Dfeduc3
+_b[Dfeduc4]*Dfeduc4+_b[Dfeduc5]*Dfeduc5 if D==1;
predictnl ATNT2=_b[D]+_b[Dage]*age+_b[Dfemale]*female+_b[Dgerman]*german+_b[Dfeduc2]*Dfeduc2+_b[Dfeduc3]*Dfeduc3
+_b[Dfeduc4]*Dfeduc4+_b[Dfeduc5]*Dfeduc5 if D==0;
#d cr

sum AT*


********************************************************************************************
* NO observed heterogeneity in causal effect, WITH unobserved heterogeneity in causal effect
********************************************************************************************

************** by hand *************************
drop hat_D lam0 lam1
quietly: probit D $X $IV // Selektionsmodell schätzen
predict hat_D, xb
gen lam0=(1-D)*(-normalden(-hat_D)/normal(-hat_D)) // 1.Korrekturterm schätzen (Folien S.21)
gen lam1=D*(normalden(-hat_D)/(1-normal(-hat_D)))  // 2.Korrekturterm schätzen (Folien S.21)
reg Y D $X lam0 lam1 // Outcomegleichung: Regression Outcome auf Treatment und Kontrollvariablen regressieren, erweitert um die beiden Korrekturterme
*lam0: Baseline Unterschiede
*lam1 - lam0: Kausale Effektheterogenität (Selbstselektion)
test lam1=lam0 // Test ob Kausale Effektheterogenität signifikant ist. Wenn nicht signifikant dann Modell verwerfen und Modell verwenden dass nur baseline aufnimmt
gen b0v=_b[lam0]
gen b1v=_b[lam1]
gen bDX=_b[D]
egen ATE3=mean(bDX)
egen m_bDX0=mean(bDX) if D==0
egen m_bDX1=mean(bDX) if D==1
gen ATT3=m_bDX1+(b1v-b0v)*lam1 if D==1
gen ATNT3=m_bDX0+(b1v-b0v)*lam0 if D==0

sum AT*

********************************************************************************************
* WITH observed heterogeneity in causal effect, WITH unobserved heterogeneity in causal effect
********************************************************************************************

************** by hand *************************

drop hat_D lam0 lam1 b0v b1v bDX m_bDX0 m_bDX1
quietly: probit D $X $IV 
predict hat_D, xb
gen lam0=(1-D)*(-normalden(-hat_D)/normal(-hat_D))
gen lam1=D*(normalden(-hat_D)/(1-normal(-hat_D)))
reg Y D $X `DX' lam0 lam1
test lam1=lam0
gen b0v=_b[lam0]
gen b1v=_b[lam1]
#d ;
gen bDX=_b[D]+_b[Dage]*age+_b[Dfemale]*female+_b[Dgerman]*german+_b[Dfeduc2]*Dfeduc2+_b[Dfeduc3]*Dfeduc3
+_b[Dfeduc4]*Dfeduc4+_b[Dfeduc5]*Dfeduc5;
#d cr
sum bDX
egen ATE4=mean(bDX)
egen m_bDX0=mean(bDX) if D==0
egen m_bDX1=mean(bDX) if D==1
gen ATT4=m_bDX1+(b1v-b0v)*lam1 if D==1
gen ATNT4=m_bDX0+(b1v-b0v)*lam0 if D==0

sum AT*













