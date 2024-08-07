use "D:\Kausalanalyse\Uebung IV\IV.dta", clear

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
* Ex. 1) IV-Schätzer umsetzen
*******************************************

ivregress 2sls Y (D=IVrural), first
*2sls: Das gewählte Verfahren, hier: Two-Stage Least Squares
*(D=IVrural): Treamtment-Variable = IV-Variable
*Option "first": Erste Stufe wird auch ausgewiesen

*Erste Tabelle: Zur Überprüfung der 1. Annahme, man möchte einen möglichst starken Effekt zwischen Z und D, also ein hohes R²

*Zweite Tabelle: Y regressiert auf den Anteils der Varianz von D der mit IV erklärt werden kann (Erste Stufe [->in erster Tabelle] )
				// Standardfehler sind angepasst dafür, dass die Ergebnisse von D geschätzt wurden
*** by hand

* 1.Stufe
reg D IVruralsoc // mit der Instrumentvariable die Treatmentvariable erklären
				 // Hierbei wird nur ein Teil der Varianz von D erklärt. Aber
				 // dieser Teil der Varianz ist 

predict D_hat  // prognostizierten Werte für D

* 2.Stufe
reg Y D_hat
*Standardfehler sind verkehrt, weil D_hat geschätzt wurde und dies nicht berücksichtigt wurde
* Deswegen besser "ivregress" benutzen, weil dort Standardfehler korrigiert (richtig) ausgewiesen werden

*******************************************
* Ex. 2) 
*******************************************

ivregress 2sls Y (D=IVfatherargue), first // ein anderes Instrument um die Bildungsrendite zu schätzen



*******************************************
* Ex. 3) 
*******************************************
ivregress 2sls Y (D=IVtwoparents), first

ivregress 2sls Y feduc2-feduc5 (D=IVtwoparents feduc2-feduc5), first
	*Kontrollvariablen in Outcome- und Treatmentgleichung
	*Instrumente nur in Outcomegleichung
