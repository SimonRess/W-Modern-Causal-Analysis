use "D:\Kausalanalyse\Uebung_Matching\matching_DID.dta", clear
*log using  "D:\Kausalanalyse\Uebung_Matching\matching_DID.log", replace


*******************************************
* global defining control variables + deleting missings
*******************************************
# d ;
global X "age female east nongerman casmin1c casmin2a casmin2bc casmin3ab expft expue firm20_199 firm200_1999 firm1999over publicservice";
# d cr

**** delete missings
egen varmiss=rowmiss(D $X)
drop if varmiss>0

set seed 12345
gen random = uniform()
sort random

****************************** Selection on observables: Propensity score matching **********************************

*******************************************
* Ex 1.1) NN(1) matching with replacement
*******************************************

psmatch2 D $X , outcome(Y03) logit common neighbor(1)
*Zweite Tabelle unmatched: Personen die Arbeitslosigkeit in der Vergangenheit erfahren haben haben einen um -4,77 Euro niedrigeren Lohn (Nativer Schätzer)
				*ATT: Der Kausale Effekt der Arbeitslosigkeit ->  Effekt der Arbeitslosigkeit auf das Einkommen für die Personen die arbeitslos waren Controls: Das hätten sie verdient wenn sie nicht arbeitslos gewesen wären
				
				*Unterschied unmatched und ATT beruht auf confounding Bias bzw. nicht geschlossenen Back-door Paths
				* -> Unterschied ist also auf Selektionsprozesse zurückzuführen

pstest $X , both


*******************************************
* Ex 1.2) other matching algorithms
*******************************************
psmatch2 D $X , outcome(Y03) logit common neighbor(5) quietly
pstest $X , both // S.E. .604363073 // Meanbias 4.8  

psmatch2 D $X , outcome(Y03) logit common neighbor(10) quietly
pstest $X , both // S.E. .510383752 // Meanbias 3.1   -> Bester Algorithmus

psmatch2 D $X , outcome(Y03) logit common kernel quietly
pstest $X , both // Hier MeanBias 17.0 und damit größer 5 -> Damit verwerfen

*******************************************
* Ex 1.3) other outcomes
*******************************************

psmatch2 D $X , outcome(Y03) logit common neighbor(10) quietly // hatte beste Performance, deswegen verwenden
*Einfluss auf Variablen zu verschiedenen Zeitpunkten schätzen
psmatch2 D $X , outcome(Y04) logit common neighbor(10) quietly
psmatch2 D $X , outcome(Y05) logit common neighbor(10) quietly

psmatch2 D $X , outcome(Y03 Y04 Y05) logit common neighbor(10) quietly //Bei gemeinsamer Schätzung beschränkt sich Stata auf die Fälle für die alle Variablen beobachtet werden
*-> Verringert die Fallzahl


****************************** Causal analysis using longitudinal data: Difference-in-difference propensity score matching **********************************

**************************************************************
* Ex 2.1) Diff-in-Diff
**************************************************************

**************************************************************************
****** Daten müssen im wide-Format vorliegen *****************************
*********** Also alle Informationen müssen in einer Zeile liegen *********
**************************************************************************

gen Ydiff0301=Y03-Y01
sum Ydiff0301 Y03 Y01 if D==0 & Y03~=. & Y01~=. // durchgehend Beschäftige: Einkommen steigt um 0,72 Euro
sum Ydiff0301 Y03 Y01 if D==1 & Y03~=. & Y01~=. // Arbeitslose: Einkommen steigt um 0,02 Euro

*DID-Schätzer
di .0209792 -.7202919 // Arbeitslosigkeitserfahrung verringert das Einkommen um -.6993127 Euro

**************************************************************
* Ex 2.2) NN(10) matching with replacement combined with Diff-in-Diff
**************************************************************

psmatch2 D $X , outcome(Ydiff0301) logit common neighbor(10) quietly
* unmatched: Ist der Difference-in-Differences Schätzer
* ATT: Ist der Difference-in-Differences propensity score matching
