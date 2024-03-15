use "D:\Kausalanalyse\Uebung_Matching\matching.dta", clear
*log using "D:\Kausalanalyse\Uebung_Matching\matching.log", replace


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
*Wichtig weil bei gleicher Differenz von Scores zwischen einer Person der Treatmentgruppe
*zu mehreren Personen der Kontrollgruppe immer die erste Person im Datensatz gewählt wird
*Durch zufällige Sortierung des Datensatzes wird damit in oben beschriebener Situation
*eine zufällige Person gewählt
set seed 12345 
gen random = uniform() 
sort random




****************************** Selection on observables: Propensity score matching **********************************

*******************************************
* Aufgabe 1) PS estimation
*******************************************
logit D $X // 1.Stufe Propensity score matching / Coef. sind Logit-Koeffizient


********************************************
* Aufgabe 2) NN(1) matching with replacement
********************************************
ssc install psmatch2

psmatch2 D $X, outcome(Y) logit common neighbor(1) ate // psmatch2 "Treatmentvar." "Kontrollvar.",outcome("interessierende Outcomevar.")

*logit: Propensity Score Berechnung mit Logitmodell
*common: Common Support Bedingung (wird zuerst umgesetzt und danach wird der nearest Neightbor gesucht
*neighbor(): Anzahl der Nachbarn
*ate: Zeigt ATNT(hier ATU genannt) und ATE an

*ATT Difference: Jemand mit hoher Bildung hat ein um 7.9196 Euro höheres Einkommen, als wenn er keine hohe Bildung erhalten hätte
*ATU Difference (ATNT): Jemand mit niedriger Bildung hätte ein um 6.29Euro höheres Einkommen, wenn er eine höhere Bildung erhalten hätte
						*Treated meint hier "treatment mit nicht Treatment" und control meint hier "treated"
*ATE Difference: Der Durchschnittliche Effekt von Bildung unabhängig von der tatsächlichen Bildung

*Zweite Tabelle: Unmachted sind die Ergebnisse des Naiven Schätzers (Mittelwertvergleich)

*** by hand
bysort D: sum Y // nach Treatmentvariable Einkommen ausgeben -> Ergebniss entspricht Naiven Schätzer

sum Y _Y if D==1 & _support==1 // ATT ausrechnen

*Vergleiche der Optionen
psmatch2 D $X, outcome(Y) logit common neighbor(1) ate // psmatch2 "Treatmentvar." "Kontrollvar.",outcome("interessierende Outcomevar.")
psmatch2 D $X, outcome(Y) logit common neighbor(1) // psmatch2 "Treatmentvar." "Kontrollvar.",outcome("interessierende Outcomevar.")
*-> Kein Unterschied


psmatch2 D $X, outcome(Y) logit common neighbor(1) // psmatch2 "Treatmentvar." "Kontrollvar.",outcome("interessierende Outcomevar.")
psmatch2 D $X, outcome(Y) logit common neighbor(10) // psmatch2 "Treatmentvar." "Kontrollvar.",outcome("interessierende Outcomevar.")
*-> Unterschied

********************************************
* Aufgabe 3) Balancing tests
********************************************

pstest age, both // Für nur eine Kontrollvariable


*** by hand
bysort D: sum age, detail
di (45.17789-39.53637)/(0.5*(88.12353+129.9279))^0.5 //Bias im nicht gematchten Zustand

sum age if D == 0 & - support== 1 [weight
di (45.17789-45.33842)/(0.5*(88.12353+86.08441))^0.5


pstest $X ,both  // Für alle Kontrollvariablen
*V(T)/V(C) wird nur bei kontinuiertlichen Variablen ausgegeben
*Varianz macht bei nominalen Variablen keinen Sinn

pstest _pscore, both // Überprüfung wie sich Propensity Scores im Mittel vor und nach Matching auf Treatment- und Kontrollgruppe verteilen
*Also wie sieht die Wahrscheinlichkeit im Mittel aus, ein Treatment zu erhalten


pstest $X ,both rubin // "rubin" gibt auch Varianzen für kategoriale Variablen an

pstest $X ,both rubin graph // Stellt "%bias" graphisch dar

pstest $X ,both rubin scatter // Stellt "%bias" & "Variance ratio of residuals" graphisch dar



********************************************
* Aufgabe 4) Analysis PS distribution
********************************************

*Balancing test for the propensity score
pstest _pscore, both //Verteilung der Propensity scores

*graph
psgraph, bin(50) //bin(): passt Anzahl der Kästchen an

*individual propensity score differences in the matched sample
sum _pdif, detail // bei 50% der Personen wird immer eine Person gematched die die gleichen Propensity scores haben
* der größte Unterschied bei Matching sind maximal ("Largest") 1,28862% des Propensity scores


*Ausprägungen der Kontrollvariablen und propensity score von Personen vergleichen
list D $X _pscore _pdif _n1 _nn if _id==1  // gematched Person ist 1876 (steht in _n1)
list D $X _pscore _pdif _n1 _nn if _id==1876 // deswegen hier if _id==1876 (_id ist die matching-ID)

*******************************************
* Aufgabe 5) other matching algorithms
*******************************************

psmatch2 D $X, outcome(Y) logit common neighbor(1)
	pstest $X, both
	
psmatch2 D $X, outcome(Y) logit common neighbor(10)
	pstest $X, both
	
psmatch2 D $X, outcome(Y) logit common kernel
	pstest $X, both
	
*ATT , S.E. & % MeanBias (Matched vergleichen) 
*Nur Verfahren vergleichen welches MeanBias unter 5 haben
*Das Verfahren suchen, welchen den niedrigsten S.E. und MeanBias hat


*********************************************
* Aufgabe 6) bootstrapping of standard errors
*********************************************

psmatch2 D $X, outcome(Y) logit common neighbor(1)
bs "psmatch2 D $X, outcome(Y) logit common neighbor(1)" "r(att)",reps(10) //bootstrap für die S.E.
*"reps(Anzahl)" Anzahl der Durchläufe. Anzahl sollte über 100 oder 200 liegen, damit man in das Gesetz der großen Zahl kommt

*Bootstrapping erst im letzten Schritt (weil dies so lange dauert) um die Ergebnisse abzusichern


*************************************************
* Aufgabe 7) conditional average treatment effect
*************************************************

*1.Alternative
*** stratified
*Matching einmal für die Männer und dann für die Frauen
psmatch2 D $X if female==0, outcome(Y) common  neighbor(1) quietly
psmatch2 D $X if female==1, outcome(Y) common  neighbor(1) quietly

*2.Alternative 
psmatch2 D $X , outcome(Y) common  neighbor(1) quietly
gen indTT=Y-_Y //ind. Kausaler Effekt von Matching berechnet
	
	*Deskriptiv
	sum indTT if D==1 & female==0 & _support==1
	sum indTT if D==1 & female==1 & _support==1
	
	*Per Regression
	reg indTT female if D==1 & _support==1 

*******************************************
* Aufgabe 8) quantile treatment effect
*******************************************



log close
