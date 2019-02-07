clear
use "/Users/ritakurban/Downloads/Insurance/data_for_analysis.dta"
keep curative gastosalud2 consulta hospital intervencion hhmujer hospinter consulta_oop formal
consulta_ins consulta_pins medicinas_oop medicinas_ins medicinas_pins Z1 eligibleZ1 telefono
internet cable educ edad mujer lima mieperho sintoma_dias enfermedad_dias recaida_dias
accidente_dias incidente_dias cronica planificacion
* Drop formally employed people since they don't qualify for health insurance
drop if formal==1
* Translate variable names to English
rename Z1 IFH
rename eligibleZ1 eligible
rename hhmujer fem_house
rename edad age
rename mujer female
rename mieperho n_household
rename cronica chronic
rename sintoma_dias symptoms
rename enfermedad_dias illness
rename recaida_dias relapse
rename accidente_dias accident
rename intervencion surgery
rename gastosalud2 expenditures
* Create a Table with Summary Statistics
cd /Users/ritakurban/Downloads
tabstat curative expenditures IFH eligible educ age female n_household chronic symptoms
illness relapse accident surgery fem_house, stat(n mean sd min max) save
return list
matlist r(StatTotal)
matrix results = r(StatTotal)'
putexcel set putexcel2.xlsx, sheet(statistics) modify
putexcel A1 = matrix(results), names nformat(number_d2)
* Histograms to Check Observable Variable Balance
histogram educ, percent by(eligible)
histogram age, percent by(eligible)
* McCrary Test
DCdensity IFH, b(0.5) breakpoint(0) generate(Xj Yj r0 fhat se_fhat)
18
* Change Variable Labels for the Cmogram Plots
label var curative "Curative Care"
label var expenditures "Total Health Expenditures"
* Create the Graphs
cmogram curative IFH, cut(0) scatter line(0)  lfitci
cmogram expenditures IFH, cut(0) scatter line(0)  lfitci
* Calculate the Bandwidths
rdbwselect curative IFH
rdbwselect expenditures IFH
* Parametric Approach for Curative
regress curative eligible IFH if IFH<12| IFH>‐12
regress curative i.eligible##c.IFH if IFH<12 | IFH>‐12
* Include Controls
regress curative eligible female age educ fem_house surgery symptoms illness relapse accident
chronic
* Parametric Approach for Expenditures
regress expenditures eligible IFH if IFH<10| IFH>‐10
regress expenditures i.eligible##c.IFH if IFH<10 | IFH>‐10
* Include Controls
regress expenditures eligible female age educ fem_house surgery symptoms illness relapse
accident  chronic
* Non‐Parametric Approach
rdrobust curative IFH, h(12)
rdrobust expenditures IFH, h(10)
