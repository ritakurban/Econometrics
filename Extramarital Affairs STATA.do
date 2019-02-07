clear 
import delimited /Users/ritakurban/Downloads/TableF17-2.csv
gen A = yrb
replace A = 1 if yrb>0

global ylist A
global xlist v1 v2 v3 v4 v5 v6 v7 v8

describe $ylist $xlist 
summarize $ylist $xlist
corr

* Probit model
probit $ylist $xlist

* Logit model
logit $ylist $xlist


* Marginal effects (at the mean and average marginal effect)

quietly logit $ylist $xlist  
margins, dydx(*) 

quietly probit $ylist $xlist 
margins, dydx(*) 


* Ordered probit model coefficients
oprobit v1 v2 v3 v4 v5 v6 v7 v8
estat ic
oprobit v1 v4 v5 v6 v8
estat ic
margins, dydx(*) atmeans

*2
clear
import delimited /Users/ritakurban/Downloads/q2.csv, delimiter(";")

poisson hospvis id female year age hsat handdum handper hhninc hhkids educ married haupts reals fachhs abitur univ working bluec whitec self beamt docvis public addon
vif, uncentered

poisson hospvis age hsat handper educ hhninc working docvis addon, vce(robust)
vif, uncentered
margins, dydx(*) atmeans
estat gof

regress hospvis age hsat handper educ hhninc working docvis addon, vce(robust)

