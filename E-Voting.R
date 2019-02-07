# Installing all the necessary libraries
library(Matching)
library(rbounds)
library(tableHTML)

# Loading the data
load("datamatch.Rdata")

# Outcome variables
outcomes <- datamatch[10:18]

#Variable names
names <- c("Qualification of poll workers","Evaluation of voting experience","Ease of voting procedure","Sure vote was counted","Confident ballot secret","Elections in Salta are clean","Speed of voting process","Agree substitute TV by EV","Select candidates electronically")

# Number of outcomes
number <- dim(outcomes)[2]

#_________________________________ Table 1 _________________________________#

# The first table visualizes the results of the survey

# Creating the matrix
tab1 <- matrix(NA, nrow = number, ncol = 6)
rownames(tab1) <- names
colnames(tab1) <- c("N", "All Voters (%)", "E-Voters (%)", "Traditional Voters (%)", "Diff.", "p-value")

for (i in 1:number) {
  # The number of responses
  tab1[i, 1] <- length(outcomes[, i])
  # Proportion of all voters
  tab1[i, 2] <- prop.table(table(outcomes[, i]))[2] * 100	
  # Proportion of electronic and traditional separately
  tab1[i, 3:4] <- rev(prop.table(table(outcomes[, i], datamatch$EV), 2)[2, ]) * 100
  # The difference
  tab1[i, 5] <- tab1[i, 3] - tab1[i, 4]	
  # Test of difference in proportions to find the p-value
  tab1[i, 6] <- prop.test(table(outcomes[, i], datamatch$EV)[2, ], n = apply(table(outcomes[, i], datamatch$EV), 2, sum))$p.value
}
tab1 <- tab1[rev(order(tab1[, "Diff."])), ]
outcomes[, i]
datamatch <- na.omit(datamatch)

#__________________________ Table 2, pre-matching __________________________#

# This part of the second table calculates the differences in the covariates before matching

EV <- datamatch[2]

covar <- datamatch[c("age.group", "educ", "white.collar", "not.full.time", "male", "tech", "pol.info")]
covar.names <- c("Age group (1-5)", "Education (1-8)", "White collar (%)", "Not full time worker (%)", "Male (%)", "Technology count (1-6)", "Political information (1-4)")

number.covar <- dim(covar)[2]

# Creating the table
tab2.pre <- matrix(NA, nrow = number.covar, ncol = 4)
rownames(tab2.pre) <- covar.names
colnames(tab2.pre) <- c("EV", "TV", "Diff.", "p-value")

# Calculating the values for electronic and traditional voters and the difference between them
tab2.pre[, 1:2] <- cbind(apply(covar[EV == 1,], 2, mean), apply(covar[EV == 0,], 2, mean))
tab2.pre[, 3] <- tab2.pre[, 1] - tab2.pre[, 2]

# Finding p-values of Kolmogorov-Smirnov tests for ordinal variables 
for (i in c(1, 2, 6, 7)){
  tab2.pre[i, 4] <- ks.boot(covar[, i][EV == 1], covar[, i][EV == 0], nboots = 500)$ks.boot.pvalue
}
# Finding p-values using a difference in proportions test for binary variables
for (i in c(3, 4, 5)){
  tab2.pre[i, 4] <- prop.test(table(covar[, i], EV$EV), n = apply(table(covar[,i],EV$EV),2, sum))$p.value
}

#__________________________ Table 3, pre-matching __________________________#

# This table presents the naive initial findings based on the unmatched dataset

outcomes.pre <- datamatch[10:18]

# Creating the table
tab3.pre <- matrix(NA,nrow = number,ncol = 4)
rownames(tab3.pre) <- names
colnames(tab3.pre) <- c("E-Voting (%)", "Traditional Voting (%)", "Diff.", "p-value")

for (i in 1:number) {
  # The proportions of electronic and traditional voters
  tab3.pre[i, 1:2] <- rev(prop.table(table(outcomes.pre[,i],datamatch$EV),2)[2,])*100
  # The difference in the outcomes
  tab3.pre[i, 3] <- tab3.pre[i, 1] - tab3.pre[i, 2]	
  # P-value using the difference in proportions test 
  tab3.pre[i, 4] <- prop.test(table(outcomes.pre[, i], datamatch$EV)[2, ], n = apply(table(outcomes.pre[, i], datamatch$EV), 2, sum))$p.value
}

#__________________________ Matching (with Match) ________________________#

# Propensity Score Matching (PMS)

# Estimating a propensity score
prop_score  <- glm(EV ~ age.group + educ + tech + + pol.info + white.collar + not.full.time + male, data=datamatch)
X  <- prop_score$fitted
Tr  <- datamatch$EV

rr  <- Match(Y = datamatch$how.clean, Tr=Tr, X=X, estimand = "ATT", caliper = 0.05);
summary(rr)
# Checking the balance
mb  <- MatchBalance(EV ~ age.group + educ + tech + + pol.info + white.collar + not.full.time + male, data = datamatch,match.out=rr, nboots=500)

# Multivariate matching
cov_names <- c("age.group", "educ", "tech", "pol.info", "white.collar", "not.full.time", "male" )
all_covs <- datamatch[, cov_names]

rr1  <- Match(Tr=Tr, X=all_covs, estimand = "ATT", caliper = 0.05);
summary(rr1)
mb1  <- MatchBalance(EV ~ age.group + educ + tech + + pol.info + white.collar + not.full.time + male, data=datamatch, match.out=rr1, nboots=500)

# Genetic Matching
# Finding the weights
genout <- GenMatch(Tr=Tr, X=all_covs, estimand="ATT", caliper=0.05,
                   pop.size=200, max.generations=100, wait.generations=10)
mout <- Match(Y = datamatch$conf.secret, Tr=Tr, X=all_covs, estimand="ATT", caliper=0.05, Weight.matrix=genout)
mb_gen <- MatchBalance(EV ~ age.group + educ + tech + + pol.info + white.collar + not.full.time + male, data=datamatch,
                       match.out=mout, nboots=500)

#__________________________ Sensitivity Analysis ________________________#
psens(mout, Gamma=3, GammaInc=.1)

# Calculating the sensitivity for the first outcome
sensitivity <- data.frame(psens(mout, Gamma=3, GammaInc=.1))
sensitivity <- sensitivity[,5:7]

# Calculating the bounds for all the outcome variables with the help of a loop
for (i in 11:18){
  mout <- Match(Y = datamatch[,i], Tr=Tr, X=all_covs, estimand="ATT", caliper=0.05, Weight.matrix=genout)
  temporal <- data.frame(psens(mout, Gamma=3, GammaInc=.1))
  temporal <- temporal[,6:7]
  sensitivity = data.frame(sensitivity,temporal)
}
colnames(sensitivity) = c("Gamma","U", "L","U", "L","U", "L", "U", "L","U", "L", "U", "L","U", "L","U", "L","U", "L")

# Creating a fancy table
n <- c("","Qualification of poll workers","Evaluation of voting experience","Ease of voting procedure","Sure vote was counted","Confident ballot secret","Elections in Salta are clean","Speed of voting process","Agree substitute TV by EV","Select candidates electronically")
tableHTML(sensitivity, 
          rownames = FALSE,
          second_header = list(c(1,2, 2, 2, 2, 2, 2, 2, 2, 2), n),
          widths = c(rep(1, 19)),
          theme = "scientific")


# Creating a matched dataset
matched.treat <- datamatch[genout$matches[,1], ]
matched.control <- datamatch[genout$matches[,2], ]
datamatched <- rbind(matched.treat, matched.control)

  
  #__________________________ Table 2, post-matching _________________________#

# Adding matched dataset to the original table to enable comparison
EV.post <- datamatched[2]

covar.post <- datamatched[, c("age.group", "educ", "white.collar", "not.full.time", "male", "tech", "pol.info")]
tab2.post <- matrix(NA, nrow = number.covar, ncol = 4)
rownames(tab2.post) <- covar.names
colnames(tab2.post) <- c("EV", "TV", "Diff.", "p-value")

tab2.post[, 1:2] <- cbind(apply(covar.post[EV.post == 1, ], 2, mean), apply(covar.post[EV.post == 0,], 2, mean))
tab2.post[, 3] <- tab2.post[, 1] - tab2.post[, 2]
for (i in c(1, 2, 6 , 7)){
  tab2.post[i, 4]<-ks.boot(covar.post[,i][EV.post==1],covar.post[,i][EV.post==0], nboots = 500)$ks.boot.pvalue
}
for (i in c(3, 4, 5)){
  tab2.post[i, 4] <- prop.test(table(covar.post[, i], EV.post$EV), n = apply(table(covar.post[, i], EV.post$EV),2 , sum))$p.value
}
 # Creating the final table
tab2 <- cbind(tab2.pre, tab2.post)
tab2[3:5, c(1:3, 5:7)] <- tab2[3:5, c(1:3, 5:7)] * 100


#__________________________ Table 3, post-matching _________________________#

# The outcomes for the matched dataset
outcomes.post <- datamatched[10:18]
tab3.post <- matrix(NA, nrow = number, ncol = 4)
rownames(tab3.post) <- names
colnames(tab3.post) <- c("E-Voting (%)", "Traditional Voting (%)", "Diff.", "p-value")

for (i in 1:number) {
  tab3.post[i, 1:2] <- rev(prop.table(table(outcomes.post[, i], datamatched$EV), 2)[2, ]) * 100
  tab3.post[i, 3] <- tab3.post[i, 1] - tab3.post[i, 2]	
  tab3.post[i, 4] <- prop.test(table(outcomes.post[, i], datamatched$EV)[2, ], n = apply(table(outcomes.post[, i], datamatched$EV), 2, sum))$p.value
}

tab3 <- cbind(tab3.pre, tab3.post)

tab3 <- tab3[rev(order(tab3[, 7])), ]
