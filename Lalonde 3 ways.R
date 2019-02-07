install.packages("Matching")
library(Matching)
data(lalonde)

#Creaing two subsets: people with high school degree and people without high school degree
nodegree <- subset(lalonde, nodegr==1) 
degree <- subset(lalonde, nodegr==0) 
mean(nodegree$re78)
#[1] 4929.842
mean(degree$re78)
#[1] 6631.499

nsw_dw <- read_dta("Downloads/nsw_dw.dta")
treat <- subset(nsw_dw, treat==1)
control <- subset(nsw_dw, treat == 0)
treat_effect <- mean(treat$re78) - mean(control$re78)
treat_effect
mean(treat$re78)
mean(control$re78)

linear <- lm(re78 ~ treat, data=nsw_dw)

#Linear model for people with a degree
lm_degree <- lm(re78 ~ treat+ educ+black+re74+re75+u75, data = degree)
summary(lm_degree) #treat 2.232e+03  1.553e+03   1.438   0.1541 <- not statistically significant
#Confidence interval for lm_degree
int_val_degree <- qnorm(0.975)*summary(lm_degree)$coefficients[2,2]
summary(lm_degree)$coefficients[2,2]
conf_int_degree <- c(summary(lm_degree)$coefficients[2,1]-int_val_degree,summary(lm_degree)$coefficients[2,1]+int_val_degree)
names(conf_int_degree) <- c("2.5%", "97.5%")
conf_int_degree #-810.7268-5275.1342

#Linear model for people without a degree
lm_nodegree <- lm(re78 ~ treat+age+ educ+black+u74+u75, data = nodegree)
summary(lm_nodegree)
#Confidence interval for lm_nodegree
int_val_nodegree <- qnorm(0.975)*summary(lm_nodegree)$coefficients[2,2]
conf_int_nodegree <- c(summary(lm_nodegree)$coefficients[2,1]-int_val_nodegree,summary(lm_nodegree)$coefficients[2,1]+int_val_nodegree)
names(conf_int_nodegree) <- c("2.5%", "97.5%")
conf_int_nodegree #-312.7511 - 2468.1903 



#2.Random forest
install.packages("randomForest")
library(randomForest)
set.seed(123)
#Subsetting to treat
treat_degree <- subset(treat, nodegr==0)
treat_nodegree <- subset(treat, nodegr==1)

#Creating a random forest trained on the whole dataset (not used in the memo)
forest <- randomForest(re78 ~.,data = lalonde, mtry = 5, importance = TRUE)
forest
summary(forest)
importance(forest)
#nodegr  -0.8072093 nodegree variable doesn't seem to be an important factor in predicting the income in 78
varImpPlot(forest)

#randomForest for the subgroup with degrees
forest_degree <- randomForest(re78 ~ treat+ educ+black+re74+re75+u75,data = degree, mtry = 5, importance = TRUE)
summary(forest_degree)
importance(forest_degree)
varImpPlot(forest_degree)

#randomForest for the subgroup without degrees
forest_nodegree <- randomForest(re78 ~treat+age+ educ+black+u74+u75,data = nodegree, mtry = 8, importance = TRUE)
forest_degree
forest_nodegree
importance(forest_nodegree)
varImpPlot(forest_nodegree)

#Creating counterfactuals
counter_degree <- treat_degree
counter_degree$treat <- rep(0, 54)
counter_nodegree <- treat_nodegree
counter_nodegree$treat <- rep(0, 131)

#Predicting the income using the forest_degree model
forest_degree_counter <- predict(forest_degree, newdata = counter_degree)
mean(treat_degree$re78) - mean(forest_degree_counter)
treat_degree$re78 - forest_degree_counter

#Predicting the income using the forest_nodegree model
forest_nodegree_counter <- predict(forest_nodegree, newdata = counter_nodegree)
mean(treat_nodegree$re78) - mean(forest_nodegree_counter)
treat_nodegree$re78 - forest_nodegree_counter


#Fisher test
#A modified function from the pre-class work
experiment <- function(input) {
    storage.vector <- NA
    k = 1
    for (i in 1:length(input)) {
      if(
        sample(x = c(1,0), size = 1, prob = c(sum(input)/length(input), 
                                              (length(input)-sum(input))/length(input))) == 1) {
        storage.vector[k] <- i
        k = k + 1
      }
    }
    return(list(treated.units = storage.vector, 
                control.units =  c(1:length(input))[-storage.vector]))
  }

#FET for all the values (not discussed in the memo)
differences <- NA
for(i in 1:1000){
  list <- experiment(lalonde$treat)
  treated_units<-unlist(list[1])
  control_units<-unlist(list[2])
  differences[i] <- mean(lalonde[treated_units,]$re78)-mean(lalonde[control_units,]$re78)
}

control <- subset(lalonde, treat==0)
observed_tr_effect <- mean(treat$re78)- mean(control$re78)
observed_tr_effect
#Calculating the p-value
p.value <- length(differences[differences>=observed_tr_effect])/length(differences)
p.value 

#Repeating the same procedure for the degree subgroup
differences_degree <- NA
for(i in 1:1000){
  d <- experiment(degree$treat)
  treated_units_degree<-unlist(d[1])
  control_units_degree<-unlist(d[2])
  differences_degree[i] <- mean(degree[treated_units_degree,]$re78)-mean(degree[control_units_degree,]$re78)
}

control_degree <- subset(degree, treat==0)
observed_tr_effect_degree <- mean(treat_degree$re78)- mean(control_degree$re78)
observed_tr_effect_degree

p.value_degree <- length(differences[differences_degree>=observed_tr_effect_degree])/length(differences_degree)
p.value_degree

#Repeating the same procedure for the nodegree subgroup
differences_nodegree <- NA
for(i in 1:1000){
  n <- experiment(nodegree$treat)
  treated_units_nodegree<-unlist(n[1])
  control_units_nodegree<-unlist(n[2])
  differences_nodegree[i] <- mean(nodegree[treated_units_nodegree,]$re78)-mean(nodegree[control_units_nodegree,]$re78)
}

control_nodegree <- subset(nodegree, treat==0)
observed_tr_effect_nodegree <- mean(treat_nodegree$re78)- mean(control_nodegree$re78)
observed_tr_effect_nodegree

p.value_nodegree <- length(differences[differences_nodegree>=observed_tr_effect_nodegree])/length(differences_nodegree)
p.value_nodegree



#Creating density plots which indicate where observed treatment effects are
density.plot <- plot(density(differences))
abline(v = observed_tr_effect, lwd = 2, col = "red")

density.plot <- plot(density(differences_degree))
abline(v = observed_tr_effect_degree, lwd = 2, col = "red")

density.plot <- plot(density(differences_nodegree))
abline(v = observed_tr_effect_nodegree, lwd = 2, col = "red")
