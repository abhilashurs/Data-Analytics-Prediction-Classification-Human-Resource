rm(list=ls()) # remove all existing objects in the environment
gc() # group consolidation to use memory more efficiently
## above two lines are very standard (personally standard)

## read in data ##
setwd("C:/Academics/ISDS 574/Project")

dat6 = read.csv('FinalDataset_Project.csv', head=T, stringsAsFactors=F, na.strings='') # 'read.csv' reads in csv file
# head tells if the first line is variable name
# na.strings tells what will be considered NA or missing
View(dat6) # 'View' gives a view of the data
dim(dat6) # 'dim' gives the dimensions of the data: # of rows, # of columns

#######Data Partition#####################
set.seed(1)     
index1 = sample(1:nrow(dat6), nrow(dat6)*.6)

train = dat6[index1,]
View(train)
dim(train)

test = dat6[-index1,]
View(test)
dim(test)



#Training a multiple linear regression model with forward selection
min.model = lm(MonthlyIncome ~ 1, data = train)
max.model = lm(MonthlyIncome ~ ., data = train)

summary(max.model)$adj.r.squared

forward.obj = step(min.model, scope = list(lower = min.model, upper = max.model), direction = 'forward')
summary(forward.obj)

obj_call = forward.obj$call
forward.model = lm(obj_call$formula, data = train)
yhat = predict(forward.model, newdata = test)

#install.packages('hydroGOF')
require('hydroGOF')

forward.rmse = rmse(dat6[-index1, 'MonthlyIncome'], yhat) 
forward.rmse

####Histogram of  residuals ########################
#install.packages('ggplot2')
library(ggplot2)
res1 <- residuals(forward.obj)
res.forward <- as.data.frame(res1)
head(res.forward)

ggplot(res.forward,aes(res.forward)) + geom_histogram(bin = 20,alpha=0.5,color = 'green', fill = 'orange') + theme_minimal()

####################################################################
plot(forward.obj) #### Residuals vs  Fittted####
forward.obj$fitted

##############

backward.obj = step(max.model, scope = list(lower = min.model, upper = max.model), direction = 'backward')
summary(backward.obj)

obj_call = backward.obj$call
backward.model = lm(obj_call$formula, data = train)
yhat1 = predict(backward.model, newdata = test)

#install.packages('hydroGOF')
require('hydroGOF')

backward.rmse = rmse(dat6[-index1, 'MonthlyIncome'], yhat1) 
backward.rmse

####Histogram of  residuals ########################

res2 <- residuals(backward.obj)
res.backward <- as.data.frame(res2)
head(res.backward)

ggplot(res.backward,aes(res.backward)) + geom_histogram(bin = 20,alpha=0.5,color = 'green', fill = 'orange') + theme_minimal()

####################################################################
plot(backward.obj) #### Residuals vs  Fittted####
backward.obj$fitted

########## Forward Selection#############################################
table(dat6$Attrition) # take a look at how many yes and no first

min.model = glm(Attrition ~ 1, data = train, family = 'binomial')
max.model = glm(Attrition ~ ., data = train, family = 'binomial')
max.formula = formula(max.model)

lreg1 = step(min.model, direction='forward', scope=max.formula) # it will print out models in each step
summary(lreg1)

########################### Misclassification Error Forward Selection###############

predictedattrition <- predict(lreg1,newdata=test,type='response')

table(test$Attrition, predictedattrition > 0.5)

Accuracy <- (443+29)/(443+29+22+56)
print(Accuracy)

Misclassificationerror1 <- (22+56)/(22+56+443+29)

print(Misclassificationerror1)




########## Backward Selection#############################################

min.model = glm(Attrition ~ 1, data = train, family = 'binomial')
max.model = glm(Attrition ~ ., data = train, family = 'binomial')
min.formula = formula(min.model)

lreg2 = step(max.model, direction='backward', scope=min.formula) # it will print out models in each step
summary(lreg2)

########################### Misclassification Error Forward Selection###############

predictedattrition1 <- predict(lreg2,newdata=test,type='response')

table(test$Attrition, predictedattrition1 > 0.5)

Accuracy <- (439 + 29)/(439 +29 + 26 +56)
print(Accuracy)

Misclassificationerror2 <- (22+56)/(22+56+443+29)

print(Misclassificationerror2)



############################## Best Subset ###########################################

library(leaps)
reg3 = regsubsets(MonthlyIncome ~.- Divorced - Department_Human.Resources - JobRole_Healthcare.Representative, data = train, nbest=5)
summary(reg3)

plot(reg3,scale="r2")
