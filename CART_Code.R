###########################################################################################

rm(list=ls())
gc()
install.packages("party")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("caret")
library(rpart)
library(rpart.plot)
library(party)
library(readr)
Final_File <- read_csv("C:/Users/Jagadeesh/Desktop/Final_File.csv")
View(Final_File)
set.seed(1)
ind <- sample(2, nrow(Final_File), replace=TRUE, prob=c(0.60,0.40))
train_data <- Final_File[ind==1,]
test_data <- Final_File[ind==2,]
View(test_data)
tree_line_reg <-rpart(train_data$MonthlyIncome ~ ., data=train_data)
rpart.plot(tree_line_reg, type=3, digits=3, fallen.leaves=TRUE)
text(tree_line_reg, pretty= 0)

p <- predict(tree_line_reg,test_data)
p

rmse = sqrt( mean( (p - test_data$MonthlyIncome)^2, na.rm = TRUE) )
rmse

tree_line_class <-rpart(train_data$Attrition ~ ., data=train_data)
rpart.plot(tree_line_class, type=3, digits=3, fallen.leaves=TRUE)
tree_line_class

p2 <- predict(tree_line_class,test_data)
p2
print(p2)
table(test_data$Attrition, p2 > 0.5)
misclassification_error <- ((62+33)/(62+33+409+27))
misclassification_error
Mis_class_error_rate <- misclassification_error*100
Mis_class_error_rate

######################################################################################################



tree_line_regression <- (Final_File$MonthlyIncome ~ Final_File$JobInvolvement+Final_File$JobLevel+Final_File$TotalWorkingYears+Final_File$YearsWithCurrManager+Final_File$JobRole_Sales_Representative+Final_File$JobRole_Research_Scientist+Final_File$JobRole_Research_Director+Final_File$JobRole_Manager+Final_File$JobRole_Laboratory_Technician)
plot(tree_line_regression)

tree_line_classification <- ctree(Final_File$Attrition ~ Final_File$JobInvolvement+Final_File$JobLevel+Final_File$TotalWorkingYears+Final_File$YearsWithCurrManager+Final_File$JobRole_Sales_Representative+Final_File$JobRole_Research_Scientist+Final_File$JobRole_Research_Director+Final_File$JobRole_Manager+Final_File$JobRole_Laboratory_Technician)
plot(tree_line_classification)

