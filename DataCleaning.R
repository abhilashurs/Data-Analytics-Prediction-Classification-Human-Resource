rm(list=ls())
gc()

setwd("C:\\CSUF\\2nd sem\\ISDS 574")
dat = read.csv('Project_Data_Description.csv', head=T, stringsAsFactors=T, na.strings='')
View(dat)

source('C:\\Users\\usre\\Downloads\\imageMatrix.R')
class(is.na(dat))
myImagePlot(is.na(dat))

id.col = which(colnames(dat) %in% c('YearsAtCompany','RelationshipSatisfaction')) 
dat1 <- dat[,-id.col]
View(dat1)

class(is.na(dat1))
myImagePlot(is.na(dat1))

dat1$EmployeeCount[which(is.na(dat1$EmployeeCount))] <- 1
class(is.na(dat1))
myImagePlot(is.na(dat1))
View(dat1)

dat1$StockOptionLevel[which(is.na(dat1$StockOptionLevel))] <- median(dat1$StockOptionLevel, na.rm = TRUE)
class(is.na(dat1))
myImagePlot(is.na(dat1))
View(dat1)

id.row = rep(NA, nrow(dat1))
for (ii in 1:nrow(dat1))
  id.row[ii] = (sum(is.na(dat1[ii,])) == 0)

dat2 <- dat1[id.row,]
myImagePlot(is.na(dat2))
View(dat2)

write.csv(dat2,file = "Cleaneddata_project.csv")
     