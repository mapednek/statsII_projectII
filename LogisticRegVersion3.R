library(dplyr)
library(sqldf)
library(ggplot2)


dat <- read.csv("train.csv", header = TRUE)
testDf <- read.csv("test.csv", header = TRUE)

data2 <-subset(dat, select=c("age", "workclass", "education_num", "marital_status", "occupation", "race", "sex", "capital_gain", "capital_loss", "hours_per_week", "native_country", "income"))
testDf2 <-subset(testDf, select=c("age", "workclass", "education_num", "marital_status", "occupation", "race", "sex", "capital_gain", "capital_loss", "hours_per_week", "native_country", "income"))
summary(data2)
summary(testDf2)
hist(data2$capital_loss)
hist(data2$capital_gain)

####################################
####Normalization of variables
####################################

## normalize education_num column
data2$educX6_25<-data2$education_num*6.25


### normalize workclass
#df$educ.f <- as.numeric(factor(df$educ , levels=c("High School Diploma" ,"Current Undergraduate", "PhD")))
workclassLevels <- c("Private", "Self-emp-not-inc", "Local-gov", "State-gov", "Self-emp-inc", "Federal-gov", "Without-pay")
#data2$workclassNorm <- as.numeric(factor(data2$workclass, levels= workclassLevels))

data2$workclassNorm <- as.numeric(factor(data2$workclass, levels= workclassLevels))
### multiply by 100/7 = 14.28
data2$workclassNorm <- data2$workclassNorm*14.28

##forWorkclass <-subset(data2, select=c("age", "workclass","workclassNorm"))

##write.csv(forWorkclass, "firstNorm.csv", row.names = FALSE)
############Marital status
workclassLevels <- c("Married-civ-spouse", "Never-married", "Divorced", "Separated", "Widowed", "Married-spouse-absent", "Married-AF-spouse")
data2$maritalStatNorm <- as.numeric(factor(data2$marital_status, levels= workclassLevels))
### multiply by 100/7 = 14.28
data2$maritalStatNorm <- data2$maritalStatNorm*14.28

####################Occupation

workclassLevels <- c("Prof-specialty", "Craft-repair", "Exec-managerial", "Adm-clerical", "Sales", "Other-service", "Machine-op-inspct", "Transport-moving", "Handlers-cleaners", "Farming-fishing", "Tech-support", "Protective-serv", "Priv-house-serv", "Armed-Forces")
data2$occupationNorm <- as.numeric(factor(data2$occupation, levels= workclassLevels))
### multiply by 100/14 = 7.14
data2$occupationNorm <- data2$occupationNorm*7.14
######################Race
workclassLevels <- c("White", "Black", "Asian-Pac-Islander", "Amer-Indian-Eskimo", "Other")
data2$raceNorm <- as.numeric(factor(data2$race, levels= workclassLevels))
### multiply by 100/5 = 20
data2$raceNorm <- data2$raceNorm*20

####################################sex

workclassLevels <- c("Male", "Female")
data2$sexNorm <- as.numeric(factor(data2$sex, levels= workclassLevels))
### multiply by 100/5 = 20
data2$sexNorm <- data2$sexNorm*50


##################################Relationship
data2$relationship <- dat$relationship
workclassLevels <- c("Husband", "Not-in-family", "Own-child", "Unmarried", "Wife", "Other-relative")
data2$relateNorm <- as.numeric(factor(data2$relationship, levels= workclassLevels))
### multiply by 100/5 = 20
data2$relateNorm <- data2$relateNorm*16.66

#######################income Norm
workclassLevels <- c("<=50K", ">50K")
data2$incomeNorm <- as.numeric(factor(data2$income, levels= workclassLevels))
### multiply by 100/5 = 20
data2$incomeNorm <- data2$incomeNorm*50


####### PCA code

data4 <- cbind(data2$age, data2$educX6_25, data2$workclassNorm,
               data2$maritalStatNorm, data2$occupationNorm, data2$raceNorm,
               data2$sexNorm, data2$relateNorm, data2$hours_per_week, data2$incomeNorm)

colnames(data4) <-  c("age", "educNumNorm", "workclassNorm", "maritalStatNorm", "occupationNorm", "raceNorm", "sexNorm", "relateNorm", "hours_per_week", "incomeNorm")
##Pairwise plots

data4 <- as.data.frame(data4)
summary(data4)
cor(data4)
###############################################################
###Pric componenet analysis
#############################################################

#install.packages("FactoMineR")
library(FactoMineR)

# apply PCA
pca = PCA(data4, graph = FALSE)

# matrix with eigenvalues
pca$eig

#correlations between variables and PCs
pca$var$coord

# PCs (aka scores)
head(pca$ind$coord)


### logistic regression
#################################################################

###########################################################
