library(tigerstats)
library(sqldf)
library(ggplot2)

###Import data
trainDf <- read.csv("train.csv", header = TRUE)
testDf <- read.csv("test.csv", header = TRUE)
summary(trainDf)

##### EDA of Variable of Interest ####
# For building model, We decided not to use "education", "relationship" variables from the summary of the data.
# Instead of "native_country", we are using "global_region"


#Percentages of above and below paid Per level of categorical variables and plot for Catergorical variables
#Percentages of above and below paid Per workclass category
colPerc(xtabs(~income+workclass, trainDf))#Variable of Interest
workclass_inc <-sqldf("select workclass, income, count(workclass) as Count from trainDf group by workclass, income")
# Plot of above paid and below paid counts per workclass level
ggplot(workclass_inc, aes(x=workclass, y=Count, fill=income)) + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('Above paid and below paid proportions per workclass level')


#Percentages of above and below paid Per education category
colPerc(xtabs(~income+education, trainDf))#Variable of Interest
education_inc <-sqldf("select education, income, count(education) as Count from trainDf group by education, income")
# Plot of above paid and below paid counts per education level
ggplot(education_inc, aes(x=education, y=Count, fill=income)) + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('Above paid and below paid proportions per education level')

#Percentages of above and below paid Per marital_status category
colPerc(xtabs(~income+marital_status, trainDf))#Variable of Interest
maritalStatus_inc <-sqldf("select marital_status, income, count(marital_status) as Count from trainDf group by marital_status, income")
# Plot of above paid and below paid counts per education level
ggplot(maritalStatus_inc, aes(x=marital_status, y=Count, fill=income)) + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('Above paid and below paid proportions per marital_status level')


#Percentages of above and below paid Per occupation category
colPerc(xtabs(~income+occupation, trainDf))#Variable of Interest
occupation_inc <-sqldf("select occupation, income, count(occupation) as Count from trainDf group by occupation, income")
# Plot of above paid and below paid counts per education level
ggplot(occupation_inc, aes(x=occupation, y=Count, fill=income)) + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('Above paid and below paid proportions per occupation level')

#Percentages of above and below paid Per race category
colPerc(xtabs(~income+race, trainDf))#Variable of Interest
race_inc <-sqldf("select race, income, count(race) as Count from trainDf group by race, income")
# Plot of above paid and below paid counts per race level
ggplot(race_inc, aes(x=race, y=Count, fill=income)) + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('Above paid and below paid proportions per race level')

#Percentages of above and below paid Per sex category
colPerc(xtabs(~income+sex, trainDf))#Variable of Interest
sex_inc <-sqldf("select sex, income, count(sex) as Count from trainDf group by sex, income")
# Plot of above paid and below paid counts per sex level
ggplot(sex_inc, aes(x=sex, y=Count, fill=income)) + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('Above paid and below paid proportions per sex level')

#Percentages of above and below paid Per global_region category
colPerc(xtabs(~income+ global_region, trainDf))#Variable of Interest
region_inc <-sqldf("select global_region, income, count(global_region) as Count from trainDf group by global_region, income")
# Plot of above paid and below paid counts per race level
ggplot(region_inc, aes(x=global_region, y=Count, fill=income)) + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('Above paid and below paid proportions per global_region level')

#Percentages of above and below paid Per hours_per_week_category
colPerc(xtabs(~income+ hours_per_week_category, trainDf))#Variable of Interest
hours_inc <-sqldf("select hours_per_week_category, income, count(hours_per_week_category) as Count from trainDf group by hours_per_week_category, income")
# Plot of above paid and below paid counts per hours_per_week_category
ggplot(hours_inc, aes(x=hours_per_week_category, y=Count, fill=income)) + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle('Above paid and below paid proportions per Hours Per Week Category level')

# Finally summary of the dependent variable
ftable(trainDf$income)
#colPerc(summary(trainDf$income))


#### Summary stats by above and below paid groups for Continuous variables

aggregate(trainDf$capital_gain~trainDf$income,data=trainDf,summary)
qplot(capital_gain, data=trainDf, geom="histogram")+theme_bw()+ggtitle('Histogram of Capital Gain')

aggregate(trainDf$capital_loss~trainDf$income,data=trainDf,summary)
qplot(capital_loss, data=trainDf, geom="histogram")+theme_bw()+ggtitle('Histogram of Capital Loss')

#####Age: Boxplot of Age shows people who earn >50k are between age of 36 to 51 with 43 as the median
boxplot(age~income,trainDf,main="Boxplot of Age")

aggregate(trainDf$age~trainDf$income,data=trainDf,summary)

#Examine the correlation between the continous predictors
## The scatterplots and correlation matrix shows no significant correlation between any of the continuous variables
#pairs(trainDf[,c("age","education_num","hours_per_week", "capital_gain", "capital_loss")])
my_cor<-cor(trainDf[,c("age","education_num","hours_per_week", "capital_gain", "capital_loss")])
my_cor

############################################################
########After EDA code#####################################
#############################################################

trainTest <- rbind(trainDf, testDf)
# when I ran logistic regression code on a modified train dataset made up of equal obs for high income and 
#low income(as per Dr. Turner's suggestion to get a good prediction model using this type of 
#stratified sampling method), the sample workclass and occupation variables had atleast one level 
#with Zero observations. Thus I am sampling 
ftable(addmargins(table(trainTest$income,trainTest$occupation))) 
ftable(addmargins(table(trainTest$income,trainTest$workclass)))

noPayobs<-trainTest[which(trainTest$workclass=="Without-pay"),]

#I decided to not include "relationship" factor variable since it seems redundant and for the sake of
#simplicity of the model.From the summary of this dataset, it seems that marital_status and sex variables 
# are capturing the information provided by "relationship" variable.
#To build the predictive model to predict who earn more than 50 K per annum, 
#we decided to include age, workclass, education_num, marital_status, occupation, 
#race, sex, capital_gain, capital_loss, hours_per_week, global_region
#after reviewing plots and the summary stats for all the variables.
trainDf2 <-subset(trainDf, select=c("age", "workclass", "education_num", "marital_status", "occupation", "race", "sex", "capital_gain", "capital_loss", "hours_per_week", "global_region", "income"))
testDf2 <-subset(testDf, select=c("age", "workclass", "education_num", "marital_status", "occupation", "race", "sex", "capital_gain", "capital_loss", "hours_per_week", "global_region", "income"))
noPayobs <-subset(noPayobs, select=c("age", "workclass", "education_num", "marital_status", "occupation", "race", "sex", "capital_gain", "capital_loss", "hours_per_week", "global_region", "income"))
#summary(trainDf2)

above50k <- trainDf2[trainDf2$income==">50K",]
below50k <- trainDf2[trainDf2$income=="<=50K",]


set.seed(111)

sampleBelow50 <- below50k[sample(nrow(below50k), size = 7508), ]
trainDf3 <-rbind(sampleBelow50, above50k, noPayobs)

#making sure that each category level of each categorical variable is represented in the sampled train data
xtabs(~income+workclass, data = trainDf3)
xtabs(~income+marital_status, data = trainDf3)
xtabs(~income+occupation, data = trainDf3)
xtabs(~income+race, data = trainDf3)
xtabs(~income+sex, data = trainDf3)
xtabs(~income+global_region, data = trainDf3)

#########################################################################
###############Logistic regression######################################
########################################################################

#install.packages("nnet")
library(nnet)
#install.packages("ROCR")
library(ROCR)
trainDf3$income<-ifelse(trainDf3$income=='>50K',1,0)
testDf2$income<-ifelse(testDf2$income=='>50K',1,0)
mymodel<- multinom(income~., data = trainDf3)

#just to get ready-made Z-values and p-values of coeffients
#Residual Deviance: 11579.03 and AIC: 11667.03 are exactly same for both models using differnt R packages
model2<-glm(income~., data = trainDf3, family = "binomial")
summary(model2)
with(model2, pchisq(null.deviance-deviance, df.null-df.residual, lower.tail = F ))

predProb <- predict(mymodel, trainDf3, type = "prob")
hist(predProb)

#see whether predProb(prediction Probalbilties) matches with actual data and it does for atleast first 6 obs
head(predProb)
head(trainDf3)

predProb <- prediction(predProb, trainDf3$income)
perfEval <- performance(predProb, "acc")
plot(perfEval)
maxYval <- which.max(slot(perfEval, "y.values")[[1]])
maxYval
acc <- slot(perfEval, "y.values")[[1]][maxYval]
acc ## thus the accuracy of the logistic regression on train data is 82.26%

###ROC
roc <- performance(predProb, "tpr", "fpr")
plot(roc)
#abline(a= 0, b=1)

###########  AUC(Area Under Curve) on Train  ###################

auc <- performance(predProb, "auc")
auc<- unlist(slot(auc, "y.values"))
auc # area under the curve for train is .9062


###############################################################
###################ROC on test data
###########################################################
myTstmodel<- multinom(income~., data = trainDf3)
predProbTst <- predict(myTstmodel, testDf2, type = "prob")
hist(predProbTst)

#see whether predProb(prediction Probalbilties) matches with actual data and it does for atleast first 6 obs
head(predProbTst)
head(testDf2)

predProbTst <- prediction(predProbTst, testDf2$income)
perfEvalTst <- performance(predProbTst, "acc")
plot(perfEvalTst)
maxYvalTst <- which.max(slot(perfEvalTst, "y.values")[[1]])
maxYvalTst
accTst <- slot(perfEvalTst, "y.values")[[1]][maxYvalTst]
accTst ## thus the accuracy of the logistic regression on test data is 84.54%

# ROC test
rocTst <- performance(predProbTst, "tpr", "fpr")
plot(rocTst)
#abline(a= 0, b=1)

###########  AUC(Area Under Curve) on Test data ###################

aucTst <- performance(predProbTst, "auc")
aucTst<- unlist(slot(aucTst, "y.values"))
aucTst #  area under the curve for train is 0.90135
legend(.4,.4, aucTst, title= "AUC")
