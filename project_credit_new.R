###############################################################################
#    Date : 30/09/2020                                                         #
#     
#    Purpose: Exploratory Data Analysis                                       #
###############################################################################
#-----------------------------
#------Package List---------
#-----------------------------

#------Chart------
#install.packages("ggplot2")
library(ggplot2)

#------Linear Model------
#install.packages('caTools')
library(caTools)

#install.packages('dplyr')
library(dplyr)

#------Random forest------
#install.packages('randomForest')
library(randomForest)

#------Missing values------
#install.packages('Amelia')
library(Amelia)

#------Correlations------ 
#install.packages("corrplot")
library(corrplot)
#install.packages("caret")
library(caret)

#------Text mining / Naive base------
#install.packages('tm')
library(tm)

#install.packages('e1071')
library(e1071)

#install.packages('wordcloud')
library(wordcloud)

#install.packages('SDMTools')
library(SDMTools)

#------Decision trees------
#install.packages('rpart')
library(rpart)

#install.packages('rpart.plot')
library(rpart.plot)

#install.packages('ISLR')
library('ISLR')

#-----kNN-------
#install.packages('class')
library(class)

#------ROC------
#install.packages('pROC')
library(pROC)

#-----Oversampeling------
#install.packages("ROSE")
library(ROSE)

#------Other Useful------
#install.packages("Cairo", "cairoDevice ")
#install.packages("gridExtra")
#install.packages("caret")
#install.packages("mice")
#install.packages("outliers")
#install.packages("moderndive")
#install.packages("effects")
#install.packages("VIM")
library(Cairo ,cairoDevice)
library(gridExtra)
library(caret)
library(mice)
library(outliers)
library(moderndive)
library(effects)
library("VIM")
#-----------------------------------

#-----------------------------------------
# Step 1: Loading of database
#-----------------------------------------
credit.raw <- read.csv("credit.csv" )
credit.df <-credit.raw
#------------------------------------
# Step 2: Explore data 
#       : check for missing data
#       : converting to factor
#------------------------------------

head(credit.df)
str(credit.df)
summary(credit.df)

credit.df$SEX <- as.factor(credit.df$SEX)
sex_levels <- c("male", "female")
levels(credit.df$SEX) <- sex_levels


credit.df$EDUCATION <-
  as.factor(credit.df$EDUCATION)
education_levels <-c("unknown","graduate.school","university", "high.school","others","unknown","unknown")
levels(credit.df$EDUCATION) <- education_levels




credit.df$MARRIAGE <-as.factor(credit.df$MARRIAGE)
marriage_levels <- c("unknown", "married", "single", "others")
levels(credit.df$MARRIAGE) <- marriage_levels

credit.df$PAY_0 <-
  as.factor(credit.df$PAY_0)
credit.df$PAY_2 <-
  as.factor(credit.df$PAY_2)
credit.df$PAY_3 <-
  as.factor(credit.df$PAY_3)
credit.df$PAY_4 <-
  as.factor(credit.df$PAY_4)
credit.df$PAY_5 <-
  as.factor(credit.df$PAY_5)
credit.df$PAY_6 <-
  as.factor(credit.df$PAY_6)

credit.df$default.payment.next.month <-
  as.factor(credit.df$default.payment.next.month)
default.payment.next.month_levels <- c("no", "yes")
levels(credit.df$default.payment.next.month) <-
  default.payment.next.month_levels

credit.df$ID <- NULL
aggr(credit.df)
#------------------------------------
# Step 3: check each attribute for anomalys 

#------------------------------------

ggplot(data = credit.df,
       aes(x = default.payment.next.month, fill = default.payment.next.month)) + geom_bar() + scale_y_continuous(breaks = seq(min(0), max(30000), by = 500), na.value = TRUE)

credit.df$BILL_AMT_SUM <-
  rowSums(credit.df[c("BILL_AMT1", "BILL_AMT2","BILL_AMT3","BILL_AMT4","BILL_AMT5","BILL_AMT6")])
credit.df$PAY_AMT_SUM <-
  rowSums(credit.df[c("PAY_AMT1","PAY_AMT2","PAY_AMT3","PAY_AMT4","PAY_AMT5","PAY_AMT6")])

credit.df$default.payment.next.month <- as.factor(credit.df$default.payment.next.month)
#age
ggplot(data = credit.df)+aes(x = AGE, fill = default.payment.next.month) + geom_histogram(bins = 50, col = "blue", alpha = 0.3) 

#sex
summary(credit.df$EDUCATION)

ggplot(data = credit.df, aes(x = SEX, fill = default.payment.next.month)) + geom_bar()

#education

ggplot(data = credit.df, aes(x = EDUCATION, fill = default.payment.next.month)) + 
  geom_bar() + 
  scale_y_continuous(breaks = seq(min(0), max(20000), by = 500), na.value = TRUE)


#delete unknown or uneven and making them evenly distabuted

credit.df2 <-credit.df
credit.df2$EDUCATION <- ifelse( credit.df2$EDUCATION == "unknown", "3" , credit.df2$EDUCATION )

credit.df2$EDUCATION <-as.factor(credit.df2$EDUCATION)
education_levels <-c("graduate.school","university","high.school","others")
levels(credit.df2$EDUCATION) <- education_levels

credit.df2$EDUCATION <- ifelse( credit.df2$EDUCATION == "others", "3" , credit.df2$EDUCATION )

credit.df2$EDUCATION <-as.factor(credit.df2$EDUCATION)
education_levels <-c("graduate.school","university","high.school")
levels(credit.df2$EDUCATION) <- education_levels

ggplot(data = credit.df2, aes(x = EDUCATION, fill = default.payment.next.month)) + 
  geom_bar() + 
  scale_y_continuous(breaks = seq(min(0), max(20000), by = 500), na.value = TRUE)
credit.df<-credit.df2

#marrige
ggplot(data = credit.df, aes(x = MARRIAGE, fill = default.payment.next.month)) + 
  geom_bar() + 
  scale_y_continuous(breaks = seq(min(0), max(20000), by = 500), na.value = TRUE)


#delete unknown or uneven and making them evenly distabuted
credit.df$MARRIAGE <- ifelse( credit.df$MARRIAGE == "unknown", "3" , credit.df$MARRIAGE )
credit.df$MARRIAGE <-as.factor(credit.df$MARRIAGE)
marriage_levels <- c("married", "single", "others")
levels(credit.df$MARRIAGE) <- marriage_levels

credit.df$MARRIAGE <- ifelse( credit.df$MARRIAGE == "others", "2" , credit.df$MARRIAGE )

credit.df$MARRIAGE <-as.factor(credit.df$MARRIAGE)
marriage_levels <- c("married", "single")
levels(credit.df$MARRIAGE) <- marriage_levels

#pay 0-5

ggplot(data = credit.df, aes(x = PAY_6, fill = default.payment.next.month)) + 
  geom_bar() + 
  scale_y_continuous(breaks = seq(min(0), max(20000), by = 500), na.value = TRUE)

# Getting rid of the edges in all Pay_0-Pay_6
credit.df$PAY_0 <-as.numeric(credit.df$PAY_0)
credit.df$PAY_0 <- ifelse( credit.df$PAY_0 >= 6, "6" , credit.df$PAY_0 )
credit.df$PAY_0 <- ifelse( credit.df$PAY_0 <= 3, "3" , credit.df$PAY_0 )
credit.df$PAY_0 <-as.factor(credit.df$PAY_0)

credit.df$PAY_2 <-as.numeric(credit.df$PAY_2)
credit.df$PAY_2 <- ifelse( credit.df$PAY_2 >= 6, "6" , credit.df$PAY_2 )
credit.df$PAY_2 <- ifelse( credit.df$PAY_2 <= 3, "3" , credit.df$PAY_2 )
credit.df$PAY_2 <-as.factor(credit.df$PAY_2)

credit.df$PAY_3 <-as.numeric(credit.df$PAY_3)
credit.df$PAY_3 <- ifelse( credit.df$PAY_3 >= 6, "6" , credit.df$PAY_3 )
credit.df$PAY_3 <- ifelse( credit.df$PAY_3 <= 3, "3" , credit.df$PAY_3 )
credit.df$PAY_3 <-as.factor(credit.df$PAY_3)

credit.df$PAY_4 <-as.numeric(credit.df$PAY_4)
credit.df$PAY_4 <- ifelse( credit.df$PAY_4 >= 6, "6" , credit.df$PAY_4 )
credit.df$PAY_4 <- ifelse( credit.df$PAY_4 <= 3, "3" , credit.df$PAY_4 )
credit.df$PAY_4 <-as.factor(credit.df$PAY_4)

credit.df$PAY_5 <-as.numeric(credit.df$PAY_5)
credit.df$PAY_5 <- ifelse( credit.df$PAY_5 >= 6, "6" , credit.df$PAY_5 )
credit.df$PAY_5 <- ifelse( credit.df$PAY_5 <= 3, "3" , credit.df$PAY_5 )
credit.df$PAY_5 <-as.factor(credit.df$PAY_5)

credit.df$PAY_6 <-as.numeric(credit.df$PAY_6)
credit.df$PAY_6 <- ifelse( credit.df$PAY_6 >= 6, "6" , credit.df$PAY_6 )
credit.df$PAY_6 <- ifelse( credit.df$PAY_6 <= 3, "3" , credit.df$PAY_6 )
credit.df$PAY_6 <-as.factor(credit.df$PAY_6)


credit.df.numeric <-
  credit.df[, sapply(credit.df, is.numeric)]

cor(credit.df.numeric)
corrplot.mixed(
  cor(credit.df.numeric),
  upper = "shade",
  lower = "number",
  tl.pos = "lt",
  addCoef.col = "black",
  number.cex = .6
)

#plotting the numeric variables

#Total credit on defult
ggplot(credit.df, aes(default.payment.next.month,ï..LIMIT_BAL,color = default.payment.next.month)) + geom_boxplot(alpha = 0.5, outlier.colour = "orange") + scale_y_continuous(breaks = seq(min(0), max(2000000), by = 50000), na.value = TRUE) + 
  theme(legend.position = "bottom")


#BILL_AMT1 on defult 
ggplot(credit.df, aes(default.payment.next.month,BILL_AMT1,color = default.payment.next.month)) + geom_boxplot(alpha = 0.5, outlier.colour = "orange") + scale_y_continuous(breaks = seq(min(0), max(2000000), by = 50000), na.value = TRUE) + 
  theme(legend.position = "bottom")


#BILL_AMT2 on defult 
ggplot(credit.df, aes(default.payment.next.month,BILL_AMT2,color = default.payment.next.month)) + geom_boxplot(alpha = 0.5, outlier.colour = "orange") + scale_y_continuous(breaks = seq(min(0), max(2000000), by = 50000), na.value = TRUE) + 
  theme(legend.position = "bottom")
#BILL_AMT3 on defult 
ggplot(credit.df, aes(default.payment.next.month,BILL_AMT3,color = default.payment.next.month)) + geom_boxplot(alpha = 0.5, outlier.colour = "orange") + scale_y_continuous(breaks = seq(min(0), max(2000000), by = 50000), na.value = TRUE) + 
  theme(legend.position = "bottom")
#BILL_AMT4 on defult 
ggplot(credit.df, aes(default.payment.next.month,BILL_AMT4,color = default.payment.next.month)) + geom_boxplot(alpha = 0.5, outlier.colour = "orange") + scale_y_continuous(breaks = seq(min(0), max(2000000), by = 50000), na.value = TRUE) + 
  theme(legend.position = "bottom")
#BILL_AMT5 on defult 
ggplot(credit.df, aes(default.payment.next.month,BILL_AMT5,color = default.payment.next.month)) + geom_boxplot(alpha = 0.5, outlier.colour = "orange") + scale_y_continuous(breaks = seq(min(0), max(2000000), by = 50000), na.value = TRUE) + 
  theme(legend.position = "bottom")
#BILL_AMT6 on defult 
ggplot(credit.df, aes(default.payment.next.month,BILL_AMT6,color = default.payment.next.month)) + geom_boxplot(alpha = 0.5, outlier.colour = "orange") + scale_y_continuous(breaks = seq(min(0), max(2000000), by = 50000), na.value = TRUE) + 
  theme(legend.position = "bottom")

#BILL_AMT_SUM on defult 
ggplot(credit.df, aes(default.payment.next.month,BILL_AMT_SUM,color = default.payment.next.month)) + geom_boxplot(alpha = 0.5, outlier.colour = "orange") + scale_y_continuous(breaks = seq(min(0), max(2000000), by = 50000), na.value = TRUE) + 
  theme(legend.position = "bottom")


#PAY_AMT1 on defult 
ggplot(credit.df, aes(default.payment.next.month,PAY_AMT1,color = default.payment.next.month)) + geom_boxplot(alpha = 0.5, outlier.colour = "orange") + scale_y_continuous(breaks = seq(min(0), max(2000000), by = 50000), na.value = TRUE) + 
  theme(legend.position = "bottom")
#PAY_AMT2 on defult 
ggplot(credit.df, aes(default.payment.next.month,PAY_AMT2,color = default.payment.next.month)) + geom_boxplot(alpha = 0.5, outlier.colour = "orange") + scale_y_continuous(breaks = seq(min(0), max(2000000), by = 50000), na.value = TRUE) + 
  theme(legend.position = "bottom")
#PAY_AMT3 on defult 
ggplot(credit.df, aes(default.payment.next.month,PAY_AMT3,color = default.payment.next.month)) + geom_boxplot(alpha = 0.5, outlier.colour = "orange") + scale_y_continuous(breaks = seq(min(0), max(2000000), by = 50000), na.value = TRUE) + 
  theme(legend.position = "bottom")
#PAY_AMT4 on defult 
ggplot(credit.df, aes(default.payment.next.month,PAY_AMT4,color = default.payment.next.month)) + geom_boxplot(alpha = 0.5, outlier.colour = "orange") + scale_y_continuous(breaks = seq(min(0), max(2000000), by = 50000), na.value = TRUE) + 
  theme(legend.position = "bottom")
#PAY_AMT5 on defult 
ggplot(credit.df, aes(default.payment.next.month,PAY_AMT5,color = default.payment.next.month)) + geom_boxplot(alpha = 0.5, outlier.colour = "orange") + scale_y_continuous(breaks = seq(min(0), max(2000000), by = 50000), na.value = TRUE) + 
  theme(legend.position = "bottom")
#PAY_AMT6 on defult 
ggplot(credit.df, aes(default.payment.next.month,PAY_AMT6,color = default.payment.next.month)) + geom_boxplot(alpha = 0.5, outlier.colour = "orange") + scale_y_continuous(breaks = seq(min(0), max(2000000), by = 50000), na.value = TRUE) + 
  theme(legend.position = "bottom")

#PAY_AMT_SUM   on defult 
ggplot(credit.df, aes(default.payment.next.month,PAY_AMT_SUM,color = default.payment.next.month)) + geom_boxplot(alpha = 0.5, outlier.colour = "orange") + scale_y_continuous(breaks = seq(min(0), max(2000000), by = 50000), na.value = TRUE) + 
  theme(legend.position = "bottom")

# Boost data
over.df<-ovun.sample(default.payment.next.month ~.,credit.df,method="over",  p=0.5)$data
under.df<-ovun.sample(default.payment.next.month ~.,credit.df, method="under",  p=0.5)$data
both.df<-ovun.sample(default.payment.next.month ~.,credit.df, method="both",  p=0.5)$data 
#show the table
table(over.df$default.payment.next.month)
table(under.df$default.payment.next.month)
table(both.df$default.payment.next.month)


########################################################################
#step 4: creating model
#########################################################################

# Creating Training and test sets (0.8, 0.2) over sampaling
filter <-sample.split(over.df$default.payment.next.month , SplitRatio = 0.80)
training_set <-subset(over.df ,filter == TRUE)
df_test <-subset(over.df ,filter == FALSE)
y<-table(over.df$default.payment.next.month)

decision_tree_model_one <-
  rpart::rpart(
    formula = default.payment.next.month ~ SEX +EDUCATION + MARRIAGE + AGE +PAY_0 +PAY_2 +PAY_3+PAY_4+PAY_5+PAY_6,
    data = training_set,
    method = "class", 
    minsplit = 2, 
    minbucket = 1,
    cp = 0.001
  )
rpart.plot(decision_tree_model_one)

# Creating Training and test sets (0.8, 0.2) under samplanig
filter <-sample.split(under.df$default.payment.next.month , SplitRatio = 0.80)
training_set <-subset(under.df ,filter == TRUE)
df_test <-subset(under.df ,filter == FALSE)
y<-table(under.df$default.payment.next.month)

decision_tree_model_one <-
  rpart::rpart(
    formula = default.payment.next.month ~ SEX +EDUCATION + MARRIAGE + AGE +PAY_0 +PAY_2 +PAY_3+PAY_4+PAY_5+PAY_6,
    data = training_set,
    method = "class", 
    minsplit = 2, 
    minbucket = 1,
    cp = 0.001
  )
rpart.plot(decision_tree_model_one)

# Creating Training and test sets (0.8, 0.2) both
filter <-sample.split(both.df$default.payment.next.month , SplitRatio = 0.80)
training_set <-subset(both.df ,filter == TRUE)
df_test <-subset(both.df ,filter == FALSE)
y<-table(both.df$default.payment.next.month)

decision_tree_model_one <-
  rpart::rpart(
    formula = default.payment.next.month ~ SEX +EDUCATION + MARRIAGE + AGE +PAY_0 +PAY_2 +PAY_3+PAY_4+PAY_5+PAY_6,
    data = training_set,
    method = "class", 
    minsplit = 2, 
    minbucket = 1,
    cp = 0.001
  )
rpart.plot(decision_tree_model_one)

#prediction on the test set 
predict.prob.DT <-predict(decision_tree_model_one,newdata = df_test)
predict.prob.DT<-predict.prob.DT[,2]
predicted.DT<- predict.prob.DT > 0.65  
predicted.DT<-as.numeric(predicted.DT)
actual.DT<-df_test$default.payment.next.month
#confusion Matrix
conf_matrix.DT<- table(predicted.DT, actual.DT)
conf_matrix.DT
TP <- conf_matrix.DT[2,2]
FP <- conf_matrix.DT[2,1]
TN <- conf_matrix.DT[1,1]
FN <- conf_matrix.DT[1,2]

precsion <- TP/(TP+FP) 
precsion
recall <- TP/(TP+FN) 
recall

accuracy.DT<- (TP+TN)/(TP+FP+TN+FN)
accuracy.DT

#optimal decition boundary for the model
decision_boundry<- function(cost.of.not.returned,cost.of.not.given,loan.test,predicted.loan.test){
  temp_lost <- 0
  desition_boundry <- 0
  confusion_matrix = table(loan.test$Loan_Status,predicted.loan.test > 0.04)
  lost<-(confusion_matrix[1,2] / (confusion_matrix[1,2] +confusion_matrix[2,2]))*cost.of.not.returned+cost.of.not.given*(confusion_matrix[2,1] / (confusion_matrix[1,1] +confusion_matrix[2,1]))
  for(var in 4:99)
  {
    confusion_matrix = table(loan.test$Loan_Status,predicted.loan.test > var/100)
    temp_lost <-(confusion_matrix[1,2] / (confusion_matrix[1,2] +confusion_matrix[2,2]))*cost.of.not.returned+cost.of.not.given*(confusion_matrix[2,1] / (confusion_matrix[1,1] +confusion_matrix[2,1]))
    if (temp_lost<lost)
      lost <- temp_lost
    show(lost)
    show(desition_boundry)
    desition_boundry <- var/100
  }
  return(desition_boundry)
}

decision <- decision_boundry(1000,300,actual.DT,predicted.DT)

under.df$default.payment.next.month <- as.factor(under.df$default.payment.next.month)

#############################
#Naive base model
############################

filter <-sample.split(under.df$default.payment.next.month , SplitRatio = 0.80)
training_set <-subset(under.df ,filter == TRUE)
df_test <-subset(under.df ,filter == FALSE)

model.NB <- naiveBayes(training_set,training_set$default.payment.next.month)

#prediction on the test set 
prediction.NB <- predict(model.NB,df_test, type = 'raw')
actual.NB<- df_test$default.payment.next.month
predicted.NB <-prediction.NB[,1]
predicted.NB <-predicted.NB <0.5 #Our Assumption
predicted.NB<-as.numeric(predicted.NB)


#confusion Matrix
conf_matrix.NB<- table(actual.NB,predicted.NB)
conf_matrix.NB
TP <- conf_matrix.NB[2,2]
FP <- conf_matrix.NB[2,1]
TN <- conf_matrix.NB[1,1]
FN <- conf_matrix.NB[1,2]

precsion <- TP/(TP+FP) 
precsion
recall <- TP/(TP+FN) 
recall


accuracy.NB<- (TP+TN)/(TP+FP+TN+FN)
accuracy.NB

#optimal decition boundary
decision_boundry<- function(cost.of.not.returned,cost.of.not.given,loan.test,predicted.loan.test){
  temp_lost <- 0
  desition_boundry <- 0
  confusion_matrix = table(loan.test$Loan_Status,predicted.loan.test > 0.04)
  lost<-(confusion_matrix[1,2] / (confusion_matrix[1,2] +confusion_matrix[2,2]))*cost.of.not.returned+cost.of.not.given*(confusion_matrix[2,1] / (confusion_matrix[1,1] +confusion_matrix[2,1]))
  for(var in 4:99)
  {
    confusion_matrix = table(loan.test$Loan_Status,predicted.loan.test > var/100)
    temp_lost <-(confusion_matrix[1,2] / (confusion_matrix[1,2] +confusion_matrix[2,2]))*cost.of.not.returned+cost.of.not.given*(confusion_matrix[2,1] / (confusion_matrix[1,1] +confusion_matrix[2,1]))
    if (temp_lost<lost)
      lost <- temp_lost
    show(lost)
    show(desition_boundry)
    desition_boundry <- var/100
  }
  return(desition_boundry)
}

decision <- decision_boundry(1000,300,actual.NB,predicted.NB)

#################################
#logistic regration model
################################
filter <-sample.split(under.df$default.payment.next.month , SplitRatio = 0.80)
training_set <-subset(under.df ,filter == TRUE)
df_test <-subset(under.df ,filter == FALSE)

model.LR <- glm(default.payment.next.month ~. , family = binomial(link = 'logit') , data = training_set)
summary(model.LR)
anova(model.LR, test="Chisq")

predicted.LR <-predict(model.LR , newdata = df_test , type = 'response')
actual.LR<-df_test$default.payment.next.month
#confusion matrix
confusion_matrix.LR <- table(as.numeric(predicted.LR >0.5) , actual.LR)
confusion_matrix.LR


TP <- confusion_matrix.LR[2,2]
FP <- confusion_matrix.LR[2,1]
TN <- confusion_matrix.LR[1,1]
FN <- confusion_matrix.LR[1,2]


precsion <- TP/(TP+FP) #0.775
precsion
recall <- TP/(TP+FN) #0.584
recall

accuracy.lr<- (TP+TN)/(TP+FP+TN+FN)
accuracy.lr

#optimal decition boundary
decision_boundry<- function(cost.of.not.returned,cost.of.not.given,loan.test,predicted.loan.test){
  temp_lost <- 0
  desition_boundry <- 0
  confusion_matrix = table(loan.test$Loan_Status,predicted.loan.test > 0.04)
  lost<-(confusion_matrix[1,2] / (confusion_matrix[1,2] +confusion_matrix[2,2]))*cost.of.not.returned+cost.of.not.given*(confusion_matrix[2,1] / (confusion_matrix[1,1] +confusion_matrix[2,1]))
  for(var in 4:99)
  {
    confusion_matrix = table(loan.test$Loan_Status,predicted.loan.test > var/100)
    temp_lost <-(confusion_matrix[1,2] / (confusion_matrix[1,2] +confusion_matrix[2,2]))*cost.of.not.returned+cost.of.not.given*(confusion_matrix[2,1] / (confusion_matrix[1,1] +confusion_matrix[2,1]))
    if (temp_lost<lost)
      lost <- temp_lost
    show(lost)
    show(desition_boundry)
    desition_boundry <- var/100
  }
  return(desition_boundry)
}

decision <- decision_boundry(1000,300,actual.LR,predicted.LR)

####################################
# random forest model
##################################
filter <-sample.split(over.df$default.payment.next.month , SplitRatio = 0.80)
training_set <-subset(over.df ,filter == TRUE)
df_test <-subset(over.df ,filter == FALSE)
y<-table(over.df$default.payment.next.month)

samp_size = nrow(training_set[training_set$default.payment.next.month=="yes",])
model.rf <- randomForest(default.payment.next.month~.,  data = training_set, ntree=150) 
model.rf
plot(model.rf)




model.rf <- randomForest(default.payment.next.month~.,  data = training_set, ntree=30, sampsize=c(samp_size,samp_size)) 
model.rf
plot(model.rf)
# Variable Importance Plot
varImpPlot(model.rf, sort = T, main="Variable Importance")


predicted.RF<- predict(model.rf, newdata = df_test)
actual.RF<-df_test$default.payment.next.month

confusion.matrix_RF<-table(predicted.RF, actual.RF)
confusion.matrix_RF


TP <- confusion.matrix_RF[2,2]
FP <- confusion.matrix_RF[2,1]
TN <- confusion.matrix_RF[1,1]
FN <- confusion.matrix_RF[1,2]

precsion <- TP/(TP+FP) 
precsion
recall <- TP/(TP+FN) 
recall


accuracy.RF<- (TP+TN)/(TP+FP+TN+FN)
accuracy.RF

#optimal decition boundary
decision_boundry<- function(cost.of.not.returned,cost.of.not.given,loan.test,predicted.loan.test){
  temp_lost <- 0
  desition_boundry <- 0
  confusion_matrix = table(loan.test$Loan_Status,predicted.loan.test > 0.04)
  lost<-(confusion_matrix[1,2] / (confusion_matrix[1,2] +confusion_matrix[2,2]))*cost.of.not.returned+cost.of.not.given*(confusion_matrix[2,1] / (confusion_matrix[1,1] +confusion_matrix[2,1]))
  for(var in 4:99)
  {
    confusion_matrix = table(loan.test$Loan_Status,predicted.loan.test > var/100)
    temp_lost <-(confusion_matrix[1,2] / (confusion_matrix[1,2] +confusion_matrix[2,2]))*cost.of.not.returned+cost.of.not.given*(confusion_matrix[2,1] / (confusion_matrix[1,1] +confusion_matrix[2,1]))
    if (temp_lost<lost)
      lost <- temp_lost
    show(lost)
    show(desition_boundry)
    desition_boundry <- var/100
  }
  return(desition_boundry)
}

decision <- decision_boundry(1000,300,actual.RF,predicted.RF)

#############################
#knn model
############################
#install.packages('class')
library(class)
#install.packages('FNN')
library(FNN)
kNN_Df<-over.df
kNN_Df$SEX<-NULL
kNN_Df$EDUCATION<-NULL
kNN_Df$MARRIAGE<-NULL
kNN_Df$PAY_0<-NULL
kNN_Df$PAY_2<-NULL
kNN_Df$PAY_3<-NULL
kNN_Df$PAY_4<-NULL
kNN_Df$PAY_5<-NULL
kNN_Df$PAY_6<-NULL
kNN_Df$default.payment.next.month<-as.numeric(kNN_Df$default.payment.next.month)
str(kNN.train)
filter_KNN <- sample.split(kNN_Df$default.payment.next.month, SplitRatio = 0.8)
kNN.train <- subset(kNN_Df, filter_KNN ==T)
kNN.test <- subset(kNN_Df, filter_KNN ==F)

kNN.150 <-  knn(kNN.train, kNN.test, kNN.train$default.payment.next.month, k=150)
sum.kNN_150<-100 * sum(kNN.test$default.payment.next.month == kNN.150)/100  # For knn =150
kNN.50 <-  knn(kNN.train, kNN.test, kNN.train$default.payment.next.month, k=50)
sum.kNN_50<-100 * sum(kNN.test$default.payment.next.month == kNN.50)/100  # For knn = 50
kNN.100 <- knn(kNN.train, kNN.test, kNN.train$default.payment.next.month, k=100)
sum.kNN_100<-100 * sum(kNN.test$default.payment.next.month == kNN.100)/100 # For knn = 100
min(sum.kNN_150 ,sum.kNN_50,sum.kNN_100)



actual.kNN<-kNN.test$default.payment.next.month
#confusion matrix
#------confusion.matrix_kNN.150
confusion.matrix_kNN.150 <- table(kNN.150 ,actual.kNN)
confusion.matrix_kNN.150

TP <- confusion.matrix_kNN.150[2,2]
FP <- confusion.matrix_kNN.150[2,1]
TN <- confusion.matrix_kNN.150[1,1]
FN <- confusion.matrix_kNN.150[1,2]

precision.kNN.150 <- TP/(TP +FP)
recall.kNN.150 <- TP/(TP + FN)
precision.kNN.150 #0.605
recall.kNN.150  #0.7137

#optimal decition boundary
decision_boundry<- function(cost.of.not.returned,cost.of.not.given,loan.test,predicted.loan.test){
  temp_lost <- 0
  desition_boundry <- 0
  confusion_matrix = table(loan.test$Loan_Status,predicted.loan.test > 0.04)
  lost<-(confusion_matrix[1,2] / (confusion_matrix[1,2] +confusion_matrix[2,2]))*cost.of.not.returned+cost.of.not.given*(confusion_matrix[2,1] / (confusion_matrix[1,1] +confusion_matrix[2,1]))
  for(var in 4:99)
  {
    confusion_matrix = table(loan.test$Loan_Status,predicted.loan.test > var/100)
    temp_lost <-(confusion_matrix[1,2] / (confusion_matrix[1,2] +confusion_matrix[2,2]))*cost.of.not.returned+cost.of.not.given*(confusion_matrix[2,1] / (confusion_matrix[1,1] +confusion_matrix[2,1]))
    if (temp_lost<lost)
      lost <- temp_lost
    show(lost)
    show(desition_boundry)
    desition_boundry <- var/100
  }
  return(desition_boundry)
}

decision <- decision_boundry(1000,300,kNN.150 ,actual.kNN)
#--------confusion.matrix_kNN.50
confusion.matrix_kNN.50 <- table(kNN.50 ,actual.kNN)
confusion.matrix_kNN.50

TP <- confusion.matrix_kNN.50[2,2]
FP <- confusion.matrix_kNN.50[2,1]
TN <- confusion.matrix_kNN.50[1,1]
FN <- confusion.matrix_kNN.50[1,2]

precision.kNN.50 <- TP/(TP +FP)
recall.kNN.50 <- TP/(TP + FN)
precision.kNN.50  #
recall.kNN.50  #


#--------confusion.matrix_kNN.100
confusion.matrix_kNN.100 <- table(kNN.100 ,actual.kNN)
confusion.matrix_kNN.100

TP <- confusion.matrix_kNN.100[2,2]
FP <- confusion.matrix_kNN.100[2,1]
TN <- confusion.matrix_kNN.100[1,1]
FN <- confusion.matrix_kNN.100[1,2]

precision.kNN.100 <- TP/(TP +FP)
recall.kNN.100 <- TP/(TP + FN)
precision.kNN.100  #0.620
recall.kNN.100  #0.695

accuracy.KNN.100<- (TP+TN)/(TP+FP+TN+FN)
accuracy.KNN.100

################
#roc chart
#############
rocNB<-roc(actual.NB  ,predicted.NB, direction = "<"  )
rocDT <- roc(actual.DT ,predicted.DT , direction = "<"  )
rocRF <-roc(actual.RF  ,as.numeric(predicted.RF ), direction = "<")
rockNN <-roc(actual.kNN  ,as.numeric(kNN.150 ), direction = "<"  )
rocLG <- roc(actual.LR,predicted.LR ,direction = "<" )



plot(rocNB , col = 'red' , main = 'ROC chart')

par(new=TRUE)

plot(rocDT , col = 'blue' , main = 'ROC chart')

par(new=TRUE)

plot(rocRF , col = 'pink' , main = 'ROC chart')

par(new=TRUE)
plot(rockNN , col = 'orange' , main = 'ROC chart')

par(new=TRUE)
plot(rocLG , col = 'black' , main = 'ROC chart')

par(new=TRUE)

auc(rocNB) 
auc(rocDT) 
auc(rocRF) 
auc(rockNN) 
auc(rocLG) 

# Financial model 
# cost of non return 1000$
# revenue for returning the credit 300$

profit<-function(result.vec,cost.of.non.return,revenue.on.return){
  revenue = 0
  for (variable in result.vec) {
    if(variable == "no"  ){
      revenue<-revenue+revenue.on.return
    }
    else{
      revenue<-revenue-cost.of.non.return
    }
  }
  return (revenue)
}

summary(credit.df$default.payment.next.month)
regular.profit <- profit(credit.df$default.payment.next.month,1000,300)
format(regular.profit, scientific = FALSE)


predicted.RF.profit<- predict(model.rf, newdata = credit.df)
summary(predicted.RF.profit)
format(RF.profit, scientific = FALSE)

predicted.NB.profit<- predict(model.NB, newdata = credit.df)
summary(predicted.NB.profit)

