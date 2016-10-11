## Importing required libraries

library(car)
library(Hmisc)
library(ROCR)
library(caret)
library(ggplot2)
library(moments)

setwd("D:\\education\\pgdda\\Course - 3\\assignment logistic regression")

## Download the data set as german_credit

german_credit <- read.csv("german.csv", header = TRUE)

View(german_credit)

#### => Checkpoint 1: Data Understanding and Data Exploration

summary(german_credit)


### Exploring numerical varaibles/features

## Duration.in.month 
summary( german_credit$Duration.in.month  )

ggplot( german_credit , aes(x = german_credit$Duration.in.month) ) + geom_histogram()

skewness(german_credit$Duration.in.month)

kurtosis(german_credit$Duration.in.month)

plot(  as.factor(german_credit$Default_status), german_credit$Duration.in.month, ylab = "Duration.in.month",
       xlab = "default status")

## Credit.amount
summary( german_credit$Credit.amount  )

ggplot( german_credit , aes(x = german_credit$Credit.amount ) ) + geom_histogram()

skewness(german_credit$Credit.amount )

kurtosis(german_credit$Credit.amount )

plot(  as.factor(german_credit$Default_status), german_credit$Credit.amount, ylab = "Credit.amount",
       xlab = "default status")


## Installment.rate.in.percentage.of.disposable.income 


summary( german_credit$Installment.rate.in.percentage.of.disposable.income  )

ggplot( german_credit , aes(x = german_credit$Installment.rate.in.percentage.of.disposable.income ) ) + geom_histogram()

skewness(german_credit$Installment.rate.in.percentage.of.disposable.income )

kurtosis(german_credit$Installment.rate.in.percentage.of.disposable.income )

plot(  as.factor(german_credit$Default_status), german_credit$Installment.rate.in.percentage.of.disposable.income,
       ylab = "Installment.rate.in.percentage.of.disposable.income",
       xlab = "default status")



## Present.residence.since


summary( german_credit$Present.residence.since  )

ggplot( german_credit , aes(x = german_credit$Present.residence.since ) ) + geom_histogram()

skewness(german_credit$Present.residence.since )

kurtosis(german_credit$Present.residence.since )

plot(  as.factor(german_credit$Default_status), german_credit$Present.residence.since,
       ylab = "Present.residence.since",
       xlab = "default status")



## Age.in.Years


summary( german_credit$Age.in.Years  )

ggplot( german_credit , aes(x = german_credit$Age.in.Years ) ) + geom_histogram()

skewness(german_credit$Age.in.Years )

kurtosis(german_credit$Age.in.Years )

plot(  as.factor(german_credit$Default_status), german_credit$Age.in.Years,
       ylab = "Age.in.Years",
       xlab = "default status")



## Number.of.existing.credits.at.this.bank.


summary( german_credit$Number.of.existing.credits.at.this.bank.  )

ggplot( german_credit , aes(x = german_credit$Number.of.existing.credits.at.this.bank. ) ) + geom_histogram()

skewness(german_credit$Number.of.existing.credits.at.this.bank. )

kurtosis(german_credit$Number.of.existing.credits.at.this.bank. )

plot(  as.factor(german_credit$Default_status), german_credit$Number.of.existing.credits.at.this.bank.,
       ylab = "Number.of.existing.credits.at.this.bank.",
       xlab = "default status")


## Number.of.people.being.liable.to.provide.maintenance.for.


summary( german_credit$Number.of.people.being.liable.to.provide.maintenance.for.  )

ggplot( german_credit , aes(x = german_credit$Number.of.people.being.liable.to.provide.maintenance.for. ) ) + geom_histogram()

skewness(german_credit$Number.of.people.being.liable.to.provide.maintenance.for. )

kurtosis(german_credit$Number.of.people.being.liable.to.provide.maintenance.for. )


plot(  as.factor(german_credit$Default_status), 
       german_credit$Number.of.people.being.liable.to.provide.maintenance.for.,
       ylab = "Number.of.people.being.liable.to.provide.maintenance.for.",
       xlab = "default status")


## EDA - Categorical varaibles


# Status of existing checking account

dat_Status.of.existing.checking.account <- data.frame(table(german_credit$Status.of.existing.checking.account,
                                                            german_credit$Default_status))

names(dat_Status.of.existing.checking.account) <- c("Status.of.existing.checking.account","Default_status","Count")

ggplot(data=dat_Status.of.existing.checking.account, aes(x=Status.of.existing.checking.account,
                                                         y=Count, fill=Default_status)) + geom_bar(position="fill", stat="identity")


# Credit history

dat_Credit.history <- data.frame(table(german_credit$Credit.history,
                                                            german_credit$Default_status))

names(dat_Credit.history) <- c("Credit.history","Default_status","Count")

ggplot(data=dat_Credit.history, aes(x=Credit.history,y=Count, fill=Default_status)) + geom_bar(position="fill", stat="identity")

# Purpose

dat_Purpose <- data.frame(table(german_credit$Purpose,
                                       german_credit$Default_status))

names(dat_Purpose) <- c("Purpose","Default_status","Count")

ggplot(data=dat_Purpose, aes(x=Purpose,y=Count, fill=Default_status)) + geom_bar(position="fill", stat="identity")

# Savings account/bonds

dat_Savings.account.bonds <- data.frame(table(german_credit$Savings.account.bonds,
                                german_credit$Default_status))

names(dat_Savings.account.bonds) <- c("Savings.account.bonds","Default_status","Count")

ggplot(data=dat_Savings.account.bonds, aes(x=Savings.account.bonds,y=Count, fill=Default_status)) + geom_bar(position="fill", stat="identity")

# Present.employment.since.

dat_Present.employment.since. <- data.frame(table(german_credit$Present.employment.since.,
                                              german_credit$Default_status))

names(dat_Present.employment.since.) <- c("Present.employment.since.","Default_status","Count")

ggplot(data=dat_Present.employment.since., aes(x=Present.employment.since.,y=Count, fill=Default_status)) + geom_bar(position="fill", stat="identity")

# Present.employment.since.

dat_Personal.status.and.sex <- data.frame(table(german_credit$Personal.status.and.sex,
                                                  german_credit$Default_status))

names(dat_Personal.status.and.sex) <- c("Personal.status.and.sex","Default_status","Count")

ggplot(data=dat_Personal.status.and.sex, aes(x=Personal.status.and.sex,y=Count, fill=Default_status)) + geom_bar(position="fill", stat="identity")

# Other.debtors...guarantors

dat_Other.debtors...guarantors <- data.frame(table(german_credit$Other.debtors...guarantors,
                                                german_credit$Default_status))

names(dat_Other.debtors...guarantors) <- c("Other.debtors...guarantors","Default_status","Count")

ggplot(data=dat_Other.debtors...guarantors, aes(x=Other.debtors...guarantors,y=Count, fill=Default_status)) + geom_bar(position="fill", stat="identity")

# Property

dat_Property <- data.frame(table(german_credit$Property,
                                                   german_credit$Default_status))

names(dat_Property) <- c("Property","Default_status","Count")

ggplot(data=dat_Property, aes(x=Property,y=Count, fill=Default_status)) + geom_bar(position="fill", stat="identity")


# Other.installment.plans

dat_Other.installment.plans <- data.frame(table(german_credit$Other.installment.plans,
                                 german_credit$Default_status))

names(dat_Other.installment.plans) <- c("Other.installment.plans","Default_status","Count")

ggplot(data=dat_Other.installment.plans, aes(x=Other.installment.plans,y=Count, fill=Default_status)) + geom_bar(position="fill", stat="identity")


# Housing.

dat_Housing. <- data.frame(table(german_credit$Housing.,
                                                german_credit$Default_status))

names(dat_Housing.) <- c("Housing.","Default_status","Count")

ggplot(data=dat_Housing., aes(x=Housing.,y=Count, fill=Default_status)) + geom_bar(position="fill", stat="identity")

# Job_status

dat_Job_status <- data.frame(table(german_credit$Job_status,
                                 german_credit$Default_status))

names(dat_Job_status) <- c("Job_status","Default_status","Count")

ggplot(data=dat_Job_status, aes(x=Job_status,y=Count, fill=Default_status)) + geom_bar(position="fill", stat="identity")

# Telephone.

dat_Telephone. <- data.frame(table(german_credit$Telephone.,
                                   german_credit$Default_status))

names(dat_Telephone.) <- c("Telephone.","Default_status","Count")

ggplot(data=dat_Telephone., aes(x=Telephone.,y=Count, fill=Default_status)) + geom_bar(position="fill", stat="identity")

# foreign.worker

dat_foreign.worker <- data.frame(table(german_credit$foreign.worker,
                                   german_credit$Default_status))

names(dat_foreign.worker) <- c("foreign.worker","Default_status","Count")

ggplot(data=dat_foreign.worker, aes(x=foreign.worker,y=Count, fill=Default_status)) + geom_bar(position="fill", stat="identity")


##### => Checkpoint 2: Data Cleaning and Transformation

#### no missing values

sum( is.na(german_credit)  )

#### outlier treatment in continuous variables

### duration in month
### no outlier treatment because insignificant number of outliers  , 
### also outliers in month variable exists only in default = 0 , evident from the plot below
### therefore retaining outliers

boxplot.stats( german_credit$Duration.in.month  )$out
plot(  as.factor(german_credit$Default_status), german_credit$Duration.in.month, ylab = "Duration.in.month",
       xlab = "default status")

### Credit.amount

boxplot.stats( german_credit$Credit.amount  )$out
plot(  as.factor(german_credit$Default_status), german_credit$Credit.amount,ylab = "Credit.amount",
       xlab = "default status")

## capping and flooring for outlier treatment
german_credit$Credit.amount[ german_credit$Credit.amount <
                               quantile( german_credit$Credit.amount , probs = 0.05 )  ] <- quantile( german_credit$Credit.amount , probs = 0.05 )

german_credit$Credit.amount[ german_credit$Credit.amount >
                               quantile( german_credit$Credit.amount , probs = 0.95 )  ] <- quantile( german_credit$Credit.amount , probs = 0.95 ) 



###  Installment.rate.in.percentage.of.disposable.income
### no outliers

boxplot.stats( german_credit$Installment.rate.in.percentage.of.disposable.income  )$out
plot(  as.factor(german_credit$Default_status), german_credit$Installment.rate.in.percentage.of.disposable.income,
       ylab = "Credit.amount",
       xlab = "default status")

### Present.residence.since
### no outliers
boxplot.stats( german_credit$Present.residence.since  )$out
plot(  as.factor(german_credit$Default_status), german_credit$Present.residence.since,
       ylab = "Credit.amount",
       xlab = "default status")

### Age.in.Years
### outlier treatment for Age.in.Years is not perfomed 
### because the number of outliers are about 2%

boxplot.stats( german_credit$Age.in.Years  )$out
plot(  as.factor(german_credit$Default_status), german_credit$Age.in.Years,
       ylab = "Credit.amount",
       xlab = "default status")

### Number.of.existing.credits.at.this.bank.
### outlier treatment for Number.of.existing.credits.at.this.bank. is not perfomed 
### because outlier for this variable is relevant
boxplot.stats( german_credit$Number.of.existing.credits.at.this.bank.  )$out
plot(  as.factor(german_credit$Default_status), german_credit$Number.of.existing.credits.at.this.bank.,
       ylab = "Credit.amount",
       xlab = "default status")

### Number.of.people.being.liable.to.provide.maintenance.for.
### outlier treatment for Number.of.people.being.liable.to.provide.maintenance.for. is not perfomed 
### because outlier for this variable is relevant
boxplot.stats( german_credit$Number.of.people.being.liable.to.provide.maintenance.for.  )$out
plot(  as.factor(german_credit$Default_status), 
       german_credit$Number.of.people.being.liable.to.provide.maintenance.for.,
       ylab = "Credit.amount",
       xlab = "default status")


#### categorical data into numerical data

### Status of existing checking account

german_credit$Status.of.existing.checking.account <- as.factor( as.character(german_credit$Status.of.existing.checking.account))

Status.of.existing.checking.account_dummy <- as.data.frame(  model.matrix(~ Status.of.existing.checking.account -1, 
                                                   data= german_credit) ) 

german_credit <- cbind( german_credit  , Status.of.existing.checking.account_dummy[,-4]  )

### Credit history

german_credit$Credit.history <- as.factor( as.character(german_credit$Credit.history))

Credit.history_dummy <- as.data.frame(  model.matrix(~ Credit.history -1, 
                                                                          data= german_credit) ) 

german_credit <- cbind( german_credit  , Credit.history_dummy[,-5]  )

### Purpose


german_credit$Purpose <- as.factor( as.character(german_credit$Purpose))

Purpose_dummy <- as.data.frame(  model.matrix(~ Purpose -1, 
                                                     data= german_credit) ) 

german_credit <- cbind( german_credit  , Purpose_dummy[,-10]  )


### Savings account/bonds


german_credit$Savings.account.bonds <- as.factor( as.character(german_credit$Savings.account.bonds))

Savings.account.bonds_dummy <- as.data.frame(  model.matrix(~ Savings.account.bonds -1, 
                                              data= german_credit) ) 

german_credit <- cbind( german_credit  , Savings.account.bonds_dummy[,-5]  )

### Present employment since


german_credit$Present.employment.since. <- as.factor( as.character(german_credit$Present.employment.since.))

Present.employment.since._dummy <- as.data.frame(  model.matrix(~ Present.employment.since. -1, 
                                                            data= german_credit) ) 

german_credit <- cbind( german_credit  , Present.employment.since._dummy[,-5]  )

### Personal status and sex


german_credit$Personal.status.and.sex <- as.factor( as.character(german_credit$Personal.status.and.sex))

Personal.status.and.sex_dummy <- as.data.frame(  model.matrix(~ Personal.status.and.sex -1, 
                                                                data= german_credit) ) 

german_credit <- cbind( german_credit  , Personal.status.and.sex_dummy[,-4]  )

### Other debtors / guarantors


german_credit$Other.debtors...guarantors <- as.factor( as.character(german_credit$Other.debtors...guarantors))

Other.debtors...guarantors_dummy <- as.data.frame(  model.matrix(~ Other.debtors...guarantors -1, 
                                                              data= german_credit) ) 

german_credit <- cbind( german_credit  , Other.debtors...guarantors_dummy[,-3]  )


### Other debtors / guarantors


german_credit$Property <- as.factor( as.character(german_credit$Property))

Property_dummy <- as.data.frame(  model.matrix(~ Property -1, data= german_credit) ) 

german_credit <- cbind( german_credit  , Property_dummy[,-4]  )

### Property


german_credit$Property <- as.factor( as.character(german_credit$Property))

Property_dummy <- as.data.frame(  model.matrix(~ Property -1, data= german_credit) ) 

german_credit <- cbind( german_credit  , Property_dummy[,-4]  )


### Other installment plans 


german_credit$Other.installment.plans <- as.factor( as.character(german_credit$Other.installment.plans))

Other.installment.plans_dummy <- as.data.frame(  model.matrix(~ Other.installment.plans -1, data= german_credit) ) 

german_credit <- cbind( german_credit  , Other.installment.plans_dummy[,-3]  )


### Housing


german_credit$Housing. <- as.factor( as.character(german_credit$Housing.))

Housing._dummy <- as.data.frame(  model.matrix(~ Housing. -1, data= german_credit) ) 

german_credit <- cbind( german_credit  , Housing._dummy[,-3]  )


### JOB
german_credit$Job_status <- as.factor( as.character(german_credit$Job_status))

Job_status_dummy <- as.data.frame(  model.matrix(~ Job_status -1, data= german_credit) ) 

german_credit <- cbind( german_credit  , Job_status_dummy[,-4]  )

### Telephone
german_credit$Telephone. <- as.factor( as.character(german_credit$Telephone.))

Telephone._dummy <- as.data.frame(  model.matrix(~ Telephone. -1, data= german_credit) ) 

german_credit <- cbind( german_credit  , Telephone._dummy[,-2] )


### foreign worker
german_credit$foreign.worker <- as.factor( as.character(german_credit$foreign.worker))

foreign.worker_dummy <- as.data.frame(  model.matrix(~ foreign.worker -1, data= german_credit) ) 

german_credit <- cbind( german_credit  , foreign.worker_dummy[,-2]  )

### Removing source categorical variable after conversion into numerical 

german_credit <- german_credit[ ,-c(1,3,4,6,7,9,10,12,14,15,17,19,20) ]



## Spliting data into train and test data

set.seed(100)

train_data_sample_size <-  ( 0.7 * nrow(german_credit)  )

train_data_index <- sample(seq_len(nrow(german_credit)), size = train_data_sample_size)

train_german_credit <- german_credit[ train_data_index ,]

test_german_credit <- german_credit[ -train_data_index , ]




## Building models

german_credit_initial_model = glm(Default_status ~ ., data = train_german_credit, family = "binomial")


summary(german_credit_initial_model)
vif(german_credit_initial_model)

# Stepwise selection of variables
german_credit_model_after_step_AIC = step(german_credit_initial_model,direction = "both")

summary(german_credit_model_after_step_AIC)

vif(german_credit_model_after_step_AIC) 


# model_1 with PropertyA121 excluded becuase of its relatively low significance

model_1 =   glm(
                  Default_status ~ Duration.in.month + Credit.amount + Installment.rate.in.percentage.of.disposable.income + 
                    Status.of.existing.checking.accountA11 + Status.of.existing.checking.accountA12 + 
                    Credit.historyA30 + Credit.historyA31 + Credit.historyA32 + 
                    Credit.historyA33 + PurposeA40 + PurposeA46 + Savings.account.bondsA61 + 
                    Present.employment.since.A74 + Personal.status.and.sexA93 + 
                    Other.debtors...guarantorsA101 + Other.debtors...guarantorsA102 + 
                    Other.installment.plansA141 + `Telephone._dummy[, -2]`
  
                    , data = train_german_credit, family = "binomial") 

# checking deviance , significance stats of model_1
summary(model_1)
# checking VIF compliance for model_1
vif(model_1) 

# model_2 with `Telephone._dummy[, -2]` excluded becuase of its relatively low significance

model_2 =   glm(
  Default_status ~ Duration.in.month + Credit.amount + Installment.rate.in.percentage.of.disposable.income + 
    Status.of.existing.checking.accountA11 + Status.of.existing.checking.accountA12 + 
    Credit.historyA30 + Credit.historyA31 + Credit.historyA32 + 
    Credit.historyA33 + PurposeA40 + PurposeA46 + Savings.account.bondsA61 + 
    Present.employment.since.A74 + Personal.status.and.sexA93 + 
    Other.debtors...guarantorsA101 + Other.debtors...guarantorsA102 + 
    Other.installment.plansA141 
  , data = train_german_credit, family = "binomial") 

# checking deviance , significance stats of model_2
summary(model_2)
# checking VIF compliance for model_2
vif(model_2) 


# model_3 with Other.installment.plansA141  excluded becuase of its relatively low significance

model_3 =   glm(
  Default_status ~ Duration.in.month + Credit.amount + Installment.rate.in.percentage.of.disposable.income + 
    Status.of.existing.checking.accountA11 + Status.of.existing.checking.accountA12 + 
    Credit.historyA30 + Credit.historyA31 + Credit.historyA32 + 
    Credit.historyA33 + PurposeA40 + PurposeA46 + Savings.account.bondsA61 + 
    Present.employment.since.A74 + Personal.status.and.sexA93 + 
    Other.debtors...guarantorsA101 + Other.debtors...guarantorsA102 
  , data = train_german_credit, family = "binomial") 

# checking deviance , significance stats of model_3
summary(model_3)
# checking VIF compliance for model_3
vif(model_3) 


#### nominating model_2 as final model for further model evaluation process

final_model = model_3

##### => Checkpoint 5: Model Evaluation

##### C - statistic 

# C- statistics for Training data = 82
train_german_credit$predict_prob <- predict(final_model , type="response")


rcorr.cens(  train_german_credit$predict_prob , train_german_credit$Default_status )

# C- statistics for Testing data = 78
test_german_credit$predict_prob <- predict(final_model , newdata = test_german_credit , type="response")


rcorr.cens(  test_german_credit$predict_prob , test_german_credit$Default_status )


##### K - statistic 

## K - statistics for training data [ Fourth decile ,  KS-value - 52 ]
K_Model_score_train <- prediction( train_german_credit$predict_prob , train_german_credit$Default_status  )

model_perf_train <- performance( K_Model_score_train  , "tpr","fpr")

ks_table_train <- attr(model_perf_train, "y.values")[[1]] - (attr(model_perf_train, "x.values")[[1]])

ks_train = max(ks_table_train)

train_data_decile_nos <- round( which(ks_table_train == ks_train)/ nrow( data.frame(ks_table_train)) * 10 )

## K - statistics for testing data [ Fourth decile ,  KS-value - 49 ]
K_Model_score_test <- prediction( test_german_credit$predict_prob , test_german_credit$Default_status  )

model_perf_test <- performance( K_Model_score_test  , "tpr","fpr")

ks_table_test <- attr(model_perf_test, "y.values")[[1]] - (attr(model_perf_test, "x.values")[[1]])

ks_test = max(ks_table_test)

test_data_decile_nos <- round( which(ks_table_test == ks_test)/ nrow( data.frame(ks_table_test)) * 10 )


##### => Checkpoint 6: Threshold value

## ROC curve
plot(model_perf_train,col = "red", lab = c(10,10,10))

##confusion matrix

## Accuracy = 0.7571, Specificity =  0.7712   , sensitivity = 0.7281 for training data
confusionMatrix(as.numeric(train_german_credit$predict_prob > 0.35),train_german_credit$Default_status
                ,positive = "1")


## Accuracy = 0.7533 , Specificity = 0.7588, sensitivity = 0.7361 for testing data
confusionMatrix(as.numeric(test_german_credit$predict_prob > 0.35),test_german_credit$Default_status, 
                positive = "1")


