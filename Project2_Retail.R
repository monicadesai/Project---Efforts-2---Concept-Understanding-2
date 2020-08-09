# Project 2 - Retail
# Language Used - R 
# Model Used - RandomForest, GBM, Stacking
# Target Variable - 'Store'
# Aim - AUC Score which should be greater than o.80




sink("Project2_Retail_Output.txt",split = TRUE) #To store output
#You can remove option split if you want output only in .txt file and not in console, writing split option gives output in both that is in 'console screen' and '.txt file'

#If you want you can copy commands from history tab on right side and then click on source option on right side which is next to run option
#or you can select entire code and put it to run option

#takes long time to run these project(It took 9hours just to execute code and get output)
# get the working directory --------------------------------------------------------------------------------------------------------------------
getwd()

## set the working directory where to work and your data exists
setwd("C:\\Users\\Monica\\Desktop\\Projects\\R Projects\\Project2\\")

## Data loading phase ---------------------------------------------------------------------------------------------------------------------------
## load the train and test data
train_data = read.csv("store_train.csv", stringsAsFactors = F)
test_data = read.csv("store_test.csv",stringsAsFactors = F)


## Data Preparation phase -----------------------------------------------------------------------------------------------------------------------
## combine data for data preparation
test_data$store = NA


# put place holder for train and test data for identification
train_data$data = 'train'
test_data$data = 'test'

# combining train and test data
All_data = rbind(train_data,test_data)


# loading library dplyer for data preparation
library(dplyr)

# view the data
glimpse(All_data)


## check all the column conating NA values
for(col in names(All_data)){
  if(sum(is.na(All_data[,col]))>0 & !(col %in% c("data","store")))
    print(col)
}


## distribution of columns containing NA values looks like
hist(x = All_data$country)
hist(x = All_data$population)

## replace the NA values
All_data[is.na(All_data[,"country"]),"country"]=median(All_data[,"country"],na.rm=T)
All_data[is.na(All_data[,"population"]),"population"]=mean(All_data[,"population"],na.rm=T)
str(All_data)
## finding names of the column which are of character type
char_logical=sapply(All_data, is.character)
cat_cols=names(All_data)[char_logical]
cat_cols

## finding names of the column which are of numeric categorical type 
char_logical1=sapply(All_data, is.numeric)
cat_cols1=names(All_data)[char_logical1]
cat_cols1
## taking only those columns which are categorical from cat_cols and cat_cols1
cat_cols = cat_cols[c(-2,-7)]
cat_cols1 = cat_cols1[c(7,8)]

## function for creating dummies ---------------------------------------------------------------------------------------------------------
CreateDummies=function(data,var,freq_cutoff=100){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    name=gsub("/","_",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  data[,var]=NULL
  return(data)
}

## creating dummy variable for all the categorical columns of character types
for(col in cat_cols){
  All_data=CreateDummies(All_data,col,1)
}

## creating dummy variable for all the categorical columns of numeric types
for(col in cat_cols1){
  All_data=CreateDummies(All_data,col,1)
}

## Separation of data into train and test
train_data=All_data %>% filter(data=='train') %>% select(-data)
test_data= All_data %>% filter(data=='test') %>% select(-data,-store)

## separate train and test data from train_data
v= sample(nrow(train_data), 0.80 * (nrow(train_data)))
training_data = train_data[v,]
testing_data = train_data[-v,]

## model making phase starts -----------------------------------------------------------------------------------------------------------------
## making linear model -----------------------------------------------------------------------------------------------------------------------
## log.fit= glm(factor(store)~.-Id-storecode,data=training_data, family = 'binomial')
## summary(log.fit)
## this model produce warnings which are given below and also some NA values---------
##Warning messages:
##  1: glm.fit: algorithm did not converge 
##2: glm.fit: fitted probabilities numerically 0 or 1 occurred

## this formula is used to finding aliased coefficents -------------------------------------------------------------------------------------------
##ld.vars <- attributes(alias(log.fit)$Complete)$dimnames[[1]]
##ld.vars
## As ld.vars gives all the alaised coefficient and they are so many such that which cannot be shown in this code---------------------------------
## as removing these coefficent take a lot of time and model also not become so much predictive so we leave it -----------------------------------
## we remove those variable which have high vif valu given by vif() function ---------------------------------------------------------------------
## I have left the linear model or logistic model as it is not providing significant results much extent ----------------------------------------
## if i want than i can do it by removing variable who have high vif than 10 and also aliased performance and gives auc score ~ 0.74 ------------


## Removing unwanted variables which cause problem in randomForest model from training_data--------------------------------------------------------
training_data$'Areaname_CumberlandCountyME(part)HUDMetroFMRArea'=NULL
training_data$'Areaname_HillsboroughCountyNH(part)HUDMetroFMRArea'=NULL
training_data$'Areaname_YorkCountyME(part)HUDMetroFMRArea'=NULL
training_data$'Areaname_BerkshireCountyMA(part)HUDMetroFMRArea'=NULL
training_data$'Areaname_FranklinCountyMA(part)HUDMetroFMRArea'=NULL
training_data$'Areaname_PenobscotCountyME(part)HUDMetroFMRArea'=NULL

## Removing unwanted variables which cause problem in randomForest model from train_data--------------------------------------------------------
train_data$'Areaname_CumberlandCountyME(part)HUDMetroFMRArea'=NULL
train_data$'Areaname_HillsboroughCountyNH(part)HUDMetroFMRArea'=NULL
train_data$'Areaname_YorkCountyME(part)HUDMetroFMRArea'=NULL
train_data$'Areaname_BerkshireCountyMA(part)HUDMetroFMRArea'=NULL
train_data$'Areaname_FranklinCountyMA(part)HUDMetroFMRArea'=NULL
train_data$'Areaname_PenobscotCountyME(part)HUDMetroFMRArea'=NULL

## build random forest model on it ------------------------------------------------------------------------------------------------------------
install.packages('randomForest')
library(randomForest)
rf.model = randomForest(factor(store)~.-Id-storecode ,data=training_data,do.trace = T)

## predicting the score on testing_data --------------------------------------------------------------------------------------------------------
predict.score=predict(rf.model,newdata = testing_data,type='prob')[,2]

## obtaining auc on testing_data ---------------------------------------------------------------------------------------------------------------
install.packages('ROCR')
library(ROCR)
install.packages('pROC')
library(pROC)
auc(roc(as.numeric(testing_data$store),as.numeric(predict.score)))
## auc score~ 0.813 for randomForest in this case
## random forest provides better score but lets try gbm it may make model better----------------------------------------------------------------

## building gbm model --------------------------------------------------------------------------------------------------------------------------
install.packages('gbm')
library(gbm)
gbm.model=gbm(factor(store)~.-Id-storecode,data=training_data,distribution = "gaussian",n.trees = 700)
## predicting the score on testing_data --------------------------------------------------------------------------------------------------------
predict.score1=predict(gbm.model,newdata = testing_data,type='response',n.trees = 700)
## obtaining auc on testing_data ---------------------------------------------------------------------------------------------------------------
auc(roc(as.numeric(testing_data$store),as.numeric(predict.score1)))
## auc score ~ 0.7449 for gbm in this case 
### gbm model provides much less better score than random forest and we have to find the best score on test data-------------------------------
## lets try stacking --------------------------------------------------------------------------------------------------------------------------


## lets make a function which makes n-folds of data for cross-validation-----------------------------------------------------------------------
#set.seed(1234)
install.packages('lattice')
library(lattice)
install.packages('robustbase')
library(robustbase)
install.packages('cvTools')
library(cvTools)
mykfolds=function(nobs,nfold=5){
  
  t=cvFolds(nobs,K=nfold,type='random')
  
  folds=list()
  
  for(i in 1:nfold){
    
    test=t$subsets[t$which==i]
    train=t$subsets[t$which!=i]
    
    folds[[i]]=list('train'=train,'test'=test)
  }
  
  return(folds)
}
## creating folds-----------------------------------------------------------------------------------------------------------------------------

myfolds=mykfolds(nrow(train_data),10)

## making data prepare for sttaking
store_train_layer=data.frame(rf_var=numeric(nrow(train_data)),
                             gbm_var=numeric(nrow(train_data)))

## making a loop for cross validation --------------------------------------------------------------------------------------------------------
library(dplyr)

install.packages('ROCR')
library(ROCR)

install.packages('stats')
library(stats)

install.packages(pROC)
library(pROC)

library(randomForest)
for(i in 1:10){
  print(c(i))
  fold=myfolds[[i]]
  
  training_data1=train_data[fold$train,]
  # for this iteration model will be built on this chunk of the data
  testing_data1=train_data[fold$test,]
  # predicitons will be made on this chunk which is not being
  # used in the modeling process
  
  print('rf')
  
  
  rf.fit=randomForest(factor(store)~.-Id-storecode,
                      mtry=25,
                      ntree=500,
                      maxnodes=100,
                      data=training_data1
  )
  
  
  rf_score=predict(rf.fit,newdata=testing_data1,type='prob')[,2]
  z=auc(roc(as.numeric(testing_data1$store),as.numeric(rf_score)))
  print(z)
  
  print('gbm')
  install.packages('gbm')
  library(gbm)
  
  gbm.fit=gbm(store~.-Id-storecode,data=training_data1,
              interaction.depth=7,
              n.trees=700,
              shrinkage=0.01,
              n.minobsinnode=5,
              distribution = "bernoulli")
  
  
  gbm_score=predict(gbm.fit,newdata=testing_data1,
                    n.trees=700,type='response')
  
  library(ROCR)
  library(pROC)
  
  k=auc(roc(as.numeric(testing_data1$store),as.numeric(gbm_score)))
  print(k)
  store_train_layer$rf_var[fold$test]=rf_score
  
  store_train_layer$gbm_var[fold$test]=gbm_score
}

## Preparing data ----------------------------------------------------------------------------------------------------------------------------
store_test_layer=data.frame(rf_var=numeric(nrow(test_data)),
                            gbm_var=numeric(nrow(test_data)))

## making randomforest model on whole train_data
full.rf=randomForest(factor(store)~.-Id-storecode,
                     mtry=25,
                     ntree=500,
                     maxnodes=100,
                     
                     data=train_data
)

## making gbm model on whole train_data -----------------------------------------------------------------------------------------------------
full.gbm=gbm(store~.-Id-storecode,data=train_data,
             interaction.depth=7,
             n.trees=700,
             shrinkage=0.01,
             n.minobsinnode=5,
             distribution = "bernoulli")

## Predicting score of these model on test_data which is used for making prediction on test_data in final model ----------------------------
store_test_layer$rf_var=predict(full.rf,newdata=test_data,type='prob')[,2]
store_test_layer$gbm_var=predict(full.gbm,newdata=test_data,n.trees=700,type='response')

## Preparing Data -----------------------------------------------------------------------------------------------------------------------------
store_train_layer$store=train_data$store

## building a linear model using the two models i.e. gbm and randomForest models as variables---------------------------------------------------
log.mod=glm(store~.,data=store_train_layer,family = "binomial")
## This is the best model from all we get so we choose this model to predict on final test data------------------------------------------------
## predicting the score on test_data -----------------------------------------------------------------------------------------------------------
test.score=predict(log.mod,newdata=store_test_layer,type='response')
## Its auc score ~ 0.8776 so it is the best model from all above--------------------------------------------------------------------------------


## writing the solution into csv file-----------------------------------------------------------------------------------------------------------
write.csv(test.score,"Monica_Desai_P2_part2.csv",row.names = F)

sink() #To close output in .txt file