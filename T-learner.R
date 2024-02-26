library(nnet)
library(ranger)
library(glmnet)
library(nnls)
library(SuperLearner)
library(methods)
library(tidyverse)
library(caret)
library(xgboost)
library(haven)
vec.pac= c("SuperLearner", "gbm", "glmnet","ranger","nnet","caret")
lapply(vec.pac, require, character.only = TRUE) 

#Importing data
dataset_postreat_1_ <- read_dta("dataset_postreat (1).dta")
#Replacing NA with means (because of Superlerner not working with missing data)
dataset_postreat_1_<- dataset_postreat_1_ %>% mutate_all(~ifelse(is.na(.), mean(., na.rm = TRUE), .))


#Learner Library:
learners <- c( "SL.glmnet","SL.xgboost", "SL.ranger","SL.nnet")

#CV Control for the SuperLearner
control <- SuperLearner.CV.control(V=5)



#Setting the output and the treatment variables
names(dataset_postreat_1_)[names(dataset_postreat_1_) == "index0_l"] <- "y"
names(dataset_postreat_1_)[names(dataset_postreat_1_) == "Tt"] <- "d"
#Setting the covariates
covariates_= c("employee_total", "meal_yes", "profits_pre","years_vending","years_kiosk")

T_learner <- function(data,covariates,learners){

  data$ID <- c(1:nrow(data))
  
 score_T <- matrix(0,nrow(data),1)

 set.seed(1234)
folds <- createFolds(data$d,k=5)


for(f in 1:(length(folds))){
  
  if(f == 1){
    data1 <- data[c(folds[[5]],folds[[2]],folds[[3]],folds[[4]]),]
    df_main <- data[folds[[1]],]
  } 
  if(f == 2){
    data1 <- data[c(folds[[1]],folds[[5]],folds[[3]],folds[[4]]),]
    df_main <- data[folds[[2]],]
  } 
  
  if(f == 3){
    data1 <- data[c(folds[[1]],folds[[2]],folds[[5]],folds[[4]]),]
    df_main <- data[folds[[3]],]
  } 
  
  if(f == 4){
    data1 <- data[c(folds[[1]],folds[[2]],folds[[3]],folds[[5]]),]
    df_main <- data[folds[[4]],]
  } 
  
  if(f == 5){
    data1 <- data[c(folds[[1]],folds[[2]],folds[[3]],folds[[4]]),]
    df_main <- data[folds[[5]],]
  } 
  
  df_aux <- data1

  # Split the training data into treatment and control observations
  aux_1 <- df_aux[which(df_aux$d==1),]
  aux_0 <- df_aux[which(df_aux$d==0),]
  
  # Train a regression model for the treatment observations
  m1_mod <- SuperLearner(Y = aux_1$y, X = aux_1[,covariates], newX = df_main[,covariates], SL.library = learners,
                         verbose = FALSE, method = "method.NNLS",cvControl = control)
  
  m1_hat <- m1_mod$SL.predict
  
  # Train a regression model for the control observations
  m0_mod <- SuperLearner(Y = aux_0$y, X = aux_0[,covariates], newX = df_main[,covariates], SL.library = learners,
                         verbose = FALSE, method = "method.NNLS",cvControl = control)
  
  m0_hat <- m0_mod$SL.predict
  
  # Estimate the CATE as the difference between the two models
  score_T[,1][df_main$ID] = predict(m1_mod,df_main[,covariates])$pred - predict(m0_mod,df_main[,covariates])$pred
  
  }
  
  return(score_T)
}

CATE_Tlearner=T_learner(data=dataset_postreat_1_, covariates = covariates_,learners=learners)
