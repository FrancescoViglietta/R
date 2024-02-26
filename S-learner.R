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




#Setting the covariates
covariates_= c("employee_total", "meal_yes", "profits_pre","years_vending","years_kiosk")


#setting the output and the treatment variables
names(dataset_postreat_1_)[names(dataset_postreat_1_) == "index0_l"] <- "y"
names(dataset_postreat_1_)[names(dataset_postreat_1_) == "Tt"] <- "d"




S_learner<- function(data, covariates, learners){
  
  data$ID <- c(1:nrow(data))
  score_S  <- matrix(0,nrow(data),1)
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
    
    
  X_train <-data.frame(cbind(df_aux[,covariates],df_aux$d)) 
    
    # Set treatment variable to 0
    X_test_0 <- data.frame(df_main[,covariates], d = 0)
     #Forced to apply this because of this error "Error in predict.xgb.Booster(object$object, newdata = newdata) : Feature names stored in `object` and `newdata` are different!"
    colnames(X_test_0) <- colnames(X_train)
    
    # Set treatment variable to 1
    X_test_1  <- data.frame(df_main[,covariates], d = 1)
    
    colnames(X_test_1) <- colnames(X_train)
    
    
    
    
    # Train a regression model using the covariates and the treatment variable
    m_mod <- SuperLearner(Y = df_aux$y, X = X_train,SL.library = learners,
                          verbose = FALSE, method = "method.NNLS",cvControl = control)
    
    
    
    # Estimate the CATE as the difference between the model with different treatment status
    score_S[,1][df_main$ID]<- predict(m_mod, X_test_1)$pred - predict(m_mod, X_test_0)$pred
    
    #cate_estimates<- predict(m_mod, X_test_1)$pred - predict(m_mod, X_test_0)$pred
    #score_S <- c(score_S, cate_estimates)
    
}
  
  
  
  return(score_S)
}


CATE_Slearner<- S_learner(data = dataset_postreat_1_, covariates = covariates_, learners=learners)

