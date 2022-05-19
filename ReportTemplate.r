library(fastDummies)
library(reshape2)
library(dplyr)
library(tibble)
library(ggplot2)
library(tidyr)
library(randomForest)


set.seed(10)

test_set=read.csv("test_set_features.csv",header=TRUE)                
training_label=read.csv("training_set_labels.csv",header=TRUE)
training_feature=read.csv("training_set_features.csv",header=TRUE)  

submission=read.csv("submission_format.csv",header = TRUE)

feature_df<-dplyr::tibble(training_feature)
labels_df<-dplyr::tibble(training_label)
training_df<-dplyr::inner_join(feature_df,labels_df)

dim(training_df)
#summary(training_df)


category_list=c()
not_category=c()
for (feature in colnames(training_df)){
  if (typeof(training_df[feature][[1]])=="character"){
    category_list<-c(category_list,feature)
  }
  else{
    not_category<-c(not_category, feature)  
  }    
}

not_category <- not_category[not_category!="respondent_id"]
all_features=c(category_list, not_category)

dummy_df <- training_df

#all_features


#replace empty strings by NA

for (col in category_list){
  dummy_df[,col][dummy_df[,col] == ""] <- NA
}

Mode <- function(x){
  #convert NA to most common value
  distinct_value <- unique(x[!is.na(x)])
  
  distinct_value[which.max(tabulate(match(x, distinct_value)))]
}





#convert categorical to numerical
for (c in category_list){
  
  dummy_df[[c]] <- unclass(factor(dummy_df[[c]]))
}

for (col in category_list){
  dummy_df[,col][is.na(dummy_df[,col])] <- Mode(dummy_df[,col])
}



head(dummy_df)



df <- sapply(dummy_df[,all_features], as.numeric)

#Correlation matrix with Pearson method
cormat <- round(cor(df, method='pearson', use='complete.obs'),2)


#Put Correaltion matrix in a form suitable to build heatmap
melted_cormat <- melt(cormat)


summary(df)

#normalizing the data
normalized_df <- df

replace_na_with_mean_value<-function(vec) {
  mean_vec<-mean(vec,na.rm=T)
  vec[is.na(vec)]<-mean_vec
  vec
}

for (col in not_category){
  normalized_df[ , col][is.na(normalized_df[ , col])] <- mean(normalized_df[ , col], na.rm = TRUE)
}


for (col in all_features){
  norm_factor=max(normalized_df[,col])
  normalized_df[,col]<-round((normalized_df[,col])/norm_factor, digits=3)
  
}




#mrmr filter

MRMR <- function(target_variable) {

  training_set <- read.csv("normalized_data.csv")
  
  n <- ncol(training_set)
  if (target_variable == "h1n1_vaccine"){
    n_var <- n-3
  }
  else{
    n_var <- n-2
  }
    
  input_variables <- training_set[,1:n_var]
  
  output_variable <- training_set[,target_variable]
  
  correlation<-abs(cor(input_variables,output_variable))
  selected<-c()
  candidates<-1:n_var
  
  for (j in 1:n_var) {
    redundancy_score<-numeric(length(candidates))
    
    if (length(selected)>0) {
      # Compute the correlation between the selected variables and the candidates on the training set
      cor_selected_candidates<-cor(training_set[,selected,drop=F],training_set[,candidates,drop=F])
      # Compute the mean correlation for each candidate variable, across the selected variables
      redundancy_score<-apply(cor_selected_candidates,2,mean)
    }
    
    # mRMR: minimum Redundancy Maximum Relevancy
    mRMR_score<-correlation[candidates]-redundancy_score
    
    
    # Select the candidate variable that maximises the mRMR score
    selected_current<-candidates[which.max(mRMR_score)]
    selected<-c(selected,selected_current)
    # Remove the selected variables from the candidates
    candidates<-setdiff(candidates,selected_current)
    
    
  }
  
  return(features <- all_features[selected])

}


features <- MRMR("seasonal_vaccine")

features


RandomForest <- function(target_variable){
  training_set <- read.csv("normalized_data.csv")
  
  n_trees <- c(200,300,400,500,600,700,800)
  
  results <- matrix(,nrow=length(features)-5, ncol=length(n_trees))
  
  if (target_variable == "h1n1_vaccine"){
    target_variable <- ncol(training_set)-2
  }
  else{
    target_variable <- ncol(training_set)-1
    
  }
  spam_idx <- sample(1:nrow(training_set))
  half_split <- floor(nrow(training_set)/2)
  
  
  for (j in 5:length(features)){
  
    
    accuracy_vec <- array(0,length(n_trees))
    
    for (i in 1:length(n_trees)){ 
      train_data <- training_set[spam_idx[1:half_split],]
      
      test_data <- training_set[spam_idx[(half_split+1):nrow(training_set)],]
      
      model <- randomForest(x=train_data[,features[1:j]],
                            y=as.factor(train_data[,c(target_variable)]),
                            xtest=test_data[,features[1:j]],
                            ytest=as.factor(test_data[,c(target_variable)]),
                            ntree=n_trees[i])
      
      accuracy_vec[i] = (model$test$confusion[1,1]+model$test$confusion[2,2])/sum(model$test$confusion)
    }
    results[j-4,] <- accuracy_vec
  }

  return(results)
}




install.packages("xgboost")

library(xgboost)


training_set <- read.csv("normalized_data.csv")



traini <- training_set[-c(ncol(training_set)-1)]
traini <- traini[2:ncol(traini)]

pred_set <- read.csv("normalized_testset.csv")
pred_set <- pred_set[2:ncol(pred_set)]


unique(traini$health_insurance)
unique(traini$employment_industry)
unique(traini$employment_occupation)


unique(pred_set$health_insurance)
unique(pred_set$employment_industry)
unique(pred_set$employment_occupation)
summary(traini)
pred_set

traini$h1n1_vaccine <- as.factor(traini$h1n1_vaccine)

data <- data.matrix(traini[,features])

label <- data.matrix(traini$h1n1_vaccine)


bst <- xgboost(data = data, label = label, max_depth = 2, eta = 0.3,
               nrounds = 2, objective = "binary:logistic")

pred_mat <- data.matrix(pred_set[,features])

features

pred <- predict(bst, pred_mat, type="prob")


plot(pred)



label2 <- data.matrix(traini$seasonal_vaccine)


bst2 <- xgboost(data = data, label = label2, max_depth = 2, eta = 0.8,
               nrounds = 2, objective = "binary:logistic")


pred2 <- predict(bst2, pred_mat, type="prob")



plot(pred2)



"

target_variable <- ncol(traini)
results

model <- randomForest(x=train_data[,features],
                      y=as.factor(train_data[,c(target_variable)]),
                      xtest=test_data[,features],
                      ytest=as.factor(test_data[,c(target_variable)]),
                      ntree=600)
model

training_set <- read.csv("normalized_data.csv")



traini <- training_set[-c(ncol(training_set))]
traini <- traini[2:ncol(traini)]

pred_set <- read.csv("normalized_testset.csv")
pred_set <- pred_set[2:ncol(pred_set)]


unique(traini$health_insurance)
unique(traini$employment_industry)
unique(traini$employment_occupation)


unique(pred_set$health_insurance)
unique(pred_set$employment_industry)
unique(pred_set$employment_occupation)
traini
pred_set

traini$seasonal_vaccine <- as.factor(traini$h1n1_vaccine)

model <- randomForest(h1n1_vaccine~., data=traini, ntree=600)

model

pred <- predict(model, pred_set, type="prob")
pred


write.csv(pred, "result.csv")



testset <- read.csv("test_set_features.csv")

h1n1_res <- read.csv("result.csv")

seas_res <- read.csv("results_seasonal.csv")


submit <- data.frame(respondent_id=c(test_set[,"respondent_id"]), h1n1_vaccine=c(h1n1_res[3]),seasonal_vaccine=c(seas_res[3]))

submit


write.csv(submit, "submission.csv", row.names=FALSE)
"







