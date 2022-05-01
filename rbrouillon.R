library(fastDummies)
library(reshape2)
library(dplyr)
library(tibble)
library(ggplot2)
setwd("/home/lucas/Documents/Big_Data/INFO-F-422/Project/")
test_set=read.csv("test_set_features.csv",header=TRUE)                
training_label=read.csv("training_set_labels.csv",header=TRUE)
training_feature=read.csv("training_set_features.csv",header=TRUE)  

submission=read.csv("submission_format.csv",header = TRUE)

feature_df<-dplyr::tibble(training_feature)
labels_df<-dplyr::tibble(training_label)
training_df<-dplyr::inner_join(feature_df,labels_df)

#checking missing values and variance of each feature

for (feature in colnames(training_df)){
  print(unique(training_df[feature]))
}
print(unique(training_df[feature]))

for (feature in colnames(feature_df)){
  cov(training_df[c(feature,'h1n1_vaccine','seasonal_vaccine')])
}
#Preprocessing of the data -> converting categorical to binary and converting NaN value to mean



for (feature in colnames(training_df)){
  print(feature)
  print(typeof(training_df[feature][[1]]))
}
category_list=c()
for (feature in colnames(training_df)){
  if (typeof(training_df[feature][[1]])=="character"){
    category_list<-c(category_list,feature)
  }
}
category_list
dummy_df<-fastDummies::dummy_cols(training_df)
no_category_df<-select(dummy_df,!category_list)
#function taken from the TP of last year :
replace_na_with_mean_value<-function(vec) {
  mean_vec<-mean(vec,na.rm=T)
  vec[is.na(vec)]<-mean_vec
  vec
}
df_preprocessed<-data.frame(apply(no_category_df,2,replace_na_with_mean_value))
for (feature in colnames(df_preprocessed)){
  print(unique(df_preprocessed[feature]))
}
df_preprocessed
cormat <- round(cor(df_preprocessed, method='pearson'),2)

melted_cormat <- melt(cormat)
pearson_heatmap<-ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()
pearson_heatmap
high_correl_variable<-dplyr::filter(melted_cormat,abs(value)>0.8)
high_correl_variable<-dplyr::filter(high_correl_variable,Var1!=Var2)
high_correl_variable
# When two feature are highly correlated we could keep only one of the two <- only old character field are concerned 
droped_variable<-c('education_','sex_Female','marital_status_','marital_status_Not.Married','rent_or_own_Own','employment_occupation_','employment_occupation_dcjcmpih','employment_industry_','employment_status_Employed')
first_selection_df<-dplyr::select(df_preprocessed,-droped_variable)

cormat <- round(cor(first_selection_df, method='pearson'),2)
melted_cormat <- melt(cormat)
pearson_heatmap<-ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()
pearson_heatmap

#LDA -> NN
#I will use LDA on the unlabelled data
first_selection_df<-dplyr::select(first_selection_df,-c('h1n1_vaccine','seasonal_vaccine'))
#need to normalize the data
for (feature in colnames(first_selection_df)){
  if (feature!='respondent_id'){
  norm_factor=max(first_selection_df[feature])
  first_selection_df[feature]<-(first_selection_df[feature])/norm_factor
  }
}
first_selection_df
mean_vector<-apply(dplyr::select(first_selection_df,-'respondent_id'),2,mean)
mean_vector
centered_df<-sweep(dplyr::select(first_selection_df,-'respondent_id'),MARGIN = 2,STATS = mean_vector,FUN = '-')
centered_df['respondent_id']<-first_selection_df['respondent_id']
mean_centered<-apply(centered_df,2,mean)
#Ok negligible 
mean_centered 
h1n1_centered<-dplyr::inner_join(centered_df,df_preprocessed[c('respondent_id','h1n1_vaccine')])
seasonal_centered<-dplyr::inner_join(centered_df,df_preprocessed[c('respondent_id','seasonal_vaccine')])
h1n1_col_means_class<-h1n1_centered%>% group_by(h1n1_vaccine)%>% summarise(across(.cols = everything(), .fns = mean))
seasonal_col_means_class<-seasonal_centered%>% group_by(seasonal_vaccine)%>% summarise(across(.cols = everything(), .fns = mean))
#Correlation table

