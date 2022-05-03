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
#Creation des dataframes :

feature_df<-dplyr::tibble(training_feature)
labels_df<-dplyr::tibble(training_label)
training_df<-dplyr::inner_join(feature_df,labels_df)

#checking all the unique feature

for (feature in colnames(training_df)){
  print(unique(training_df[feature]))
}
print(unique(training_df[feature]))

#Preprocessing of the data -> converting categorical to binary and converting NaN value to mean -> fastDummies::dummy_cols does the job

category_list=c()
for (feature in colnames(training_df)){
  if (typeof(training_df[feature][[1]])=="character"){
    category_list<-c(category_list,feature)
  }
}
category_list
dummy_df<-fastDummies::dummy_cols(training_df,remove_most_frequent_dummy = TRUE)
no_category_df<-dplyr::select(dummy_df,!category_list)

# Replace NaN value for integer feature with the mean of the feature
replace_na_with_mean_value<-function(vec) {
  mean_vec<-mean(vec,na.rm=T)
  vec[is.na(vec)]<-mean_vec
  vec
}
df_preprocessed<-data.frame(apply(no_category_df,2,replace_na_with_mean_value))
for (feature in colnames(df_preprocessed)){
  print(unique(df_preprocessed[feature]))
}
#Preprocessed dataframe
df_preprocessed

#Correlation matrix with Pearson method
cormat <- round(cor(df_preprocessed, method='pearson'),2)

#Put Correaltion matrix in a form suitable to build heatmap
melted_cormat <- melt(cormat)

#Heatmap
pearson_heatmap<-ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()
pearson_heatmap

#Check for highly correlated value (>0.8)
high_correl_variable<-dplyr::filter(melted_cormat,abs(value)>0.8)
high_correl_variable<-dplyr::filter(high_correl_variable,Var1!=Var2)
high_correl_variable

#!!!! ATENTION
# When two feature are highly correlated we could keep only one of the two <- only old character field are concerned 
droped_variable<-c('marital_status_','employment_occupation_dcjcmpih','employment_status_')
first_selection_df<-dplyr::select(df_preprocessed,-droped_variable)


#Rebuild new Heatmap after feature suppression
cormat <- round(cor(first_selection_df, method='pearson'),2)
melted_cormat <- melt(cormat)
pearson_heatmap<-ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()
pearson_heatmap

#LDA -> NN

#Start of the LDA algorithm to select feature
#first_selection_df<-df_preprocessed

#Normalization of the feature to [0,1]
first_selection_df<-dplyr::select(first_selection_df,-c('h1n1_vaccine','seasonal_vaccine'))

for (feature in colnames(first_selection_df)){
  if (feature!='respondent_id'){
  norm_factor=max(first_selection_df[feature])
  first_selection_df[feature]<-(first_selection_df[feature])/norm_factor
  }
}
first_selection_df

#Get the mean of each feature to be able to center the data
mean_vector<-apply(dplyr::select(first_selection_df,-'respondent_id'),2,mean)
mean_vector

#Center the data
centered_df<-sweep(dplyr::select(first_selection_df,-'respondent_id'),MARGIN = 2,STATS = mean_vector,FUN = '-')
centered_df['respondent_id']<-first_selection_df['respondent_id']

#Check that it is indeed centered
mean_centered<-apply(centered_df,2,mean)
mean_centered 

#Bringing the labels back in the picture
h1n1_centered<-dplyr::inner_join(centered_df,df_preprocessed[c('respondent_id','h1n1_vaccine')])
seasonal_centered<-dplyr::inner_join(centered_df,df_preprocessed[c('respondent_id','seasonal_vaccine')])

#Get the mean when grouping with respect to label value (=mean per category) :  
h1n1_col_means_class<-h1n1_centered%>% group_by(h1n1_vaccine)%>% summarise(across(.cols = -c('respondent_id'), .fns = mean))
seasonal_col_means_class<-seasonal_centered%>% group_by(seasonal_vaccine)%>% summarise(across(.cols = -c('respondent_id'), .fns = mean))

h1n1_m0<-data.matrix(dplyr::select(h1n1_col_means_class%>%filter(h1n1_vaccine==0),-'h1n1_vaccine'))
h1n1_m1<-data.matrix(dplyr::select(h1n1_col_means_class%>%filter(h1n1_vaccine==1),-'h1n1_vaccine'))
  
seasonal_m0<-data.matrix(dplyr::select(seasonal_col_means_class%>%filter(seasonal_vaccine==0),-'seasonal_vaccine'))
seasonal_m1<-data.matrix(dplyr::select(seasonal_col_means_class%>%filter(seasonal_vaccine==1),-'seasonal_vaccine'))
#Get the covariance matrix for each -> create matrix

centered_h1n1_0<-data.matrix(dplyr::select(h1n1_centered %>% filter(h1n1_vaccine==0),-c('respondent_id','h1n1_vaccine')))
centered_h1n1_1<-data.matrix(dplyr::select(h1n1_centered %>% filter(h1n1_vaccine==1),-c('respondent_id','h1n1_vaccine')))

centered_seasonal_0<-data.matrix(dplyr::select(seasonal_centered %>% filter(seasonal_vaccine==0),-c('respondent_id','seasonal_vaccine')))
centered_seasonal_1<-data.matrix(dplyr::select(seasonal_centered %>% filter(seasonal_vaccine==1),-c('respondent_id','seasonal_vaccine')))

#Get the between-class scatter matrix
h1n1_Sb<-(t(h1n1_m0-h1n1_m1))%*%(h1n1_m0-h1n1_m1)
seasonal_Sb<-(t(seasonal_m0-seasonal_m1))%*%(seasonal_m0-seasonal_m1)
# Get the total within-class scatter matrix
h1n1_cov0 <- (t(centered_h1n1_0) %*% centered_h1n1_0)
h1n1_cov1 <- (t(centered_h1n1_1) %*% centered_h1n1_1)

seasonal_cov0 <- (t(centered_seasonal_0) %*% centered_seasonal_0)
seasonal_cov1 <- (t(centered_seasonal_1) %*% centered_seasonal_1)
dim(seasonal_Sb)
h1n1_Sw <- h1n1_cov0 + h1n1_cov1
seasonal_Sw = seasonal_cov0 + seasonal_cov1

#Obtain the director vector of the best linear projection

#first invert Sw (since not invertible -> Moore Penrose pseudoinverse instead)
Sw_invert_h1n1<-inv(h1n1_Sw)
Sw_invert_seasonal<-inv((seasonal_Sw))

#Our director vector will be proportionnal to

v_h1n1_unormalized<-Sw_invert_h1n1%*%t(h1n1_m0-h1n1_m1)
v_seasonal_unormalized<-Sw_invert_seasonal%*%t(seasonal_m0-seasonal_m1)

v_h1n1<-v_h1n1_unormalized/sqrt((t(v_h1n1_unormalized)%*%v_h1n1_unormalized)[1])
v_seasonal<-v_seasonal_unormalized/sqrt((t(v_seasonal_unormalized)%*%v_seasonal_unormalized)[1])

#Lets choose an arbitrary tresholds to keep only component(=feature) of the vector whor are above it : threshold=0.05

selector_h1n1<-abs(v_h1n1)>0.06
selector_seasonal<-abs(v_seasonal)>0.06

LDA_dummy_feature_h1n1<-colnames(dplyr::select(centered_df,-'respondent_id')[,selector_h1n1[,1]])
LDA_dummy_feature_seasonal<-colnames(dplyr::select(centered_df,-'respondent_id')[,selector_seasonal[,1]])

#The actual kept feature who seems off
LDA_dummy_feature_h1n1
LDA_dummy_feature_seasonal

# Since many of this feature are boolean created from categorical feature we can group them back under the original feature name

LDA_feature_h1n1<-c(colnames(feature_df[,colnames(feature_df)%in%LDA_dummy_feature_h1n1]),'employment_industry','age_group','employment_occupation')
LDA_feature_seasonal<-c(colnames(feature_df[,colnames(feature_df)%in%LDA_dummy_feature_seasonal]),'employment_industry','age_group','employment_occupation')

LDA_feature_h1n1
LDA_feature_seasonal