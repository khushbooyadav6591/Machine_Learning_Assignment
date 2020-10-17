#---------------------------------------------
# Importing data from csv file
#---------------------------------------------

UniversalBank <- read.csv("~/Downloads/UniversalBank.csv")
View(UniversalBank)
summary(UniversalBank)
str(UniversalBank)
#-----------------------------------
#Dropping 2 columns since it is not required
#----------------------------------
UniversalBank<-UniversalBank[,c(-1,-5)] 
summary(UniversalBank)
head(UniversalBank)


#----------------------------------------
#Identifying Numerical variable and categorical variable
#All the variables in the dataset are of Integer/Num data types
# after viewing the data Numerical varibale can be identified as :- Age , Experience,Income,CCAvg,Mortgage
# Categorical Variables are Family ,Education, however Personal Loan , Securities Account , CD account , 
# Online and CreditCard.
#We need to convert the categorical variable into factor in order to see the levels
#-----------------------------------------
UniversalBank_Numerical<-UniversalBank[c(1:3,5,7)]
UniversalBank_Categorical<-UniversalBank[,-c(1:3,5,7)]
head(UniversalBank_Categorical)
str(UniversalBank_Categorical)   


#---------------------------------------------
## Dummy Variables creation and factor conversion of all the 
#-------------------------------------------

UniversalBank[,'Education'] <- as.factor(UniversalBank[,'Education'])
UniversalBank[,'Family'] <- as.factor(UniversalBank[,'Family'])
UniversalBank[,'Personal.Loan'] <- as.factor(UniversalBank[,'Personal.Loan'])
UniversalBank[,'Securities.Account'] <- as.factor(UniversalBank[,'Securities.Account'])
UniversalBank[,'CD.Account'] <- as.factor(UniversalBank[,'CD.Account'])
UniversalBank[,'Online'] <- as.factor(UniversalBank[,'Online'])
UniversalBank[,'CreditCard'] <- as.factor(UniversalBank[,'CreditCard'])
levels(UniversalBank$Education)
levels(UniversalBank$Family)
dummy_model1 <- dummyVars(~Education,data=UniversalBank)
head(predict(dummy_model1,newdata = UniversalBank))
dummy_model2 <- dummyVars(~Family,data=UniversalBank)
head(predict(dummy_model2,newdata = UniversalBank))
str(UniversalBank)
head(UniversalBank)
#------------------------------------------------------
#Splitting of data
#------------------------------------------------------
library(caret)
library(ISLR)
library(dplyr)

set.seed(15)

Train1_Index_UniBank = createDataPartition(UniversalBank$Personal.Loan,p=0.60, list=FALSE) 
Train1_Data_UniBank = UniversalBank[Train1_Index_UniBank,]
Validation_Data1_UniBank = UniversalBank[-Train1_Index_UniBank,] 
summary(Train1_Index_UniBank)
summary(Train1_Data_UniBank)
summary(Validation_Data1_UniBank)
nrow(Train1_Data_UniBank)
nrow(Validation_Data1_UniBank)
#------------------------------
#-----------------------------------------------------------
#Normalization of the Numerical values
#----------------------------------------------------------
# Copy the original data
train_norm_df <- Train1_Data_UniBank
valid_norm_df <- Validation_Data1_UniBank

# use preProcess() from the caret package to normalize 
norm_values <- preProcess(Train1_Data_UniBank, method=c("center", "scale"))


train_norm_df <- predict(norm_values, Train1_Data_UniBank) # Replace first two columns with normalized values
valid_norm_df <- predict(norm_values, Validation_Data1_UniBank) ##need to change the naming convention

summary(train_norm_df)
var(train_norm_df)
summary(valid_norm_df)
var(valid_norm_df)
class(train_norm_df)


#knn
#----------------------------------------

## KNN Hypertuning using Validation

set.seed(111)
library(caret)
library(e1071)
library(FNN)
accuracy_knn <- data.frame(k = seq(1, 15, 1), accuracy = rep(0, 15))

# compute knn for different k on validation.
for(i in 1:15) {
  knn_pred <- knn(train_norm_df[, c(1:3,5,7)], valid_norm_df[, c(1:3,5,7)], 
                  cl = train_norm_df[,8], k = i)
  accuracy_knn[i, 2] <- confusionMatrix(knn_pred,valid_norm_df[,8])$overall[1] 
}
accuracy_knn

#----------------------------
#Higher the vale of K , lesser the chance of error. Hence ,
#k=4  with accuracy value = 0.9130.
#With k=1 ,0.907,accuracy seems to be good however with k=4 accuracy seems to be more better.
#----------------------------------------------
#error rate
#-----------------
#---------Merging the Train set and Validation set 
#-----------------
UniversalBank_combined<-rbind.data.frame(train_norm_df,valid_norm_df)
nrow(UniversalBank_combined)
summary(UniversalBank_combined)
#------------------------------------------------------------------
#Re-split the data(50%:30:20%)
#---------------------------------------------------------------
set.seed(156)

Test_Index_UniBank = createDataPartition(UniversalBank_combined$Personal.Loan,p=0.20, list=FALSE) #test data
Test_Data_UniBank = UniversalBank_combined[Test_Index_UniBank,]
Traval_Data_UniBank = UniversalBank_combined[-Test_Index_UniBank,] #left 80 prcnt of data
Train2_Index_UniBank = createDataPartition(Traval_Data_UniBank$Personal.Loan,p=0.625, list=FALSE) #50prcnt of the 80 prcnt data
Train2_Data_UniBank = Traval_Data_UniBank[Train2_Index_UniBank,]
#left 30 prcnt data
Validation2_Data_UniBank=Traval_Data_UniBank[-Train2_Index_UniBank,]

nrow(Train2_Data_UniBank)
nrow(Validation2_Data_UniBank)
nrow(Test_Data_UniBank)
nrow(Traval_Data_UniBank)

#-----------------------------------------------------------------
# Since we have already normalized the data before , we dont need to normalize it again
#After splitting the normalized data , we can use the optimum selected k value for prediction

#-------------------------------------------------------------------------------------
#confusion matrix for  the validation data set 30% and traning dataset 50%
#---------------------------------------------------------------------
install.packages("gmodels") # install if necessary
library("gmodels")
Train_Predictors<-Train2_Data_UniBank[, c(1:3,5,7)]
Val_Predictors<-Validation2_Data_UniBank[, c(1:3,5,7)]
Val_labels<-Validation2_Data_UniBank[,8]
Train_labels<-Train2_Data_UniBank[,8]
Predicted_Val_labels<- knn(Train_Predictors, 
                            Val_Predictors, 
                            cl=Train_labels, 
                            k=4 ) 
CrossTable(x=Val_labels,y=Predicted_Val_labels, prop.chisq = FALSE)
#--------------------------------------------------------------------
#Confusion Matrix for the training dataset and testdataset
#--------------------------------------------
#Train_Predictors<-Train2_Data_UniBank[, c(1:3,5,7)]
Test_Predictors<-Test_Data_UniBank[, c(1:3,5,7)]
Test_labels<-Test_Data_UniBank[,8]
#Train_labels<-Train2_Data_UniBank[,8]
Predicted_Test_labels<- knn(Train_Predictors, 
                            Test_Predictors, 
                            cl=Train_labels, 
                            k=4 ) 
CrossTable(x=Test_labels,y=Predicted_Test_labels, prop.chisq = FALSE)
#--------------------------------------------------------------------
#Confusion Matrix for the validation dataset and test set
#-----------------------------------------------------------
#Train_Predictors<-Train2_Data_UniBank[, c(1:3,5,7)]
#Test_Predictors<-Test_Data_UniBank[, c(1:3,5,7)]
Test_labels<-Test_Data_UniBank[,8]
Val_labels<-Validation2_Data_UniBank[,8]
#Train_labels<-Train2_Data_UniBank[,8]
Predicted_TestV_labels<- knn(Val_Predictors, 
                            Test_Predictors, 
                            cl=Val_labels, 
                            k=4 ) 
x<-CrossTable(x=Test_labels,y=Predicted_TestV_labels, prop.chisq = FALSE)

#---------------------------------------------------------
#New Customer classification using K=1, 0.9030 , K=4,0.9130
#--------------------------------------------------------
New_Customer<-data.frame(
'Age' = 40, 
'Experience' = 10,
'Income' = 84,
'Family' = 2, 
'CCAvg' = 2,
'Education' = 1,
#'Education_2' = 1,
#'Education_3' = 0,
'Mortgage' = 0, 
'Securities_Account' = 0, 
'CD_Account' = 0,
'Online' = 1,
'Credit_Card' = 1)
class(New_Customer)
#with K=1,
#--------------------------------------------------------------
knn_pred_new <- knn(Traval_Data_UniBank[,c(1:7,9:12)], New_Customer, 
                    cl = Traval_Data_UniBank[,8], k = 1)
#row.names(Traval_Data_UniBank)[attr(nn, "nn.index")]
