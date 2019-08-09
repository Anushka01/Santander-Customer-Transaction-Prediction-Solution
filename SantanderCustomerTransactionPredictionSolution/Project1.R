rm(list=ls())
setwd("F://backup F ddrive/anushka/assignments/R")
getwd()


## Read the data
train = read.csv("C:/Users/prince patel/Downloads/train.csv", header = T)
test = read.csv("F:/backup F ddrive/anushka/test.csv", header = T)
###########################################Explore the data##########################################
str(train)


##################################Missing Values Analysis###############################################
missing_val = data.frame(apply(train,2,function(x){sum(is.na(x))}))


############################################Outlier Analysis#############################################
# ## BoxPlots - Distribution and Outlier Check
 cnames = colnames(train[,3:4])

# # #Remove outliers using boxplot method

df = train
train = df
# # #loop to remove from all variables
cnames = colnames(train[,3:202])
for(i in cnames){
  print(i)
  val = train[,i][train[,i] %in% boxplot.stats(train[,i])$out]
  print(length(val))
  marketing_train = train[which(!train[,i] %in% val),]
}

##################################Feature Selection################################################
## Correlation Plot
install.packages("corrgram")
library(corrgram)
corrgram(train[1:30,3:202], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

##################################Feature Scaling################################################
#Normality check
qqnorm(train$var_1)
cnames = colnames(train[,2:202])

for(i in cnames){
  print(i)
  train[,i] = (train[,i] - min(train[,i]))/
    (max(train[,i] - min(train[,i])))
}


###################################Model Development#######################################
rmExcept("train")

#Divide data into train and test using stratified sampling method
set.seed(12234)
install.packages('caret')
library(caret)
train.index = createDataPartition(train$target, p = .80, list = FALSE)
train = train[ train.index,]
test  = train[-train.index,]

##Decision tree for classification
install.packages('C50')
library(C50)
#Develop Model on training data
str(train$target)
train$target = as.factor(train$target)

C50_model = C5.0(target ~., trn, trials = 1, rules = TRUE)

#Summary of DT model
summary(C50_model)

#write rules into disk
write(capture.output(summary(C50_model)), "c50Rules.txt")

#Lets predict for test cases
C50_Predictions = predict(C50_model, test[,-2], type = "class")

##Evaluate the performance of classification model
ConfMatrix_C50 = table(test$target, C50_Predictions)
confusionMatrix(ConfMatrix_C50)

#False Negative rate
FNR = FN/FN+TP 

#Accuracy: 90.89%
#FNR: 63.09%



#Logistic Regression
logit_model = glm(target ~ ., data = trn, family = "binomial")

#summary of the model
summary(logit_model)

#predict using logistic regression
logit_Predictions = predict(logit_model, newdata = test, type = "response")

#convert prob
logit_Predictions = ifelse(logit_Predictions > 0.5, 1, 0)


##Evaluate the performance of classification model
ConfMatrix_RF = table(test$target, logit_Predictions)

#False Negative rate
FNR = FN/FN+TP 

#Accuracy: 90.89
#FNR: 67.85




#naive Bayes
library(e1071)

#Develop model
NB_model = naiveBayes(target ~ ., data = trn)

#predict on test cases #raw
NB_Predictions = predict(NB_model, test[,3:202], type = 'class')


#Look at confusion matrix
Conf_matrix = table(observed = test[,2], predicted = NB_Predictions)
confusionMatrix(Conf_matrix)

#Accuracy: 92.16
#Recall: 0.93 
#precision: 0.63
#specivity:0.70
2052/(2052+1169)
