data<-read.csv("germancredit.csv",header=TRUE)
str(data)
data$Default=as.factor(data$Default)
data$installment=as.factor(data$installment)
data$residence=as.factor(data$residence)
data$cards=as.factor(data$cards)
data$liable=as.factor(data$liable)

######## a) Perform Exploratory analysis
library(PerformanceAnalytics)
attach(data)
#chart.Correlation(data)

#separating qualitative and quantitaive predictors
qual=quan=c()
for(i in 2:ncol(data))
{
  if(class(data[,i])=="integer")
  {
    quan=cbind(quan,i)
  }
  else{qual=cbind(qual,i)}
}
qual 
quan

data_quan=cbind(data[,quan])
data_qual=data[,qual]

for(i in 1: length(data_qual))
{
  data_qual[,i]=as.numeric(data_qual[,i])
}
class(data_qual[,1])
chart.Correlation(data_quan)

# Boxplots for quantitative predictors
par(mfrow=c(1,3))
for(i in quan)
{
  boxplot(data[,i]~data$Default, data=data,xlab="Default status", ylab=paste(colnames(data)[i]))
}

# barplots for qualitative predictors
par(mfrow=c(3,3))
for(i in qual)
{ 
  if(i!=1){
    barplot(table(data[,i], data$Default), beside = TRUE, xlab="Default Status", ylab=paste(colnames(data)[i]))
  }
}

# significance of quantitatve predictor variables
pval2=c()
for (i in quan)
{
  result=summary(glm(Default~data[,i],family="binomial", data=data))
  pval2=c(pval2,result$coefficients[2,4]) # record p-values of t-test
}
M1=NULL
M1=data.frame(pval2)
M1=cbind(colnames(data)[quan],M1,ifelse(pval2<0.05,'Yes','No'))
names(M1)=c('Predictor',' p-value','Significant')
print(M1)

# significance of qualitative predictor variables
fit_intercept<-glm(Default~1,family="binomial", data=data)
pval2 = c()
for (i in qual)
{
  fit<-glm(Default~data[,i],family="binomial", data=data)
  result <- anova(fit,fit_intercept, test="Chisq")
  res=result$`Pr(>Chi)` # record p-values of chisquare test
  pval2=c(pval2,res[!is.na(res)])
}
M1=NULL
M1=data.frame(pval2)
M1=cbind(colnames(data)[qual],M1,ifelse(pval2<0.05,'Yes','No'))
names(M1)=c('Predictor',' p-value','Significant')
print(M1)


######## b) building reasonably good model
fit1<-glm(Default~.,family="binomial",data=data)

# use subset selection to find a the best model with lowest AIC
step(fit1, direction = "backward", k = 2)

final_model <- glm(Default ~ checkingstatus1 + duration + history + purpose  + amount +
                     savings + installment + status + others + residence + otherplans + 
                     housing + tele + foreign, family="binomial",data=data)

summary(final_model )

print(xtable::xtable(as.data.frame(summary(final_model)$coefficients),digits=4))


#### c) training error for the model
lr.prob<-predict(final_model,data, type="response")
lr.pred<-ifelse(lr.prob>=0.5,"1","0")
##Training error rates
1-mean(lr.pred==data[,1])

##################################################
data<-read.csv("germancredit.csv",header=TRUE)
str(data)
data$Default=as.factor(data$Default)
data$installment=as.factor(data$installment)
data$residence=as.factor(data$residence)
data$cards=as.factor(data$cards)
data$liable=as.factor(data$liable)
str(data)
library(pROC)

######## a) Logistic regression with all predictors
fit1<-glm(Default~.,family="binomial",data=data)

lr.prob<-predict(fit1,data, type="response")
lr.pred<-ifelse(lr.prob>=0.5,"1","0")

##Training error rates
1-mean(lr.pred==data[,1])

#confusion matrix
conf_mat_train = table(lr.pred,data[,1])

##Sensitivity & Specificity
TP = conf_mat_train[2, 2]  # True Positives
FN = conf_mat_train[1, 2]  # False Negatives
sensitivity = TP / (TP + FN)
sensitivity

# Calculate specificity
TN = conf_mat_train[1, 1]  # True Negatives
FP = conf_mat_train[2, 1]  # False Positives
specificity = TN / (TN + FP)
specificity

roc.lr <- roc(data[,1], lr.prob, levels=c("1","0"))

######## b) own code to estimate error using LOOCV

library(class)
acc3 = NULL
#10-fold validation
set.seed(1)
for(i in 1:nrow(data))
{
  dataTest <- data[i, ]
  dataTrain <- data[-i, ]
  
  set.seed(1234)
  lm.pred<-predict(glm(formula = Default~., family="binomial", data=dataTrain),dataTest,type="response")
  mod.test<-ifelse(lm.pred>=0.5,1,0)
  misClass_Error <- (dataTest[,1]!=mod.test)
  acc3[i] <- misClass_Error
}
mean(acc3)

######## c) verify error using package
library(caret)
model.lr<-train(Default~., data, method="glm", family="binomial",metric="Accuracy", 
                trControl=trainControl(method = "LOOCV"))
model.lr

######## d) Perform LDA
library(MASS)
#building model
lda.fit <- lda(Default~.,data)
lda.pred<-predict(lda.fit,data)

##Training error rates
1-mean(lda.pred$class==data[,1])

conf_mat_train = table(lda.pred$class,data[,1])

##Sensitivity & Specificity
TP = conf_mat_train[2, 2]  # True Positives
FN = conf_mat_train[1, 2]  # False Negatives
sensitivity = TP / (TP + FN)
sensitivity

# Calculate specificity
TN = conf_mat_train[1, 1]  # True Negatives
FP = conf_mat_train[2, 1]  # False Positives
specificity = TN / (TN + FP)
specificity

#roc
roc.lda <- roc(data[,1],lda.pred$posterior[,2])

#calculate error rate using LOOCV
model_final.lda<-train(Default~., data, method="lda",metric="Accuracy", 
                       trControl=trainControl(method = "LOOCV"))

model_final.lda

######## d) Perform QDA

#building model
qda.fit <- qda(Default~.,data)
qda.pred<-predict(qda.fit,data)

##Training error rates
1-mean(qda.pred$class==data[,1])

conf_mat_train = table(qda.pred$class,data[,1])

##Sensitivity & Specificity
TP = conf_mat_train[2, 2]  # True Positives
FN = conf_mat_train[1, 2]  # False Negatives
sensitivity = TP / (TP + FN)
sensitivity

# Calculate specificity
TN = conf_mat_train[1, 1]  # True Negatives
FP = conf_mat_train[2, 1]  # False Positives
specificity = TN / (TN + FP)
specificity

#roc
roc.qda <- roc(data[,1],qda.pred$posterior[,2])

#calculate error rate using LOOCV
model_final.qda<-train(Default~., data, method="qda",metric="Accuracy", 
                       trControl=trainControl(method = "LOOCV"))

model_final.qda

######## f) Perform knn

model.knn<-train(Default~., data,method="knn",trControl=trainControl(method = "LOOCV"),
                 tuneGrid=expand.grid(k=seq(from=1,to=100,by=1)))
print(model.knn)

conv.num <- function(x)
{
  if(is.numeric(x)){
    return(x)
  }
  return(as.numeric(x))
}

new.data<- sapply(data,conv.num)

knn.fit<-knn(new.data[,-1],new.data[,-1],new.data[,1],k=77,prob=T)

conf_mat_train = table(new.data[,1],knn.fit)

##Sensitivity & Specificity
TP = conf_mat_train[2, 2]  # True Positives
FN = conf_mat_train[1, 2]  # False Negatives
sensitivity = TP / (TP + FN)
sensitivity

# Calculate specificity
TN = conf_mat_train[1, 1]  # True Negatives
FP = conf_mat_train[2, 1]  # False Positives
specificity = TN / (TN + FP)
specificity


roc.knn<- roc(data[,1],attr(knn.fit,"prob"),levels=c(0,1))


######## f) Repeat for logistic model proposed earlier
fit.red<- glm(Default ~ checkingstatus1 + duration + history + purpose  + amount +
                savings + installment + status + others + residence + otherplans + 
                housing + tele + foreign, family="binomial",data=data)

lr.prob.red<-predict(fit.red,data, type="response")
lr.pred.red<-ifelse(lr.prob.red>=0.5,"1","0")

##Training error rates
1-mean(lr.pred.red==data[,1])

conf_mat_train = table(lr.pred.red,data[,1])

##Sensitivity & Specificity
TP = conf_mat_train[2, 2]  # True Positives
FN = conf_mat_train[1, 2]  # False Negatives
sensitivity = TP / (TP + FN)
sensitivity

# Calculate specificity
TN = conf_mat_train[1, 1]  # True Negatives
FP = conf_mat_train[2, 1]  # False Positives
specificity = TN / (TN + FP)
specificity

roc.lr.red <- roc(data[,1], lr.prob.red, levels=c("1","0"))

model_final.lr<-train(Default ~ checkingstatus1 + duration + history + purpose  + amount +
                        savings + installment + status + others + residence + otherplans + 
                        housing + tele + foreign, data, method="glm", family="binomial",metric="Accuracy", 
                      trControl=trainControl(method = "LOOCV"))
model_final.lr

##plotting ROC curves
pdf("roc_curves.pdf", width = 8,height=8)
par(mfrow=c(1,1))
plot(roc.lr, col="BLUE")
plot(roc.lda,add=T, col="RED")
plot(roc.qda,add=T, col="BLACK")
plot(roc.knn,add=T, col="GREEN")
plot(roc.lr.red,add=T, col="ORANGE")
dev.off()

######################################################
library(pROC)
library(caret)

germanCredit<-read.csv("germancredit.csv",header=TRUE)

germanCredit$Default=as.factor(germanCredit$Default)
germanCredit$installment=as.factor(germanCredit$installment)
germanCredit$residence=as.factor(germanCredit$residence)
germanCredit$cards=as.factor(germanCredit$cards)
germanCredit$liable=as.factor(germanCredit$liable)

### a) logistic regression model using all predictors

logReg <-glm(Default~.,family="binomial",data=germanCredit)


model.lr<-train(Default~., germanCredit, method="glm", family="binomial",metric="Accuracy", 
                trControl=trainControl(method = "LOOCV"))
model.lr
fit_coef <- summary(logReg)$coefficients

### (b) Fit Ridge logistic regression model

library(glmnet)
y <- data$Default
x <- model.matrix(Default ~ ., data)[, -1]

# using leave one out cross validation to find best lambda value
cv_ridge <- cv.glmnet(x,y,alpha = 0, family = "binomial", nfolds = 1000)
best_lambda1 <- cv_ridge$lambda.min
# fitting model with best lambda
ridge_reg <- glmnet(x,y, alpha = 0, family = "binomial", lambda = best_lambda1)
fit_coef_ridge <- coef(ridge_reg)
# using leave one out cross validation to find test MSE
fit_ridge <- train(Default ~.
                   , data = germanCredit, method = "glmnet", family = "binomial",
                   tuneGrid= expand.grid(alpha = 0, lambda = best_lambda1), trControl = trainControl(method = "LOOCV"))
MSE_ridge <- 1-fit_ridge$results[3]
MSE_ridge

### (c) Fit Lasso logistic regression model

# using leave one out cross validation to find best lambda value
cv_lasso <- cv.glmnet(x, y, alpha = 1, nfolds = 1000, family = "binomial")
best_lambda1 <- cv_lasso$lambda.min
# fitting model with best lambda
lasso_reg <- glmnet(x,y, alpha = 1, family = "binomial", lambda = best_lambda1)
fit_coef_lasso <- coef(lasso_reg)
# using leave one out cross validation to find test MSE
fit_lasso <- train(Default ~.
                   , data = germanCredit, method = "glmnet", family = "binomial",
                   tuneGrid= expand.grid(alpha = 1, lambda = best_lambda1), trControl = trainControl(method = "LOOCV"))
MSE_lasso <- 1-fit_lasso$results[3]
MSE_lasso

### d) Comparing Estimates

opts <- options(knitr.kable.NA = "")
table1 <- cbind(fit_coef_ridge[ sort(row.names(fit_coef_ridge)), 1],
                fit_coef_lasso[sort(row.names(fit_coef_lasso)), 1])
table2 <- merge(fit_coef[,1],table1, by = "row.names", all = TRUE)
colnames(table2) <- c("Coefficients","Logistic","Ridge","Lasso")
xtabl::extable(table2,digits = 4)

