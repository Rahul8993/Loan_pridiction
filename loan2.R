library(ROCR)
library(VIM)
library(xgboost)
# setting directory
setwd('~/')
#train data
loan_t<-read.csv('~/train_loan.csv', sep=',', header = TRUE)
# test dataa
loan_te<-read.csv('~/test_loan.csv', sep=',', header = TRUE)
#imputing  the null values and blank coloumns 
loan_t<-sapply(loan_t,function(x) gsub("^$|^ $", NA, x))
loan_t<-kNN(loan_t)
loan_te<-sapply(loan_te,function(x) gsub("^$|^ $", NA, x))
loan_te<-kNN(loan_te)
loan_t<-as.data.frame(loan_t)
loan_te<-as.data.frame(loan_te)
levels(loan_t$Dependents)[4] <- "3"
levels(loan_te$Dependents)[4] <- "3"
#removing the extra columns 
loan_te<-loan_te[,1:12]
loan_t<-loan_t[,1:13]
# cross check the data frame 
View(loan_t)
View(loan_te)
# making cat. variable in usable format 
# train data set
loan_t$Gender<-factor(loan_t$Gender, labels = c("0","1"))
loan_t$Self_Employed<-factor(loan_t$Self_Employed, labels = c("0","1"))
loan_t$Credit_History<-factor(loan_t$Credit_History, labels = c("0","1"))
loan_t$Dependents<-factor(loan_t$Dependents, labels = c("0","1","2","3"))
loan_t$Married<-factor(loan_t$Married, labels = c("0","1"))
loan_t$Education<-factor(loan_t$Education, labels = c("0","1"))
loan_t$Loan_Status<-factor(loan_t$Loan_Status, labels = c("0","1"))
loan_t$Property_Area<-factor(loan_t$Property_Area, labels = c("0","1","2"))
#test Data set 
loan_te$Gender<-factor(loan_te$Gender, labels = c("0","1"))
loan_te$Self_Employed<-factor(loan_te$Self_Employed, labels = c("0","1"))
loan_te$Credit_History<-factor(loan_te$Credit_History, labels = c("0","1"))
loan_te$Married<-factor(loan_te$Married, labels = c("0","1"))
loan_te$Education<-factor(loan_te$Education, labels = c("0","1"))
#loan_te$Loan_Status<-factor(loan_te$Loan_Status, labels = c("0","1"))
loan_te$Property_Area<-factor(loan_te$Property_Area, labels = c("0","1","2"))
#Dividing the data set in train  and test 

train_data<-loan_t[1:floor(nrow(loan_t)*0.70),]
test_data<-loan_t[-(1:floor(nrow(loan_t)*0.30)),]

#converting all the coloumnin numeric format 
N_train<-as.data.frame(apply(train_data[,2:13], function(x) 
  {as.numeric(x)}, MARGIN = 2))

N_test<-as.data.frame(apply(test_data[,2:13], 
                            function(x) {as.numeric(x)}, MARGIN = 2))
# Runing Xgboost  model
model.cv <- xgb.cv(data=data.matrix(as.numeric(unlist(N_train))), label=data.matrix(as.numeric(unlist(N_test))), nfold = 10,nrounds = 20,
                   objective="binary:logistic",eval_metric="auc")

N_test <- N_test[1:429,]

 model.cv <- xgb.cv(data=data.matrix(as.numeric(unlist(as.character(N_train)))), 
                   label=data.matrix(as.numeric(unlist(as.character(N_test))),
                         nfold = 10,nrounds = 20, objective="binary:logistic",eval_metric="auc"))
# SUGGESTED 
 N_test <- N_test[1:429,]
 model.cv <- xgb.cv(data=data.matrix(N_train), label=data.matrix(N_test$Loan_Status), nfold=10,
                    nrounds=200, objective="binary:logistic",eval_metric="auc")
 N_test <- N_test[1:429,]
 N_train<-N_train[1:429,]
model1 <- xgboost(data=as.matrix(N_train), label=as.matrix(N_train$Loan_Status), objective="binary:logistic", 
                     nrounds=200,  subsample=0.75, eval_metric="auc")
pred <- predict(model1, as.matrix(loan_te))
## submission
submit <- data.frame("ID"=X_ids, "Business_Sourced"=pred)
write.csv(submit, "./submit.csv", row.names=F)

model1 <- xgboost(Loan_Status ~Gender+Married+Credit_History,family=binomial(link='logit'),data=train_data)
fitted.results <- stats::predict(model1, loan_te, outputMargin=TRUE)
sres <-cbind.data.frame(loan_te$Loan_ID,fitted.results)
colnames(sres) <- c("Loan_ID", "Loan_Status")
write.csv(file="~/Desktop/sumbit1.csv", sres)
#predecting values 
p <- predict(model1,newdata=subset(test_data,select=c(Gender,Self_Employed,Married,Credit_History)),type='response')
pr <- prediction(p, test_data$Loan_Status)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[1]
auc

