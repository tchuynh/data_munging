
rm(list=ls())
library(ggplot2)
library(Matrix)
library(glmnet)
library(Matrix)
library(doParallel)
library(plyr)
library(randomForest)
library(corrgram)
library(AUC)
library(caret)
registerDoParallel(cores=as.numeric(6))

setwd("~/avant")

# input_data<-read.table(file = 'data.tsv', sep = '\t', header = TRUE)

data_csv<-read.csv(file="data.csv", sep=",", header=T)

count_summary_function<-function(data_csv){
  data_names<-names(data_csv)
  data_names
  
  null_counts<-sapply(1:length(data_names), function(x) sum(is.na(data_csv[,data_names[x]])))
  null_counts
  
  cardinality_counts<-sapply(1:length(data_names), function(x) length(unique(data_csv[,data_names[x]])))
  cardinality_counts
  
  col_type<-sapply(1:length(data_names), function(x) typeof(data_csv[,data_names[x]]))
  col_type
  
  col_class<-sapply(1:length(data_names), function(x) class(data_csv[,data_names[x]]))
  col_class
  
  count_summary<-cbind(data_names, null_counts, cardinality_counts, col_type, col_class)
  count_summary<-data.frame(count_summary)
  count_summary
}


count_summary<-count_summary_function(data_csv)
count_summary
data_names<-names(data_csv)


pdf("histogram_of_variables.pdf")
for(name in data_names){
  if(class(data_csv[,name])=="factor"){
    counts <- table(data_csv[,name])
    barplot(counts, main=name)
  } else{
    plot_data<-log10(as.numeric(as.character(data_csv[,name])))
    plot_data<-as.data.frame(plot_data)
    names(plot_data)<-"value"
    hist(plot_data$value, main=name)
  }
}
corrgram(data_csv)
dev.off()

month_to_number<-function(month_name){
  
  switch(month_name,
         Jan = 1,
         Feb = 2,
         Mar = 3,
         Apr = 4,
         May = 5,
         Jun = 6,
         Jul = 7,
         Aug = 8,
         Sep = 9,
         Oct = 10,
         Nov = 11,
         Dec = 12)
}


# notes about the data
# lonn_amnt has been rescaled and has a left tail
# installment is almost normal with longer left tail
# 
# emp_lenth has a mode at 10+, add an indicator variable for 10+ and <=10
# 
# annual_inc has high kurtosis, take log
# 
# issue_d can be used to create number of payments remaining, maybe percentage of payments paid
# 
# purpose, mostly home improvement
# 
# earliest_cr_line, get the distance to now, how long theyve had credit
# 
# acc_now_deling, i would imagine most are zero and very predictive of default.stringsAsFactors(




data_csv$employment_length_categories<-rep(NA, dim(data_csv)[1])
data_csv$employment_length_categories[which(data_csv$emp_length %in% c("1 year", "2 years", "3 years", "4 years", "5 years", "6 years", "7 years", "8 years", "9 years"))]<-"medium"
data_csv$employment_length_categories[which(data_csv$emp_length %in% c("< 1 year"))]<-"low"
data_csv$employment_length_categories[which(data_csv$emp_length %in% c("10+ years"))]<-"high"
data_csv$employment_length_categories[which(data_csv$emp_length %in% c("n/a"))]<-"none"  # maybe unemployed?

# making label, default is the target of prediction so it is 1
# fully paid is 0, and test set will be null
# labels are reasonably balanced.  roughly 2:1
data_csv$label<-rep(NA, dim(data_csv)[1])
data_csv$label[which(data_csv$loan_status == "Fully Paid")]<-0
data_csv$label[which(data_csv$loan_status == "Default")]<-1


########################## compute time since 02/01/2017 closest month in the future from loan status #################################################
table(data_csv$earliest_cr_line)
typeof(data_csv$earliest_cr_line)
class(data_csv$earliest_cr_line)
head(as.character(data_csv$earliest_cr_line))



data_csv$earliest_cr_month<-sapply(1:dim(data_csv)[1], function(x) strsplit(head(as.character(data_csv$earliest_cr_line[x])), "-", fixed=T)[[1]][1])
data_csv$earliest_cr_year<-sapply(1:dim(data_csv)[1], function(x) as.numeric(strsplit(head(as.character(data_csv$earliest_cr_line[x])), "-", fixed=T)[[1]][2]))
data_csv$earliest_cr_month_numeric<-sapply(1:dim(data_csv)[1], function(x) month_to_number(data_csv$earliest_cr_month[x]))
data_csv$months_since_first_credit_line <- 12*(2017 - data_csv$earliest_cr_year - 1) + (12 - data_csv$earliest_cr_month_numeric + 1) +1
data_csv$months_since_first_credit_line <- 12*(2017 - data_csv$earliest_cr_year )  - data_csv$earliest_cr_month_numeric +2

hist(data_csv$earliest_cr_line)
hist(data_csv$months_since_first_credit_line)



########################## computing months since issuance #################################################
table(data_csv$issue_d)
typeof(data_csv$issue_d)
class(data_csv$issue_d)
head(as.character(data_csv$issue_d))
data_csv$issue_d_month<-sapply(1:dim(data_csv)[1], function(x) strsplit(head(as.character(data_csv$issue_d[x])), "-", fixed=T)[[1]][1])
data_csv$issue_d_year<-sapply(1:dim(data_csv)[1], function(x) as.numeric(strsplit(head(as.character(data_csv$issue_d[x])), "-", fixed=T)[[1]][2]))
data_csv$issue_d_month_numeric<-sapply(1:dim(data_csv)[1], function(x) month_to_number(data_csv$issue_d_month[x]))
data_csv$months_since_issue_d <- 12*(2017 - data_csv$issue_d_year - 1) + (12 - data_csv$issue_d_month_numeric + 1) +1
data_csv$months_since_issue_d <- 12*(2017 - data_csv$issue_d_year )  - data_csv$issue_d_month_numeric +2
####################################################################################################



########################## imputing nulls #########################################################
table(data_csv$mths_since_last_delinq, useNA = 'always')
data_csv$last_delinq_flag<-rep(NA, dim(data_csv)[1])
data_csv$last_delinq_flag[which(!is.na(data_csv$mths_since_last_delinq))]<-1
data_csv$last_delinq_flag[which(is.na(data_csv$mths_since_last_delinq))]<-0
table(data_csv$last_delinq_flag, useNA = 'always')

table(data_csv$mths_since_last_record, useNA = 'always')
data_csv$last_record_flag<-rep(NA, dim(data_csv)[1])
data_csv$last_record_flag[which(!is.na(data_csv$mths_since_last_record))]<-1
data_csv$last_record_flag[which(is.na(data_csv$mths_since_last_record))]<-0
table(data_csv$last_record_flag, useNA = 'always')


data_csv$mths_since_last_delinq[which(is.na(data_csv$mths_since_last_delinq))]<-max(data_csv$mths_since_last_delinq, na.rm=T)
data_csv$mths_since_last_record[which(is.na(data_csv$mths_since_last_record))]<-max(data_csv$mths_since_last_record, na.rm=T)

data_csv[,"inq_last_12m"]<-replace(data_csv[,"inq_last_12m"], which(is.na(data_csv[,"inq_last_12m"])), 0)

count_summary_function(data_csv)

####################################################################################################


##################### creating new features #######################################################
data_csv$term_numeric<-rep(NA, dim(data_csv)[1])
data_csv$term_numeric<-sapply(1:dim(data_csv)[1], function(x) as.numeric(strsplit(head(as.character(data_csv$term[x])), " ", fixed=T)[[1]][1]))
data_csv$payments_remaining<-data_csv$term_numeric- data_csv$months_since_issue_d
data_csv$amount_remaining<-data_csv$payments_remaining*data_csv$installment

data_csv$months_since_first_credit_line

data_csv$fico_avg<-(data_csv$fico_range_high + data_csv$fico_range_low)/2
hist(data_csv$fico_avg)

data_csv$last_fico_avg<-(data_csv$last_fico_range_high + data_csv$last_fico_range_low)/2
hist(data_csv$last_fico_avg)

#################################### getting training data ##########################################

table(data_csv$acc_now_delinq, useNA = 'always')
table(data_csv$employment_length_categories, useNA = 'always')

# putting acc_now_delinq to avoid computation and coding issues, it is probably non linear

training_data<-data_csv[which(!is.na(data_csv$label)),]
categorical_variables<-c("term", "emp_length", "home_ownership", "verification_status", "purpose", "addr_state" ,"last_delinq_flag", "last_record_flag")

numerical_variables<-c("loan_amnt", "installment", "payments_remaining", "amount_remaining", "months_since_issue_d", "dti","months_since_first_credit_line", "acc_now_delinq", "delinq_amnt", "delinq_2yrs", "inq_last_6mths", "inq_last_12m")

interactions<-c("verification_status:annual_inc", "mths_since_last_delinq:last_delinq_flag", "mths_since_last_record:last_record_flag")


head(training_data[,c("loan_amnt", "installment", "payments_remaining", "amount_remaining", "months_since_issue_d", "dti",
                      "months_since_first_credit_line", "acc_now_delinq", "delinq_amnt", "delinq_2yrs", "inq_last_6mths", "inq_last_12m")])




xfactors <- model.matrix(label ~ ., data=training_data[,c("label", categorical_variables)])[, -1]


predictors<-cbind(data.matrix(training_data[,numerical_variables]), xfactors)

response<-as.factor(training_data$label)
levels(designMatrix$label)<-c("negative", "positive")
levels(response)<-c("negative", "positive")




designMatrix<-cbind(response, predictors)
designMatrix<-data.frame(designMatrix)
names(designMatrix)[1]<-"label"
designMatrix$label<-factor(designMatrix$label)
levels(designMatrix$label)<-c("negative", "positive")



####################### fitting models ##################################################

set.seed(998)
inTraining <- createDataPartition(response, p = .75, list = FALSE)
training <- designMatrix[ inTraining,]
testing  <- designMatrix[-inTraining,]




fitControl <- trainControl(
  method = "cv",
  number = 4,
  classProbs = TRUE, summaryFunction = twoClassSummary
  )

typeof(designMatrix$label)
designMatrix$label<-factor(designMatrix$label)



gbmFit1 <- train(label ~ ., data = x, 
                 method = "gbm", 
                 trControl = fitControl,
                 metric = "ROC",
                 verbose = FALSE)
gbmFit1
relative.influence(gbmFit1$finalModel)
plot.gbm(gbmFit1$finalModel)
summary(gbmFit1$finalModel)



rfFit1 <- train(label ~ ., data = designMatrix, 
                 method = "rf", 
                 trControl = fitControl,
                metric = "ROC",
                 verbose = FALSE,
                tuneGrid = expand.grid(.mtry = 2^(1:6)))
rfFit1

varImpPlot(rfFit1$finalModel, sort=T)

importance(rfFit1$finalModel)[order(importance(rfFit1$finalModel)[,1], decreasing=T),]



fitControl <- trainControl(## 10-fold CV
  method = "cv",
  number = 4,
  classProbs = TRUE,
  summaryFunction = twoClassSummary)

glmnetFit1 <- train(predictors, response, 
                             method = "glmnet", 
                             trControl = fitControl,
                             metric = "ROC",
                             preProc = c("center", "scale"),
                             tuneGrid = expand.grid(.alpha = seq(0.0, 1, length = 15),
                                                    .lambda = c((1:5)/10)))

plot(glmnetFit1$finalModel)
glmnetFit1$finalModel$lambda.min
glmnetFit1$finalModel$beta[,46]
glmnetFit1$finalModel$beta
glmnetFit1$finalModel$lambda
glmnetFit1$finalModel$lambdaOpt

glm_rel_imp<-glmnetFit1$finalModel$beta[,46]/sum(glmnetFit1$finalModel$beta[,46])
barplot(glm_rel_imp)

glm_rel_imp[order(glm_rel_imp, decreasing=T)]


####################### saving data ##################################################

save(designMatrix, file="designMatrix.Rdata")
write.csv(data_csv, file="cleanedData.csv")
