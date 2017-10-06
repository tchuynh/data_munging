
rm(list=ls())
library(ggplot2)
library(Matrix)
library(glmnet)
library(Matrix)
library(doParallel)
library(plyr)
library(randomForest)
registerDoParallel(cores=as.numeric(2))



setwd("C:/Users/tchuynh/Downloads/frameio")

input_data<-read.table(file = 'data.tsv', sep = '\t', header = TRUE)

data_csv<-read.csv(file="data.csv", sep=",", header=T)

names(data_csv)<-c(
  "company", 
  "free_collaborators", 
  "free_comments", 
  "free_files", 
  "free_projects", 
  "free_team_members",
  "free_accounts", 
  "free_signins_greater_30days", 
  "free_signins_lesser_30days", 
  "paid_collaborators",
  "paid_comments", 
  "paid_files", 
  "paid_projects", 
  "paid_team_members", 
  "paid_accounts", 
  "paid_signins_greater_30days", 
  "paid_signins_lesser_30days",
  "employee_count_estimate", 
  "country", 
  "industry")


data_csv$total_accounts<- data_csv$paid_accounts + data_csv$free_accounts


data_csv$proportion_paid_accounts<-data_csv$paid_accounts/replace(data_csv$total_accounts, which(data_csv$total_accounts==0), 1)


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

# 
# pdf("histogram_of_variables.pdf")
# for(name in data_names){
#   if(class(data_csv[,name])=="factor"){
#     counts <- table(data_csv[,name])
#     barplot(counts, main=name)
#   } else{
#     plot_data<-log10(as.numeric(as.character(data_csv[,name])))
#     plot_data<-as.data.frame(plot_data)
#     names(plot_data)<-"value"
#     hist(plot_data$value, main=name)
#   }
# }
# dev.off()



########################### country fields are being parsed ########################################
metadata_name<-"country"


metadata_parser<-function(data_csv,metadata_name){
print(metadata_name)
data_csv[,paste0(metadata_name, "_first_part")]<-rep(NA, dim(data_csv)[1])
data_csv[,paste0(metadata_name, "_second_part")]<-rep(NA, dim(data_csv)[1])
data_csv[,paste0(metadata_name, "_ratio")]<-rep(NA, dim(data_csv)[1])

counts <- table(data_csv[,metadata_name])
print(counts)
barplot(counts, main=paste0(metadata_name, " counts"))

data_csv[,paste0(metadata_name, "_first_part")]<-sapply(1:dim(data_csv)[1], function(x)  unlist(strsplit(toString(data_csv[x,metadata_name]), ":", fixed = TRUE))[1])
counts <- table(data_csv[,paste0(metadata_name, "_first_part")])
print(counts)
barplot(counts, main=paste0(metadata_name, " first part counts"))


data_csv[,paste0(metadata_name, "_second_part")]<-sapply(1:dim(data_csv)[1], function(x)  unlist(strsplit(toString(data_csv[x,metadata_name]), ":", fixed = TRUE))[2])
counts <- table(data_csv[,paste0(metadata_name, "_second_part")])
print(counts)
barplot(counts, main=paste0(metadata_name, " second part counts"))

data_csv[,paste0(metadata_name, "_ratio")]<-sapply(1:dim(data_csv)[1], function(x) as.numeric(strsplit(data_csv[x,paste0(metadata_name, "_second_part")], "/", fixed=T)[[1]][1])/as.numeric(strsplit(data_csv[x,paste0(metadata_name, "_second_part")], "/", fixed=T)[[1]][2]))
counts <- table(data_csv[,paste0(metadata_name, "_ratio")])
print(counts)
barplot(counts, main=paste0(metadata_name, " proportion existing metadata counts"))


data_csv[,paste0(metadata_name, "_counts")]<-sapply(1:dim(data_csv)[1], function(x) as.numeric(strsplit(data_csv[x,paste0(metadata_name, "_second_part")], "/", fixed=T)[[1]][1]))
counts <- table(data_csv[,paste0(metadata_name, "_counts")])
print(counts)
barplot(counts, main=paste0(metadata_name, " metadata existing counts"))

head(data_csv)
data_csv
}


parsed_data<-metadata_parser(data_csv,metadata_name="employee_count_estimate")
parsed_data<-metadata_parser(data_csv=parsed_data,metadata_name="country")
parsed_data<-metadata_parser(data_csv=parsed_data,metadata_name="industry")
####################################################################################################

count_summary<-count_summary_function(parsed_data)
count_summary





parsed_data[,"employee_count_estimate_counts"]<-replace(parsed_data[,"employee_count_estimate_counts"], which(is.na(parsed_data[,"employee_count_estimate_counts"])), 0)
parsed_data[,"country_counts"]<-replace(parsed_data[,"country_counts"], which(is.na(parsed_data[,"country_counts"])), 0)
parsed_data[,"industry_counts"]<-replace(parsed_data[,"industry_counts"], which(is.na(parsed_data[,"industry_counts"])), 0)




xfactors <- sparse.model.matrix(proportion_paid_accounts ~ ., data=parsed_data[,c("proportion_paid_accounts", "country_first_part", "industry_first_part")])[, -1]

# col_class_2<-sapply(1:length(data_names_2), function(x) class(training_data2[,data_names_2[x]]))

predictors<-cbind(data.matrix(parsed_data[,c(  "free_collaborators", 
                                               "free_comments", 
                                               "free_files", 
                                               "free_projects", 
                                               "free_team_members",
                                               "free_accounts", 
                                               "free_signins_greater_30days", 
                                               "free_signins_lesser_30days")]), xfactors)


predictors_numeric<-cbind(data.matrix(parsed_data[,c(  "free_collaborators", 
                                               "free_comments", 
                                               "free_files", 
                                               "free_projects", 
                                               "free_team_members",
                                               "free_accounts", 
                                               "free_signins_greater_30days", 
                                               "free_signins_lesser_30days")]))


###### checking label balance ################

counts <- table(c(rep(0, sum(parsed_data$free_accounts)), rep(1, parsed_data$paid_accounts)))
print(counts)
barplot(counts, main=paste0(metadata_name, " proportion existing metadata counts"))





response=cbind(as.numeric(1-parsed_data$proportion_paid_accounts),as.numeric(parsed_data$proportion_paid_accounts) )


cv.out.auc<-cv.glmnet(x=predictors, y=response,weights=parsed_data$total_accounts,  nfolds=5,family="binomial", type.measure="auc", parallel= T)
plot(cv.out.auc)
cv.out.auc$lambda.min
cv.out.auc$glmnet.fit$beta[,24]
which(cv.out.auc$glmnet.fit$lambda==cv.out.auc$lambda.min)



cv.out.numeric.auc<-cv.glmnet(x=predictors_numeric, y=response,weights=parsed_data$total_accounts,  nfolds=5,family="binomial", type.measure="auc", parallel= T)

jpeg("auc_glmnet_continuous.jpg")
plot(cv.out.numeric.auc)
dev.off()

cv.out.numeric.auc$lambda.min
cv.out.numeric.auc$glmnet.fit$beta[,which(cv.out.numeric.auc$glmnet.fit$lambda==cv.out.numeric.auc$lambda.min)]



cv.out.numeric<-cv.glmnet(x=predictors_numeric, y=response,weights=parsed_data$total_accounts,  nfolds=5,family="binomial",  parallel= T)
plot(cv.out.numeric)
cv.out.numeric$lambda.min
cv.out.numeric$glmnet.fit$beta[,which(cv.out.numeric$glmnet.fit$lambda==cv.out.numeric$lambda.min)]
which(cv.out.numeric$glmnet.fit$lambda==cv.out.numeric$lambda.min)






minimum_sample_size_index<-parsed_data$industry_first_part %in%names(counts[which(counts>10)])

cv.out.numeric.auc.industry<-cv.glmnet(x=predictors_numeric[minimum_sample_size_index,], y=factor(parsed_data$industry_first_part[minimum_sample_size_index]),weights=parsed_data$total_accounts[minimum_sample_size_index],  nfolds=5,family="multinomial", type.measure="auc", parallel= T)
plot(cv.out.numeric.auc.industry)
cv.out.numeric.auc.industry$lambda.min
cv.out.numeric.auc.industry$glmnet.fit$beta[,which(cv.out.numeric.auc.industry$glmnet.fit$lambda==cv.out.numeric.auc.industry$lambda.min)]






###################### prediction using random forests #############################################
designMatrix<-cbind(parsed_data$proportion_paid_accounts, predictors)
designMatrix<-data.matrix(designMatrix)
designMatrix<-data.frame(designMatrix)
names(designMatrix)[1]<-"proportion_paid_accounts"
# random forests
my.rf <- randomForest(proportion_paid_accounts ~ .,data=designMatrix , importance=TRUE, na.action=na.omit)
print(my.rf)
varImpPlot(my.rf)
round(importance(my.rf), 2)


my.rf.2 <- randomForest(proportion_paid_accounts ~ free_collaborators + free_comments+free_files+free_projects+free_team_members+free_accounts+free_signins_greater_30days+free_signins_lesser_30days,data=parsed_data)
varImpPlot(my.rf.2)
round(importance(my.rf.2), 2)


rf.cv.out<-rfcv(trainx=predictors_numeric, trainy=parsed_data$proportion_paid_accounts)
rf.cv.out$error.cv





continuous_variables<-c(  "free_collaborators", 
    "free_comments", 
    "free_files", 
    "free_projects", 
    "free_team_members",
    "free_accounts", 
    "free_signins_greater_30days", 
    "free_signins_lesser_30days")




metadata_name<-"industry"
counts <- table(parsed_data[,paste0(metadata_name, "_first_part")])
print(counts)
barplot(counts, main=paste0(metadata_name, " first part counts"))

industry_means<-ddply(parsed_data, .(industry_first_part), summarise, free_accounts=mean(free_accounts), paid_accounts=mean(paid_accounts))


overall_means<-apply(parsed_data[,c(continuous_variables, "paid_accounts")], 2,mean)
overall_means<-data.frame(t(overall_means), industry_first_part="overall")
overall_means



jpeg("companies_paid_free_wnames.jpg")
ggplot(data=parsed_data, aes(x=free_accounts, y=paid_accounts, color=industry_first_part )) + 
  geom_point() + 
  geom_point(data=industry_means, aes(x=free_accounts,y=paid_accounts, color=industry_first_part), size=10, alpha=.01)+ theme(legend.position="none") +
  geom_text(aes(label=company),hjust=0, vjust=0)+
  ggtitle("all companies")
dev.off()

jpeg("companies_paid_free.jpg")
ggplot(data=parsed_data, aes(x=free_accounts, y=paid_accounts, color=industry_first_part )) + 
  geom_point() + 
  geom_point(data=industry_means, aes(x=free_accounts,y=paid_accounts, color=industry_first_part), size=10, alpha=.01)+ theme(legend.position="none") +
  ggtitle("companies")
dev.off()



jpeg("industries_paid_free_wnames.jpg")
ggplot(data=industry_means, aes(x=free_accounts,y=paid_accounts, color=industry_first_part)) + 
  geom_point() +
  geom_point(data=overall_means, aes(x=free_accounts,y=paid_accounts), size=20, alpha=.1)+ theme(legend.position="none") +
  geom_text(aes(label=industry_first_part),hjust=0, vjust=0)+
  ggtitle("industries")
dev.off()


jpeg("industries_paid_free.jpg")
  ggplot(data=industry_means, aes(x=free_accounts,y=paid_accounts, color=industry_first_part)) + 
    geom_point() +
    geom_point(data=overall_means, aes(x=free_accounts,y=paid_accounts), size=20, alpha=.1)+ theme(legend.position="none") +
    ggtitle("industries")
  dev.off()

  
  free_loaders<-industry_means[which(industry_means$paid_accounts==0),][order(industry_means$free_accounts[which(industry_means$paid_accounts==0)], decreasing=T),]
  
  
  most_important_variables<-c(  "free_collaborators", 
                            "free_comments", 
                            "free_files", 
                            "free_projects", 
                            "free_accounts",
                            "paid_accounts")
  

  
  
  
  ############################# squared distance for each important feature for industry ###########
  squared_distance<-data.frame(company=parsed_data$company, industry=parsed_data$industry_first_part, (parsed_data[,most_important_variables]- rep(as.numeric(overall_means[, most_important_variables]), dim(parsed_data)[1]) )^2)
  
  head(squared_distance)
  
  industry_distance_for_each_variable<-ddply(parsed_data, .(industry_first_part), summarise, free_collaborators = mean(free_collaborators),
                                             free_comments = mean(free_comments),
                                             free_files = mean(free_files),
                                             free_projects = mean(free_projects),
                                             free_accounts = mean(free_accounts),
                                             paid_accounts = sum(paid_accounts)/(sum(free_accounts)+sum(paid_accounts))
                                             )

  jpeg("free_collaborators_industry.jpg")
  print(ggplot(industry_distance_for_each_variable, aes(x=industry_first_part, y=free_collaborators)) + geom_point()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("free_collaborators"))
  dev.off()
  
  jpeg("free_comments_industry.jpg")
  print(ggplot(industry_distance_for_each_variable, aes(x=industry_first_part, y=free_comments)) + geom_point()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("free_comments"))
  dev.off()
  
  jpeg("free_files_industry.jpg")
  print(ggplot(industry_distance_for_each_variable, aes(x=industry_first_part, y=free_files)) + geom_point()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("free_files"))
  dev.off()
  
  jpeg("free_accounts_industry.jpg")
  print(ggplot(industry_distance_for_each_variable, aes(x=industry_first_part, y=free_projects)) + geom_point()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("free_projects"))
  dev.off()
  
  jpeg("free_accounts_industry.jpg")
  print(ggplot(industry_distance_for_each_variable, aes(x=industry_first_part, y=free_accounts)) + geom_point()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("free_accounts"))
  dev.off()
  
  jpeg("paid_accounts_industry.jpg")
  print(ggplot(industry_distance_for_each_variable, aes(x=industry_first_part, y=paid_accounts)) + geom_point()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("paid_accounts"))
  dev.off()
  
  
  for(var in most_important_variables){
  print(var)
  print(industry_distance_for_each_variable[order(industry_distance_for_each_variable[, var], decreasing=T),][1:4,c("industry_first_part", var)])
  print(industry_distance_for_each_variable[order(industry_distance_for_each_variable[, var]),][1:4,c("industry_first_part", var)])
  }
  
  overall_means[,most_important_variables]
  ###################### overall euclidean distance ###############################################

  scaled_data<-scale(parsed_data[, most_important_variables])
  scaled_data<-cbind(parsed_data$industry_first_part, scaled_data)
  parsed_data$euclidean_distance<-apply((scaled_data[,most_important_variables] )^2, 1, sum)
  
  industry_euclidean_distances<-ddply(parsed_data, .(industry_first_part), summarise, euclidean_distance = mean(euclidean_distance))
  
  barplot(table(industry_euclidean_distances))
  industry_euclidean_distances_ordered<-industry_euclidean_distances[order(industry_euclidean_distances$euclidean_distance, decreasing=T),]
  industry_euclidean_distances_ordered$industry_first_part<-factor(industry_euclidean_distances_ordered$industry_first_part)
  industry_euclidean_distances_ordered$industry_first_part<-factor(industry_euclidean_distances_ordered$industry_first_part)
  jpeg("euclidean_distance_industry.jpg")
  print(ggplot(industry_euclidean_distances_ordered, aes(x=industry_first_part, y=euclidean_distance)) + geom_point()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("euclidean distances"))
  dev.off()
  
  head(industry_euclidean_distances_ordered)
  
  typeof(industry_euclidean_distances_ordered$industry_first_part)
  class(industry_euclidean_distances_ordered$industry_first_part)  
  
  typeof(industry_euclidean_distances_ordered$euclidean_distance)
  class(industry_euclidean_distances_ordered$euclidean_distance)
  
  
  
  
  
  
  
  ############################# squared distance for each important feature for country ############
  squared_distance<-data.frame(company=parsed_data$company, industry=parsed_data$country_first_part, (parsed_data[,most_important_variables]- rep(as.numeric(overall_means[, most_important_variables]), dim(parsed_data)[1]) )^2)
  
  head(squared_distance)
  
  country_distance_for_each_variable<-ddply(parsed_data, .(country_first_part), summarise, free_collaborators = mean(free_collaborators),
                                             free_comments = mean(free_comments),
                                             free_files = mean(free_files),
                                             free_projects = mean(free_projects),
                                             free_accounts = mean(free_accounts),
                                            paid_accounts = sum(paid_accounts)/(sum(free_accounts)+sum(paid_accounts)))
  
  
  jpeg("free_collaborators_country.jpg")
  print(ggplot(country_distance_for_each_variable, aes(x=country_first_part, y=free_collaborators)) + geom_point()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("free_collaborators"))
  dev.off()
  
  jpeg("free_comments_country.jpg")
  print(ggplot(country_distance_for_each_variable, aes(x=country_first_part, y=free_comments)) + geom_point()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("free_comments"))
  dev.off()
  
  jpeg("free_files_country.jpg")
  print(ggplot(country_distance_for_each_variable, aes(x=country_first_part, y=free_files)) + geom_point()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("free_files"))
  dev.off()
  
  jpeg("free_projects_country.jpg")
  print(ggplot(country_distance_for_each_variable, aes(x=country_first_part, y=free_projects)) + geom_point()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("free_projects"))
  dev.off()
  
  jpeg("free_accounts_country.jpg")
  print(ggplot(country_distance_for_each_variable, aes(x=country_first_part, y=free_accounts)) + geom_point()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("free_accounts"))
  dev.off()
  
  jpeg("paid_accounts_country.jpg")
  print(ggplot(country_distance_for_each_variable, aes(x=country_first_part, y=paid_accounts)) + geom_point()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("paid_accounts"))
  dev.off()
  
  ###################### overall euclidean distance ################################################
  caled_data<-scale(parsed_data[, most_important_variables])
  parsed_data$euclidean_distance<-apply((scaled_data )^2, 1, sum)
  
  parsed_data$euclidean_distance<-apply((parsed_data[,most_important_variables]- rep(as.numeric(overall_means[, most_important_variables]), dim(parsed_data)[1]) )^2, 1, sum)
  country_euclidean_distances<-ddply(parsed_data, .(country_first_part), summarise, euclidean_distance = mean(euclidean_distance))
  barplot(table(country_euclidean_distances))
  country_euclidean_distances_ordered<-country_euclidean_distances[order(country_euclidean_distances$euclidean_distance, decreasing=T),]
  country_euclidean_distances_ordered$country_first_part<-factor(country_euclidean_distances_ordered$country_first_part)
  country_euclidean_distances_ordered$country_first_part<-factor(country_euclidean_distances_ordered$country_first_part)
  jpeg("euclidean_distance_country.jpg")
  print(ggplot(country_euclidean_distances_ordered, aes(x=country_first_part, y=euclidean_distance)) + geom_point()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("euclidean distances"))
  dev.off()
  
  
  head(country_euclidean_distances_ordered)
  
  for(var in most_important_variables){
    print(var)
    print(country_distance_for_each_variable[order(country_distance_for_each_variable[, var], decreasing=T),][1:4,c("country_first_part", var)])
  }
  
  
  country_distance_for_each_variable[order(country_distance_for_each_variable)]
  
  overall_means[,most_important_variables]
  
  sum(parsed_data$paid_accounts)/(sum(parsed_data$free_accounts)+sum(parsed_data$paid_accounts))
  
  
  typeof(country_euclidean_distances_ordered$country_first_part)
  class(country_euclidean_distances_ordered$country_first_part)  
  
  typeof(country_euclidean_distances_ordered$euclidean_distance)
  class(country_euclidean_distances_ordered$euclidean_distance)
  
  
  jpeg("hist_free_accounts.jpg")
  hist(log10(parsed_data$free_accounts), main="counts of free accounts")
  dev.off()
  
  jpeg("hist_paid_accounts.jpg")
  hist(log10(parsed_data$paid_accounts), main="counts of paid accounts")
  dev.off()
  
  
