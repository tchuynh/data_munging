################################### uptake case study #########################
# author:  Tam Huynh (tchuynh)
# date  :  09/03/2017
#
# description:  
#               Case study for predicting potential donors
##############################################################################
rm(list=ls())
library(ggplot2)
library(glmnet)
library(Matrix)
#install.packages('doParallel')
library(doParallel)
registerDoParallel(cores=as.numeric(6))

training_data<-read.csv("train.csv")
#testing_data<-read.csv("test.csv")

head(training_data)

data_names<-names(training_data)
data_names

null_counts<-sapply(1:length(data_names), function(x) sum(is.na(training_data[,data_names[x]])))
null_counts

cardinality_counts<-sapply(1:length(data_names), function(x) length(unique(training_data[,data_names[x]])))
cardinality_counts

col_type<-sapply(1:length(data_names), function(x) typeof(training_data[,data_names[x]]))
col_type

col_class<-sapply(1:length(data_names), function(x) class(training_data[,data_names[x]]))
col_class

count_summary<-cbind(data_names, null_counts, cardinality_counts, col_type, col_class)
count_summary<-data.frame(count_summary)
count_summary


rdate_names<-paste0("rdate_",3:24)

adate_names<-paste0("adate_",2:24)

variables_to_remove<-c(rdate_names, adate_names, "rfa_2r", "rfa_2f", "rfa_2a", "mdmaud_r", "mdmaud_f", "mdmaud_a", "hphone_d", "id", "dob", "nextdate", "timelag", "msa", "adi", "dma")




# pdf("histogram_of_variables.pdf")
# for(name in data_names){
#   if(class(training_data[,name])=="factor"){
#     counts <- table(training_data[,name])
#     barplot(counts, main=name)
#   } else{
#     plot_data<-as.numeric(as.character(training_data[,name]))
#     plot_data<-as.data.frame(plot_data)
#     names(plot_data)<-"value"
#     hist(plot_data$value, main=name)
#   }
# }
# dev.off()




head(training_data$dob)

# large missing in dob coded as 0
# use age variable instead
# gender has values other than listed
# wealth is surprisingly linear
print(table(training_data[,"gender"]))

# filter solp3!=0
print(table(training_data[,"solp3"]))

# filter solih=0
print(table(training_data[,"solih"]))

# wealth2 each has a different meaning

# pepstrfl what is this?
print(table(training_data[,"pepstrfl"]))

# population data is heavily right skewed

# filter maxadate to be before solih and solp3! filters

# aggregate up to id




# replacing nulls with 0s where it makes sense
print(table(training_data[,"numchld"]))
training_data[,"numchld"]<-replace(training_data[,"numchld"], which(is.na(training_data[,"numchld"])), 0)
training_data[,"mbcraft"]<-replace(training_data[,"mbcraft"], which(is.na(training_data[,"mbcraft"])), 0)
training_data[,"mbgarden"]<-replace(training_data[,"mbgarden"], which(is.na(training_data[,"mbgarden"])), 0)
training_data[,"mbbooks"]<-replace(training_data[,"mbbooks"], which(is.na(training_data[,"mbbooks"])), 0)
training_data[,"mbcolect"]<-replace(training_data[,"mbcolect"], which(is.na(training_data[,"mbcolect"])), 0)
training_data[,"magfaml"]<-replace(training_data[,"magfaml"], which(is.na(training_data[,"magfaml"])), 0)
training_data[,"magfem"]<-replace(training_data[,"magfem"], which(is.na(training_data[,"magfem"])), 0)
training_data[,"magmale"]<-replace(training_data[,"magmale"], which(is.na(training_data[,"magmale"])), 0)
training_data[,"mbgarden"]<-replace(training_data[,"mbgarden"], which(is.na(training_data[,"mbgarden"])), 0)
training_data[,"pubgardn"]<-replace(training_data[,"pubgardn"], which(is.na(training_data[,"pubgardn"])), 0)
training_data[,"pubculin"]<-replace(training_data[,"pubculin"], which(is.na(training_data[,"pubculin"])), 0)
training_data[,"pubhlth"]<-replace(training_data[,"pubhlth"], which(is.na(training_data[,"pubhlth"])), 0)
training_data[,"pubdoity"]<-replace(training_data[,"pubdoity"], which(is.na(training_data[,"pubdoity"])), 0)
training_data[,"pubdoity"]<-replace(training_data[,"pubdoity"], which(is.na(training_data[,"pubdoity"])), 0)
training_data[,"pubnewfn"]<-replace(training_data[,"pubnewfn"], which(is.na(training_data[,"pubnewfn"])), 0)
training_data[,"pubphoto"]<-replace(training_data[,"pubphoto"], which(is.na(training_data[,"pubphoto"])), 0)
training_data[,"pubopp"]<-replace(training_data[,"pubopp"], which(is.na(training_data[,"pubopp"])), 0)
training_data[,"amount"]<-replace(training_data[,"amount"], which(is.na(training_data[,"amount"])), 0)

vars_to_coalesce<-paste0("ramnt_", 3:24)

for (var in vars_to_coalesce){
  training_data[,var]<-replace(training_data[,var], which(is.na(training_data[,var])), 0)
}

# creating a null category, turn into factor
training_data[,"geocode"]<-replace(training_data[,"geocode"], which(is.na(training_data[,"geocode"])), 0)
training_data[,"lifesrc"]<-replace(training_data[,"lifesrc"], which(is.na(training_data[,"lifesrc"])), 0)
training_data[,"geocode"]<-factor(training_data[,"geocode"])
training_data[,"lifesrc"]<-factor(training_data[,"lifesrc"])




# replacing nulls with means where it makes sense
training_data[,"cluster"]<-replace(training_data[,"cluster"], which(is.na(training_data[,"cluster"])), mean(training_data[,"cluster"], na.rm=T))
training_data[,"age"]<-replace(training_data[,"age"], which(is.na(training_data[,"age"])), mean(training_data[,"age"], na.rm=T))


training_data[,"income_range"]<-replace(training_data[,"income_range"], which(is.na(training_data[,"income_range"])), mode(training_data[,"income_range"]))
training_data[,"wealth1"]<-replace(training_data[,"wealth1"], which(is.na(training_data[,"wealth1"])), mode(training_data[,"wealth1"]))
training_data[,"wealth2"]<-replace(training_data[,"wealth2"], which(is.na(training_data[,"wealth2"])), mode(training_data[,"wealth2"]))


# training_data[,"wealth1"]<-replace(training_data[,"wealth1"], which(is.na(training_data[,"wealth1"])), mean(training_data[,"wealth1"], na.rm=T))
# training_data[,"wealth2"]<-replace(training_data[,"wealth2"], which(is.na(training_data[,"wealth2"])), mean(training_data[,"wealth2"], na.rm=T))
training_data[,"cluster2"]<-replace(training_data[,"cluster2"], which(is.na(training_data[,"cluster2"])), mean(training_data[,"cluster2"], na.rm=T))

# don't know what these are but they look numeric
training_data[,"msa"]<-replace(training_data[,"msa"], which(is.na(training_data[,"msa"])), mean(training_data[,"msa"], na.rm=T))
training_data[,"adi"]<-replace(training_data[,"adi"], which(is.na(training_data[,"adi"])), mean(training_data[,"adi"], na.rm=T))
training_data[,"dma"]<-replace(training_data[,"dma"], which(is.na(training_data[,"dma"])), mean(training_data[,"dma"], na.rm=T))



training_data[,"solp3"]<-replace(training_data[,"solp3"], which(is.na(training_data[,"solp3"])), 1)
training_data[,"solih"]<-replace(training_data[,"solih"], which(is.na(training_data[,"solih"])), 1)



adate_names
adate_index<-which(names(training_data)%in%adate_names)

training_data[, adate_index]=( -(((training_data[, adate_index])) %%100 - 6) + (floor((training_data[, adate_index])/100)-97)*-12)

# head( -((na.omit(training_data[, 362:384])) %%100 - 6) + (floor(na.omit(training_data[, 362:384])/100)-97)*-12)
#dt[, (newcol) := c(NA, diff(a))]



training_data$label_only_12<-as.numeric(training_data$adate_6<=12) + as.numeric(training_data$adate_16<=12)  + as.numeric(training_data$adate_10<=12) + as.numeric(training_data$adate_20<=12)
sum(training_data$label_only_12, na.rm=T)

# MAP NK - 3,14,24 , SK - 5
training_data$blank_wlabel_12<-as.numeric(training_data$adate_3<=12) + as.numeric(training_data$adate_14<=12)  + as.numeric(training_data$adate_24<=12) + as.numeric(training_data$adate_5<=12)
sum(training_data$blank_wlabel_12, na.rm=T)

# MAP x1 - 11, 21, G1 - 7, 17
training_data$notepad_12<-as.numeric(training_data$adate_11<=12) + as.numeric(training_data$adate_21<=12)  + as.numeric(training_data$adate_7<=12) + as.numeric(training_data$adate_17<=12)
sum(training_data$notepad_12, na.rm=T)

training_data$notepad_12<-as.numeric(training_data$adate_11<=12) + as.numeric(training_data$adate_21<=12)  + as.numeric(training_data$adate_7<=12) + as.numeric(training_data$adate_17<=12)
sum(training_data$notepad_12, na.rm=T)

# map cc 9, 19
training_data$calendar_12<-as.numeric(training_data$adate_9<=12) + as.numeric(training_data$adate_19<=12)
sum(training_data$calendar_12, na.rm=T)

# map fs 13, 23
training_data$folds_12<-as.numeric(training_data$adate_13<=12) + as.numeric(training_data$adate_23<=12)
sum(training_data$folds_12, na.rm=T)

# map tk 4,15
training_data$ty_12<-as.numeric(training_data$adate_4<=12) + as.numeric(training_data$adate_15<=12)
sum(training_data$ty_12, na.rm=T)

# map gk 8, 18
training_data$gc_12<-as.numeric(training_data$adate_8<=12) + as.numeric(training_data$adate_18<=12)
sum(training_data$gc_12, na.rm=T)

training_data$messages_12 <- rowSums(training_data[,362:384]<12, na.rm=T)
sum(training_data$messages_12, na.rm=T)

training_data$messages_24 <- rowSums(training_data[,362:384]<24, na.rm=T)
sum(training_data$messages_24, na.rm=T)

head(training_data[,413:434])





rdate_index<-adate_index<-which(names(training_data)%in%rdate_names)


training_data[, rdate_index]=( -(((training_data[, rdate_index])) %%100 - 6) + (floor((training_data[, rdate_index])/100)-97)*-12)
head(training_data[, rdate_index])

training_data$gifts_12 <- rowSums(training_data[,413:434]<12, na.rm=T)
sum(training_data$gifts_12, na.rm=T)

training_data$gifts_24 <- rowSums(training_data[,413:434]<24, na.rm=T)
sum(training_data$gifts_24, na.rm=T)



vars_to_coalesce<-c("label_only_12", "blank_wlabel_12", "notepad_12", "calendar_12", "folds_12", "ty_12", "gc_12")

for (var in vars_to_coalesce){
  training_data[,var]<-replace(training_data[,var], which(is.na(training_data[,var])), 0)
}





head( -((na.omit(training_data[, 362:384])) %%100 - 6) + (floor(na.omit(training_data[, 362:384])/100)-97)*-12)
head(na.omit(training_data[, 362:384]%%100))


# diagnosing area
print(table(training_data[,"income_range"]))
print( unique(training_data[,"income_range"]))
sum(is.na(training_data[,"income_range"]))




# dont model people that unmessageable
training_data2<-training_data[which(training_data$solp3!=0 & training_data$solih!=0),]

#recount after filter
data_names_2<-names(training_data2)
cardinality_counts_2<-sapply(1:length(names(training_data2)), function(x) length(unique(training_data2[,data_names_2[x]])))


training_data2<-training_data2[,-which(as.numeric(cardinality_counts_2)==1)]

data_names_2<-names(training_data2)
col_class_2<-sapply(1:length(data_names_2), function(x) class(training_data2[,data_names_2[x]]))
col_class_2

length(which(col_class=="factor")) + length(which(col_class_2!="factor"))

remove_index<-which( data_names_2 %in% variables_to_remove)



training_data2<-training_data2[,-remove_index]
data_names_2<-names(training_data2)
col_class_2<-sapply(1:length(data_names_2), function(x) class(training_data2[,data_names_2[x]]))

positive_labels<-which(training_data2$responded==1)


negative_labels<-which(training_data2$responded==0)
negative_labels<-sample(negative_labels, length(positive_labels)*4)

training_data2<-training_data2[c(positive_labels,negative_labels),]

xfactors <- sparse.model.matrix(responded ~ ., data=training_data2[,c("responded",data_names_2[which(col_class_2=="factor" & !data_names_2%in%  c("responded", "amount"))])])[, -1]

col_class_2<-sapply(1:length(data_names_2), function(x) class(training_data2[,data_names_2[x]]))

# predictors<-cbind(data.matrix(training_data2[,which(col_class_2!="factor")]), xfactors)

predictors<-data.matrix(training_data2[,which(col_class_2=="numeric" & !data_names_2%in%  c("responded", "amount"))])


# predictors<-cbind(data.matrix(training_data2[,data_names_2[which(col_class_2=="numeric" & !data_names_2%in%  c("responded", "amount"))]]), xfactors)

sum(is.na(predictors))

design_matrix<-replace(predictors, which(is.na(predictors), arr.ind=T), 0)

unique( which(is.na(predictors), arr.ind=T))



#fit = glmnet(x=design_matrix, y=training_data2$amount  )
cv.out.auc<-cv.glmnet(x=design_matrix, y=as.factor(training_data2$responded),  nfolds=5,family="binomial", type.measure="auc", parallel= T)
plot(cv.out.auc)


cv.out<-cv.glmnet(x=design_matrix, y=as.factor(training_data2$responded),  nfolds=5,family="binomial",  parallel= T)
plot(cv.out)

linear.cv.out<-cv.glmnet(x=design_matrix, y=as.numeric(training_data2$amount) ,  nfolds=5,parallel= T)
plot(linear.cv.out)
