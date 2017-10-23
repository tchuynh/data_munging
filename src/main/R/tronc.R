
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

library(formattable) # nice output

library(RColorBrewer)
library(tm)
library(SnowballC)
library(wordcloud)


library(ggthemes)

library(caTools)
library(xgboost)
registerDoParallel(cores=as.numeric(4))

setwd("~/data_munging/src/main")







library("jsonlite")
json_file <-  "C:/Users/tchuy_000/Documents/data_munging/src/main/posts.json"
json_data <- stream_in(file(json_file))


dim(json_data)
typeof(json_data)
class(json_data)
names(json_data)
head(json_data)


# message
head(json_data$message)

# link 
head(json_data$link)

# link_to_post
head(json_data$link_to_post)

# name
# create categorical variables for anything over 1
head(json_data$name)
head(order(table(json_data$name, useNA ="always"), decreasing=T))
head(table(json_data$name, useNA ="always")[order(table(json_data$name, useNA ="always"), decreasing=T)])
typeof(table(json_data$name, useNA ="always"))
class(table(json_data$name, useNA ="always"))

# description
head(json_data$description)

# id
head(json_data$id)

# picture
head(json_data$picture)

# created_time
head(json_data$created_time)
plot.ts(json_data$created_time)

typeof(json_data$created_time[1])

class(json_data$created_time[1])

# story
head(na.omit(json_data$story), n=20)
plot.ts(json_data$created_time)

# feed_targeting
head(json_data$feed_targeting)
json_data$feed_targeting<-json_data$feed_targeting[[1]]
table(json_data$feed_targeting, useNA ="always")


data_names<-names(json_data)
count_summary<-count_summary_function(json_data)
count_summary


# insights
insights<-json_data$insights
head(insights)
typeof(insights)
class(insights)
names(insights)

insights_summary<-count_summary_function(insights)
insights_summary


insight_names<-names(insights)
insights_counts<-insights



null_counts<-insights
for(name in insight_names){
  print(name)
  null_counts[,name]<-sapply(1:8639, function(x) sum(is.na(unlist(insights[,name]$values[[x]]))))
}

apply(null_counts, 2, sum, na.rm=T)


value_counts<-insights
for(name in insight_names){
  print(name)
  value_counts[,name]<-sapply(1:8639, function(x) mean(as.numeric(unlist(insights[,name]$values[[x]])), na.omit=T))
}


save_histograms(plot_name="value_means.pdf", input_data_frame=value_counts)

apply(value_counts, 2, mean, na.rm=T)


# length of each value entry
value_lengths<-insights
for(name in insight_names){
  print(name)
  value_lengths[,name]<-sapply(1:8639, function(x) length(as.numeric(unlist(insights[,name]$values[[x]]))))
}

apply(value_lengths, 2, mean, na.rm=T)
apply(value_lengths, 2, summary, na.rm=T)
count_summary_function(value_lengths)




value_unique_length<-rep(NA, dim(insights)[2])
names(value_unique_length)<-names(insights)

for(name in insight_names){
  print(name)
  value_unique_length[name]<-length(unique(unlist(sapply(1:8639, function(x) unlist(insights[,name]$values[[x]])))))
}

barplot(
  value_unique_length,
  main = "Term Frequencies",
  horiz = T,
  angle = 180,
  las = 1,
  xlab = "Frequency"
)



# most of these are zero but some of them have very high values
low_cardinality<-c("post_video_complete_views_paid_unique",
                   "post_video_complete_views_30s_paid",
                   "post_video_views_paid",
                   "post_impressions_paid",
                   "post_impressions_fan_paid_unique",
                   "post_impressions_paid_unique",
                   "post_impressions_fan_paid",
                   "post_video_views_paid_unique",
                   "post_video_complete_views_paid")


for(name in low_cardinality){
  print(name)
print(table(unlist(sapply(1:8639, function(x) as.numeric(unlist(insights[,name]$values[[x]])))), useNA = c("always")))
print(length(unique(unlist(sapply(1:8639, function(x) unlist(insights[,name]$values[[x]]))))))
}

head(as.numeric(unlist(sapply(1:8639, function(x) unlist(insights[,name]$values[[x]])))))

head(as.numeric(unlist(sapply(1:8639, function(x) unlist(insights[,"post_video_retention_graph_clicked_to_play"]$values[[x]])))))

print(name)
print(head(insights[, "post_negative_feedback_by_type_unique"]))




############ kaggle #####################
corpus <- Corpus(VectorSource(json_data$description))

# convert to lower case letter
corpus <- tm_map(corpus, tolower)

# remove punctuation
corpus <- tm_map(corpus, removePunctuation)
# remove numbers
corpus <- tm_map(corpus, removeNumbers)
# remove english stopwords
corpus <- tm_map(corpus, removeWords, stopwords("english"))

corpus <- tm_map(corpus, stripWhitespace)


json_data$description[1]
corpus[[1]]$content[1]


tdm <- TermDocumentMatrix(corpus)
tdm


sparse <- removeSparseTerms(tdm, 0.985)
sparse



################ bar plot ########################

termFreq <- rowSums(as.matrix(sparse))
termFreq <- subset(termFreq, termFreq >= 300)


barplot(
  termFreq,
  main = "Term Frequencies",
  horiz = TRUE,
  angle = 180,
  las = 1,
  xlab = "Frequency"
)



############# word cloud ###########################
dtm <- removeSparseTerms(DocumentTermMatrix(corpus), 0.99)

# convert to data.frame
sparseDF <- as.data.frame(as.matrix(dtm))


# setting color palette for wordcloud
pal <- brewer.pal(7, "Dark2")
# create wordcloud
wordcloud(
  colnames(sparseDF),
  colSums(sparseDF),
  scale = c(2.5, 0.75),
  max.words = 150,
  colors = pal
)




############## following tutorial ##############################

docterm_corpus <- DocumentTermMatrix(corpus)
dim(docterm_corpus)


new_docterm_corpus <- removeSparseTerms(docterm_corpus,sparse = 0.99)
dim(new_docterm_corpus)


#find frequent terms
colS <- colSums(as.matrix(new_docterm_corpus))
length(colS)
doc_features <- data.table(name = attributes(colS)$names, count = colS)

#most frequent and least frequent words
doc_features[order(-count)][1:10] #top 10 most frequent words
doc_features[order(count)][1:10] #least 10 frequent words




ggplot(doc_features[count>200],aes(name, count)) +geom_bar(stat = "identity",fill='lightblue',color='black')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


################# TRAINING SET #####################################

#create data set for training
processed_data <- as.data.table(as.matrix(new_docterm_corpus))

#combing the data
data_one <- cbind(data.table(listing_id =json_data$id , interest_level = value_counts$post_video_retention_graph_clicked_to_play),processed_data)

#merging the features
data_one <- fdata[data_one, on="listing_id"]
train_one <- data_one

#split the data set into train and test
train_one <- data_one[interest_level != "None"]
test_one <- data_one[interest_level == "None"]
test_one[,interest_level := NULL]







######################  FITTING MODELS ###########################################


set.seed(998)
response = value_counts$post_video_retention_graph_clicked_to_play
predictors<- processed_data
designMatrix <- cbind(data.table(listing_id =json_data$id , label =response ),processed_data)

fitControl <- trainControl(
  method = "cv",
  number = 4)


gbmFit1 <- train(label ~ ., data = designMatrix, 
                 method = "gbm", 
                 trControl = fitControl,
                 verbose = FALSE)


gbmFit1
relative.influence(gbmFit1$finalModel)
plot.gbm(gbmFit1$finalModel)
summary(gbmFit1$finalModel)




rfFit1 <- train(label ~ ., data = designMatrix, 
                method = "rf", 
                trControl = fitControl,
                verbose = TRUE)
rfFit1
                 
varImpPlot(rfFit1$finalModel, sort=T)

importance(rfFit1$finalModel)[order(importance(rfFit1$finalModel)[,1], decreasing=T),]



response<-replace(response, which(is.na(response)), 0)

glmnetFit1 <- train(predictors, response, 
                    method = "glmnet", 
                    trControl = fitControl,
                    preProc = c("center", "scale"),
                    tuneGrid = expand.grid(.alpha = seq(0.0, 1, length = 15),
                                           .lambda = c((1:5)/10))
                    , na.action = na.pass)


######################## n grams ############################################

install.packages("RWeka")
library(RWeka)

#bigram function
Bigram_Tokenizer <- function(x){` `NGramTokenizer(x, Weka_control(min=2, max=2))` `}

#create a matrix
bi_docterm_matrix <- DocumentTermMatrix(text_corpus, control = list(tokenize = Bigram_Tokenizer))





#stratified splitting the data
sp <- sample.split(Y = train_one$interest_level,SplitRatio = 0.6)

#create data for xgboost
xg_val <- train_one[sp]
listing_id <- train_one$listing_id
target <- train_one$interest_level

xg_val_target <- target[sp]

d_train <- xgb.DMatrix(data = as.matrix(train_one[,-c("listing_id","interest_level"),with=F]),label = target)
d_val <- xgb.DMatrix(data = as.matrix(xg_val[,-c("listing_id","interest_level"),with=F]), label = xg_val_target)
d_test <- xgb.DMatrix(data = as.matrix(test_one[,-c("listing_id"),with=F]))

param <- list(booster="gbtree", eta = .02, gamma = 1, max_depth = 4, min_child_weight = 1, subsample = 0.7, colsample_bytree = 0.5)

set.seed(2017)
watch <- list(val=d_val, train=d_train)

xgb2 <- xgb.train(data = d_train,  watchlist=watch, nrounds = 500, print_every_n = 10)





#################### PRINTING HISTOGRAMS ##########################
save_histograms<-function(plot_name, input_data_frame){
data_names<-names(input_data_frame)
pdf(plot_name)
for(name in data_names){
  if(class(input_data_frame[,name])=="factor"){
    counts <- table(input_data_frame[,name])
    barplot(counts, main=name)
  } else{
    plot_data<-log10(as.numeric(as.character(input_data_frame[,name])))
    plot_data<-as.data.frame(plot_data)
    names(plot_data)<-"value"
    hist(plot_data$value, main=name)
  }
}
corrgram(input_data_frame)
dev.off()
}

table(input_data_frame$id)



# one id is repeated
length(unique(json_data$id))
dim(json_data)




############### getting summary of data frames ################################
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



################# gettting average counts for values ########################

value_counts<-insights
for(name in insight_names){
  print(name)
  # print(head(insights[, name]$values))
  value_counts[,name]<-mean(sapply(1:8639, function(x) length(unlist(insights[, name]$values[[x]]))))
}
apply(value_counts, 2, mean)



