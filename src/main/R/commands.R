
####################### fitting models ##################################################


designMatrix <- cbind(data.table(listing_id =json_data$id , label =response ),processed_data)


response = value_counts$post_video_retention_graph_clicked_to_play
designMatrix <- processed_data

set.seed(998)
inTraining <- createDataPartition(response, p = .75, list = FALSE, na.rm=T)
training <- designMatrix[ inTraining,]
testing  <- designMatrix[-inTraining,]




fitControl <- trainControl(## 10-fold CV
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
