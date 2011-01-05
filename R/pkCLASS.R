pkCLASS <-
function(dat)
{
set.seed(30)
dat=dat[,-1]
classes=as.factor(c(rep(0,dim(dat)[2]/2),rep(1,dim(dat)[2]/2)))
colnames(dat)=as.character(dat[1,])
 
dat=t(dat)
rf <- randomForest(x=dat, y = classes, ntree=500, importance=TRUE)
 
 # check the confusion matrix
 conf.matrix <- rf$confusion[,-3]
 (acc.test <- sum(diag(conf.matrix))/length(classes))
 (sens <- conf.matrix[1,1]/sum(conf.matrix[,1]))
 (spec <- conf.matrix[2,2]/sum(conf.matrix[,2]))
 (prec <- conf.matrix[1,1]/sum(conf.matrix[1,]))

 # plot ROC curve and calculate AUC
 pred <- prediction(rf$votes[,2], classes)
 perf <- performance(pred,"tpr","fpr")
 auc <- performance(pred,"auc")
 perf10 <- performance(pred, "sens", "spec")

return(list(accuracy=acc.test,sensitivity = sens,specificity = spec,precision=prec,
auc=auc@y.values,perf = perf))
}

