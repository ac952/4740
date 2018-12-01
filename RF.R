#Random forest
library(randomForest)
data_new=read.csv("train.csv") #replace with Xiaowen's code instead
data_new=data_new[,-1]

#determine ntree
OOBerr=array()
nt=seq(100,2000,by = 100)
set.seed(1)
for (i in 1:length(nt)){
  print(i)
  RF=randomForest(Category~.,data=data_new,ntree=nt[i])
  OOBerr[i]=1-sum(RF$predicted==data_new[,1])/nrow(data_new) #This can be viewed as test error
}
plot(nt,OOBerr,xlab="ntree",ylab="Out-of-bag error")
lines(nt,OOBerr)
#accuracy gets saturated when ntree>500 

#determine mtry
accurate_rate2=array()
OOBerr2=array()
set.seed(1)
mt=seq(50,700,by=50)
for (i in 1:14){
  print(i)
  RF=randomForest(Category~.,data=data_new,ntree=500,mtry=mt[i])
  OOBerr2[i]=1-sum(RF$predicted==data_new[,1])/nrow(data_new) #This can be viewed as test error
}
plot(mt,OOBerr2,xlab="mtry",ylab="Out-of-bag error")
lines(mt,OOBerr2)
#optimal range 50-150

#cross validation 
set.seed(1)
n_folds <- 10
folds_i <- sample(rep(1:n_folds, length.out = nrow(data_new)))
cv_err=array()
oob_err=array()
train_err=array()
for (k in 1:n_folds) {
  print(k)
  test_i <- which(folds_i == k)
  train_set <- data_new[-test_i, ]
  test_set <- data_new[test_i, ]
  RF=randomForest(Category~.,data=train_set,ntree=500,mtry=100)
  cv_err[k]=1-sum(predict(RF,test_set[,-1])==test_set[,1])/nrow(test_set)
  oob_err[k]=1-sum(RF$predicted==train_set[,1])/nrow(train_set)
  train_err[k]=1-sum(predict(RF,train_set[,-1])==train_set[,1])/nrow(train_set)
}
boxplot(data.frame(cv_error=cv_err,obb_error=oob_err))
#oob error is a good estimate of test error

#Training
set.seed(1)
RF=randomForest(Category~.,data=data_new,ntree=500,mtry=100)
1-sum(RF$predicted==data_new[,1])/nrow(data_new) #oob error
imp=data.frame(rownames(RF$importance),RF$importance)
imp=imp[order(imp[,2],decreasing = T),]
par(las=1)
barplot(rev(imp[1:10,2]), main="Most important feathers", horiz=TRUE,
        names.arg=rev(imp[1:10,1]),col="red",xlab="Mean Gini decreased")
par(las=0)
