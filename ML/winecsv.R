library(e1071)
library(randomForest)
setwd("C:/Users/neel jambhekar/Desktop/persistent")
E=read.csv("wine.csv")
data=E

cl=as.factor(data[,1])
data=data[,-1]

s=sample(1:nrow(E),nrow(E)/5) #randomly sample 20 % for test

train=E[-s,]
cl_train=cl[-s]
val=E[s,]
cl_val=cl[s]

svm_cv=svm(train,cl_train,cross=5,
           kernel = "polynomial",
           cost = 100,gamma = 0.5,degree = 2,coef0 = 1)
print(svm_cv$accuracies)
print(svm_cv$tot.accuracy)

weight_svm=c(0.3,0.3,0.4)
names(weight_svm)=levels(cl_train)
svm_weighted=svm(train,cl_train,cross = 5,class.weights = weight_svm)

#build final model with entire training set (with tuned parameters) and use this to predict on validation set
model_svm=svm(train,cl_train,kernel = "polynomial",cost = 2.30,gamma = 0.1,degree = 1,coef0 = 1)
pred_svm=predict(model_svm,val)

#find accuracy
t_svm=table(pred_svm,cl_val)
acc_svm=sum(diag(t_svm))/sum(t_svm)
print(acc_svm)

#create submission
pred_mat=matrix(nrow = nrow(val),ncol = 2)
pred_mat[,1]=pred_svm
pred_mat[,2]=cl_val
colnames(pred_mat)=c("predicted","actual")
write.csv(pred_mat,"submit.csv",row.names = FALSE)

#rfcv
rf_cv=rfcv(train,cl_train,cv.fold = 5)
print(rf_cv$error.cv)

pred_rf=predict(model_rf,val)
t_rf=table(pred_rf,cl_val)
acc_rf=sum(diag(t_rf))/sum(t_rf)

#rf 
#train_control <- trainControl(method="cv", number=10)
#model <- train(train,cl_train,trControl= train_control,method = "rf",mrty=4,ntree= 100) 
model=train(train,cl_train,method= "cv",number = 10)
print(model)
