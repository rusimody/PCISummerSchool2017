library(e1071)
library(randomForest)

#read dataset
setwd("F:/ML/cms/data")
E=read.csv("car.csv")

#have a look
print(head(E))
str(E)

#convert to numeric
E=data.matrix(E)

#separate class and convert to factor
cl=as.factor(E[,7])
E=E[,-7]

#split into train-test
s=sample(1:nrow(E),nrow(E)/5) #randomly sample 20 % for test
train=E[-s,]
cl_train=cl[-s]
val=E[s,]
cl_val=cl[s]

train_control <- trainControl(method="cv", number=10)
model <- train(train,cl_train,trControl= train_control,method = "svmLinear") 
print(model)
pred_svm=predict(model,test)
t_svm=table(pred_svm,cl_test)
acc_svm=sum(diag(t_svm))/sum(t_svm)
print(acc_svm)
