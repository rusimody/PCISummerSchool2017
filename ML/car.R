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
test=E[s,]
cl_test=cl[s]


#the codes will need the data to be in numeric form
model_svm=svm(train,cl_train)
pred_svm=predict(model_svm,test)
t_svm=table(pred_svm,cl_test)
acc_svm=sum(diag(t_svm))/sum(t_svm)

model_rf=randomForest(train,cl_train)
pred_rf=predict(model_rf,test)
t_rf=table(pred_rf,cl_test)
acc_rf=sum(diag(t_rf))/sum(t_rf)

model_nb=naiveBayes(train,cl_train)
pred_nb=predict(model_nb,test)
t_nb=table(pred_nb,cl_test)
acc_nb=sum(diag(t_nb))/sum(t_nb)

#we can see that rf gives a better result.
#one reason is because the data contains more categorical variables.