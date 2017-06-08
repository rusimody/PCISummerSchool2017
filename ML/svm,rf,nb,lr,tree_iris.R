library(e1071)
library(randomForest)
library(rpart)
library(rpart.plot)

data=iris
cl=data[,5]
data=data[,-5]

s=sample(1:nrow(data),nrow(data)/5)

train=data[-s,]
cl_train=cl[-s]
test=data[s,]
cl_test=cl[s]

#svm

model_svm=svm(train,cl_train)
pred_svm=predict(model_svm,test)
t_svm=table(pred_svm,cl_test)
acc_svm=sum(diag(t_svm))/sum(t_svm)
print(acc_svm)

#rf

model_rf=randomForest(train,cl_train)
pred_rf=predict(model_rf,test)
t_rf=table(pred_rf,cl_test)
acc_rf=sum(diag(t_rf))/sum(t_rf)
print(acc_rf)

#naive b

model_nb=naiveBayes(train,cl_train)
pred_nb=predict(model_nb,test)
t_nb=table(pred_nb,cl_test)
acc_nb=sum(diag(t_nb))/sum(t_nb)
print(acc_nb)

# logistic regression

data=iris

data$virginica <- data$Species == "virginica"
data$Species <- NULL
#plot(x, col=x$virginica+1)

s=sample(1:nrow(data),nrow(data)/5)

train=data[-s,]
#cl_train=cl[-s]
test=data[s,]
#cl_test=cl[s]

model <- glm(virginica ~ .,family = binomial(logit), data=train)
model2 <- step(model, data = train)

pr <- predict(model2, test, type="response")
pr=round(pr, 2)

t_lr=table(actual=test$virginica, predicted=pr>0.5)
acc_lr=sum(diag(t_lr))/sum(t_lr)
print(acc_lr)


#trees rpart

data=iris
s=sample(1:nrow(data),nrow(data)/5)
train=data[-s,]
test=data[s,]

model_tree <- rpart(Species ~ ., data = train, method = "class")
rpart.plot(model_tree)

pred_tree=predict(model_tree,test,type="class")
t_tree=table(pred_tree,test$Species)
acc_tree=sum(diag(t_tree))/sum(t_tree)
print(acc_tree)
