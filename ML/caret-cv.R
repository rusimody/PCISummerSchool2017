data=iris
cl=data[,5]
data=data[,-5]

s=sample(1:nrow(data),nrow(data)/5)

train=data[-s,]
cl_train=cl[-s]
test=data[s,]
cl_test=cl[s]

train_control <- trainControl(method="cv", number=10)
model <- train(train,cl_train,trControl= train_control,method = "svmLinear") 