
library(mlbench)
library(e1071)
library(nnet)
library(rpart)
library(rpart.plot)
library(randomForest)
data(Vehicle)
data = Vehicle


#decision tree
n = dim(data)[1]
index = sample(n, 0.7*n)
appren = data[index,]
test = data[-index,]


#arbre de decision
t1 = Sys.time()
tree = rpart(Class~., data=appren)
t2 = Sys.time()

print("temps execution")
print(t2 - t1)

pred = predict(tree, test, type="class")
tab = table(test$Class, pred)


err<- 1-sum(diag(tab))/sum(tab)
err
#####################################################


#random forest
t1 = Sys.time()
rf = randomForest(Class~., data=appren)
t2 = Sys.time()
print(t2 - t1)


pred2 = predict(rf, test, type="class")
tab2 = table(test$Class, pred2)
err2<- 1-sum(diag(tab2))/sum(tab2)
err2


####################################################
#svm

t1 = Sys.time()
model <- svm(Class ~ ., data = appren)
t2 = Sys.time()
print(t2 - t1)

model
# Predict on the testing set
pred3 <- predict(model, newdata = test)

tab3 = table(test$Class, pred3)
err3 = 1-sum(diag(tab3))/sum(tab3)
err3
####################################################


t1 = Sys.time()
RN <- nnet(Class~., appren, size = 4,entropy=TRUE)
t2 = Sys.time()
print(t2 - t1)

pred5=predict(RN,test, type="class")
tab5 = table(test$Class,pred5)
err5<- 1-sum(diag(tab5))/sum(tab5)
err5

















