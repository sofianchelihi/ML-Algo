library(nnet)
library(mlbench)
data(Vehicle)
data=Vehicle
dim(data)
str(data)

n=dim(data)[1]
index = sample(n, 0.7 * n)
Appren = data[index, ]
Test = data[-index, ]
RN <- nnet(Class~., Appren, size = 4,entropy=TRUE)
summary(RN)

pred=predict(RN,Test, type="class")
pred
Confusion = table(Test$Class,pred)
Confusion

err<- 1-sum(diag(Confusion))/sum(Confusion)
err

library(e1071)

tune.nnet(Class~.,data=data,size=4:5,decay=1:10)
RN <- nnet(Class ~ ., data = Appren,decay=5, size = 4, entropy=TRUE, maxit=500)
summary(RN)
pred=predict(RN,Test, type="class")
pred
Confusion = table(Test$Class,pred)
Confusion

err<- 1-sum(diag(Confusion))/sum(Confusion)
err

pred2 = predict(RN, Appren, type="class")
conf2 = table(Appren$Class, pred2)
conf2

err2 = 1 - sum(diag(conf2) ) / sum(conf2)
err2

library(FSelector)
weights = information.gain(Class~., data)
weights

subset = cutoff.k.percent(weights, 0.5)
subset

f = as.simple.formula(subset, "Class")
f

RN2 = nnet(f, data=Appren, size=4, entropy=TRUE, maxit=500)
RN2


summary(RN2)


pred3 = predict(RN2, Test, type="class")
conf3 = table(Test$Class, pred3)
conf3




