# Prepare Data
data <- Cardiotocographic
str(data)
#Partition data into Training and Validation datasets
set.seed(1234)
pd <- sample(2,nrow(data),replace = TRUE,prob = c(0.8,0.2))
train <-data[pd==1,]
validate <- data[pd==2,]
# Decision Tree with party
library(party)
tree <- ctree(NSPF~LB+AC+FM,data = train,controls = ctree_control(mincriterion = 0.99,minsplit = 500))
tree
plot(tree)
#Predict
predict(tree,validate)
#Decision tree with rpart
library(rpart)
tree1 <- rpart(NSPF~LB+AC+FM,train)
library(rpart.plot)
rpart.plot(tree1,extra=4)
#Prediction
predict(tree1,validate)

#Misclassification error for 'train' data
tab <- table(predict(tree),train$NSPF)
print(tab)
1-sum(diag(tab))/sum(tab)
library(nnet)
mymodel <- multinom(out~LB+AC+FM,data = data)
#K MEANS
data.f = data
data.f$Type <-NULL
View(data.f)
data.stand <- scale(data.f[-1])
results <- kmeans(data.features,3)
results
results$size
results$cluster
table(data$NSPF,results$cluster)
