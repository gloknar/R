library(caret)
library(pROC)
data(iris)
iris <- iris[iris$Species == "virginica" | iris$Species == "versicolor", ]
iris$Species <- factor(iris$Species)  # setosa should be removed from factor

samples <- sample(NROW(iris), NROW(iris) * .5) # Otra manera de hacer el CreateDatapartition de caret
data.train <- iris[samples, ]
data.test <- iris[-samples, ]
forest.model <- train(Species ~., data.train)

result.predicted.prob <- predict(forest.model, data.test, type = "prob") # Prediction

result.roc <- roc(data.test$Species, result.predicted.prob$versicolor) # Draw ROC curve.
print(result.roc$auc)
plot(result.roc, print.thres="best", print.thres.best.method="closest.topleft")

result.coords <- coords(result.roc, "best", best.method="closest.topleft", ret=c("threshold", "accuracy"))
print(result.coords)#to get threshold and accuracy




