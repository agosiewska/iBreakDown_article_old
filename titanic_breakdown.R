library(DALEX2)
library(iBreakDown)
library(reticulate)

titanic_train <- read.csv("titanic_bd.csv")
head(titanic_train)

X_train <- titanic_train[,-c(1,2)]
Y_train <- titanic_train$Survived

rf <- py_load_object("rf_classifier.pkl", pickle = "pickle")

predict_function <- function(model, newdata){
  model$predict_proba(newdata)[,2]
}
predict_function(rf, X_train[137, ])


# iBreakDown
rf_explain <- explain(rf, data = X_train,
                      y = Y_train, label = "Random Forest",
                      predict_function = predict_function)

i <- 638
passanger <- X_train[i,]
rf_la <- local_attributions(rf_explain, new_observation = passanger)
plot(rf_la)

#609
#138