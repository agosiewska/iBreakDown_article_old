library(randomForest)
library(breakDown2)
library(shapper)
library(ggplot2)
library(DALEX2)
library(lime)
library(caret)

calculate_y <- function(x1, x2){
  n <- length(x1)
  res <- ifelse( x1*x2 >= 0, 1, -1)
  return(res)
}
calculate_y2 <-function(x1, x2){
  x1*x2
}
calculate_y3 <-function(x1, x2){
  res <- ifelse (abs(x1)<=1 & abs(x2)<=1, x1*x2, calculate_y(x1,x2))
  res
}


num_obs <-3000


set.seed(123)
x1<- runif(num_obs, -5, 5)
x2<- runif(num_obs, -5, 5)
y <- calculate_y3(x1, x2) + rnorm(num_obs, 0, 0.001)
dat <- data.frame(x1 = x1, x2 = x2, y = y)

model <- train(y ~ x1 + x2, data = dat, method = "rf", trControl=trainControl(method="none"))

explainer <- DALEX2::explain(model = model, data = dat[,-3], y = y)

new_observation <- data.frame(x1 = 0, x2 = 0)
predict(model, newdata = new_observation)

bd <- local_interactions(explainer,
                            new_observation =  new_observation,
                            keep_distributions = TRUE)
bd
plot(bd)
ggsave("bd_toy.pdf", width = 4, height = 2)

ive <- shap(explainer, new_observation = new_observation)
ive
plot(ive)
ggsave("shap_toy.pdf", width = 4, height = 2.5)

expl_lime <- lime(dat, model)
lme <- explain(new_observation, expl_lime, n_features = 2)
plot_features(lme)
ggsave("lime_toy.pdf", width = 4, height = 2.5)



# ALternative 
calculate_y4 <-function(x1, x2){
  res <- ifelse (abs(x1)<=1 & abs(x2)<=1, x1*x2, 1)
  res
}



set.seed(123)
x1<- runif(num_obs, -5, 5)
x2<- runif(num_obs, -5, 5)
y <- calculate_y4(x1, x2) + rnorm(num_obs, 0, 0.001)
dat <- data.frame(x1 = x1, x2 = x2, y = y)

model <- train(y ~ x1 + x2, data = dat, method = "rf", trControl=trainControl(method="none"))

explainer <- DALEX2::explain(model = model, data = dat[,-3], y = y)

new_observation <- data.frame(x1 = 0, x2 = 0)
predict(model, newdata = new_observation)

bd <- local_interactions(explainer,
                         new_observation =  new_observation,
                         keep_distributions = TRUE)
bd
plot(bd)
ggsave("bd_toy2.pdf", width = 4, height = 2)

ive <- shap(explainer, new_observation = new_observation)
ive
plot(ive)
ggsave("shap_toy2.pdf", width = 4, height = 2.5)

expl_lime <- lime(dat, model)
lme <- explain(new_observation, expl_lime, n_features = 2)
plot_features(lme)
ggsave("lime_toy2.pdf", width = 4, height = 2.5)






