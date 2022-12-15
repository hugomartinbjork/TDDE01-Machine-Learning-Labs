######################## Lab2 Assignment1 ######################################
################################################################################

######################### SETUP ################################################

data <- read.csv("data/tecator.csv")
n <- dim(data)[1]
set.seed(12345)
index <- sample(1:n, floor(n * 0.5))
train <- data[index, ]

# Retrieving Test set
index1 <- setdiff(1:n, index)
test <- data[index1, ]

############################### Exercise 1 #####################################
library(dplyr)
train.features <- as.matrix(train %>% select(Channel1:Fat))
test.features <- as.matrix(test %>% select(Channel1:Fat))
fit.train <- lm(Fat ~ ., data = as.data.frame(train.features))
plot(fit.train)
summary(fit.train)
pred.train <- predict(object = fit.train, newdata = train, type = "response")
mse.train <- mean(residuals(fit.train)^2)

pred.test <- predict(object = fit.train, newdata = test, type = "response")
mse.test <- mean((test$Fat - pred.test)^2)
plot(pred.test, col = "blue", ylim = c(0, 110), type = "p")
points(test$Fat, col = "red", type = "p")
############################ Exercise 1 ########################################

############################## Lasso reg #######################################
# install.packages("glmnet")
library(glmnet)
train.x <- train.features[, -101]
train.y <- train.features[, 101]
####### Lasso repgression (alpha=1) with cross validation folds (defalut=10)
# and 100 values for lambda
# Set to the Gaussian family since we're doing linear regression, if it was
# logistic --> binomial
fit.lasso <- cv.glmnet(train.x, train.y, alpha = 1, family = "gaussian")
best_lambda <- fit.lasso$lambda.min
plot(fit.lasso$glmnet.fit, xvar = "lambda")

plot(coef(fit.lasso, s = "lambda.min"),
  pch = 20, col = "black",
  ylab = "Coefficents"
)

plot(fit.lasso, xlim = c(-4.1, -2.7), ylim = c(11, 18))
log(best_lambda)


df3 <- which(fit.lasso$glmnet.fit$df == 3)
fit.lasso$glmnet.fit$lambda[df3]
fit.lasso.best <- glmnet(train.x, train.y,
  alpha = 1, lambda = best_lambda,
  family = "gaussian"
)

fit.ridge <- cv.glmnet(train.x, train.y, alpha = 0, family = "gaussian")
plot(fit.ridge$glmnet.fit, xvar = "lambda")



# I make a prediction on the test.features using my best Lasso regression from 
#the training data
pred <- predict(fit.lasso, newx = test.features[, -101], s = "lambda.min")

mse.test.2 <- mean((test$Fat - pred)^2)

plot(pred, pch = 20, col = "darkgreen", type = "b", ylab = "Fat")
points(test.features[, 101], pch = 20, col = "darkred", type = "b")
legend("topleft",
  legend = c("Predicted values", "Test results"),
  col = c("darkgreen", "darkred"), lty = 1:2, cex = 0.8, bg = "lightblue"
)


#################### My own lasso and opt ######################################
## I made my won Lasso to compare with the built in glmnet function, the mean
## Square error where very similar but actually a little better for my own
## Lasso.

Lasso <- function(x, y, lambda, theta) {
  return(sum((y - x %*% theta)^2) + (lambda * sum(abs(theta))))
}
# My ridge optimization tries different values for theta, and lambda 
#to find the optimal values.
LassoOpt <- function(theta, lambda, x, y) {
  return(optim(
    par = theta, fn = Lasso,
    x = x, y = y, lambda = lambda, method = "BFGS"
  ))
}
theta.opt.1 <- LassoOpt(
  theta = as.matrix(rep(1, 100)), lambda = best_lambda,
  x = train.features[, -101], y = train.features[, 101]
)
plot(test.features[, 101], pch = 20, col = "darkred", type = "b", 
     ylim = c(0, 60))
points(test.features[, -101] %*% theta.opt.1[["par"]],
  pch = 20,
  col = "darkgreen", type = "b", ylab = "Fat"
)
a <- pred.test - test.features[, -101] %*% theta.opt.1[["par"]]
mse.test3 <- mean((test$Fat - test.features[, -101] %*% theta.opt.1[["par"]])^2)
############################ Lab2 Assignment1 ##################################
