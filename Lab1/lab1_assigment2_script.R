############################################ Lab 1 #######################################################

## function pascal
## variables .
# install.packages("styler")
########################################### Assignment 2 #################################################
data <- read.csv("data/parkinsons.csv")

n <- dim(data)[1]

# Training
set.seed(12345)
index <- sample(1:n, floor(n * 0.6))
train <- data[index, ]
# Test
index1 <- setdiff(1:n, index)
test <- data[index1, ]

# install.packages("caret")
library(caret)
scaler <- preProcess(train)
trainS <- predict(scaler, train)
testS <- predict(scaler, test)

#lm is used to fit linear models. Can be used for analysis.
fit.train <- lm(motor_UPDRS ~ . - subject. - age - sex - test_time - total_UPDRS, data = trainS)
fit.test <- lm(motor_UPDRS ~ . - subject. - age - sex - test_time - total_UPDRS, data = testS)
summary(fit.train)
summary(fit.test)
predict(fit.train)
predict(fit.test)
coefficients(fit.train)
coefficients(fit.test)
mse.train <- mean(residuals(fit.train)^2) # residuals
mse.test <- mean(residuals(fit.test)^2)

## (3) Loglikelihood, Ridge, RidgeOpt, DF
# install.packages("dplyr")
library(dplyr)
X <- as.matrix(trainS %>% select(Jitter...:PPE))
Y <- as.matrix(trainS$motor_UPDRS)
N <- dim(train)[1]

# The log-likelihood function is used to calculate the probability of a set of data given a model.
LogLikelihood <- function(theta) {
  sigma <- tail(theta, n = 1)
  return(-N/2*log(2 * pi*sigma^2) - ((1 / (2 *sigma^2))*
                                       (sum((Y - X %*% theta[-17])^2) )))
}
# Ridge way one
Ridge <- function(theta, lambda) {
  return( (lambda * sum(theta[-17]^2)) - LogLikelihood(theta))
}

#My ridge optimization tries different values for theta, and lambda to find the optimal values.
RidgeOpt <- function(theta, lambda) {
return(optim(theta, fn = Ridge, lambda=lambda, method = "BFGS"))
}

theta.opt.1 <- RidgeOpt(theta=rep(1,17),lambda=1)
theta.opt.100 <- RidgeOpt(theta=rep(1,17),lambda=100)
theta.opt.1000 <- RidgeOpt(theta=rep(1,17),lambda=1000)

## A function to find out the degrees of freedom.
DF <- function(X, lambda){
  P <- X %*% ((t(X)%*%X + lambda*diag(dim(t(X)%*%X)[1]))^-1) %*% t(X)
  return(sum(diag(P)))
}

deg.fred <- DF(as.matrix(trainS %>% select(Jitter...:PPE)), 1)
deg.fred100 <- DF(as.matrix(trainS %>% select(Jitter...:PPE)), 100)
deg.fred1000 <- DF(as.matrix(trainS %>% select(Jitter...:PPE)), 1000)

# My predictions 
pred.train.1 <-  as.matrix(trainS %>% select(Jitter...:PPE)) %*% as.matrix(theta.opt.1[["par"]][-17])
pred.train.100 <-  as.matrix(trainS %>% select(Jitter...:PPE)) %*% as.matrix(theta.opt.100[["par"]][-17])
pred.train.1000 <-  as.matrix(trainS %>% select(Jitter...:PPE)) %*% as.matrix(theta.opt.1000[["par"]][-17])
pred.test.1 <-  as.matrix(testS %>% select(Jitter...:PPE)) %*% as.matrix(theta.opt.1[["par"]][-17])
pred.test.100 <-  as.matrix(testS %>% select(Jitter...:PPE)) %*% as.matrix(theta.opt.100[["par"]][-17])
pred.test.1000 <-  as.matrix(testS %>% select(Jitter...:PPE)) %*% as.matrix(theta.opt.1000[["par"]][-17])

#Mean squared error train
mse.train.1 <- mean((trainS$motor_UPDRS - pred.train.1)^2)
mse.train.100 <- mean((trainS$motor_UPDRS - pred.train.100)^2)
mse.train.1000 <- mean((trainS$motor_UPDRS - pred.train.1000)^2)

#Mean squared error tes
mse.test.1 <- mean((testS$motor_UPDRS - pred.test.1)^2)
mse.test.100 <- mean((testS$motor_UPDRS - pred.test.100)^2)
mse.test.1000 <- mean((testS$motor_UPDRS - pred.test.1000)^2)
plot(trainS$motor_UPDRS[1:30], frame = FALSE, pch = 19,
     col = "blue" )
lines(pred.train.1[1:30], pch=19, col = "red",)
lines(predict(fit.train)[1:30], pch=20, col = "green",)




