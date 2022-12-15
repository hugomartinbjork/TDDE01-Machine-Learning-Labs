################################################################################
############################### Lab2 Assignment3 ###############################
################################################################################

########################## Functions  ##########################################
multi.mse.train <- c()
multi.mse.test <- c()
LmOptimization <- function(theta, train.x, train.y, test.x, test.y) {
  pred.train <- train.x %*% theta
  pred.test <- test.x %*% theta
  mse.train <- mean((train.y - pred.train)^2)
  mse.test <- mean((test.y - pred.test)^2)

  multi.mse.train <<- c(multi.mse.train, mse.train)
  multi.mse.test <<- c(multi.mse.test, mse.test)

  return(mse.train)
}
################################### Functions ##################################
################################### EXERCISE 1 #################################

# (1) Scale all variables except violent crimes per pop
data <- read.csv("data/communities.csv", stringsAsFactors = T)
library(dplyr)
data_no_v <- select(data, -ViolentCrimesPerPop)
data.scaled <- as.data.frame(cbind(
  scale(select(data, -ViolentCrimesPerPop)),
  subset(data, select = ViolentCrimesPerPop)
))

# Extracting eigenvalues way one#
cov <- cov(data.scaled)
eigen <- eigen(cov)
# Extracting eigenvalues way two#
res <- prcomp(data.scaled)
lambda <- res$sdev^2
# proportion of variation
sprintf("%2.3f", eigen$values / sum(eigen$values) * 100)
screeplot(res)
sum(lambda[1:34])

############################### EXERCISE 1 #####################################
############################## EXERCISE 2 ######################################
res.2 <- princomp(data.scaled)
summary(res.2)
u <- res.2$loadings
top.5 <- head(sort(abs(u[, 1]), decreasing = T), n = 5)
index.top.5 <- which(abs(u[, 1]) %in% top.5)
plot(u[, 1],
  main = "Traceplot, PC1", col = "lightgreen", pch = 8,
  ylab = "Weight"
)
points(index.top.5, u[index.top.5, 1], col = "red", pch = 20)

# install.packages("ggfortify")
library(ggfortify)
autoplot(res.2,
  colour = "ViolentCrimesPerPop", label = F, label.size = 2,
  frame = TRUE, frame.type = "norm"
)
############################# EXERCISE 2 #######################################
############################# EXERCISE 3 #######################################

# Training set
n <- dim(data)[1]
set.seed(12345)
index <- sample(1:n, floor(n * 0.5))
train <- data[index, ]
library(caret)
scaler <- preProcess(train)
train <- predict(scaler, newdata = train)

# Test
index1 <- setdiff(1:n, index)
test <- data[index1, ]

test <- predict(scaler, newdata = test)

# Estimate linear regression
linear.reg <- glm(ViolentCrimesPerPop ~ ., data = train, family = "gaussian")
mse.train <- mean(residuals(linear.reg)^2)
pred.test <- predict(linear.reg, newdata = test, type = "response")
plot(pred.test, pch = 20, col = "green")
mse.test <- mean((test$ViolentCrimesPerPop - pred.test)^2)
plot(mse.train, mse.test, xlab = "Mean square error train", ylab = "Mean square
     error test", pch = 20, col = "green")

plot(test$ViolentCrimesPerPop, pred.test,
  col = "lightgreen", pch = 20,
  ylim = c(-1, 3), xlab = "Actual", ylab = "Predicted"
)
abline(0, 1, col = "black")

################################ EXERCISE 3 ####################################
################################ EXERCISE 4 ####################################
theta <- rep(0, 100)
(optim(
  par = theta, fn = LmOptimization,
  train.x = as.matrix(select(train, -ViolentCrimesPerPop)),
  train.y = as.matrix(train["ViolentCrimesPerPop"]),
  test.x = as.matrix(select(test, -ViolentCrimesPerPop)),
  test.y = as.matrix(test["ViolentCrimesPerPop"]), method = "BFGS"
))

stopping.opt.test <- which.min(multi.mse.test)
stopping.opt.train <- which.min(multi.mse.train)
plot(multi.mse.train[500:21000],
  col = "lightgreen", main = "OPTIM MSE",
  pch = 20, ylab = "MEAN SQUARE ERROR", xlab = "ITERATIONS",
  ylim = c(0, 0.8), type = "p"
)
points(multi.mse.test[500:21000],
  col = "lightblue", main = "OPTIM MSE",
  pch = 20, type = "p"
)
points(stopping.opt.test - 500, multi.mse.test[stopping.opt.test],
  col = "black"
)
points(stopping.opt.train - 500, multi.mse.train[stopping.opt.train],
  col = "black"
)
legend("topright",
  legend = c("TRAIN", "TEST"),
  col = c("Green", "lightblue"), pch = 20, cex = 0.8, bg = "white"
)
############################### EXERCISE 4  ####################################
