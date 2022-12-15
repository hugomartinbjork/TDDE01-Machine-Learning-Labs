################################################################################
########################## Lab2 Assignment1 ####################################
################################################################################

##################################### SETUP ####################################

# (1) Importing the data and partitioning it (40%/30%/30%)
data <- read.csv2("data/bank-full.csv", stringsAsFactors = T)
library(dplyr)
data <- select(data, -duration)

# Training set
n <- dim(data)[1]
set.seed(12345)
index <- sample(1:n, floor(n * 0.4))
train <- data[index, ]

# Validation
index1 <- setdiff(1:n, index)
set.seed(12345)
index2 <- sample(index1, floor(n * 0.3))
valid <- data[index2, ]

# Test
index3 <- setdiff(index1, index2)

test <- data[index3, ]

################################# SETUP ########################################

############################### Functions ######################################
MisClassNormal <- function(X, X1) {
  n <- length(X)
  return(1 - sum(diag(table(X, X1))) / n)
}

RocFunction <- function(probs) {
  pi <- seq(from = 0.05, to = 0.95, by = 0.05)
  tpr <- c()
  fpr <- c()
  for (i in 1:length(pi)) {
    decision <- ifelse(probs[, 2] > pi[i], "yes", "no")
    cm.temp <- table(decision, test$y)
    true.pos <- cm.temp[4]
    false.pos <- cm.temp[2]
    pos <- cm.temp[3] + cm.temp[4]
    neg <- false.pos + cm.temp[1]
    tpr <- c(tpr, true.pos / pos)
    fpr <- c(fpr, false.pos / neg)
  }

  plot(fpr, tpr,
    pch = 20, col = "lightblue", type = "b", main = "ROC opt tree",
    ylab = "TPR | Recall | Sensativity", xlab = "False Positive Rates"
  )
  values <- cbind(fpr, tpr)
  # text(fpr, tpr, labels=pi)
  return(values)
}
################################ Functions #####################################

################################### Exercise 2 #################################
# install.packages("tree")
library(tree)
tree.a <- tree(as.factor(y) ~ ., data = train)
# Misclass on trees
misclass.train.a <- summary(tree.a)
misclass.train.a <- misclass.train.a$misclass
misclass.train.a <- misclass.train.a[1] / misclass.train.a[2]

plot(tree.a, col = "green")
text(tree.a)

## tree.b gives the equally small misclass (both training and validation)
# rate as tree.a  but is less complex so I think its the best
tree.b <- tree(as.factor(y) ~ ., data = train, minsize = 7000)
misclass.train.b <- summary(tree.b)
misclass.train.b <- misclass.train.b$misclass
misclass.train.b <- misclass.train.b[1] / misclass.train.b[2]

plot(tree.b, col = "green")
text(tree.b)

tree.c <- tree(as.factor(y) ~ ., data = train, mindev = 0.0005)
misclass.train.c <- summary(tree.c)
misclass.train.c <- misclass.train.c$misclass
misclass.train.c <- misclass.train.c[1] / misclass.train.c[2]

plot(tree.c, col = "green")

# Predictions
pred.valid.a <- predict(tree.a, newdata = valid, type = "class")
pred.valid.b <- predict(tree.b, newdata = valid, type = "class")
pred.valid.c <- predict(tree.c, newdata = valid, type = "class")

# Misclass validation way one
misclass.valid.a <- mean(pred.valid.a != valid$y)
misclass.valid.b <- mean(pred.valid.b != valid$y)
misclass.valid.c <- mean(pred.valid.c != valid$y)

# Misclass validation way two
misclass.valid.a <- MisClassNormal(valid$y, pred.valid.a)
misclass.valid.b <- MisClassNormal(valid$y, pred.valid.b)
misclass.valid.c <- MisClassNormal(valid$y, pred.valid.c)

### Visulization
my.tab <- matrix(c(
  misclass.train.a, misclass.valid.a, misclass.train.b,
  misclass.valid.b, misclass.train.c, misclass.valid.c
), ncol = 3)
colnames(my.tab) <- c("Tree A", "Tree B", "Tree C")
rownames(my.tab) <- c("Train", "Valid")
as.table(my.tab)
############################## Exercise 3 ######################################
score.train <- rep(0, 50)
score.valid <- rep(0, 50)
for (i in 2:50) {
  pruned.tree <- prune.tree(tree.c, best = i)
  pred <- predict(pruned.tree, newdata = valid, type = "tree")
  score.train[i] <- deviance(pruned.tree)
  score.valid[i] <- deviance(pred)
}
opt.leaves.train <- which.min(score.train[2:50]) + 1
opt.leaves.valid <- which.min(score.valid[2:50]) + 1

plot(score.train[2:50],
  type = "b", col = "red", pch = 20, ylim = c(8000, 12000),
  ylab = "Deviance", xlab = "Leaves"
)
points(score.valid[2:50], type = "b", col = "blue", pch = 20)
points(opt.leaves.train - 1, score.train[opt.leaves.train], col = "green", pch = 20)
points(opt.leaves.valid - 1, score.valid[opt.leaves.valid], col = "green", pch = 20)
legend("topright",
  legend = c("Train", "Valid", "Optimal stopping point"),
  col = c("red", "blue", "green"), pch = 20, cex = 0.8, bg = "white"
)


############################## Exercise 3 ######################################
############################## Exercise 4 ######################################

# install.packages("MLmetrics")
library(MLmetrics)
optimal.tree <- prune.tree(tree.c, best = opt.leaves.valid)
pred.test <- predict(optimal.tree, newdata = test, type = "class")
cm.test <- table(pred.test, test$y)
test.acc <- Accuracy(pred.test, test$y)
f1.score.no <- F1_Score(pred.test, test$y)
# library(caret)
# cm.test.caret <- confusionMatrix(pred.test, test$y)
## Extracting a few commonly used variables
true.pos <- cm.test[4]
false.pos <- cm.test[2]
pos <- cm.test[3] + cm.test[4]
neg <- false.pos + cm.test[1]

## TPR also called sensitivity or recall
recall <- true.pos / pos
precision <- true.pos / (true.pos + false.pos)
f1.score.yes <- (2 * precision * recall) / (precision + recall)

########################## Exercise 4 ##########################################
########################## Exercise 5 ##########################################
### Here I performed a decision tree classification of test data
### using a given cost matrix
probs.opt.tree <- predict(optimal.tree, newdata = test)
losses <- probs.opt.tree %*% matrix(c(0, 5, 1, 0), nrow = 2)
best.i <- apply(losses, MARGIN = 1, FUN = which.min)
pred <- levels(test$y)[best.i]
table(pred, test$y)
########################## Exercise 5 ##########################################
########################## Exercise 6 ##########################################

######### Plotting ROC values for the optimal tree ############
roc.opt <- RocFunction(probs.opt.tree)

# Logistic regression model
log.regr.model <- glm(y ~ ., data = test, family = "binomial")
probs.log.regr.model <- predict(log.regr.model, type = "response")
probs.log.regr.model <- cbind(1 - probs.log.regr.model, probs.log.regr.model)

####### Plotting ROC values for the logistic regression model ##########
roc.reg <- RocFunction(probs.log.regr.model)


######### Plotting togheter ########
plot(roc.opt,
  pch = 20, col = "green", type = "l", main = "ROC opt tree",
  ylab = "TPR | Recall | Sensativity", xlab = "False Positive Rates"
)
points(roc.reg,
  pch = 20, col = "blue", type = "l", main = "ROC opt tree",
  ylab = "TPR | Recall | Sensativity", xlab = "False Positive Rates"
)
legend("topleft",
  legend = c("Optimal Tree", "Regression"),
  col = c("green", "blue"), pch = 20, cex = 0.8, bg = "white"
)
###################### Exercise 6 ##############################################
