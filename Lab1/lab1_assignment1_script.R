############################################ Lab 1 #######################################################

## function pascal
## variables .
#install.packages("styler")
########################################### Assignment 1 #################################################


# (1) Importing the data (50%/25%/25%) by partitioning
data <- read.csv("data/optdigits.csv", header = F)

# Id is the row row index for our numbers file
# Training set
n <- dim(data)[1]
set.seed(12345)
index <- sample(1:n, floor(n * 0.5))
train <- data[index, ]
# Validation
index1 <- setdiff(1:n, index)
set.seed(12345)
index2 <- sample(index1, floor(n * 0.25))
valid <- data[index2, ]

# Test
index3 <- setdiff(index1, index2)

test <- data[index3, ]

# (2) Use training data to fit 30-nearest neighbor classifier with function kknn() and kernel=”rectangular”
# install.packages("kknn") Dependency install

library(kknn)

## update row 26 to asfactor
print(train$V65)
train$V65 <- as.factor(train$V65)
test$V65 <- as.factor(test$V65)
print(train$V65)



# Formula that says column X.026 is a function of ".", where dot is every other column in the space but X.026.
# CL is the actual value (65 row value) of each row, C is the indexes in our scrambled
# training set of the k rows that are most similar. Fitted values is the mean of the CL values which is the same as predict func.
kknn.train <- kknn(V65 ~ ., train = train, test = train, k = 30, kernel = "rectangular")
kknn.test <- kknn(V65 ~ ., train = train, test = test, k = 30, kernel = "rectangular")

# Making a prediction of my kknn_train
kknn.train.prediction <- predict(kknn.train)

# Making a prediction of my kknn_test
kknn.test.prediction <- predict(kknn.test)

## Confusion Matrix for kknn_train vs actual
cm.train <- table(kknn.train.prediction, train$V65)
## Confusion Matrix for kknn_test vs actual
cm.test <- table(kknn.test.prediction, test$V65)


# Miss classification functions. Two ways

MissClassNormal <- function(X, X1) {
  n <- length(X)
  return(1 - sum(diag(table(X, X1))) / n)
}

MissClassMatrix <- function(X) {
  return(1 - sum(diag(X)) / sum(X))
}

miss.train <- MissClassMatrix(cm.train)
miss.test <- MissClassMatrix(cm.test)

## Step 1 was to make new data sets of the train and kknn train data to get the rows that where actually a 8. This is so that the
# index in probabilities.kknn.8 will match the train.8

## Getting all the lines where it actually was 8 from the train
train.8 <- train[which(train$V65 %in% 8), ]

## Getting all the probabilities from where it actually was 8 from the kknn.train
probabilities.kknn.8 <- kknn.train[["prob"]][which(train$V65 %in% 8), ]

## Extracting the index of our top 3 worst probabilities for probabilities.kknn.8 that were actually 8
worst.probs <- sort(probabilities.kknn.8[, 9], decreasing = F, index.return = T)$ix[1:3]
## Extracting the rows via the index found out in top_worst_in_prob
train.worst.8 <- train.8[worst.probs, ]

## Extracting the index of our top 2 best probabilities for 8 that were actually 8
best.probs <- sort(probabilities.kknn.8[, 9], decreasing = T, index.return = T)$ix[1:2]

## Extracting the rows via the index found out in best_in_prob
train.best <- train.8[best.probs, ]

## train_worst_8 takes the first 64 column values and makes them 8x8 matrix and then transforming it to a heatmap
train.worst.8 <- train.8[worst.probs, ]
worst.1 <- heatmap(t(matrix(as.numeric(train.worst.8[1, -65]), nrow = 8, ncol = 8)), Colv = "Rowv", Rowv = NA)
worst.2 <- heatmap(t(matrix(as.numeric(train.worst.8[2, -65]), nrow = 8, ncol = 8)), Colv = "Rowv", Rowv = NA)
worst.3 <- heatmap(t(matrix(as.numeric(train.worst.8[3, -65]), nrow = 8, ncol = 8)), Colv = "Rowv", Rowv = NA)


## train_best takes the first 64 column values and makes them 8x8 matrix and then transforming it to a heatmap
best.1 <- heatmap(t(matrix(as.numeric(train.best[1, -65]), nrow = 8, ncol = 8)), Colv = NA, Rowv = NA)
best.2 <- heatmap(t(matrix(as.numeric(train.best[2, -65]), nrow = 8, ncol = 8)), Colv = "Rowv", Rowv = NA)

## EXCERCISE 4 ##
multi.missclass.train <- c()
multi.missclass.valid <- c()
for (x in 1:30) {
  ## train
  multi.train.kknn <- kknn(V65 ~ ., train = train, test = train, k = x, kernel = "rectangular")
  pred.train <- multi.train.kknn$fitted.values

  multi.missclass.train <- c(multi.missclass.train, MissClassNormal(pred.train, train$V65))
  ## valid
  multi.valid.kknn <- kknn(V65 ~ ., train = train, test = valid, k = x, kernel = "rectangular")
  pred.valid <- multi.valid.kknn$fitted.values

  multi.missclass.valid <- c(multi.missclass.valid, MissClassNormal(pred.valid, valid$V65 ))
}



best.k <- which.min(multi.missclass.valid)

test1.kknn <- kknn(V65 ~ ., train = train, test = test, k = best.k, kernel = "rectangular")
pred.test1 <- test1.kknn$fitted.values
baby <- MissClassNormal(pred.test1, test$V65 )



# Create a first line
plot(multi.missclass.train[1:30] * 100,
  type = "b", frame = FALSE, pch = 19,
  col = "red", ylim = c(0, 7), xlab = "Amount of k-nearest neighbors", ylab = "Missclassification error rate %"
)
# Add a second line
lines(multi.missclass.valid[1:30] * 100, pch = 18, col = "blue", type = "b")
lines(best.k, baby*100, pch = 10, col = "green", type = "b")
# Add a legend to the plot
legend("topleft",
  legend = c("Missclassificaton in training data", "Missclassificaton in validation data"),
  col = c("red", "blue"), lty=1:2, cex = 0.8, bg="green"
)


## EXCERCISE 5 ##





