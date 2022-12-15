################################################################################
################################ Assignment 1 ##################################
################################################################################

library(kernlab)
set.seed(1234567890)

data(spam)
foo <- sample(nrow(spam))
spam <- spam[foo,]
spam[,-58]<-scale(spam[,-58])
tr <- spam[1:3000, ]
va <- spam[3001:3800, ]
trva <- spam[1:3800, ]
te <- spam[3801:4601, ] 

by <- 0.3
err.va <- NULL
for(i in seq(by,5,by)){
  filter <- ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=i,scaled=FALSE)
  mailtype <- predict(filter,va[,-58])
  t <- table(mailtype,va[,58])
  err.va <-c(err.va,(t[1,2]+t[2,1])/sum(t))
}

filter0 <- ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err.va)*by,scaled=FALSE)
mailtype <- predict(filter0,va[,-58])
t <- table(mailtype,va[,58])
err0 <- (t[1,2]+t[2,1])/sum(t)
err0

filter1 <- ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err.va)*by,scaled=FALSE)
mailtype <- predict(filter1,te[,-58])
t <- table(mailtype,te[,58])
err1 <- (t[1,2]+t[2,1])/sum(t)
err1

filter2 <- ksvm(type~.,data=trva,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err.va)*by,scaled=FALSE)
mailtype <- predict(filter2,te[,-58])
t <- table(mailtype,te[,58])
err2 <- (t[1,2]+t[2,1])/sum(t)
err2

filter3 <- ksvm(type~.,data=spam,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err.va)*by,scaled=FALSE)
mailtype <- predict(filter3,te[,-58])
t <- table(mailtype,te[,58])
err3 <- (t[1,2]+t[2,1])/sum(t)
err3

# Questions

# 1. Which filter do we return to the user ? filter0, filter1, filter2 or filter3? Why?

#Filter3 do not holdout any data which means it will have high variance
#with new data. In other words it is overfittet to our data.
# Filter0 uses the min error for the validation data on the
#validation data. However this model also wont generalize well since we cant
# know the optimal regularization parameter C that will generate the lowest error 
# for new data. 
# err1 = 0.08489388
# err2 = 0.082397
# The error is the lowest when we use both the tr and va data to train our model,
# therefore we return filter2.



# 2. What is the estimate of the generalization error of the filter returned to the user? err0, err1, err2 or err3? Why?
# See answer above.
# err1 = 0.08489388
# err2 = 0.082397
#filter3 Doesn't really have an estimated generalization error since it isn't 
# tested on unseen data.

# 3. Implementation of SVM predictions.

# The alpha index returns the indexes of the support vectors. These are the data points that
#are closest to a decision boundary and have the greatest impacts on a models predictions

# Get the support vectors, coefficients, and intercept of the SVM model
sv <- alphaindex(filter3)[[1]]
support.vectors <- spam[sv,-58]
coefficients <- coef(filter3)[[1]]
intercept <- -b(filter3)

# Create a kernel function to calculate the dot product between two samples
kernel.function <- rbfdot(0.05)

# Loop over the samples in the dataset to make predictions
k <- NULL
predictions <- NULL
dot_products <- NULL
for (i in 1:10) {
  # Loop over the support vectors to calculate the dot product between the sample and each support vectora
  for (j in 1:length(sv)) {
    support.vector <- unlist(support.vectors[j,])
    sample <- unlist(spam[i,-58])
    dot.product <- kernel.function(sample, support.vector)
    
    # Each iteration we obtain 1561 dot products of the sample we want to predict
    #and each and everyone of our support vectors
    dot_products <- c(dot_products, dot.product)
  }
  
  # Calculate the predicted class label or class probability for the sample
  start.index <- 1 + length(sv) * (i - 1)
  end.index <- length(sv) * i
  prediction <- coefficients %*% dot_products[start.index:end.index] + intercept
  predictions <- c(predictions, prediction)
}

# Print my predictions
predictions
sign(predictions)

# Compare with the predict function
predict(filter3,spam[1:10,-58], type = "decision")

#This is an example of how to make predictions with an SVM model using the dot 
#product between the data points and the support vectors. The dot product is a 
#measure of similarity between two vectors and is used in the kernel function 
#to map the data points into a higher dimensional space, where they can be 
#linearly separated by the SVM. By looping over the samples in the data set and 
#calculating the dot product between each sample and the support vectors, the 
#code is able to make predictions for each sample. The predictions can then be 
#compared to the actual class labels of the samples to evaluate the accuracy of 
#the model.











