################################################################################
################################ Assignment 3 ##################################
################################################################################

################################# Setup ########################################
library(neuralnet)
set.seed(1234567890)
Var <- runif(500, 0, 10)
mydata <- data.frame(Var, Sin=sin(Var))
tr <- mydata[1:25,] # Training
te <- mydata[26:500,] # Test
a <- function(x){x}
b <-function(x){
  ifelse(x>0, x, 0)
} 
c <- function(x){log(1 + exp(x)) }

winit <- runif(10,-1,1)
  nn <- neuralnet(Sin~., data = tr, hidden = 10, threshold = 0.01, startweights = winit)
    # Plot of the training data (black), test data (blue), and predictions (red)
    plot(tr, cex=2, col="red")
    points(te, col = "blue", cex=1)
    points(te[,1],predict(nn,te), col="green",type ="p",  cex=1)
    
    nn.2.a <- neuralnet(Sin~., data = tr, hidden = 10, threshold = 0.01, startweights = winit, act.fct = a)
    nn.2.b <- neuralnet(Sin~., data = tr, hidden = 10, threshold = 0.01, startweights = winit, act.fct = b)
    nn.2.c <- neuralnet(Sin~., data = tr, hidden = 10, threshold = 0.01, startweights = winit, act.fct = c)
    plot(tr, cex=2, col="red")
    points(te, col = "blue", cex=1)
    points(te[,1],predict(nn.2.a,te), col="green",type ="p",  cex=1)
    
    plot(tr, cex=2, col="red")
    points(te, col = "blue", cex=1)
    points(te[,1],predict(nn.2.b,te), col="green",type ="p",  cex=1)

    plot(tr, cex=2, col="red")
    points(te, col = "blue", cex=1)
    points(te[,1],predict(nn.2.c,te), col="green",type ="p",  cex=1)
    
    
    
############################# Exercise 3 #######################################
    set.seed(1234567890)
    Var<- runif(500, 0, 50)
    mydata2 <- data.frame(Var, Sin=sin(Var))
    plot(mydata2, col="blue", cex=2, ylim = cbind(-11,1))
    points(mydata2[,1],predict(nn, mydata2), col="orange",type ="p",  cex=1 )
    redict(nn, mydata2)[which.min(predict(nn, mydata2))]
############################# Exercise 3 #######################################
############################# Exercise 4 #######################################
plot(nn)
############################# Exercise 4 ######################################
############################# Exercise 5 #######################################
    set.seed(1234567890)
    Var<- runif(500, 0, 10)
    mydata3 <-data.frame(Sin=sin(Var), Var)
    nn.5 <- neuralnet(Var~., data = mydata3, hidden = 10, threshold = 0.1, 
                      startweights = winit)
    plot(mydata3, col="blue", cex=2, ylim=cbind(-1,11))
    points(mydata3[,1],predict(nn.5, mydata3), col="red",type ="p",  cex=1)
############################# Exercise 5 #######################################
    