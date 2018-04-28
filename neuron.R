
######################################
## Author:   Riti Kumari (rxk172630@utdallas.edu)
## Date:     2018-04-26
## Title:    Neuron/ Perceptron 
## Purpose:  Designing of XOR logical gate using the bias and the weight
######################################


#### Sigmoid Function ##########

sigmoid <- function(x)
{
 return (1/(1+exp(-x)))
}

#################################
    
#### Step Function ##############

binaryStep <- function(x){
 return (ifelse(x < 0 ,0,1))
}

#################################

### Activation function plot ####

x <- seq(-5,5,.01)
plot(x, sigmoid(x), col='green')
plot(x, binaryStep(x), col='blue')

###################################

### Input Function ################

input <-function(b,w,x) {
    sum <- sum(w * x)
    return (b+sum)
}

####################################

### Input After activation #########

neuron.sigmoid <- function(input){
  return (sigmoid(input))
}

neuron.step <- function(input){
  return (binaryStep(input))
}


######################################

######## Weights & Inputs ############

x <- matrix(c(1,1,0,1,1,0,0,0),byrow = TRUE, nrow = 4)
w <- c(20,-20,20) 
b <- c(-10,30,-30)

#######################################

########## Output Function with Step #####

y<- c()
for (i in 1:nrow(x)){
  h1 <- neuron.step(input(b[1],w[1],c(x[i,])))
  h2 <- neuron.step(input(b[2],w[2],c(x[i,])))
  y[i] <- neuron.step(input(b[3],w[3],c(h1,h2)))
}
y <- matrix(y)

Output <- cbind.data.frame(x,y)
colnames(Output) <- c("X1", "X2","X1 Xor X2")
Output
##########################################

##### Output with User Input #############
k <- c()
a1 <- readline("Enter First Binary Input : ")
while (a1!= "0" & a1!= "1") {
  print("Please enter the binary variable as 0 or 1")
  a1 <- readline("Enter First Input : ")
}

a2 <- readline("Enter Second Input : ")
while (a2!= "0" & (a2)!= "1") {
  print("Please enter the binary variable as 0 or 1")
  a2 <- readline("Enter Second Input : ")
}

a <- matrix(c(as.numeric(a1),as.numeric(a2)),byrow = TRUE, nrow = 1)

hi1 <- neuron.step(input(b[1],w[1],c(a[1,])))
hi2 <- neuron.step(input(b[2],w[2],c(a[1,])))

k <- neuron.step(input(b[3],w[3],c(hi1,hi2)))

Out <- cbind.data.frame(a,k)
colnames(Out) <- c("X1", "X2","X1 Xor X2")
Out

###########################################

#### Output Function with Sigmoid ######

y<- c()
for (i in 1:nrow(x)){
  h1 <- neuron.sigmoid(input(b[1],w[1],c(x[i,])))
  h2 <- neuron.sigmoid(input(b[2],w[2],c(x[i,])))
  y[i] <- round(neuron.sigmoid(input(b[3],w[3],c(h1,h2))))
}
y <- matrix(y)

Output <- cbind.data.frame(x,y)
colnames(Output) <- c("X1", "X2","X1 Xor X2")
Output
##########################################
  