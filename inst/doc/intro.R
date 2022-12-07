## -----------------------------------------------------------------------------
#Swap elements at i and j positions in an array
swap <- function(arr,i,j){
  temp <- arr[i]
  arr[i] <- arr[j]
  arr[j] <- temp
  return(arr)
}

## -----------------------------------------------------------------------------
#bubble sort , arr is the array to be sorted,reverse indicates whether to sort in ascending or descending order.
bubbleSort <- function(arr,reverse=FALSE){
  if(length(arr)<2){
    return(arr)
  }
  if(reverse==FALSE){
    for (i in length(arr):2){
      for (j in 1:(i-1)) {
        if(arr[j]>arr[j+1]){ #Compare two adjacent numbers in turn
          arr = swap(arr,j,j+1)
        }
      }
    }
  }else{
    for (i in length(arr):2){
      for (j in 1:(i-1)) {
        if(arr[j]<arr[j+1]){
          arr = swap(arr,j,j+1)
        }
      }
    }
  }
  return(arr)
}

## -----------------------------------------------------------------------------
#select sort , arr is the array to be sorted,reverse indicates whether to sort in ascending or descending order.
selectSort <- function(arr,reverse=FALSE){
  if(length(arr)<2){
    return(arr)
  }
  if(reverse==FALSE){
    for (i in 1:(length(arr)-1)) {
      minIndex <- i  #Starting from the ith position, the position of the smallest element in the array.
      for (j in (i+1):length(arr)) {
        if(arr[minIndex]>arr[j]){
          minIndex <- j 
        }
      }
      arr = swap(arr,i,minIndex)
    }
  }else{
    for (i in 1:(length(arr)-1)) {
      maxIndex <- i  #Starting from the ith position, the position of the largest element in the array
      for (j in (i+1):length(arr)){
        if(arr[maxIndex]<arr[j]){
          maxIndex <- j 
        }
      }
      arr <- swap(arr,i,maxIndex)
    }
  }
  return(arr)
}

## -----------------------------------------------------------------------------
#insert sort , arr is the array to be sorted,reverse indicates whether to sort in ascending or descending order.
insertSort <- function(arr,reverse=FALSE){
  if(length(arr)<2){
    return(arr)
  }
  if(reverse==FALSE){
    for (i in 2:length(arr)) {
      for (j in (i-1):1) {
        if(arr[j]>arr[j+1]){
          arr <- swap(arr,j,j+1)
        }
      }
    }
  }else{
    for (i in 2:length(arr)) {
      for (j in (i-1):1) {
        if(arr[j]<arr[j+1]){
          arr <- swap(arr,j,j+1)
        }
      }
    }
  }
  return(arr)
}

## -----------------------------------------------------------------------------
#quick sort , arr is the array to be sorted,reverse indicates whether to sort in ascending or descending order.
quickSort<-function(arr,reverse=FALSE){
  num<-length(arr)
  if(num==0||num==1){
    return(arr)
  }
  if(reverse==FALSE){
    a<-arr[1]
    y<-arr[-1]
    lower<-y[y<a] # choose which number in the array is smaller than a
    upper<-y[y>=a] # choose which number in the array is bigger than a
    return(c(quickSort(lower),a,quickSort(upper)))
  }else{
    a<-arr[1]
    y<-arr[-1]
    lower<-y[y<a]
    upper<-y[y>=a]
    return(c(quickSort(upper,reverse=TRUE),a,quickSort(lower,reverse=TRUE)))
  }
}

## ----eval=FALSE---------------------------------------------------------------
#  #bubble sort , arr is the array to be sorted.
#  NumericVector bubbleSortCpp(NumericVector arr){
#    double tem = 0;
#    if(arr.size()<2){
#      return arr;
#    }
#    int n = arr.size();
#    NumericVector cloArr = clone(arr);
#    for(int i = n-1; i>0;i--){
#      for(int j = 0; j < i; j++){
#        if(arr[j]>arr[j+1]){
#          tem = arr[j];
#          arr[j] = arr[j+1];
#          arr[j+1] = tem;
#          cloArr[j] = cloArr[j+1];
#          cloArr[j+1] = tem;
#        }
#      }
#    }
#    return cloArr;
#  }

## -----------------------------------------------------------------------------
library(StatComp22092)
library(microbenchmark)
library(Rcpp)
sourceCpp('../src/StatCompC.cpp')
dt <- sample(1:100)  #Generate a random sequence of integers
ts <- microbenchmark(bubble_sort=bubbleSort(dt),insert_sort = insertSort(dt),select_sort = selectSort(dt),quick_sort=quickSort(dt),bubble_sort_cpp=bubbleSortCpp(dt))
summary(ts)[,c(1,3,5,6)]

## -----------------------------------------------------------------------------
#sigmoid function
sigmoidFun <- function(x){
 1/(1+exp(-x)) 
}

## -----------------------------------------------------------------------------
#derivative of sigmoid function
sigmoidDerivate <- function(x){
 sigmoidFun(x) *(1-sigmoidFun(x)) 
}

## -----------------------------------------------------------------------------
#Relu function
reluFun <- function(x){
 max(0,x) 
}

## -----------------------------------------------------------------------------
#derivative of Relu function
reluDerivate <- function(x){
   as.numeric(x>0)
}

## -----------------------------------------------------------------------------
#derivative of tanh function
tanhDerivate <- function(x){
 1 - tanh(x)^2
}

## -----------------------------------------------------------------------------
# Construct neural network; layers is an array ,the i-th element in the array represents the number of nodes in the i-th layer of the neural network;activationFun is the activation function.
myNeuralNet <- function(layers,activationFun){
  # choose activation function
  if(activationFun == 'tanh'){
    activation<- tanh
    activationDerivate <- tanhDerivate
  }else if(activationFun == 'sigmoid'){
    activation <- sigmoidFun
    activationDerivate <- sigmoidDerivate
  }else if(activationFun == 'relu'){
    activation <- reluFun
    activationDerivate <- reluDerivate
  }else {
    return('activation is not support,please choose tanh,relu or sigmoid')
  }

  initialWeights <- list()
  length(initialWeights) <- length(layers)-1
  
  for (i in 1:(length(layers)-2)) {
    rows <- layers[i]+1
    cols <- layers[i+1]+1
    
    initialWeights[[i]]<- matrix(nrow = rows,ncol = cols,rnorm(rows*cols))  # Initialize Network  Weights
  }
  
  lastRow <- layers[length(initialWeights)]+1
  lastCol <- layers[length(initialWeights)+1]
  initialWeights[[length(initialWeights)]] <- matrix(nrow = lastRow,ncol = lastCol,rnorm(lastCol*lastRow))
  
  return(initialWeights) 
}

## -----------------------------------------------------------------------------
#x is the training set; y is label;epochs indicate how many times to run the data.
training <- function(x,y,learningRate,initialWeights,activationFun,epochs=100){
  if(activationFun == 'tanh'){
    activation<- tanh
    activationDerivate <- tanhDerivate
  }else if(activationFun == 'sigmoid'){
    activation <- sigmoidFun
    activationDerivate <- sigmoidDerivate
  }else if(activationFun == 'relu'){
    activation <- reluFun
    activationDerivate <- reluDerivate
  }else {
    return('activation is not support,please choose tanh,relu or sigmoid')
  }
  initialweights <- initialWeights
  x <- as.matrix(cbind(1,x))
  for (i in 1:epochs) {
    samples <- sample(1:length(x[,1]),1) #Randomly select a sample for training
    compWeights <- list(x[samples,])
    length(compWeights) <-length(initialweights)
    
    #Calculate Predicted Value
    for (j in 1:length(initialweights)) {
      values <- compWeights[[j]] %*% initialweights[[j]]
      compWeights[[j+1]]<-activation(values)
    }
    error <- y[samples,] - compWeights[[length(compWeights)]] #Calculate the error 
    gradient <- list(error* activationDerivate(compWeights[[length(compWeights)]])) #Calculate the  gradient 
    for (k in (length(compWeights)-1):2) {
      length(gradient) <- length(gradient) + 1
      gradient[[length(gradient)]] <- gradient[[length(gradient)-1]]%*%t(initialweights[[k]])*activationDerivate(compWeights[[k]])
    }
    
    gradientReverse <- list()
    length(gradientReverse) <- length(gradient)
    for (m in 1:length(gradient)) {
      gradientReverse[[m]] <- gradient[[(length(gradient))-m+1]]
    }
    # Update  network weights
    for (s in 1:length(initialweights)) {
      layerWeight <- as.numeric(compWeights[[s]])
      gradient <- gradientReverse[[s]]
      initialweights[[s]]<- initialweights[[s]] + learningRate*(layerWeight%*%gradient)
    }
  }
  print(initialweights)
  return(initialweights)
}

## -----------------------------------------------------------------------------
# x is the sample ;fitWeights is the trained weight.
prediction <- function(x,fitWeights,activationFun){
  if(activationFun == 'tanh'){
    activation<- tanh
  }else if(activationFun == 'sigmoid'){
    activation <- sigmoidFun
  }else if(activationFun == 'relu'){
    activation <- reluFun
  }else {
    return('activation is not support,please choose tanh,relu or sigmoid')
  }
  for (i in 1:length(fitWeights)) {
    dotValues <- x%*%fitWeights[[i]]
    x <- activation(dotValues)
  }
  return(x)
}

predictionMat <- function(x,fitWeights,activationFun){
  if(activationFun == 'tanh'){
    activation<- tanh
  }else if(activationFun == 'sigmoid'){
    activation <- sigmoidFun
  }else if(activationFun == 'relu'){
    activation <- reluFun
  }else {
    return('activation is not support,please choose tanh,relu or sigmoid')
  }
  newX <- cbind(1,x)
  predictValues <- apply(newX, 1,function(x)prediction(x,fitWeights,activationFun))
  print(predictValues)
}

## -----------------------------------------------------------------------------
# use the data Boston from MASS to make a presentation.
library(MASS)
samplesSelect<-sample(c(1:length(Boston[,1])),length(Boston[,1])*0.7)

#Divide training set and test set
trainData<-Boston[samplesSelect,]
testData<-Boston[-samplesSelect,]

# medv is the label,  the house price to be predicted
trainX<-trainData[-which(colnames(Boston)=="medv")]
trainY<-trainData[which(colnames(Boston)=="medv")]

testX<-testData[-which(colnames(Boston)=="medv")]
testY<-testData[which(colnames(Boston)=="medv")]

#Normalize the data
trainX<-apply(trainX,2,function(x)scale(x,center=T,scale=T))
trainY<-scale(trainY,center=T,scale=T)
testX<-apply(testX,2,function(x)scale(x,center=T,scale=T))
testY<-scale(testY,center=T,scale=T)

## -----------------------------------------------------------------------------
initialWeights <- myNeuralNet(c(13,6,3,2,1),'tanh')
fitWeights<-training(trainX,trainY,0.001,initialWeights,'tanh',20000)
predictValues<-predictionMat(testX,initialWeights,'tanh')

## -----------------------------------------------------------------------------
trainYOrig<-as.matrix(trainData[which(colnames(Boston)=="medv")])
testYOrig<-as.matrix(testData[which(colnames(Boston)=="medv")])
predictValuesMyNet<-predictValues*sd(trainYOrig)+mean(trainYOrig)
mae<-mean(abs(testYOrig-predictValuesMyNet))
mae

## -----------------------------------------------------------------------------
# use neuralnet package as a control
library(neuralnet)
network <- neuralnet(medv~crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat,trainData,hidden=4)
network$weights

## -----------------------------------------------------------------------------
predictionNeuralnet<-compute(network,testX)
head(data.frame(testData$medv,predictValuesMyNet,predictionNeuralnet$net.result),40)

