#' @title swap two numbers
#' @description exchange two numbers
#' @param arr the array to be swaped
#' @param i the first number index
#' @param j the second number index
#' @return  an  array
#' @examples
#' \dontrun{
#' arr <- c(2,3,5,4,1)
#' swap(arr,1,2)
#' }
#' @export
swap <- function(arr,i,j){
  temp <- arr[i]
  arr[i] <- arr[j]
  arr[j] <- temp
  return(arr)
}



#' @title bubble sort algorithm
#' @description bubble sort algorithm
#' @param arr the array to be sorted
#' @param reverse whether to sort in ascending or descending order
#' @return  an ordered array
#' @examples
#' \dontrun{
#' arr <- c(2,3,5,4,1)
#' bubbleSort(arr)
#' }
#' @export
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


#' @title select sort algorithm
#' @description select sort algorithm
#' @param arr the array to be sorted
#' @param reverse whether to sort in ascending or descending order
#' @return  an ordered array
#' @examples
#' \dontrun{
#' arr <- c(2,3,5,4,1)
#' selectSort(arr)
#' }
#' @export
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


#' @title insert sort algorithm
#' @description insert sort algorithm
#' @param arr the array to be sorted
#' @param reverse whether to sort in ascending or descending order
#' @return  an ordered array
#' @examples
#' \dontrun{
#' arr <- c(2,3,5,4,1)
#' insertSort(arr)
#' }
#' @export
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


#' @title quick sort algorithm
#' @description quick sort algorithm
#' @param arr the array to be sorted
#' @param reverse whether to sort in ascending or descending order
#' @return  an ordered array
#' @examples
#' \dontrun{
#' arr <- c(2,3,5,4,1)
#' quickSort(arr)
#' }
#' @export
quickSort<-function(arr,reverse=FALSE){
  num<-length(arr)
  if(num==0||num==1){
    return(arr)
  }
  if(reverse==FALSE){
    a<-arr[1]
    y<-arr[-1]
    lower<-y[y<a]
    upper<-y[y>=a]
    return(c(quickSort(lower),a,quickSort(upper)))
  }else{
    a<-arr[1]
    y<-arr[-1]
    lower<-y[y<a]
    upper<-y[y>=a]
    return(c(quickSort(upper,reverse=TRUE),a,quickSort(lower,reverse=TRUE)))
  }
}


#' @title Benchmark R and Rcpp functions.
#' @name benchmarks
#' @description Use R package \code{microbenchmark} to compare the performance of C functions and Cpp functions 
#' @examples
#' \dontrun{
#' dt <- sample(1:100)
#' ts <- microbenchmark(bubble_sort=bubbleSort(dt),insert_sort = insertSort(dt),
#' select_sort = selectSort(dt),quick_sort=quickSort(dt),
#' bubble_sort_cpp=bubbleSortCpp(dt))
#' summary(ts)[,c(1,3,5,6)]
#' }
#' @import microbenchmark
#' @importFrom Rcpp  evalCpp
#' @importFrom stats rgamma  rnorm
#' @useDynLib StatComp22092
NULL


#' @title sigmoid function
#' @description sigmoid function
#' @param x  independent variable 
#' @return dependent variable
#' @examples
#' \dontrun{
#' sigmoidFun(2)
#' }
#' @export
sigmoidFun <- function(x){
  1/(1+exp(-x)) 
}

#' @title sigmoid function Derivative
#' @description sigmoid function Derivative
#' @param x  independent variable 
#' @return dependent variable
#' @examples
#' \dontrun{
#' sigmoidDerivate(2)
#' }
#' @export
sigmoidDerivate <- function(x){
  sigmoidFun(x) *(1-sigmoidFun(x)) 
}

#' @title Relu function
#' @description Relu function
#' @param x  independent variable 
#' @return dependent variable
#' @examples
#' \dontrun{
#' reluFun(2)
#' }
#' @export
reluFun <- function(x){
  max(0,x) 
}


#' @title relu function Derivative
#' @description relu function Derivative
#' @param x  independent variable 
#' @return dependent variable
#' @examples
#' \dontrun{
#' reluDerivate(2)
#' }
#' @export
reluDerivate <- function(x){
  as.numeric(x>0)
}

#' @title tanh function Derivative
#' @description tanh function Derivative
#' @param x  independent variable 
#' @return dependent variable
#' @examples
#' \dontrun{
#' tanhDerivate(2)
#' }
#' @export
tanhDerivate <- function(x){
  1 - tanh(x)^2
}


#' @title neural network
#' @description construct fully connected neural network
#' @param layers an array ,the i-th element in the array represents the number of nodes in the i-th layer of the neural network
#' @param activationFun choose an activation function
#' @return Initial weight of network
#' @examples
#' \dontrun{
#' myNeuralNet(c(13,6,3,2,1),'tanh')
#' }
#' @export
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
    
    initialWeights[[i]]<- matrix(nrow = rows,ncol = cols,rnorm(rows*cols))  # Initialize Weights
  }
  
  lastRow <- layers[length(initialWeights)]+1
  lastCol <- layers[length(initialWeights)+1]
  initialWeights[[length(initialWeights)]] <- matrix(nrow = lastRow,ncol = lastCol,rnorm(lastCol*lastRow))
  
  return(initialWeights) 
}



#' @title training the  network
#' @description use the traing set training the defined network
#' @param x training set
#' @param y labels
#' @param learningRate learning Rate
#' @param initialWeights Initial weight of network
#' @param activationFun activation function
#' @param epochs how many times to run the data
#' @return Trained weight
#' @examples
#' \dontrun{
#' library(MASS)
#' samplesSelect<-sample(c(1:length(Boston[,1])),length(Boston[,1])*0.7)
#' trainData<-Boston[samplesSelect,]
#' testData<-Boston[-samplesSelect,]
#' trainX<-trainData[-which(colnames(Boston)=="medv")]
#' trainY<-trainData[which(colnames(Boston)=="medv")]
#' initialWeights <- myNeuralNet(c(13,6,3,2,1),'tanh')
#' fitWeights<-training(trainX,trainY,0.001,initialWeights,'tanh',20000)
#' }
#' @export
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
    samples <- sample(1:length(x[,1]),1)
    compWeights <- list(x[samples,])
    length(compWeights) <-length(initialweights)
    
    for (j in 1:length(initialweights)) {
      values <- compWeights[[j]] %*% initialweights[[j]]
      compWeights[[j+1]]<-activation(values)
    }
    error <- y[samples,] - compWeights[[length(compWeights)]]
    gradient <- list(error* activationDerivate(compWeights[[length(compWeights)]]))
    for (k in (length(compWeights)-1):2) {
      length(gradient) <- length(gradient) + 1
      gradient[[length(gradient)]] <- gradient[[length(gradient)-1]]%*%t(initialweights[[k]])*activationDerivate(compWeights[[k]])
    }
    
    gradientReverse <- list()
    length(gradientReverse) <- length(gradient)
    for (m in 1:length(gradient)) {
      gradientReverse[[m]] <- gradient[[(length(gradient))-m+1]]
    }
    
    for (s in 1:length(initialweights)) {
      layerWeight <- as.numeric(compWeights[[s]])
      gradient <- gradientReverse[[s]]
      initialweights[[s]]<- initialweights[[s]] + learningRate*(layerWeight%*%gradient)
    }
  }
  print(initialweights)
  return(initialweights)
}


#' @title predict
#' @description input a sample , output predicted value
#' @param x sample
#' @param fitWeights Trained weight
#' @param activationFun activationFun
#' @return prediction
#' @export
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

#' @title predict
#' @description input some samples , output predicted values
#' @param x samples
#' @param fitWeights Trained weight
#' @param activationFun activation function
#' @return prediction
#' @examples
#' \dontrun{
#' library(MASS)
#' samplesSelect<-sample(c(1:length(Boston[,1])),length(Boston[,1])*0.7)
#' trainData<-Boston[samplesSelect,]
#' testData<-Boston[-samplesSelect,]
#' trainX<-trainData[-which(colnames(Boston)=="medv")]
#' trainY<-trainData[which(colnames(Boston)=="medv")]
#' testX<-testData[-which(colnames(Boston)=="medv")]
#' testY<-testData[which(colnames(Boston)=="medv")]
#' trainX<-apply(trainX,2,function(x)scale(x,center=T,scale=T))
#' trainY<-scale(trainY,center=T,scale=T)
#' testX<-apply(testX,2,function(x)scale(x,center=T,scale=T))
#' testY<-scale(testY,center=T,scale=T)
#' initialWeights <- myNeuralNet(c(13,6,3,2,1),'tanh')
#' fitWeights<-training(trainX,trainY,0.001,initialWeights,'tanh',20000)
#' predictValues<-predictionMat(testX,initialWeights,'tanh')
#' }
#' @export
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

