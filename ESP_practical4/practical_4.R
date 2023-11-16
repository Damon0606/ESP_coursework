
netup <- function(d) {
  network <- list()
  network$h <- vector("list", length(d))
  network$W <- vector("list", length(d) - 1)
  network$b <- vector("list", length(d) - 1)
  for (l in 1:(length(d) - 1)) {
    network$W[[l]] <- matrix(runif(d[l+1] * d[l], min = 0, max = 0.2), nrow = d[l+1], ncol = d[l])
    network$b[[l]] <- runif(d[l + 1], min = 0, max = 0.2)
    print(dim(network$W[[l]]))
  }
  return(network)
}

forward <- function(nn, inp) {
  nn$h[[1]] <- inp
  for (l in 2:length(nn$h)) {
    Zeros <- rep(0, dim(nn$W[[l - 1]])[1])
    nn$h[[l]] <- pmax(Zeros, nn$W[[l - 1]] %*% nn$h[[l - 1]] + nn$b[[l - 1]])
  }
  return(nn)
}


backward <- function(nn, k) {
  n <- length(nn$h)
  nn$dh <- vector("list", n)
  nn$dW <- vector("list", n - 1)
  nn$db <- vector("list", n - 1)
  nn$d  <- vector("list", n)
  # Loss
  # L = -sum(log(nn$dh[[n]])/n)
  ## 计算dh^L&d
  nn$dh[[n]] <- exp(nn$h[[n]]) / sum(exp(nn$h[[n]]))
  nn$dh[[n]][k] <- nn$dh[[n]][k]-1
  
  for (l in n:2) {
    h_length <- length(nn$h[[l]])
    for (j in 1:h_length) {
      if(nn$h[[l]][j] <= 0){
        nn$d[[l]][j] = 0
      }else{
        nn$d[[l]][j] = nn$dh[[l]][j]
      }
    }
    nn$db[[l-1]] <- nn$d[[l]]
    nn$dh[[l-1]] <- t(nn$W[[l-1]]) %*% nn$d[[l]]
    nn$dW[[l-1]] <- nn$d[[l]] %*% t(nn$h[[l-1]])
  }
  
  nn$d[[1]] <- nn$dh[[1]]
  return(nn)
}


# ————————————————————————————————————————————————————————————————————————————
## Object: Obtain the average of derivatives for each sample in every batch 
## Input: {batch_list: a list of ten sub-lists and each sub-list contains three 
##                    matrices or vectors}
## Output: {mean_list: a list of three sub-lists and each sub-list contains
##                    an average matrix of ten matrices of same layer.}
meanList <- function(batch_list){
  # initialize three sub-lists with first value of first sub-list in batch_list
  mean_1 <- batch_list[[1]][1]
  mean_2 <- batch_list[[1]][2]
  mean_3 <- batch_list[[1]][3]
  mean_list <- list()
  # obtain the sum of ten matrices of same layer
  for (i in 2:length(batch_list)){
    mean_1[[1]] <- mean_1[[1]] + batch_list[[i]][[1]]
    mean_2[[1]] <- mean_2[[1]] + batch_list[[i]][[2]]
    mean_3[[1]] <- mean_3[[1]] + batch_list[[i]][[3]]
  }
  # obtain the average of ten matrices of same layer
  mean_list <- append(mean_list, list(mean_1[[1]]/length(batch_list)))
  mean_list <- append(mean_list, list(mean_2[[1]]/length(batch_list)))
  mean_list <- append(mean_list, list(mean_3[[1]]/length(batch_list)))
  return (mean_list)
}


# ————————————————————————————————————————————————————————————————————————————
## Object: Train the network using forward and backward functions with given
##         parameters
## Input: {nn: network after initialization
##         inp: matrix of training set
##         k: vector the class of iris corresponding to inp
##         eta: step size of updating parameters
##         mb: the number of training data to randomly sample to compute the 
##             average gradient
##         nstep: the number of optimization steps using forward and 
##                backward function}
## Output: {nn: a network consists of updated weight(W) and bias(b)}
train <- function(nn, inp, k, eta = .01, mb = 10, nstep = 10000) {
  # iterate each step
  for (epoch in 1:nstep){
    # initialize lists to record weight(W) and bias(b) for each sample in batch
    batch_dW <- list()
    batch_db <- list()
    # generate index of samples randomly
    batch_index <- sample(1:nrow(inp), mb, replace = FALSE)
    for (i in batch_index){
      # apply forward function
      # nn is used to store weight(W) and bias(b)
      network <- forward(nn, inp[i, 1:4])
      # apply backward function
      # network is used to calculate gradients
      network <- backward(network, k[i])
      # record weight(W) and bias(b) in lists
      batch_dW <- append(batch_dW, list(network$dW))
      batch_db <- append(batch_db, list(network$db))
    }
    # obtain the average of derivatives for each sample in every batch 
    mean_dW <- meanList(batch_dW)
    mean_db <- meanList(batch_db)
    # update weight(W) and bias(b) using average derivatives
    for (l in 1:length(network$W)) {
      nn$W[[l]] <- nn$W[[l]] - eta * mean_dW[[l]]
      nn$b[[l]] <- nn$b[[l]] - eta * mean_db[[l]]
    }
  }
  return (nn)
}


# ————————————————————————————————————————————————————————————————————————————
## Object: Calculate the misclassification rate for the test set
## Input: {nn: network after updating weight(W) and bias(b)
##         test_data: matrix of test set
##         test_k: vector the class of iris corresponding to test_data}
## Output: {Compute the misclassification rate message}
misclassification <- function(nn, test_data, test_k){
  test_prediction <- list()
  # iterate each test sample
  for (i in 1:(nrow(test_data))){
    # obtain prediction scores of final layer
    prediction <- forward(nn, test_data[i, 1:4])$h[[4]]
    # obtain the index of maximum value of prediction scores
    prediction_label <- which.max(prediction)
    # add prediction class to a list
    test_prediction <- append(test_prediction, prediction_label)
  }
  # obtain the number of correctly classifiction by comparing with true 
  # classification and get the accuracy devided by length of test_data
  test_accuracy <- sum(unlist(test_prediction) == test_k) / nrow(test_data)
  # obtain misclassification rate
  misclassificationRate <- 1-test_accuracy
  cat("Misclassification Rate on test set:", misclassificationRate, "\n")
}


# ————————————————————————————————————————————————————————————————————————————
## Object: Train a 4-8-7-3 network to classify iris to species. Test data is 
##         selected by every 5th row of iris data set
set.seed(1116)
# Import iris data
data(iris)
iris$Species <- as.numeric(factor(iris$Species, levels = unique(iris$Species)))
iris <- as.matrix(iris)
# select test indices by every 5th row of iris data set
test_indices <- seq(5, nrow(iris), by = 5)
# the remaining indices are training indices
train_indices <- setdiff(1:nrow(iris), test_indices)
# matrix of training data
inp <- iris[train_indices, 1:4]
# vector of the class of iris corresponding to training data (inp)
k <- iris[train_indices, 5]
# matrix of testing data
test_data <- iris[test_indices, 1:4]
# vector of the class of iris corresponding to testing data (test_data)
test_k <- iris[test_indices, 5]

## Initialize network
nn <- netup(c(4, 8, 7, 3))
## Train the network
network <- train(nn, inp, k, eta=.01, mb=10, nstep=10000)
## Compute misclassification rate
misclassification(network, test_data, test_k)
