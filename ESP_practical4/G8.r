# Huantong Hou (s2481591), Yuqi Shi (s2508879), Zukai Li (s2505721)

# https://github.com/Damon0606/ESP_coursework.git

# Contribution:
#### Huantong Hou (s2481591): 35%
###### (1) Code and debug 'meanList', 'train' and 'misclassification' functions.
###### (2) Write comments for the corresponding section.

#### Yuqi Shi (s2508879): 32%
###### (1) Code and debug 'forward', 'backward' functions.
###### (2) Write comments for the corresponding section.

#### Zukai Li (s2505721): 33%
###### (1) Code and debug 'netup', 'forward', 'backward' functions.
###### (2) Write comments for the corresponding section.


# ——————————————————————————————————————————————————————————————————————————————
### Code Objective and Functional Description:
# The objective of this code is to implement a MLP neural network model for
# category prediction.
# It includes functions for initializing the network ('netup' function),
# performing forward and backward propagation ('forward' and 'backward'
# functions), training the network ('train' function), and evaluating its
# performance on a test dataset ('misclassification' function).
# Finally, train a 4-8-7-3 neural network as a testing example to predict the
# type of iris based on its four characteristics, and evaluate the prediction
# results of the model (based on the misclassification rate).


### The code implements training, testing, and result evaluation of neural
#   networks through five main functions：

## 1. The netup function is defined to initialize the neural network.
# It takes a vector d as input, which specifies the number of neurons in each
# layer of the network. It creates a network object and initializes the
# weights and biases for each layer randomly.

## 2. The forward function performs the forward propagation step in the neural network.
# It takes a network object nn and an input vector inp as inputs. It computes
# the activations of all hidden layers based on the weights, biases, and input
# values, and updates the hidden layer activations in the network object.

## 3. The backward function performs the backward propagation step in the neural
# network. It takes a network object nn and a vector k representing the true
# labels of the training data. It computes the gradients of the weights and
# biases using the backpropagation algorithm and updates the network object
# accordingly.

## 4. The train function trains the neural network using the backpropagation algorithm.
# It takes a network object nn, training input data inp, training labels k,
# and optional parameters such as learning rate (eta), mini-batch size (mb),
# and number of training steps (nstep). It performs mini-batch gradient
# descent by randomly selecting mini-batches of training data, propagating the
# inputs forward and the errors backward, and updating the network parameters
# accordingly.

## 5. The misclassification function is defined to evaluate the performance of
#     the trained network on the test dataset.
# It takes the trained network object nn, test input data test_data, and true
# labels test_k. It predicts the labels for the test data using the forward
# propagation step and calculates the misclassification rate.



### Function: netup
# ——————————————————————————————————————————————————————————————————————————————
## Function description:
# This function initializes a neural network with random weights and biases.
# It sets up the structure of the network based on the provided dimensions.
# The function returns the initialized network.

##  Function inputs:
# - d: a vector containing the dimensions of the neural network layers.
#   The length of the vector represents the number of layers in the network;
#   each element in the vector represents the number of neurons in that layer.

## Function outputs:
# - network: a list object representing the initialized neural network which
#   contains the following components:
#   - h: a list of length d, representing the hidden layers of the network;
#     each element in the list is a vector representing the neurons in that layer.
#   - W: a list of length (d - 1), representing the weight matrices between layers;
#     each element in the list is a matrix of size (neurons_in_next_layer x neurons_in_current_layer).
#   - b: a list of length (d - 1), representing the offset vectors for each layer;
#     each element in the list is a vector of size (neurons_in_next_layer).

netup <- function(d) {
  network <- list() # initialize the whole neural network (represented as a list)
  network$h <- vector("list", length(d)) # initialize the hidden layers list
  network$W <- vector("list", length(d) - 1) # initialize the weight matrices list
  network$b <- vector("list", length(d) - 1) # initialize the offset vectors list

  # For each layer of the neural network, initialize the weight matrix and offset vector
  # with Uniform distribution(0,0.2) random deviates
  for (l in 1:(length(d) - 1)) {
    # Generate a weight matrix of random values between 0 and 0.2
    # The size of the matrix is (neurons_in_next_layer x neurons_in_current_layer)
    # Store the matrix in the weight matrices list
    network$W[[l]] <- matrix(runif(d[l + 1] * d[l], min = 0, max = 0.2), nrow = d[l + 1], ncol = d[l])

    # Generate a offset vector of random values between 0 and 0.2
    # The size of the vector is (neurons_in_next_layer x 1)
    # Store the matrix in the offset vector list
    network$b[[l]] <- runif(d[l + 1], min = 0, max = 0.2)
  }
  return(network)
}


### Function: forward
# ——————————————————————————————————————————————————————————————————————————————
##  Function description:
# This function performs the forward propagation step in a neural network.
# Given a neural network and an input vector, it computes the activations of
# all hidden layers.
# The function updates the hidden layer activations in the network object and
# returns the updated network.

##  Function inputs:
# - nn: a list representing the neural network contains the following components:
#   - h: a list of length d, representing the hidden layers of the network;
#     each element in the list is a vector representing the neurons in that layer.
#   - W: a list of length (d - 1), representing the weight matrices between layers;
#     each element in the list is a matrix of size (neurons_in_next_layer x neurons_in_current_layer).
#   - b: a list of length (d - 1), representing the offset vectors for each layer;
#     each element in the list is a vector of size (neurons_in_next_layer).
# - inp: A vector representing the input to the neural network;
#   the size of the vector should match the number of neurons in the input layer.

## Function outputs:
# - nn: the updated neural network with the hidden layer activations computed.

forward <- function(nn, inp) {
  # Set the input layer activations to the provided input vector
  nn$h[[1]] <- inp

  # For each layer of the neural network, calculate the value of hidden layer h
  # based on the weight matrix W and offset vector
  for (l in 2:length(nn$h)) {
    # Create a vector of zeros with the same dimension as the current hidden layer h[[l]]
    Zeros <- rep(0, dim(nn$W[[l - 1]])[1])

    # Compute the activations of the current hidden layer using the formula:
    # activations = max(0, weights * previous_activations + biases)
    # And store the computed activations in the network object
    nn$h[[l]] <- pmax(Zeros, nn$W[[l - 1]] %*% nn$h[[l - 1]] + nn$b[[l - 1]])
  }

  return(nn)
}


### Function: backward
# ——————————————————————————————————————————————————————————————————————————————
##  Function description:
# Computing the derivatives of the loss corresponding to output class k and
# calculating the derivatives of the loss function with respect to nodes, weight
# matrices, and offset vectors.

##  Function inputs:
# - nn: returned from the 'forward' function; contains three lists:
#   - h: all layers calculated based on input, weight matrices, and offset vectors.
#   - w: initial weight matrices for each layer.
#   - b: initial offset vectors for each layer.
# - k: the class to which the input sample belongs.

## Function outputs:
# - nn: 'backward' funciton returns the updated list 'nn' including the following elements:
#   - h: all layers calculated based on input, weight matrices, and offset vectors.
#   - w: initial weight matrices for each layer.
#   - b: initial offset vectors for each layer.
#   - db: derivatives of the loss function with respect to offset vectors for each layer.
#   - dh: derivatives of the loss function with respect to nodes.
#   - dw: derivatives of the loss function with respect to weight matrices for each layer.

backward <- function(nn, k) {
  # Initialize lists for the derivatives
  n <- length(nn$h)
  nn$dh <- vector("list", n)
  nn$dW <- vector("list", n - 1)
  nn$db <- vector("list", n - 1)
  d <- vector("list", n)

  # Loop to calculate derivatives of the loss function with respect to nodes,
  # weight matrices, and offset vectors for each layer
  # For the derivative of the loss function with respect to nodes in the last
  # layer, subtract 1 from the general calculation when j is equal to outclass
  nn$dh[[n]] <- exp(nn$h[[n]]) / sum(exp(nn$h[[n]]))
  nn$dh[[n]][k] <- nn$dh[[n]][k] - 1

  # For the intermediate variable d, when the node is greater than 0,
  # d is equal to the derivative of the loss function with respect to that node
  for (l in n:2) {
    h_length <- length(nn$h[[l]])
    for (j in 1:h_length) {
      if (nn$h[[l]][j] <= 0) {
        d[[l]][j] <- 0
      } else {
        d[[l]][j] <- nn$dh[[l]][j]
      }
    }

    # Using the recursive rule, calculate the derivatives of the loss function
    # with respect to parameters for each layer by backward propagation from the
    # last layer
    nn$db[[l - 1]] <- d[[l]]
    nn$dh[[l - 1]] <- t(nn$W[[l - 1]]) %*% d[[l]]
    nn$dW[[l - 1]] <- d[[l]] %*% t(nn$h[[l - 1]])
  }

  # According to the recursive rule, the intermediate variable for the first
  # layer should be equal to the derivative of the loss function with respect
  # to nodes in the first layer
  d[[1]] <- nn$dh[[1]]

  return(nn)
}


### Function: meanList
# ——————————————————————————————————————————————————————————————————————————————
##  Function description:
# Obtain the average of derivatives for each sample in every batch.

##  Function inputs:
# - batch_list: a list of ten sub-lists and each sub-list contains three
#   matrices or vectors.

## Function outputs:
# - mean_list: a list of three sub-lists and each sub-list contains an average
#   matrix of ten matrices of same layer.

meanList <- function(batch_list) {
  # initialize three sub-lists with first value of first sub-list in batch_list
  mean_1 <- batch_list[[1]][1]
  mean_2 <- batch_list[[1]][2]
  mean_3 <- batch_list[[1]][3]
  mean_list <- list()

  # obtain the sum of ten matrices of same layer
  for (i in 2:length(batch_list)) {
    mean_1[[1]] <- mean_1[[1]] + batch_list[[i]][[1]]
    mean_2[[1]] <- mean_2[[1]] + batch_list[[i]][[2]]
    mean_3[[1]] <- mean_3[[1]] + batch_list[[i]][[3]]
  }

  # obtain the average of ten matrices of same layer
  mean_list <- append(mean_list, list(mean_1[[1]] / length(batch_list)))
  mean_list <- append(mean_list, list(mean_2[[1]] / length(batch_list)))
  mean_list <- append(mean_list, list(mean_3[[1]] / length(batch_list)))

  return(mean_list)
}


### Function: train
# ——————————————————————————————————————————————————————————————————————————————
##  Function description:
# Train the network using forward and backward functions with given parameters.

##  Function inputs:
# - nn: network after initialization.
# - inp: matrix of training set.
# - k: vector the class of iris corresponding to inp.
# - eta: step size of updating parameters.
# - mb: the number of training data to randomly sample to compute the average gradient.
# - nstep: the number of optimization steps using forward and backward function.

## Function outputs:
# - nn: a network consists of updated weight (W) and bias (b):

train <- function(nn, inp, k, eta = .01, mb = 10, nstep = 10000) {
  # iterate each step
  for (epoch in 1:nstep) {
    # initialize lists to record weight(W) and bias(b) for each sample in batch
    batch_dW <- list()
    batch_db <- list()
    # generate index of samples randomly
    batch_index <- sample(1:nrow(inp), mb, replace = FALSE)

    for (i in batch_index) {
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
  return(nn)
}


### Function: misclassification
# ——————————————————————————————————————————————————————————————————————————————
##  Function description:
# Calculate the misclassification rate for the test set.

##  Function inputs:
# - nn: network after updating weight (W) and bias (b).
# - test_data: matrix of test set.
# - test_k: vector the class of iris corresponding to test_data.

## Function outputs:
# Compute and print the misclassification rate message.

misclassification <- function(nn, test_data, test_k) {
  test_prediction <- list()

  # iterate each test sample
  for (i in 1:(nrow(test_data))) {
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
  misclassificationRate <- 1 - test_accuracy

  cat("Misclassification Rate on test set:", misclassificationRate, "\n")
}



### Testing example (using iris data set)
# ————————————————————————————————————————————————————————————————————————————
## Object: Train a 4-8-7-3 network to classify iris to species. Test data is
##         selected by every 5th row of iris data set.
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

# Initialize network
nn <- netup(c(4, 8, 7, 3))
# Train the network
network <- train(nn, inp, k, eta = .01, mb = 10, nstep = 10000)
# Compute misclassification rate
misclassification(network, test_data, test_k)
