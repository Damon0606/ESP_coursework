# practical 4 draft
data(iris)
iris <- iris[order(iris$Species), ]
inp <- as.matrix(iris[, 1:4])
# ———————————————————————————————————————————————————————————————————————————————
# netup函数：创建表示网络的列表
# d: a vector giving the number of nodes in each layer of a network
netup <- function(d) {
  network <- list()
  network$h <- vector("list", length(d))
  network$W <- vector("list", length(d) - 1)
  network$b <- vector("list", length(d) - 1)
  
  # 初始化权重和偏置
  for (l in 1:(length(d) - 1)) {
    network$W[[l]] <- matrix(runif(d[l] * d[l + 1], min = 0, max = 0.2), nrow = d[l], ncol = d[l + 1])
    network$b[[l]] <- runif(d[l + 1], min = 0, max = 0.2)
    print(dim(network$W[[l]]))
  }
  
  
  
  return(network)
}

# Test
nn <- netup(c(4, 8, 7, 3))
# a 4-8-7-3 network
# for the first layer:
# W: 4 layers-(4-1) weight parameter matrix
# W^1: 4*8; W^2: 8*7; W^3: 7*3
# b: 4 layers-(4-1) offset parameters
# b^1: 1*8; b^2: 1*7; b^3: 1*3

# ———————————————————————————————————————————————————————————————————————————————
# forward函数：计算网络中每个节点的值
# nn: a network list as returned by "netup"
# inp: a vector of input values for the first layer
forward <- function(nn, inp) {
  nn$h[[1]] <- inp
  
  for (l in 2:length(nn$h)) {
    Zeros <- matrix(0, nrow = dim(nn$h[[l - 1]])[1], ncol = dim(nn$W[[l - 1]])[2])
    nn$h[[l]] <- pmax(Zeros, nn$h[[l - 1]] %*% nn$W[[l - 1]] + nn$b[[l - 1]])
  }
  
  return(nn)
}

# Test
forward(nn, inp)

# ———————————————————————————————————————————————————————————————————————————————
# backward函数：计算损失函数对节点、权重和偏置的导数
# nn: the return from "forward"
# k: the loss corresponding to output class k for network nn
backward <- function(nn, k) {
  n <- length(nn$h)
  
  nn$dh <- vector("list", n)
  nn$dW <- vector("list", n - 1)
  nn$db <- vector("list", n - 1)
  
  # Loss
  # L = -sum(log(nn$dh[[n]])/n)
  
  # Compute the derivative of the loss for k_i w.r.t. h^L_j
  
  # For classes that j ≠ k_i, the derivative remains unchanged.
  nn$dh[[n]] <- exp(nn$h[[n]]) / sum(exp(nn$h[[n]]))
  # For the correct class (i.e., j = k_i), we need to subtract 1 from its derivative
  # Ensure that only the output nodes corresponding to the correct class contribute to the derivative of the loss function.
  nn$dh[[n]][k] <- nn$dh[[n]][k] - 1
  
  # 反向传播计算导数
  for (l in (n - 1):2) {
    cat("W dimensions:", typeof(nn$W[[l]]), "\n")
    cat("h dimensions:", typeof(nn$dh[[l + 1]]]), "\n")
    nn$dh[[l]] <- ifelse(nn$h[[l]] > 0, nn$W[[l]] %*% nn$dh[[l + 1]], 0)
  }
  
  # 计算权重和偏置的导数
  for (l in 1:(n - 1)) {
    nn$dW[[l]] <- nn$dh[[l + 1]] %*% t(nn$h[[l]])
    nn$db[[l]] <- nn$dh[[l + 1]]
  }
  
  return(nn)
}








# practical 4 draft
data(iris)
iris <- iris[order(iris$Species), ]
inp <- as.matrix(iris[, 1:4])
# ———————————————————————————————————————————————————————————————————————————————
# netup函数：创建表示网络的列表
# d: a vector giving the number of nodes in each layer of a network
netup <- function(d) {
  network <- list() # 用空list初始化神经网络
  network$h <- vector("list", length(d)) # 用空list初始化数据集
  network$W <- vector("list", length(d) - 1) # 用空list初始化weight parameter matrix
  network$b <- vector("list", length(d) - 1) # 用空list初始化offset parameters

  # 初始化权重和偏置
  for (l in 1:(length(d) - 1)) {
    network$W[[l]] <- matrix(runif(d[l + 1] * d[l], min = 0, max = 0.2), nrow = d[l + 1], ncol = d[l])
    network$b[[l]] <- runif(d[l + 1], min = 0, max = 0.2)
  }

  print(network$W)
  print(network$h)
  print(network$b)

  return(network)
}

# Test
nn <- netup(c(4, 8, 7, 3))
# a 4-8-7-3 network
# for the first layer:
# h:
# h^1: 4*n; h^2: 8*n; h^3: 7*n; h^4: 3*n;
# W: 4 layers-(4-1) weight parameter matrix
# W^1: 8*4; W^2: 7*8; W^3: 3*7
# b: 4 layers-(4-1) offset parameters
# b^1: 1*8; b^2: 1*7; b^3: 1*3

# ———————————————————————————————————————————————————————————————————————————————
# forward函数：计算网络中每个节点的值
# nn: a network list as returned by "netup"
# inp: a vector of input values for the first layer
forward <- function(nn, inp) {
  nn$h[[1]] <- t(inp)

  for (l in 2:length(nn$h)) {
    cat("W dimensions:", typeof(nn$W[[l - 1]]), "\n")
    cat("h dimensions:", typeof(nn$h[[l - 1]]), "\n")
    cat("b dimensions:", typeof(nn$b[[l - 1]]), "\n")
    Zeros <- matrix(0, nrow = dim(nn$W[[l - 1]])[1], ncol = dim(t(nn$h[[l - 1]]))[2])
    nn$h[[l]] <- pmax(Zeros, nn$W[[l - 1]] %*% nn$h[[l - 1]] + t(nn$b[[l - 1]]))
  }

  return(nn)
}

# Test
forward(nn, inp)



update netup & forward 11.14

# practical 4 draft
data(iris)
iris <- iris[order(iris$Species), ]
inp <- as.matrix(iris[, 1:4])
# ———————————————————————————————————————————————————————————————————————————————
# netup函数：创建表示网络的列表
# d: a vector giving the number of nodes in each layer of a network
netup <- function(d) {
  network <- list()
  network$h <- vector("list", length(d))
  network$W <- vector("list", length(d) - 1)
  network$b <- vector("list", length(d) - 1)
  
  # 初始化权重和偏置
  for (l in 1:(length(d) - 1)) {
    network$W[[l]] <- matrix(runif(d[l+1] * d[l], min = 0, max = 0.2), nrow = d[l+1], ncol = d[l])
    network$b[[l]] <- runif(d[l + 1], min = 0, max = 0.2)
    print(dim(network$W[[l]]))
  }
  
  
  
  return(network)
}

# Test
nn <- netup(c(4, 8, 7, 3))
# a 4-8-7-3 network
# for the first layer:
# W: 4 layers-(4-1) weight parameter matrix
# W^1: 4*8; W^2: 8*7; W^3: 7*3
# b: 4 layers-(4-1) offset parameters
# b^1: 1*8; b^2: 1*7; b^3: 1*3

# ———————————————————————————————————————————————————————————————————————————————
# forward函数：计算网络中每个节点的值
# nn: a network list as returned by "netup"
# inp: a vector of input values for the first layer

forward <- function(nn, inp) {
  nn$h[[1]] <- t(inp)
  
  for (l in 2:length(nn$h)) {
    Zeros <- matrix(0, nrow = dim(nn$W[[l - 1]])[1], ncol = dim(nn$h[[l - 1]])[2])
    nn$h[[l]] <- pmax(Zeros, nn$W[[l - 1]] %*% nn$h[[l - 1]] + nn$b[[l - 1]])
  }
  
  return(nn)
}

# Test

nn<-forward(nn, inp)
numeric_category <- as.numeric(factor(iris$Species, levels = unique(iris$Species)))
# ———————————————————————————————————————————————————————————————————————————————
# backward函数：计算损失函数对节点、权重和偏置的导数
# nn: the return from "forward"
# k: the loss corresponding to output class k for network nn
backward <- function(nn, k) {
  n <- length(nn$h)
  class_number <- length(unique(k))
  ############# 只改了这些
  nn$dh <- nn$h
  nn$dW <- nn$W
  nn$db <- nn$b
  nn$d <- nn$h
  # network <- list()
  # network$h <- vector("list", length(d))
  # network$W <- vector("list", length(d) - 1)
  # network$b <- vector("list", length(d) - 1)
  #############
  sample_size <- dim(nn$h[[1]])[2]
  # Loss
  # L = -sum(log(nn$dh[[n]])/n)
  ## 计算d^L
  for (j in 1:sample_size) {
    for (i in 1:class_number) {
      if(i == k[j]){
        nn$dh[[n]][i,j] <- (exp(nn$h[[n]][i,j]) / sum(exp(nn$h[[n]])[,j]))-1
      }else{
        nn$dh[[n]][i,j] <- exp(nn$h[[n]][i,j]) / sum(exp(nn$h[[n]])[,j])
      }
    }
  }
  
  ## 计算剩余层的d,dw,db,

  for (l in n:2) {
    ## 计算各层的d & db
    for (j in 1:sample_size) {
      for (i in 1:class_number) {
        if(nn$dh[[l]][i,j]>0){
          nn$d[[l]][i,j] <- nn$dh[[l]][i,j]
        }else{
          nn$d[[l]][i,j] <- 0
        }
      }
    }
    nn$db[[l-1]] = nn$d[[l]]
    nn$dh[[l-1]] = t(nn$W[[l-1]]) %*% nn$d[[l]]
    nn$dw[[l-1]] = nn$d[[l]] %*% t(nn$h[[l-1]])
  }
  
  return(nn)
}


#### 加的train函数
set.seed(123)
test_indices <- seq(5, nrow(inp), by = 5)
train_indices <- setdiff(1:nrow(inp), test_indices)
test_data <- inp[test_indices, ]
numeric_category <- as.numeric(factor(iris$Species, levels = unique(iris$Species)))
test_labels <- numeric_category[test_indices]
train_data <- inp[train_indices, ]
train_labels <- numeric_category[train_indices]

train <- function(nn, inp, k, eta=.01, mb=10, nstep=10000){
  n <- nrow(inp)
  for (epoch in 1:nstep) {
    batches <- sample(1:n, mb, replace = FALSE)
    for (i in batches) {
      nn <- forward(nn, inp[i, ])
      nn <- backward(nn, k[i])
      for (l in 1:length(nn$W)){
        nn$W[[l]] <- nn$W[[l]] - eta * t(nn$dW[[l]])
        nn$b[[l]] <- nn$b[[l]] - eta * nn$db[[l]]
      }
    }
  }
}
train(nn, train_data, train_labels, eta=.01, mb=10, nstep=10000)

misclassification <- function(nn, test_data, test_k){
  prediction <- forward(nn, test_data)
  prediction_label <- max.col(prediction)+1
  test_accuracy <- sum(prediction_label == test_k) / nrow(test_data)
  misclassificationRate <- 1-test_accuracy
  cat("Misclassification Rate on test set:", misclassificationRate, "\n")
}
misclassification(nn, test_data, test_labels)




#——————————————————————————————————————————————————————————
#——————————————————————————————————————————————————————————
#——————————————————————————————————————————————————————————
# 2023.11.14 update
# practical 4 draft

# ———————————————————————————————————————————————————————————————————————————————
# netup函数：创建表示网络的列表
# d: a vector giving the number of nodes in each layer of a network
netup <- function(d) {
  network <- list()
  network$h <- vector("list", length(d))
  network$W <- vector("list", length(d) - 1)
  network$b <- vector("list", length(d) - 1)

  # 初始化权重和偏置
  for (l in 1:(length(d) - 1)) {
    network$W[[l]] <- matrix(runif(d[l + 1] * d[l], min = 0, max = 0.2), nrow = d[l + 1], ncol = d[l])
    network$b[[l]] <- runif(d[l + 1], min = 0, max = 0.2)
  }
  return(network)
}


# ———————————————————————————————————————————————————————————————————————————————
# forward函数：计算网络中每个节点的值
# nn: a network list as returned by "netup"
# inp: a vector of input values for the first layer
forward <- function(nn, inp) {
  nn$h[[1]] <- t(inp)

  for (l in 2:length(nn$h)) {
    Zeros <- matrix(0, nrow = dim(nn$W[[l - 1]])[1], ncol = dim(nn$h[[l - 1]])[2])
    nn$h[[l]] <- pmax(Zeros, nn$W[[l - 1]] %*% nn$h[[l - 1]] + nn$b[[l - 1]])
  }

  return(nn)
}


# ———————————————————————————————————————————————————————————————————————————————
# backward函数：计算损失函数对节点、权重和偏置的导数
# nn: the return from "forward"
# k: the loss corresponding to output class k for network nn
backward <- function(nn, k) {
  n <- length(nn$h)
  class_number <- length(unique(k))
  nn$dh <- nn$h
  nn$dW <- nn$W
  nn$db <- nn$b
  nn$d <- nn$h
  sample_size <- dim(nn$h[[1]])[2]

  ## 计算d^L
  for (j in 1:sample_size) {
    for (i in 1:class_number) {
      if (i == k[j]) {
        nn$dh[[n]][i, j] <- (exp(nn$h[[n]][i, j]) / sum(exp(nn$h[[n]])[, j])) - 1
      } else {
        nn$dh[[n]][i, j] <- exp(nn$h[[n]][i, j]) / sum(exp(nn$h[[n]])[, j])
      }
    }
  }

  ## 计算剩余层的d,dw,db,
  for (l in n:2) {
    ## 计算各层的d & db
    for (j in 1:sample_size) {
      for (i in 1:class_number) {
        if (nn$dh[[l]][i, j] > 0) {
          nn$d[[l]][i, j] <- nn$dh[[l]][i, j]
        } else {
          nn$d[[l]][i, j] <- 0
        }
      }
    }
    nn$db[[l - 1]] <- nn$d[[l]]
    nn$dh[[l - 1]] <- t(nn$W[[l - 1]]) %*% nn$d[[l]]
    nn$dw[[l - 1]] <- nn$d[[l]] %*% t(nn$h[[l - 1]])
  }
  return(nn)
}

# data(iris)
# iris <- iris[order(iris$Species), ]
# inp <- as.matrix(iris[, 1:4])
# test_indices <- seq(5, nrow(inp), by = 5)
# train_indices <- setdiff(1:nrow(inp), test_indices)
# test_data <- inp[test_indices, ]
# train_data <- inp[train_indices, ]
# numeric_category <- as.numeric(factor(iris$Species, levels = unique(iris$Species)))
# test_labels <- numeric_category[test_indices]
# train_labels <- numeric_category[train_indices]
# 
# 
# n <- nrow(train_data)
# batch_index <- sample(1:n, 10, replace = FALSE)
# inp[batch_index, ]
# train_labels[batch_index]
# ff1 <- forward(nn, inp[batch_index, ])
# ff1 <- backward(ff1, train_labels[batch_index])
# 
# for (l in 1:length(ff1$W)) {
#   nn$W[[l]] <- nn$W[[l]] - .01 * nn$dW[[l]]
#   nn$b[[l]] <- nn$b[[l]] - .01 * nn$db[[l]]
# }

# ———————————————————————————————————————————————————————————————————————————————
# train函数：训练网络
train <- function(nn, inp, k, eta = .01, mb = 10, nstep = 10000) {
  n <- nrow(inp)
  for (epoch in 1:nstep) {
    batch_index <- sample(1:n, mb, replace = FALSE)
    nn <- forward(nn, inp[batch_index, ])
    nn <- backward(nn, k[batch_index])

    for (l in 1:length(nn$W)) {
      nn$W[[l]] <- nn$W[[l]] - eta * nn$dW[[l]]
      nn$b[[l]] <- nn$b[[l]] - eta * nn$db[[l]]
    }
  cat(nn$W[[1]], '\n')
  }
  return(nn)
}


# ———————————————————————————————————————————————————————————————————————————————
# 根据iris数据集训练网络

# 划分训练集和测试集
data(iris)
iris <- iris[order(iris$Species), ]
inp <- as.matrix(iris[, 1:4])
test_indices <- seq(5, nrow(inp), by = 5)
train_indices <- setdiff(1:nrow(inp), test_indices)
test_data <- inp[test_indices, ]
train_data <- inp[train_indices, ]
numeric_category <- as.numeric(factor(iris$Species, levels = unique(iris$Species)))
test_labels <- numeric_category[test_indices]
train_labels <- numeric_category[train_indices]

# 创建并训练网络
nn <- netup(c(4, 8, 7, 3))
network <- train(nn, train_data, train_labels, eta = .01, mb = 10, nstep = 10000)




## 11.15 update (expect train)
# practical 4 draft
data(iris)
iris <- iris[order(iris$Species), ]
inp <- as.matrix(iris[1, 1:4])
# ———————————————————————————————————————————————————————————————————————————————
# netup函数：创建表示网络的列表
# d: a vector giving the number of nodes in each layer of a network
netup <- function(d) {
  network <- list()
  network$h <- vector("list", length(d))
  network$W <- vector("list", length(d) - 1)
  network$b <- vector("list", length(d) - 1)
  
  # 初始化权重和偏置
  for (l in 1:(length(d) - 1)) {
    network$W[[l]] <- matrix(runif(d[l+1] * d[l], min = 0, max = 0.2), nrow = d[l+1], ncol = d[l])
    network$b[[l]] <- runif(d[l + 1], min = 0, max = 0.2)
    print(dim(network$W[[l]]))
  }
  

  return(network)
}

# Test
nn <- netup(c(4, 8, 7, 3))
# a 4-8-7-3 network
# for the first layer:
# W: 4 layers-(4-1) weight parameter matrix
# W^1: 4*8; W^2: 8*7; W^3: 7*3
# b: 4 layers-(4-1) offset parameters
# b^1: 1*8; b^2: 1*7; b^3: 1*3

# ———————————————————————————————————————————————————————————————————————————————
# forward函数：计算网络中每个节点的值
# nn: a network list as returned by "netup"
# inp: a vector of input values for the first layer

forward <- function(nn, inp) {
  nn$h[[1]] <- t(inp)
  

  for (l in 2:length(nn$h)) {
    Zeros <- matrix(0, nrow = dim(nn$W[[l - 1]])[1], ncol = 1)
    nn$h[[l]] <- pmax(Zeros, nn$W[[l - 1]] %*% nn$h[[l - 1]] + nn$b[[l - 1]])
  }
  
  return(nn)
}

# Test

nn<-forward(nn, inp)
numeric_category <- as.numeric(factor(iris$Species, levels = unique(iris$Species)))
# ———————————————————————————————————————————————————————————————————————————————
# backward函数：计算损失函数对节点、权重和偏置的导数
# nn: the return from "forward"
# k: the loss corresponding to output class k for network nn
inp_test_k <- numeric_category[1]

backward <- function(nn, k) {
  n <- length(nn$h)
  nn$dh <- nn$h; nn$dW <- nn$W;nn$db <- nn$b; nn$d <- nn$h
  # Loss
  # L = -sum(log(nn$dh[[n]])/n)
  ## 计算dh^L
  nn$dh[[n]] <- exp(nn$h[[n]]) / sum(exp(nn$h[[n]]))
  nn$dh[[n]][k] <- nn$dh[[n]][k]-1
  
  
  ## 计算剩余层的d,dW,db,
  for (l in n:2) {
    h_length <- length(nn$h[[l]])
    for (j in 1:h_length) {
      if(nn$h[[l]][j] <=0){
        nn$d[[l]][j] = 0
      }else{
        nn$d[[l]][j] = nn$dh[[l]][j]
      }
    }
    nn$db[[l-1]] <- nn$d[[l]]
    nn$dh[[l-1]] <- t(nn$W[[l-1]]) %*% nn$d[[l]]
    nn$dW[[l-1]] <- nn$d[[l]] %*% t(nn$h[[l-1]])
  }
  return(nn)
}

nn <- backward(nn,inp_test_k)

nn <- backward(nn,inp_test_k)