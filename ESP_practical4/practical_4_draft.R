# practical 4 draft
data(iris)
iris <- iris[order(iris$Species),]
inp <- as.matrix(iris[,1:4])
#———————————————————————————————————————————————————————————————————————————————
# netup函数：创建表示网络的列表
netup <- function(d) {
  network <- list()
  network$h <- vector("list", length(d))
  network$W <- vector("list", length(d)-1)
  network$b <- vector("list", length(d)-1)
  
  # 初始化权重和偏置
  for (l in 1:(length(d)-1)) {
    network$W[[l]] <- matrix(runif(d[l]*d[l+1], min=0, max=0.2), nrow=d[l], ncol=d[l+1])
    network$b[[l]] <- runif(d[l+1], min=0, max=0.2)
  }
  
  return(network)
}

# Test
nn = netup(c(4, 8, 7, 3))
# a 4-8-7-3 network
# for the first layer:
# W: 4 layers-(4-1) weight parameter matrix
# W^1: 4*8; W^2: 8*7; W^3: 7*3
# b: 4 layers-(4-1) offset parameters
# b^1: 1*8; b^2: 1*7; b^3: 1*3


#———————————————————————————————————————————————————————————————————————————————
# forward函数：计算网络中每个节点的值
# nn: a network list as returned by "netup"
# inp: a vector of input values for the first layer
forward <- function(nn, inp) {
  nn$h[[1]] <- inp
  
  for (l in 2:length(nn$h)) {
    Zeros = matrix(0, nrow = dim(nn$h[[l-1]])[1], ncol = dim(nn$W[[l-1]])[2])
    nn$h[[l]] <- pmax(Zeros, nn$h[[l-1]] %*% nn$W[[l-1]] + nn$b[[l-1]])
  }
  
  return(nn)
}

forward(nn, inp)


#———————————————————————————————————————————————————————————————————————————————
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
  for (l in (n-1):2) {
    nn$dh[[l]] <- ifelse(nn$h[[l]] > 0, nn$W[[l]] %*% nn$dh[[l+1]], 0)
  }
  
  # 计算权重和偏置的导数
  for (l in 1:(n-1)) {
    nn$dW[[l]] <- nn$dh[[l+1]] %*% t(nn$h[[l]])
    nn$db[[l]] <- nn$dh[[l+1]]
  }
  
  return(nn)
}