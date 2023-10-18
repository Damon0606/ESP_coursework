set.seed(123)
qsim <- function(mf,mb,a.rate,trb,trf,tmb,tmf,maxb){
  car_timetable <- sample(c(0, 1), size=2*60*60, replace = TRUE, prob=c(0.9, 0.1))
  french_time<- matrix(nrow = 5,ncol = 2*60*60)
  british_time<- matrix(nrow = 5,ncol = 2*60*60)                     
  for (i in 1:5) {
    french_time[i,]=round(runif(n=2*60*60, min=tmf, max=tmf+trf), 0)
    british_time[i,] <- round(runif(n=2*60*60, min=tmb, max=tmb+trb), 0)
  }
  return (list(car_timetable, french_time, british_time))
}
qsim(5,5,.1,40,40,30,30,20)



### 1 country 1 stations
set.seed(123)

# total_time <- 2 * 60 * 60
# closed_time <- total_time - 30 * 60
total_time <- 100
closed_time <- 85
tmf <- 15
trf <- 5

# Simulated original data
car_timetable <- sample(c(0, 1), size = total_time, replace = TRUE, prob = c(0.9, 0.1))
car_timetable[(closed_time + 1):total_time] <- 0 # check-in closes 30 minutes before departure
arrive_time <- which(car_timetable == 1)
num_car <- length(arrive_time)
french_time <- round(runif(n = num_car, min = tmf, max = tmf + trf), 0)

# Initialization
waiting_time <- rep(0, num_car)
finish_time <- rep(0, num_car)
queue <- rep(0, num_car)
### !!!假设同一秒中可以同时完成车辆的驶出和驶入!!!

# car_timetable: 每秒钟是否有车到达
# arrive_time: 所有车到达的时间点
# num_car: 在规定的时间内总共有多少车到达
# french_time[i]: 闸口处理第i辆车的时间
# watiting_time[i]: 第i辆车排队等待的时间
# finish_time[i]: 第i辆车完成的时间
# queue[i]: 代表第i辆车到达时排队队伍的长度

for (i in 1:num_car) {
  time_point <- arrive_time[i]
  if (queue[i] == 0) {
    finish_time[i] <- time_point + french_time[i]
  } else if (queue[i] == 1) {
    waiting_time[i] <- finish_time[i - 1] - time_point
    # if (waiting_time[i] < 0) {
    #   queue[i + 1] =+ 1
    #   waiting_time[i] <- abs(waiting_time[i])
    #   break
    # }
    finish_time[i] <- time_point + waiting_time[i] + french_time[i]
  } else {
    waiting_time[i] <- sum(french_time[(i - queue[i] + 1):(i - 1)]) + (finish_time[i - queue[i]] - time_point)
    finish_time[i] <- time_point + waiting_time[i] + french_time[i]
    # if (waiting_time[i] < 0) {
    #   queue[i + 1] =+ 1
    #   waiting_time[i] <- abs(waiting_time[i])
    #   break
    # }
  }
  if (i != num_car){
    queue[i + 1] <- i - sum(finish_time[1:i] < arrive_time[i+1])
  }
}

### End of the simple case



# 1 country 5 stations
total_time <- 500
closed_time <- 490
tmf <- 40
trf <- 30
mf <- 5


# Simulated original data
set.seed(66)
car_timetable <- sample(c(0, 1), size = total_time, replace = TRUE, prob = c(0.9, 0.1))
car_timetable[(closed_time + 1):total_time] <- 0 # check-in closes 30 minutes before departure
arrive_time_f <- which(car_timetable == 1)
num_car <- length(arrive_time)
french_time <- round(runif(n = num_car, min = tmf, max = tmf + trf), 0) ##把french time随机到每个车上


# Initialization
waiting_time_f <- rep(0, num_car)
finish_time_f <- rep(0, num_car)
station_choice_f <- rep(0, num_car)
queue_f <- matrix(0, nrow = num_car, ncol = mf)


for (i in 1:num_car) {
  time_point <- arrive_time_f[i]
  queue_index <- which(queue_f[i,1:5] == min(queue_f[i, ]))[1]
  station_choice_f[i] <- queue_index
  if (queue_f[i, queue_index] == 0) {
    finish_time_f[i] <- time_point + french_time[i]
  } else if (queue_f[i, queue_index] == 1) {
    waiting_time_f[i] <- finish_time_f[i - 1] - time_point
    finish_time_f[i] <- time_point + waiting_time_f[i] + french_time[i]
  } else {
    waiting_time_f[i] <- sum(french_time[(i - queue_f[i, queue_index] + 1):(i - 1)]) + (finish_time_f[i - queue_f[i, queue_index]] - time_point)
    finish_time_f[i] <- time_point + waiting_time_f[i] + french_time[i]
  }
  if (i != num_car) {
    for (j in 1:mf) {
      k <- which(station_choice_f == j)
      queue_f[i + 1, j] <- length(k) - sum(finish_time_f[k] < arrive_time_f[i + 1])
    }
  }
}

# cat(
#   "arrive_time:", arrive_time_f, "\n",
#   "station_choice:", station_choice_f, "\n",
#   "waiting_time:", waiting_time_f, "\n",
#   "french_time:", french_time, "\n",
#   "finish_time:", finish_time_f, "\n",
#   "queue:", "\n"
# )
# print(queue_f)
# 
# combined_mat <- cbind(arrive_time_f, station_choice_f, waiting_time_f, french_time, finish_time_f)
# wb <- createWorkbook()
# addWorksheet(wb, "Data")
# writeData(wb, sheet = "Data", combined_mat, startCol = 1)
# writeData(wb, sheet = "Data", queue_f, startCol = 6)
#saveWorkbook(wb, file = "Output.xlsx", overwrite = TRUE)

## add birtish time
waiting_time_b <- rep(0, num_car)
finish_time_b <- rep(0, num_car)
station_choice_b <- rep(0, num_car)
queue_b <- matrix(0, nrow = num_car, ncol = mf)
arrive_time_b <- finish_time_f
british_time <- round(runif(n = num_car, min = tmf, max = tmf + trf), 0) 


for (i in 1:num_car) {
  time_point <- arrive_time_b[i]
  queue_index <- which(queue_b[i,1:5] == min(queue_b[i, ]))[1]
  station_choice_b[i] <- queue_index
  if (queue_b[i, queue_index] == 0) {
    finish_time_b[i] <- time_point + british_time[i]
  } else if (queue_f[i, queue_index] == 1) {
    waiting_time_b[i] <- finish_time_b[i - 1] - time_point
    finish_time_b[i] <- time_point + waiting_time_b[i] + french_time[i]
  } else {
    waiting_time_b[i] <- sum(french_time[(i - queue_b[i, queue_index] + 1):(i - 1)]) + (finish_time_b[i - queue_b[i, queue_index]] - time_point)
    finish_time_b[i] <- time_point + waiting_time_b[i] + french_time[i]
  }
  if (i != num_car) {
    for (j in 1:mf) {
      k <- which(station_choice_b == j)
      queue_b[i + 1, j] <- length(k) - sum(finish_time_b[k] < arrive_time_b[i + 1])
    }
  }
}



# 2 countries 5 stations
total_time <- 500
closed_time <- 490
tmf <- 40
trf <- 30
mf <- 5
tmb <- 40
trb <- 30
mb <- 5


# Simulated original data for French stations
set.seed(66)
car_timetable <- sample(c(0, 1), size = total_time, replace = TRUE, prob = c(0.9, 0.1))
car_timetable[(closed_time + 1):total_time] <- 0 # check-in closes 30 minutes before departure
arrive_time_f <- which(car_timetable == 1)
num_car <- length(arrive_time)
french_time <- round(runif(n = num_car, min = tmf, max = tmf + trf), 0) ## 把french time随机到每个车上


# Initialization French stations
waiting_time_f <- rep(0, num_car)
finish_time_f <- rep(0, num_car)
station_choice_f <- rep(0, num_car)
queue_f <- matrix(0, nrow = num_car, ncol = mf)


# French stations
for (i in 1:num_car) {
  time_point <- arrive_time_f[i]
  queue_index <- which(queue_f[i, 1:5] == min(queue_f[i, ]))[1]
  station_choice_f[i] <- queue_index
  if (queue_f[i, queue_index] == 0) {
    finish_time_f[i] <- time_point + french_time[i]
  } else if (queue_f[i, queue_index] == 1) {
    waiting_time_f[i] <- finish_time_f[i - 1] - time_point
    finish_time_f[i] <- time_point + waiting_time_f[i] + french_time[i]
  } else {
    waiting_time_f[i] <- sum(french_time[(i - queue_f[i, queue_index] + 1):(i - 1)]) + (finish_time_f[i - queue_f[i, queue_index]] - time_point)
    finish_time_f[i] <- time_point + waiting_time_f[i] + french_time[i]
  }
  if (i != num_car) {
    for (j in 1:mf) {
      k <- which(station_choice_f == j)
      queue_f[i + 1, j] <- length(k) - sum(finish_time_f[k] < arrive_time_f[i + 1])
    }
  }
}


# cat(
#   "arrive_time:", arrive_time_f, "\n",
#   "station_choice:", station_choice_f, "\n",
#   "waiting_time:", waiting_time_f, "\n",
#   "french_time:", french_time, "\n",
#   "finish_time:", finish_time_f, "\n",
#   "queue:", "\n"
# )
# print(queue_f)
#
# combined_mat <- cbind(arrive_time_f, station_choice_f, waiting_time_f, french_time, finish_time_f)
# wb <- createWorkbook()
# addWorksheet(wb, "Data")
# writeData(wb, sheet = "Data", combined_mat, startCol = 1)
# writeData(wb, sheet = "Data", queue_f, startCol = 6)
# saveWorkbook(wb, file = "Output.xlsx", overwrite = TRUE)


# Initialization British stations
british_time <- round(runif(n = num_car, min = tmb, max = tmb + trb), 0)

# Simulated original data for British stations
waiting_time_b <- rep(0, num_car)
finish_time_b <- rep(0, num_car)
station_choice_b <- rep(0, num_car)
queue_b <- matrix(0, nrow = num_car, ncol = mf)
arrive_time_b <- finish_time_f


# British stations
for (i in 1:num_car) {
  if (sum(queue_b[i, ] == 20) == 5) {
    earliest_index <- which(finish_time_b == min(finish_time_b[(finish_time_b - arrive_time_b[i]) > 0]))
    extra_time <- finish_time_b[earliest_index] - arrive_time_b[i]

    # station_choice_in_f <- station_choice_f[i]
    # tp <- finish_time_f[i]
    # i_behind <- which(finish_time_f == min(arrive_time_f[(arrive_time_f-tp)>0))
    # q <- queue_f[i_behind, station_choice_in_f]
    # waiting_time_f[i + 1:i + q] <- waiting_time_f[i + 1:i + q] + extra_time
  }

  time_point <- arrive_time_b[i]
  queue_index <- which(queue_b[i, 1:5] == min(queue_b[i, ]))[1]
  station_choice_b[i] <- queue_index
  if (queue_b[i, queue_index] == 0) {
    finish_time_b[i] <- time_point + british_time[i]
  } else if (queue_f[i, queue_index] == 1) {
    waiting_time_b[i] <- finish_time_b[i - 1] - time_point
    finish_time_b[i] <- time_point + waiting_time_b[i] + french_time[i]
  } else {
    waiting_time_b[i] <- sum(french_time[(i - queue_b[i, queue_index] + 1):(i - 1)]) + (finish_time_b[i - queue_b[i, queue_index]] - time_point)
    finish_time_b[i] <- time_point + waiting_time_b[i] + french_time[i]
  }
  if (i != num_car) {
    for (j in 1:mf) {
      k <- which(station_choice_b == j)
      queue_b[i + 1, j] <- length(k) - sum(finish_time_b[k] < arrive_time_b[i + 1])
    }
  }
}



### 2023.10.15 ———————————————————————————————————————————————————————————————————————————————
# 2 country 5 stations
# Basic parameters
# total_time <- 500
# closed_time <- 490
total_time <- 2 * 60 * 60
closed_time <- total_time - 30 * 60

tmf <- 30
trf <- 40
mf <- 5

tmb <- 40
trb <- 40
mb <- 5

# Simulate cars
set.seed(66)
car_timetable <- sample(c(0, 1), size = total_time, replace = TRUE, prob = c(0.9, 0.1))
car_timetable[(closed_time + 1):total_time] <- 0 # check-in closes 30 minutes before departure
arrive_time_f <- which(car_timetable == 1)
num_car <- length(arrive_time_f)


#———————————————————————————————————————————————————————————————————————————————
### French Part
# Simulated original data for French stations
french_time <- round(runif(n = num_car, min = tmf, max = tmf + trf), 0) ## 把french time随机到每个车上

# Initialization French stations
waiting_time_f <- rep(0, num_car)
finish_time_f <- rep(0, num_car)
station_choice_f <- rep(0, num_car)
queue_f <- matrix(0, nrow = num_car, ncol = mf)
stations_record_f <- list(fs1 = NULL, fs2 = NULL, fs3 = NULL, fs4 = NULL, fs5 = NULL)


# French stations
for (i in 1:num_car) {
  time_point <- arrive_time_f[i]
  queue_index <- which.min(queue_f[i,])[1]
  station_choice_f[i] <- queue_index
  group <- stations_record_f[[queue_index]]
  group_length <- length(group)
  if (queue_f[i, queue_index] == 0) {
    finish_time_f[i] <- time_point + french_time[i]
  } else if (queue_f[i, queue_index] == 1) {
    waiting_time_f[i] <- group[[group_length]] - time_point
    finish_time_f[i] <- time_point + waiting_time_f[i] + french_time[i]
  } else {
    processing_car <- (group[group_length - queue_f[i, queue_index]] - time_point)
    waiting_time_f[i] <- processing_car + sum(group[(group_length - (queue_f[i, queue_index]) + 1):group_length])
    finish_time_f[i] <- time_point + waiting_time_f[i] + french_time[i]
  }
  stations_record_f[[queue_index]][group_length + 1] <- finish_time_f[i]
  if (i != num_car) {
    for (j in 1:mf) {
      k <- which(station_choice_f == j)
      queue_f[i + 1, j] <- length(k) - sum(finish_time_f[k] < arrive_time_f[i + 1])
    }
  }
}

###
cat(
  "arrive_time:", arrive_time_f, "\n",
  "station_choice:", station_choice_f, "\n",
  "waiting_time:", waiting_time_f, "\n",
  "french_time:", french_time, "\n",
  "finish_time:", finish_time_f, "\n",
  # "stations_record_f", stations_record_f, "\n",
  "queue:", "\n"
)
print(queue_f)

library(openxlsx)
combined_mat_1 <- cbind(arrive_time_f, station_choice_f, waiting_time_f, french_time, finish_time_f)
wb1 <- createWorkbook()
addWorksheet(wb1, "Data")
writeData(wb1, sheet = "Data", combined_mat_1, startCol = 1)
writeData(wb1, sheet = "Data", queue_f, startCol = 6)
saveWorkbook(wb1, file = "Output_1.xlsx", overwrite = TRUE)
###

# put all the french data into a data frame
df_french <- cbind(arrive_time_f, station_choice_f, queue_f, waiting_time_f, french_time, finish_time_f)
colnames(df_french) <- c("arrive_time_f", "station_choice_f", "qf1", "qf2", "qf3", "qf4", "qf5", "waiting_time_f", "french_time", "finish_time_f")


#———————————————————————————————————————————————————————————————————————————————
### British Part
# Simulated original data for British stations
british_time <- round(runif(n = num_car, min = tmb, max = tmb + trb), 0)

# Initialization British stations
arrive_time_b <- finish_time_f
waiting_time_b <- rep(0, num_car)
finish_time_b <- rep(0, num_car)
station_choice_b <- rep(0, num_car)
queue_b <- matrix(0, nrow = num_car, ncol = mf)
stations_record_b <- list(bs1 = NULL, bs2 = NULL, bs3 = NULL, bs4 = NULL, bs5 = NULL)



# British stations
for (i in 1:num_car) {
  if (sum(queue_b[i, ] == 20) == 5) {
    
    earliest_index <- which(finish_time_b == min(finish_time_b[(finish_time_b - arrive_time_b[i]) > 0]))
    extra_time <- finish_time_b[earliest_index] - arrive_time_b[i]
    
    station_choice_in_f <- station_choice_f[i]
    tp <- finish_time_f[i]
    selected_ft <- stations_record_f[[station_choice_in_f]]
    
    selected_at <-  df_french[df_french$finish_time_f == selected_ft, ][arrive_time_f]
    selected_car <- which.min(df_french$arrive_time_f[selected_at - tp > 0])
    length_behind <- data_french[selected_car][station_choice_in_f]
 
    waiting_time_f[length_behind] <- waiting_time_f[stop_index] + extra_time
    finish_time_f <- waiting_time_f[stop_index] + extra_time
  }
  time_point <- arrive_time_b[i]
  queue_index <- which.min(queue_b[i,])[1]
  station_choice_b[i] <- queue_index
  group <- stations_record_b[[queue_index]]
  group_length <- length(group)
  if (queue_b[i, queue_index] == 0) {
    finish_time_b[i] <- time_point + british_time[i]
  } else if (queue_b[i, queue_index] == 1) {
    waiting_time_b[i] <- group[[group_length]] - time_point
    finish_time_b[i] <- time_point + waiting_time_b[i] + british_time[i]
  } else {
    processing_car <- (group[group_length - queue_b[i, queue_index]] - time_point)
    waiting_time_b[i] <- processing_car + sum(group[(group_length - (queue_b[i, queue_index]) + 1):group_length])
    finish_time_b[i] <- time_point + waiting_time_b[i] + british_time[i]
  }
  stations_record_b[[queue_index]][group_length + 1] <- finish_time_b[i]
  if (i != num_car) {
    for (j in 1:mb) {
      k <- which(station_choice_b == j)
      queue_b[i + 1, j] <- length(k) - sum(finish_time_b[k] < arrive_time_b[i + 1])
    }
  }
}

###
combined_mat_2 <- cbind(arrive_time_b, station_choice_b, waiting_time_b, british_time, finish_time_b)
wb2 <- createWorkbook()
addWorksheet(wb2, "Data")
writeData(wb2, sheet = "Data", combined_mat_2, startCol = 1)
writeData(wb2, sheet = "Data", queue_b, startCol = 6)
saveWorkbook(wb2, file = "Output_2.xlsx", overwrite = TRUE)
###

# put all the data into a data frame
df_french <- cbind(arrive_time_f, station_choice_f, queue_f, waiting_time_f, french_time, finish_time_f)
colnames(df_french) <- c("arrive_time_f", "station_choice_f", "qf1", "qf2", "qf3", "qf4", "qf5", "waiting_time_f", "french_time", "finish_time_f")
df_british <- cbind(arrive_time_b, station_choice_b, queue_b, waiting_time_b, british_time, finish_time_b)
colnames(df_british) <- c("arrive_time_b", "station_choice_b", "qb1", "qb2", "qb3", "qb4", "qb5", "waiting_time_b", "british_time", "finish_time_b")
df_simulation <- cbind(df_french, df_british)
wb <- createWorkbook()
addWorksheet(wb, "Data")
writeData(wb, sheet = "Data", df_simulation, startCol = 1)
saveWorkbook(wb, file = "Output_final.xlsx", overwrite = TRUE)










### 2023.10.17
# 2 country 5 stations
# Basic parameters
# total_time <- 500
# closed_time <- 490
total_time <- 2 * 60 * 60
closed_time <- total_time - 30 * 60

tmf <- 30
trf <- 40
mf <- 5

tmb <- 60
trb <- 40
mb <- 5

# Simulate cars
set.seed(66)
car_timetable <- sample(c(0, 1), size = total_time, replace = TRUE, prob = c(0.9, 0.1))
car_timetable[(closed_time + 1):total_time] <- 0 # check-in closes 30 minutes before departure
arrive_time_f <- which(car_timetable == 1)
num_car <- length(arrive_time_f)


#———————————————————————————————————————————————————————————————————————————————
### French Part
# Simulated original data for French stations
french_time <- runif(n = num_car, min = tmf, max = tmf + trf) ## 把french time随机到每个车上

# Initialization French stations
waiting_time_f <- rep(0, num_car)
finish_time_f <- rep(0, num_car)
station_choice_f <- rep(0, num_car)
queue_f <- matrix(0, nrow = num_car, ncol = mf)
stations_record_f <- list(fs1 = NULL, fs2 = NULL, fs3 = NULL, fs4 = NULL, fs5 = NULL)


# French stations
for (i in 1:num_car) {
  time_point <- arrive_time_f[i]
  queue_index <- which.min(queue_f[i,])[1]
  station_choice_f[i] <- queue_index
  group <- stations_record_f[[queue_index]]
  group_length <- length(group)
  if (queue_f[i, queue_index] == 0) {
    finish_time_f[i] <- time_point + french_time[i]
  } else if (queue_f[i, queue_index] == 1) {
    waiting_time_f[i] <- group[[group_length]] - time_point
    finish_time_f[i] <- time_point + waiting_time_f[i] + french_time[i]
  } else {
    processing_car <- group[group_length - queue_f[i, queue_index] + 1] - time_point
    waiting_time_f[i] <- processing_car + sum(french_time[(group_length - (queue_f[i, queue_index]) + 2):group_length])
    finish_time_f[i] <- time_point + waiting_time_f[i] + french_time[i]
  }
  stations_record_f[[queue_index]][group_length + 1] <- finish_time_f[i]
  if (i != num_car) {
    for (j in 1:mf) {
      k <- which(station_choice_f == j)
      queue_f[i + 1, j] <- length(k) - sum(finish_time_f[k] < arrive_time_f[i + 1])
    }
  }
}


#———————————————————————————————————————————————————————————————————————————————
### British Part
# Simulated original data for British stations
british_time <- runif(n = num_car, min = tmb, max = tmb + trb)

# Initialization British stations
arrive_time_b <- finish_time_f
waiting_time_b <- rep(0, num_car)
finish_time_b <- rep(0, num_car)
station_choice_b <- rep(0, num_car)
queue_b <- matrix(0, nrow = num_car, ncol = mf)
stations_record_b <- list(bs1 = NULL, bs2 = NULL, bs3 = NULL, bs4 = NULL, bs5 = NULL)


# British stations
for (i in 1:num_car) {
  time_point <- arrive_time_b[i]
  queue_index <- which.min(queue_b[i,])[1]
  station_choice_b[i] <- queue_index
  group <- stations_record_b[[queue_index]]
  group_length <- length(group)
  if (queue_b[i, queue_index] == 0) {
    finish_time_b[i] <- time_point + british_time[i]
  } else if (queue_b[i, queue_index] == 1) {
    waiting_time_b[i] <- group[[group_length]] - time_point
    finish_time_b[i] <- time_point + waiting_time_b[i] + british_time[i]
  } else {
    processing_car <- group[group_length - queue_b[i, queue_index] + 1] - time_point
    waiting_time_b[i] <- processing_car + sum(british_time[(group_length - (queue_b[i, queue_index]) + 2):group_length])
    finish_time_b[i] <- time_point + waiting_time_b[i] + british_time[i]
  }
  stations_record_b[[queue_index]][group_length + 1] <- finish_time_b[i]
  if (i != num_car) {
    for (j in 1:mb) {
      k <- which(station_choice_b == j)
      queue_b[i + 1, j] <- length(k) - sum(finish_time_b[k] < arrive_time_b[i + 1])
    }
  }
}


queue_f_sum <- rowSums(queue_f)
queue_b_sum <- rowSums(queue_b)

for (i in 1:num_car) {
  if (queue_b_sum[i] > 100){
    queue_f_sum[i] =+ queue_b_sum[i] - 100
    queue_b_sum[i] <- queue_b_sum[i] - 100
  }
}





# ——————————————————————————————————————————————————————————————————————————————— #
### 2023.10.18
start_time <- Sys.time() # 记录开始时间

# 2 country 5 stations
# Basic parameters
# total_time <- 500
# closed_time <- 490
total_time <- 2 * 60 * 60
closed_time <- total_time - 30 * 60

tmf <- 30
trf <- 40
mf <- 5

tmb <- 60
trb <- 40
mb <- 5

# Simulate cars
set.seed(66)
car_timetable <- sample(c(0, 1), size = total_time, replace = TRUE, prob = c(0.9, 0.1))
car_timetable[(closed_time + 1):total_time] <- 0 # check-in closes 30 minutes before departure
arrive_time_f <- which(car_timetable == 1)
num_car <- length(arrive_time_f)


# ———————————————————————————————————————————————————————————————————————————————
### French Part
# Simulated original data for French stations
french_time <- runif(n = num_car, min = tmf, max = tmf + trf) ## 把french time随机到每个车上

# Initialization French stations
waiting_time_f <- rep(0, num_car)
finish_time_f <- rep(0, num_car)
station_choice_f <- rep(0, num_car)
queue_f <- matrix(0, nrow = num_car, ncol = mf)
stations_record_f <- list(fs1 = NULL, fs2 = NULL, fs3 = NULL, fs4 = NULL, fs5 = NULL)

# French stations
for (i in 1:num_car) {
  time_point <- arrive_time_f[i]
  queue_index <- which.min(queue_f[i, ])[1]
  station_choice_f[i] <- queue_index
  group <- stations_record_f[[queue_index]]
  group_length <- length(group)
  if (queue_f[i, queue_index] == 0) {
    finish_time_f[i] <- time_point + french_time[i]
  } else if (queue_f[i, queue_index] == 1) {
    waiting_time_f[i] <- group[[group_length]] - time_point
    finish_time_f[i] <- time_point + waiting_time_f[i] + french_time[i]
  } else {
    processing_car <- group[group_length - queue_f[i, queue_index] + 1] - time_point
    waiting_time_f[i] <- processing_car + sum(french_time[(group_length - (queue_f[i, queue_index]) + 2):group_length])
    finish_time_f[i] <- time_point + waiting_time_f[i] + french_time[i]
  }
  stations_record_f[[queue_index]][group_length + 1] <- finish_time_f[i]
  if (i != num_car) {
    for (j in 1:mf) {
      k <- which(station_choice_f == j)
      queue_f[i + 1, j] <- length(k) - sum(finish_time_f[k] < arrive_time_f[i + 1])
    }
  }
}

# put all the french data into a data frame
df_french <- as.data.frame(cbind(arrive_time_f, station_choice_f, queue_f, waiting_time_f, french_time, finish_time_f))
colnames(df_french) <- c("arrive_time_f", "station_choice_f", "qf1", "qf2", "qf3", "qf4", "qf5", "waiting_time_f", "french_time", "finish_time_f")


# ———————————————————————————————————————————————————————————————————————————————
### British Part
# Simulated original data for British stations
british_time <- runif(n = num_car, min = tmb, max = tmb + trb)

# Initialization British stations
arrive_time_b <- finish_time_f
waiting_time_b <- rep(0, num_car)
finish_time_b <- rep(0, num_car)
station_choice_b <- rep(0, num_car)
queue_b <- matrix(0, nrow = num_car, ncol = mf)
stations_record_b <- list(bs1 = NULL, bs2 = NULL, bs3 = NULL, bs4 = NULL, bs5 = NULL)

# British stations
for (i in 1:num_car) {
  time_point <- arrive_time_b[i]
  queue_index <- which.min(queue_b[i, ])[1]
  station_choice_b[i] <- queue_index
  group <- stations_record_b[[queue_index]]
  group_length <- length(group)
  if (queue_b[i, queue_index] == 0) {
    finish_time_b[i] <- time_point + british_time[i]
  } else if (queue_b[i, queue_index] == 1) {
    waiting_time_b[i] <- group[[group_length]] - time_point
    finish_time_b[i] <- time_point + waiting_time_b[i] + british_time[i]
  } else {
    processing_car <- group[group_length - queue_b[i, queue_index] + 1] - time_point
    waiting_time_b[i] <- processing_car + sum(british_time[(group_length - (queue_b[i, queue_index]) + 2):group_length])
    finish_time_b[i] <- time_point + waiting_time_b[i] + british_time[i]
  }
  stations_record_b[[queue_index]][group_length + 1] <- finish_time_b[i]
  if (i != num_car) {
    for (j in 1:mb) {
      k <- which(station_choice_b == j)
      queue_b[i + 1, j] <- length(k) - sum(finish_time_b[k] < arrive_time_b[i + 1])
    }
  }
}

# put all the british data into a data frame
df_british <- as.data.frame(cbind(arrive_time_b, station_choice_b, queue_b, waiting_time_b, british_time, finish_time_b))
colnames(df_british) <- c("arrive_time_b", "station_choice_b", "qb1", "qb2", "qb3", "qb4", "qb5", "waiting_time_b", "british_time", "finish_time_b")


# ———————————————————————————————————————————————————————————————————————————————
# 分别计算French队伍的总长度和平均长度
df_french$queue_f_sum <- rowSums(df_french[, c("qf1", "qf2", "qf3", "qf4", "qf5")], na.rm = TRUE)
df_french$queue_f_ave <- (1 / 5) * df_french$queue_f_sum

# 分别计算British队伍的总长度和平均长度
df_british$queue_b_sum <- rowSums(df_british[, c("qb1", "qb2", "qb3", "qb4", "qb5")], na.rm = TRUE)
df_british$queue_b_ave <- (1 / 5) * df_british$queue_b_sum

# 将British的车移回French
for (i in 1:num_car) {
  if (df_british$queue_b_sum[i] > 100) {
    df_french$queue_f_sum[i] <- df_british$queue_b_sum[i] - 100
    df_british$queue_b_sum[i] <- df_british$queue_b_sum[i] - 100
  }
}

# 把所有需要的数据整合进一个datafrmae中
df_time <- data.frame(time = 1:total_time)
df_final <- merge(df_time, df_french[, c("arrive_time_f", "finish_time_f", "queue_f_sum", "queue_f_ave")], by.x = "time", by.y = "arrive_time_f", all.x = T, all.y = T)
df_final <- merge(df_final, df_british[, c("arrive_time_b", "finish_time_b", "queue_b_sum", "queue_b_ave")], by.x = "time", by.y = "arrive_time_b", all.x = T, all.y = T)

# 处理没有车辆到达的时间点
### ！！！这部分还没写完，想到的填充方法都很复杂，有个zoo包里面有rollapply()和na.locf函数挺有用但是不是base……
for (i in 1:(total_time - 1)) {
  
  # 对French的排队情况
  if (is.na(df_final$queue_f_sum[i])) { ## 找到空值的位置
    ## 找到位置在queue_f_sum空值之后的最近的值
    replacement_value <- 
    ## 计算在i时间点 最近的前一辆车到达时间 ～ 最近的后一辆车到达时间 之间有几辆车离开
    car_leave <- 
    df_final$queue_f_sum[i] <- replacement_value - car_leave
    df_final$queue_f_ave[i] <- df_final$queue_f_sum[i]/5
  }
  
  # 对British的排队情况
  if (is.na(df_final$queue_b_sum[i])) { ## 找到空值的位置
    ## 找到位置在queue_b_sum空值之后的最近的值
    replacement_value <- 
      ## 计算在i时间点 最近的前一辆车到达时间 ～ 最近的后一辆车到达时间 之间有几辆车离开
      car_leave <- 
      df_final$queue_b_sum[i] <- replacement_value - car_leave
    df_final$queue_b_ave[i] <- df_final$queue_b_sum[i]/5
  }
}

end_time <- Sys.time() # 记录结束时间

elapsed_time <- end_time - start_time # 计算代码段运行的时间差

elapsed_time # 输出运行时间

# ———————————————————————————————————————————————————————————————————————————————

# Final function
# qsim <- function(mf, mb, a.rate, trb, trf, tmb, tmf, maxb) {
#   car_timetable <- sample(c(0, 1), size = 2 * 60 * 60, replace = TRUE, prob = c(0.9, 0.1))
#   french_time <- round(runif(n = 2 * 60 * 60, min = tmf, max = tmf + trf), 0)
#   british_time <- round(runif(n = 2 * 60 * 60, min = tmb, max = tmb + trb), 0)
#   return(cbind(car_timetable, french_time, british_time))
# }
# qsim(5, 5, .1, 40, 40, 30, 30, 20)


### GPT写的函数，可以参考一下这个结构还有画图
qsim <- function(mf, mb, a.rate, trb, trf, tmb, tmf, maxb) {
  # # Simulation parameters
  # total_time <- 2*60*60  # 2 hours in seconds
  # checkin_close_time <- total_time-30*60  # 30 minutes before departure

  # # Initialize queues and variables
  # nf <- rep(0, total_time)  # French queue length
  # nb <- rep(0, total_time)  # British queue length
  # eq <- rep(0, total_time)  # Expected waiting time in French queue

  # french_queues <- rep(0, mf)  # French station queues
  # british_queues <- rep(0, mb)  # British station queues

  # # Run simulation
  # for (t in 1:total_time) {
  #   # Check if check-in is closed
  #   if (t > checkin_close_time) break
  
  #   # Update French queues
  #   for (i in 1:mf) {
  #     # Process the next car if the previous one has left
  #     if (is.na(french_queues[i]) || french_queues[i] == 0) {
  #       if (length(french_queues) > 1) french_queues <- french_queues[-1]  # Remove the processed car
  #       processing_time <- runif(1, tmf, tmf + trf)  # Random processing time
  #       french_queues[length(french_queues)] <- processing_time  # Add the processing time to the last position
  #     } else {
  #       french_queues[i] <- french_queues[i] - 1  # Decrease the remaining processing time
  #     }
  #   }

  #   # Update British queues
  #   for (i in 1:mb) {
  #     # Process the next car if the previous one has left
  #     if (is.na(british_queues[i]) || british_queues[i] == 0) {
  #       if (length(british_queues) > 1) british_queues <- british_queues[-1]  # Remove the processed car
  #       processing_time <- runif(1, tmb, tmb + trb)  # Random processing time
  #       british_queues[length(british_queues)] <- processing_time  # Add the processing time to the last position
  #     } else {
  #       british_queues[i] <- british_queues[i] - 1  # Decrease the remaining processing time
  #     }
  #   }

  #   # Calculate queue lengths and expected waiting time
  #   nf[t] <- sum(french_queues)
  #   nb[t] <- sum(british_queues)
  #   eq[t] <- nf[t] * (tmf + trf) / 2  # Average expected waiting time in the French queue

  #   # Car arrivals
  #   if (runif(1) < a.rate) {
  #     shortest_queue <- which.min(french_queues)  # Find the shortest French queue
  #     french_queues[shortest_queue] <- french_queues[shortest_queue] + 1  # Add a car to the queue
  #   }
  # }

  # Return the simulation results
  return(list(nf = nf, nb = nb, eq = eq))
}

# Run the simulation with default parameters
simulation <- qsim(mf = 5, mb = 5, a.rate = 0.1, trb = 40, trf = 40, tmb = 30, tmf = 30, maxb = 20)

# Plot the results
par(mfrow = c(2, 2))

# Plot French queue length
plot(simulation$nf, type = "l", xlab = "Time", ylab = "French Queue Length")

# Plot British queue length
plot(simulation$nb, type = "l", xlab = "Time", ylab = "British Queue Length")

# Plot expected waiting time in French queue
plot(simulation$eq, type = "l", xlab = "Time", ylab = "Expected Waiting Time in French Queue")



# Run the simulation with default parameters
simulation <- qsim(mf = 5, mb = 40, a.rate = 0.1, trb = 40, trf = 40, tmb = 30, tmf = 30, maxb = 20)

# Plot the results
par(mfrow = c(2, 2))

# Plot French queue length
plot(simulation$nf, type = "l", xlab = "Time", ylab = "French Queue Length")

# Plot British queue length
plot(simulation$nb, type = "l", xlab = "Time", ylab = "British Queue Length")

# Plot expected waiting time in French queue
plot(simulation$eq, type = "l", xlab = "Time", ylab = "Expected Waiting Time in French Queue")
