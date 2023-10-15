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
total_time <- 500
closed_time <- 490
tmf <- 40
trf <- 30
mf <- 5
tmb <- 40
trb <- 30
mb <- 5

# Simulate cars
set.seed(66)
car_timetable <- sample(c(0, 1), size = total_time, replace = TRUE, prob = c(0.9, 0.1))
car_timetable[(closed_time + 1):total_time] <- 0 # check-in closes 30 minutes before departure
num_car <- length(arrive_time_f)


# Simulated original data for French stations
french_time <- round(runif(n = num_car, min = tmf, max = tmf + trf), 0) ## 把french time随机到每个车上

# Initialization French stations
arrive_time_f <- which(car_timetable == 1)
waiting_time_f <- rep(0, num_car)
finish_time_f <- rep(0, num_car)
station_choice_f <- rep(0, num_car)
queue_f <- matrix(0, nrow = num_car, ncol = mf)
stations_record_f <- list(fs1 = NULL, fs2 = NULL, fs3 = NULL, fs4 = NULL, fs5 = NULL)


# French stations
for (i in 1:num_car) {
  time_point <- arrive_time_f[i]
  queue_index <- which(queue_f[i, 1:5] == min(queue_f[i, ]))[1]
  station_choice_f[i] <- queue_index
  group <- stations_record_f[[queue_index]]
  group_length <- length(group)
  if (queue_f[i, queue_index] == 0) {
    finish_time_f[i] <- time_point + french_time[i]
  } else if (queue_f[i, queue_index] == 1) {
    waiting_time_f[i] <- group[[group_length]] - time_point
    finish_time_f[i] <- time_point + waiting_time_f[i] + french_time[i]
  } else {
    waiting_time_f[i] <- sum(group[[(group_length - (queue_f[i, queue_index]) + 1):group_length]]) + (group[group_length - queue_f[i, queue_index]] - time_point)
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


cat(
  "arrive_time:", arrive_time_f, "\n",
  "station_choice:", station_choice_f, "\n",
  "waiting_time:", waiting_time_f, "\n",
  "french_time:", french_time, "\n",
  "finish_time:", finish_time_f, "\n",
  "stations_record_f", stations_record_f, "\n",
  "queue:", "\n"
)
print(queue_f)

combined_mat <- cbind(arrive_time_f, station_choice_f, waiting_time_f, french_time, finish_time_f)
wb <- createWorkbook()
addWorksheet(wb, "Data")
writeData(wb, sheet = "Data", combined_mat, startCol = 1)
writeData(wb, sheet = "Data", queue_f, startCol = 6)
saveWorkbook(wb, file = "Output_1.xlsx", overwrite = TRUE)


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

    selected <- stations_record_f[[station_choice_in_f]]

    arrive_time_selected <- arrive_time_f[stations_record_f[[station_choice_in_f]]]
    length_behind <- queue_f[which(arrive_time_f == max(arrive_time_selected[(tp - arrive_time_selected) > 0])), station_choice_in_f]
    
    stop_index <- 
    waiting_time_f[length_behind] <- waiting_time_f[stop_index] + extra_time
    finish_time_f <- waiting_time_f[stop_index] + extra_time
  }




  time_point <- arrive_time_b[i]
  queue_index <- which(queue_b[i, 1:5] == min(queue_b[i, ]))[1]
  station_choice_b[i] <- queue_index
  group <- stations_record_b[[queue_index]]
  group_length <- length(group)
  if (queue_b[i, queue_index] == 0) {
    finish_time_b[i] <- time_point + british_time[i]
  } else if (queue_b[i, queue_index] == 1) {
    waiting_time_b[i] <- group[[group_length]] - time_point
    finish_time_b[i] <- time_point + waiting_time_b[i] + british_time[i]
  } else {
    waiting_time_b[i] <- sum(group[[(group_length - (queue_b[i, queue_index]) + 1):group_length]]) + (group[group_length - queue_b[i, queue_index]] - time_point)
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




qsim <- function(mf, mb, a.rate, trb, trf, tmb, tmf, maxb) {
  car_timetable <- sample(c(0, 1), size = 2 * 60 * 60, replace = TRUE, prob = c(0.9, 0.1))
  french_time <- round(runif(n = 2 * 60 * 60, min = tmf, max = tmf + trf), 0)
  british_time <- round(runif(n = 2 * 60 * 60, min = tmb, max = tmb + trb), 0)
  return(cbind(car_timetable, french_time, british_time))
}
qsim(5, 5, .1, 40, 40, 30, 30, 20)