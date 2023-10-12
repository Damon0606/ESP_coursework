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



### Simple case
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