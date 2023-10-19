start_time <- Sys.time() # 记录开始时间
total_time <- 2 * 60 * 60
closed_time <- total_time - 30 * 60

qsim <- function(mf, mb, a.rate, trb, trf, tmb, tmf, maxb) {
  # Simulate cars
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
  
  df_french$queue_f_sum <- rowSums(df_french[, c("qf1", "qf2", "qf3", "qf4", "qf5")], na.rm = TRUE)
  df_british$queue_b_sum <- rowSums(df_british[, c("qb1", "qb2", "qb3", "qb4", "qb5")], na.rm = TRUE)
  df_french<-as.data.frame(cbind(df_french$arrive_time_f,df_french$queue_f_sum))
  
  df_british<-as.data.frame(cbind(df_british$arrive_time_b,df_british$queue_b_sum))
  colnames(df_french)<-c("arrive_time_f","queue_f_sum")
  colnames(df_british)<-c("arrive_time_b","queue_b_sum")
  
  last_number_f<-df_french$queue_f_sum[length(df_french$queue_f_sum)]+1
  df_french_new<-matrix(0,last_number_f,2)
  df_french_new[,1] <- sort(head(sort(finish_time_f, decreasing = TRUE), last_number_f),decreasing = FALSE)
  df_french_new[,2] <- seq(last_number_f-1, 0)
  colnames(df_french_new)<-c("arrive_time_f","queue_f_sum")
  df_french<-rbind(df_french,df_french_new)
  
  last_number_b<-df_british$queue_b_sum[length(df_british$queue_b_sum)]+1
  df_british_new_all<-sort(head(sort(finish_time_b, decreasing = TRUE), last_number_b),decreasing = FALSE)
  df_british_new_part<-df_british_new_all[df_british_new_all<=7200]
  df_british_new<-matrix(0,length(df_british_new_part),2)
  df_british_new[,1]<-df_british_new_part
  df_british_new[,2]<-seq(last_number_b-1, last_number_b-length(df_british_new_part))
  colnames(df_british_new)<-c("arrive_time_b","queue_b_sum")
  df_british<-rbind(df_british,df_british_new)
  
  
  df_time <- data.frame(time = 1:total_time)
  df_final <- merge(df_time, df_french, by.x = "time", by.y = "arrive_time_f", all.x = T, all.y = T)
  df_final <- merge(df_final,df_british,by.x = "time", by.y = "arrive_time_b", all.x = T, all.y = T)
  
  # 填充
  reversed_df <- df_final[nrow(df_final):1, ]
  for (col in 1:ncol(reversed_df)) {
    for (row in 2:nrow(reversed_df)) {
      if (is.na(reversed_df[row, col])) {
        reversed_df[row, col] <- reversed_df[row - 1, col]
      }
    }
  }
  reversed_df$queue_f_sum[is.na(reversed_df$queue_f_sum)] <- reversed_df$queue_f_sum[!is.na(reversed_df$queue_f_sum)][1]
  reversed_df$queue_b_sum[is.na(reversed_df$queue_b_sum)] <- reversed_df$queue_b_sum[!is.na(reversed_df$queue_b_sum)][1]
  df_final <- reversed_df[nrow(reversed_df):1, ]
  
  for (i in 1:length(df_final$time)) {
    if (df_final$queue_b_sum[i] > 100) {
      df_final$queue_f_sum[i]<-df_final$queue_f_sum[i]+(df_final$queue_b_sum[i]-100)
      df_final$queue_b_sum[i] <- 100
    }
  }
  # 分别计算French队伍的总长度和平均长度
  df_final$queue_f_ave <- (1 / 5) * df_final$queue_f_sum
  # 分别计算British队伍的总长度和平均长度
  df_final$queue_b_ave <- (1 / 5) * df_final$queue_b_sum
  df_final$expected_time <-df_final$queue_f_ave*((tmf+tmf+trf)/2)+df_final$queue_b_ave*((tmb+tmb+trb)/2)
  result <- list(nf = df_final$queue_f_ave,nb = df_final$queue_b_ave,eq=df_final$expected_time)

  return(result)
}


simulation1<-qsim(5, 5, .1, 40, 40, 30, 30, 20)
end_time <- Sys.time() # 记录结束时间
elapsed_time <- end_time - start_time # 计算代码段运行的时间差
elapsed_time # 输出运行时间
simulation2<-qsim(5, 5, .1, 40, 40, 40, 30, 20)
par(mfrow = c(2, 2))
plot(simulation1$nf, col = "red", type = 'l',xlab = "Time", ylab = "Average Lengths ", main = "Two Types of Stations in Simulation1(tmb=30)",xlim = c(0,7200),ylim = c(0,max(simulation1$nb)*1.5))
lines(simulation1$nb, col = "blue")
legend("topright", legend = c("French", "British"), col = c("red", "blue"), lty = 1)

plot(simulation1$eq,col = 'black',type = 'l',xlab = "Time", ylab = "Average Expected Waiting Time ",xlim = c(0,7200),ylim = c(0,max(simulation1$eq)))

plot(simulation2$nf, col = "red", type = 'l',xlab = "Time", ylab = "Average Lengths ", main = "Two Types of Stations in Simulation2(tmb=40)",xlim = c(0,7200),ylim = c(0,max(simulation2$nb)*1.5))
lines(simulation2$nb, col = "blue")
legend("topleft", legend = c("French", "British"), col = c("red", "blue"), lty = 1)

plot(simulation2$eq,col = 'black',type = 'l',xlab = "Time", ylab = "Average Expected Waiting Time ",xlim = c(0,7200),ylim = c(0,max(simulation2$eq)))


start_time <- Sys.time() # 记录开始时间
missing_number<-c()
for (i in 1:100) {
  simulation_temp<-qsim(5, 5, .1, 40, 40, 30, 30, 20)## when tmb = 50,p=0.95;tmb = 60,p=1
  if(simulation_temp$nb[length(simulation_temp$nb)]>0){
    missing_number[i]<-1
  }else{
    missing_number[i]<-0
  }
}
probability_missing<-sum(missing_number)/100
end_time <- Sys.time() # 记录结束时间
elapsed_time <- end_time - start_time # 计算代码段运行的时间差
elapsed_time # 输出运行时间 macbook pro 2021(m1 pro 32GB/1TB)为1.14分钟，单个simulation 为0.7-0.8秒



########## HHT coment
# Huantong Hou (s2481591), Yuqi Shi (s2508879), Zukai Li (s2505721)

# Contribution:
#### Huantong Hou (s2481591): 33%
###### (1)
###### (2)
###### (3)

#### Yuqi Shi (s2508879): 35%
###### (1)
###### (2)
###### (3)

#### Zukai Li (s2505721): 32%
###### (1)
###### (2)
###### (3)


# ———————————————————————————————————————————————————————————————————————————————
### Code Objective and Functional Description:
# This practical is about writing a function to simulate cars passing through
# French and then British passport control at a French ferry terminal.


### This code is divided into the following 3 main sections：
## 1. Write a function, qsim, to simulate vehicle passing through French passport
# control and British passport control at French ferry terminal:
  # (1) Set up fixed parameters and simulate the car arrivals randomly from them;
  # (2) Analyse car queues at each station in France;
  # (3) Analyse car queues at each station in Britain;
  # (4) Integrate and process the simulated data, and compute the data needed 
  #     for the function to return.
  # The  

## 2. Plotting the simulation results:
  # Generate a 4-panel, 2-row, 2-column plot using the written qsim function to
  # show the change in queue length over time for France and the UK and the 
  # expected queue processing time over time for two different sets of parameter 
  # conditions, respectively.

## 3. Estimate the probability of missing the ferry departure:
  # Estimate the probability that at least one car will miss the ferry departure 
  # by running the written qsim function 100 times.


### Assumptions for the simulation model:
## 1. Cars are only likely to arrive 2 hours before ferry departure and no cars 
    # arrive in the last 30 minutes before departure (but can handle those that 
    # have arrived). At the beginning there were no cars lined up at either the 
    # French or British stations.
## 2. The time step of the simulation is 1 second, and the probability of a 
    # vehicle arriving at a French station at each second is 0.1, but the 
    # probability of 2 or more vehicles arriving at the same second is ignored.
## 3. At both French and British stations, cars always choose the shortest queue 
    # to line up in upon arrival and do not change queues midway through.
## 4. For both French and British stations, the time taken between the leaving 
    # of one car and the start of processing the next is ignored. 
## 5. The time taken by the vehicle to move from the French station to the 
    # British station is also not taken into account.
## 6. There can only be a maximum of 20 cars queuing at the same time at each 
    # British station, and when there is a situation where all the UK stations 
    # are full, the cars that have completed their processing at the French 
    # station must wait in place until at least one empty space appears at one 
    # of the UK stations before they can enter the UK station to be processed.
## 7. The processing time of the car at the French station and the processing 
    # time at the British station is a random number following a uniform 
    # distribution [tmf, tmf+trf] and a uniform distribution [tmb, tmb+trb], 
    # respectively.
## 8. In showing the changing queue lengths over time at French and UK stations, 
    # data obtained by calculating the average length of queues at all five 
    # stations in France and the Britain, respectively, are used.
## 9. In showing expected queuing time over time, the total expected queuing 
    # time is approximated by French queue length on arrival multiplied by
    # calculating expected French handling time + mean British queue length at 
    # that time multiplied by the expected UK handling time, not taking into 
    # account the blocking time due to the British queue being full.

# ———————————————————————————————————————————————————————————————————————————————

### Initialize  simulation period
total_time <- 2 * 60 * 60 # 2 hours totally, 60minutes/hour, 60seconds/minute
closed_time <- total_time - 30 * 60 # period for car arrival (check-in closes 30 minutes before departure)


## Simulation of queues of French stations and British stations
## Input: {mf: number of French stations, mb: number of British stations, 
##         a.rate: probability of an arrival each second, trb: , trf:,
##         tmb: minimum British handling time, tmf: minimum French handling time,
##         maxb: available number of cars in each British station's queue}
## Purpose: Simulate the entire process of each car passing through the French and British stations
## Output: {nf: the average length of french queues, for each simulation second,
##          nb: the average length of british queues, for each simulation second,
##          eq: the average expected waiting time (sum of each stations' queue lengths*average handling time)}
qsim <- function(mf, mb, a.rate, trb, trf, tmb, tmf, maxb) {
  # generate all time points of car arrival by simulating vectors of 0 and 1, 1 means car arrival
  # probability of 0.1 for obtaining 1 means probability of 0.1 of an arrival each second
  car_timetable <- sample(c(0, 1), size = total_time, replace = TRUE, prob = c(0.9, 0.1))
  car_timetable[(closed_time + 1):total_time] <- 0 # no cars arriving within the 30 minutes after the stations closed
  # to facilitate simulating the queue situation for each car
  arrive_time_f <- which(car_timetable == 1) # find a list of all the time points when cars arrived
  num_car <- length(arrive_time_f) # to iterate through each car
  
  ### ———————————————————————————————————————————————————————————————————————————————
  ### Simulation of French stations Part
  # simulate french handling time for each car to obtain waiting time for queued cars
  french_time <- runif(n = num_car, min = tmf, max = tmf + trf)
  ## Initialization situations of each arrival car in French stations
  ## Since car's waiting time would impact its finish time and the finish time would impact queue lengths 
  ## and the queue lengths would impact car's station choice
  waiting_time_f <- rep(0, num_car) # time period for a car from its arrival at the station until it can be processed
  finish_time_f <- rep(0, num_car) # departing time from the french station, indexed by car's arrival time
  # help to find queue length of chosen station
  station_choice_f <- rep(0, num_car) 
  # help to calculate average queue length
  queue_f <- matrix(0, nrow = num_car, ncol = mf) # queue lengths for each station just before the car arrives
  # in order to track the finish times for each station and 
  # allow coming cars to make decisions on station choice regarding the station with the earliest finish time.
  stations_record_f <- list(fs1 = NULL, fs2 = NULL, fs3 = NULL, fs4 = NULL, fs5 = NULL)# same data as finish_time_f but indexed by station
  
  ## totally consider three scenarios for cars arriving french stations based on different waiting_time: 
  ## All of finish_time is (arrival time point + waiting time + handling time)
  ## 1. at least one station is available, allowing the car to be processed immediately
  ## 2. each station has only one car being processed
  ## 3. at least one station has two or more cars in the queue, requiring the car to wait its turn for handling.
  ## the difference between scenario 2 and 3 is: waiting time for cars in queue
  for (i in 1:num_car) {
    time_point <- arrive_time_f[i]
    # decide choice of station
    queue_index <- which.min(queue_f[i, ])[1]# find station index of shortest queue
    station_choice_f[i] <- queue_index
    # find the scenario of french station based on queue length of chosen station
    group <- stations_record_f[[queue_index]] # "[[]]" means find a element while "[]"means find a series
    group_length <- length(group) # choice of scenario based on queue length
    if (queue_f[i, queue_index] == 0) { # first scenario
      finish_time_f[i] <- time_point + french_time[i] # no waiting time, only handling time
    } else if (queue_f[i, queue_index] == 1) { # second scenario
      # the remaining handling time for the car being processed when a new car arrives
      waiting_time_f[i] <- group[[group_length]] - time_point 
      finish_time_f[i] <- time_point + waiting_time_f[i] + french_time[i]
    } else {# third scenario
      # the remaining handling time of processed car
      processing_car <- group[group_length - queue_f[i, queue_index] + 1] - time_point 
      # ???解释不清楚+2
      waiting_time_f[i] <- processing_car + sum(french_time[(group_length - (queue_f[i, queue_index]) + 2):group_length])
      finish_time_f[i] <- time_point + waiting_time_f[i] + french_time[i]
    }
    # update the finish time records of station after a car handled
    stations_record_f[[queue_index]][group_length + 1] <- finish_time_f[i]
    # when 
    if (i != num_car) {
      for (j in 1:mf) {
        k <- which(station_choice_f == j)# 为什么不可以直接用station_choice_f?是更新一整行的排队情况吗？
        # 下一辆车过来时候车站的排队情况就是现在的总队伍长度-下一辆车来之前走掉的车的数量？
        queue_f[i + 1, j] <- length(k) - sum(finish_time_f[k] < arrive_time_f[i + 1])
      }
    }
  }
  
  ### ———————————————————————————————————————————————————————————————————————————————
  ### Simulation of British stations Part
  # simulate british handling time for each car to obtain waiting time for queued cars
  british_time <- runif(n = num_car, min = tmb, max = tmb + trb)
  ## Initialization situations of each arrival car in British stations
  ## Since car's waiting time would impact its finish time and the finish time would impact queue lengths 
  ## and the queue lengths would impact car's station choice (same as french station)
  arrive_time_b <- finish_time_f # ignore the time period from french station to british station
  waiting_time_b <- rep(0, num_car) # time period for a car from its arrival at the station until it can be processed
  finish_time_b <- rep(0, num_car) # departing time from the british station, indexed by car's arrival time
  # help to find queue length of chosen station
  station_choice_b <- rep(0, num_car)
  # help to calculate average queue length
  queue_b <- matrix(0, nrow = num_car, ncol = mf) # queue lengths for each station just before the car arrives
  # in order to track the finish times for each station and 
  # allow coming cars to make decisions on station choice regarding the station with the earliest finish time.
  stations_record_b <- list(bs1 = NULL, bs2 = NULL, bs3 = NULL, bs4 = NULL, bs5 = NULL)
  
  ## totally consider three similar scenarios for cars arriving british stations as frensh stations
  ## but one different thing is the maximum queue number of british station is 20.
  ## here, 我们把多余的队伍长度在后面加到法国站？
  for (i in 1:num_car) {
    time_point <- arrive_time_b[i]
    # decide choice of station
    queue_index <- which.min(queue_b[i, ])[1]# find station index of shortest queue
    station_choice_b[i] <- queue_index
    # find the scenario of british station based on queue length of chosen station
    group <- stations_record_b[[queue_index]] # "[[]]" means find a element while "[]"means find a series
    group_length <- length(group)# choice of scenario based on queue length
    if (queue_b[i, queue_index] == 0) {# first scenario
      finish_time_b[i] <- time_point + british_time[i]# no waiting time, only handling time
    } else if (queue_b[i, queue_index] == 1) {# second scenario
      # the remaining handling time for the car being processed when a new car arrives
      waiting_time_b[i] <- group[[group_length]] - time_point
      finish_time_b[i] <- time_point + waiting_time_b[i] + british_time[i]
    } else {# third scenario
      # the remaining handling time of processed car
      processing_car <- group[group_length - queue_b[i, queue_index] + 1] - time_point
      # ???解释不清楚+2
      waiting_time_b[i] <- processing_car + sum(british_time[(group_length - (queue_b[i, queue_index]) + 2):group_length])
      finish_time_b[i] <- time_point + waiting_time_b[i] + british_time[i]
    }
    # update the finish time records of station after a car handled
    stations_record_b[[queue_index]][group_length + 1] <- finish_time_b[i]
    if (i != num_car) {
      for (j in 1:mb) {
        k <- which(station_choice_b == j)# 为什么不可以直接用station_choice_f?是更新一整行的排队情况吗？
        # 下一辆车过来时候车站的排队情况就是现在的总队伍长度-下一辆车来之前走掉的车的数量？
        queue_b[i + 1, j] <- length(k) - sum(finish_time_b[k] < arrive_time_b[i + 1])
      }
    }
  }