# Huantong Hou (s2481591), Yuqi Shi (s2508879), Zukai Li (s2505721)

# Contribution:
#### Huantong Hou (s2481591): 33%
###### (1) Analyze and calculate various situations that may occur during the simulation, debug for queues of British station part;
###### (2) writing codes for the filling NA by reversed data frame,plot the result and calculate the probability.

#### Yuqi Shi (s2508879): 35%
###### (1) Analyze and calculate various situations that may occur during the simulation, calculate the excess waiting time for French station;
###### (2) writing codes for the average lengths in rest time(after the finial car leave the stations),plot the result and calculate the probability.

#### Zukai Li (s2505721): 32%
###### (1) Write the main part of the code that randomly simulates the vehicle queuing process at the French and British stations;
###### (2) integrate and process the simulated data.


# ———————————————————————————————————————————————————————————————————————————————
### Code Objective and Functional Description:
# This practical is about writing a function to simulate cars passing through -
# - French and then British passport control at a French ferry terminal.


### This code is divided into the following 3 main sections：
## 1. Write a function, "qsim", to simulate vehicle passing through French passport -
# - control and British passport control at French ferry terminal:
# (1) Randomly generate vehicle arrival times based on set parameters,
#     and also the time each vehicle is processed at the French and British stations, respectively.
# (2) Analyse the queue at each French station when a car arrives (according to the time of arrival at the French station),
#     in order to confirm which French queue the vehicle enters and to update the queue at each French station
# (3) Analyse the queue at each British station when a car arrives (according to the time of leaving the French station),
#     in order to confirm which French queue the vehicle enters and to update the queue at each British station.
# (4) Integrate and process the simulated data, and compute the average queue lengths at the French and British stations at each time point,
#     and also the average expected waiting time per car，which are needed for the function to return.

## 2. Plotting the simulation results:
# Generate a 4 panel, 2 row, 2 column plot using the written "qsim" function to
# show the changes in queue length over time for France and the Britain and the
# expected queue processing time over time for two different sets of parameter
# conditions, respectively.

## 3. Estimate the probability of missing the ferry departure:
# Estimate the probability that at least one car will miss the ferry departure
# by running the written "qsim" function 100 times.


### Assumptions for the simulation model are placed at the end of the codes
# ———————————————————————————————————————————————————————————————————————————————
### Initialize  simulation period
total_time <- 2 * 60 * 60 # 2 hours totally
closed_time <- total_time - 30 * 60 # period for car arrival (check-in closes 30 minutes before departure)

## Simulation of queues of French stations and British stations
## Input: {mf: number of French stations, mb: number of British stations,
##         a.rate: probability of an arrival each second,
##         trb: range of the uniform distribution for British stations,
##         trf:range of the uniform distribution for French stations,
##         tmb: minimum British handling time, tmf: minimum French handling time,
##         maxb: maximum number of cars in each British station's queue}
## Purpose: Simulate the entire process of each car passing through the French and British stations
## Output: {nf: the average length of french queues, for each simulation second,
##          nb: the average length of British queues, for each simulation second,
##          eq: the average expected waiting time (sum of each stations' queue lengths*average handling time)}
qsim <- function(mf, mb, a.rate, trb, trf, tmb, tmf, maxb) {
  # generate all time points of car arrival by simulating vectors of 0 and 1, 1 means car arrival
  # probability of 0.1 for obtaining 1 means probability of 0.1 of an arrival each second
  car_timetable <- sample(c(0, 1), size = total_time, replace = TRUE, prob = c(0.9, 0.1))
  car_timetable[(closed_time + 1):total_time] <- 0 # no cars arriving within the 30 minutes after the stations closed
  arrive_time_f <- which(car_timetable == 1) # find a list of all the time points when cars arrived
  num_car <- length(arrive_time_f) # number of arrived cars

  ### ———————————————————————————————————————————————————————————————————————————————
  ### Simulation of French stations Part
  french_time <- runif(n = num_car, min = tmf, max = tmf + trf) # simulate french handling time for each car
  waiting_time_f <- rep(0, num_car) # time period for a car from its arrival at the station until it can be processed
  finish_time_f <- rep(0, num_car) # departing time from the french station, indexed by car's arrival time
  station_choice_f <- rep(0, num_car) # station choices of each car
  queue_f <- matrix(0, nrow = num_car, ncol = mf) # queue lengths for each station just before the car arrives
  # in order to track the finish times for each station and -
  # - allow coming cars to make decisions on station choice regarding the station with the earliest finish time
  stations_record_f <- list(fs1 = NULL, fs2 = NULL, fs3 = NULL, fs4 = NULL, fs5 = NULL) # same data as finish_time_f but indexed by station

  ## totally consider three scenarios for cars arriving french stations based on different waiting_time:
  ## All of finish_time is (arrival time point + waiting time + handling time)
  ## 1. at least one station is available, allowing the car to be processed immediately
  ## 2. each station has only one car being processed
  ## 3. at least one station has two or more cars in the queue, requiring the car to wait its turn for handling.
  ## the difference between scenario 2 and 3 is waiting time for cars in queues
  for (i in 1:num_car) {
    time_point <- arrive_time_f[i]
    queue_index <- which.min(queue_f[i, ])[1] # decide choice of station (index of shortest queue)
    station_choice_f[i] <- queue_index
    group <- stations_record_f[[queue_index]] # "group" includes the finish time of cars in chosen station
    group_length <- length(group) # choice of scenario based on queue length
    if (queue_f[i, queue_index] == 0) { # first scenario
      finish_time_f[i] <- time_point + french_time[i] # no waiting time, only handling time
    } else if (queue_f[i, queue_index] == 1) { # second scenario
      waiting_time_f[i] <- group[[group_length]] - time_point # waiting time is the remaining handling time for the car being processed when the car[i] arrives
      finish_time_f[i] <- time_point + waiting_time_f[i] + french_time[i]
    } else { # third scenario
      # "group[group_length - queue_f[i, queue_index] + 1]" means the finish time of the processing car in chosen line
      processing_car <- group[group_length - queue_f[i, queue_index] + 1] - time_point # the remaining handling time of processed car when the car[i] arrives.
      # "sum(french_time[(group_length - (queue_f[i, queue_index]) + 2):group_length])" means the sum of handling time for queuing cars in chosen line
      waiting_time_f[i] <- processing_car + sum(french_time[(group_length - (queue_f[i, queue_index]) + 2):group_length])
      finish_time_f[i] <- time_point + waiting_time_f[i] + french_time[i]
    }
    # add the finish time of car[i] to the records of chosen line's finish time
    stations_record_f[[queue_index]][group_length + 1] <- finish_time_f[i]
    ##  when car[i] is in queue, update the queue lengths based on the station selection
    if (i != num_car) {
      for (j in 1:mf) {
        k <- which(station_choice_f == j) # indices of all the cars choose station j
        # "sum(finish_time_f[k] < arrive_time_f[i + 1])" means number of processed cars in station j when car[i+1] arrives
        # "length(k)" means number of all cars choose station j before car[i+1] arrives
        # "queue_f[i + 1, j]" means queue length of station j when car[i+1] arrives
        queue_f[i + 1, j] <- length(k) - sum(finish_time_f[k] < arrive_time_f[i + 1])
      }
    }
  }

  ### ———————————————————————————————————————————————————————————————————————————————
  ### Simulation of British stations Part (same as French stations part)
  british_time <- runif(n = num_car, min = tmb, max = tmb + trb) # simulate British handling time for each car
  arrive_time_b <- finish_time_f # time point for cars arriving British stations
  waiting_time_b <- rep(0, num_car) # time period for a car from its arrival at the station until it can be processed
  finish_time_b <- rep(0, num_car) # departing time from the British station, indexed by car's arrived time
  station_choice_b <- rep(0, num_car) # station choices of each car
  queue_b <- matrix(0, nrow = num_car, ncol = mf) # queue lengths for each station just before the car arrives
  # in order to track the finish times for each station and -
  # - allow coming cars to make decisions on station choice regarding the station with the earliest finish time.
  stations_record_b <- list(bs1 = NULL, bs2 = NULL, bs3 = NULL, bs4 = NULL, bs5 = NULL) # same data as finish_time_f but indexed by station

  ## totally consider three similar scenarios for cars arriving British stations as French stations
  for (i in 1:num_car) {
    time_point <- arrive_time_b[i]
    queue_index <- which.min(queue_b[i, ])[1] # decide choice of station (index of shortest queue)
    station_choice_b[i] <- queue_index
    group <- stations_record_b[[queue_index]] # "group" includes the finish time of cars in chosen station
    group_length <- length(group) # choice of scenario based on queue length
    if (queue_b[i, queue_index] == 0) { # first scenario
      finish_time_b[i] <- time_point + british_time[i] # no waiting time, only handling time
    } else if (queue_b[i, queue_index] == 1) { # second scenario
      waiting_time_b[i] <- group[[group_length]] - time_point # waiting time is the remaining handling time for the car being processed when the car[i] arrives
      finish_time_b[i] <- time_point + waiting_time_b[i] + british_time[i]
    } else { # third scenario
      # "group[group_length - queue_b[i, queue_index] + 1]" means the finish time of the processing car in chosen line
      processing_car <- group[group_length - queue_b[i, queue_index] + 1] - time_point # the remaining handling time of processed car when the car[i] arrives.
      # "sum(british_time[(group_length - (queue_b[i, queue_index]) + 2):group_length])" means the sum of handling time for queuing cars in chosen line
      waiting_time_b[i] <- processing_car + sum(british_time[(group_length - (queue_b[i, queue_index]) + 2):group_length])
      finish_time_b[i] <- time_point + waiting_time_b[i] + british_time[i]
    }
    # add the finish time of car[i] to the records of chosen line's finish time
    stations_record_b[[queue_index]][group_length + 1] <- finish_time_b[i]
    ## when car[i] is in queue, update the queue lengths based on the station selection
    if (i != num_car) {
      for (j in 1:mb) {
        k <- which(station_choice_b == j) # indices of all the cars choose station j
        # "sum(finish_time_b[k] < arrive_time_b[i + 1])" means number of processed cars in station j when car[i+1] arrives
        # "length(k)" means number of all cars choose station j before car[i+1] arrives
        # "queue_f[i + 1, j]" means queue length of station j when car[i+1] arrives
        queue_b[i + 1, j] <- length(k) - sum(finish_time_b[k] < arrive_time_b[i + 1])
      }
    }
  }

  ### Organize the data from the French and British stations in loop
  ## Calculate the total queues length(use 'rowSums()') at the French and British stations for each car arrived at the station-
  ## -and create two data frames(df_french and df_british) with the length (queue_f/b_sum) and cars arrived time(arrive_time_f/b)
  df_french <- as.data.frame(cbind(arrive_time_f, rowSums(queue_f, na.rm = TRUE)))
  colnames(df_french) <- c("arrive_time_f", "queue_f_sum")
  df_british <- as.data.frame(cbind(arrive_time_b, rowSums(queue_b, na.rm = TRUE)))
  colnames(df_british) <- c("arrive_time_b", "queue_b_sum")

  ### Calculate queues length after the last car arrived the station
  ## For the French station, find the total queues length when the last vehicle enters the station. (last_number_f)
  ## Then, find the finish time for queued cars at the French station.
  ## These time points are considered as the time the total queues length changed. (df_french_new[,1])
  ## Finally, the total queues length decreases until there are no cars in the queue(df_french_new[,2])
  last_number_f <- df_french$queue_f_sum[length(df_french$queue_f_sum)] + 1
  df_french_new <- matrix(0, last_number_f, 2)
  df_french_new[, 1] <- sort(head(sort(finish_time_f, decreasing = TRUE), last_number_f), decreasing = FALSE) # find the finish time for queued cars at the French station
  df_french_new[, 2] <- seq(last_number_f - 1, 0)
  colnames(df_french_new) <- c("arrive_time_f", "queue_f_sum")
  df_french <- rbind(df_french, df_french_new) # Merge ‘df_french_new’ to the end of ‘df_french’ by columns
  ## Unlike the French station, the British station may not have processed all the queued vehicles within 7200 seconds.
  ## Therefore, consider the finish times of cars that were in the queue when the last vehicle entered the station and could be processed within 7200 seconds. (df_british_new_part)
  ## These time points are used to track changes in the total queues length, which gradually decreases until the station is closed (df_british_new[,2])
  last_number_b <- df_british$queue_b_sum[length(df_british$queue_b_sum)] + 1
  df_british_new_all <- sort(head(sort(finish_time_b, decreasing = TRUE), last_number_b), decreasing = FALSE)
  df_british_new_part <- df_british_new_all[df_british_new_all <= 7200]
  df_british_new <- matrix(0, length(df_british_new_part), 2)
  df_british_new[, 1] <- df_british_new_part
  df_british_new[, 2] <- seq(last_number_b - 1, last_number_b - length(df_british_new_part))
  colnames(df_british_new) <- c("arrive_time_b", "queue_b_sum")
  df_british <- rbind(df_british, df_british_new) # Merge ‘df_british_new’ to the end of ‘df_british’ by columns

  ## Merge the total queues length at the station when each car arrive the stations to the time vector of 7200 seconds
  df_time <- data.frame(time = 1:total_time)
  df_final <- merge(df_time, df_french, by.x = "time", by.y = "arrive_time_f", all.x = T, all.y = T)
  df_final <- merge(df_final, df_british, by.x = "time", by.y = "arrive_time_b", all.x = T, all.y = T)

  ## After the processing in the previous step, ‘df_final’ have some NA at some seconds when no car entered
  ## Assumption: There will be no queued cars leaving the station between two consecutive entering cars
  ##             (based on the data, it indeed appears that such a situation does not exist)
  ## fill the NA at the seconds between two cars with the data from the later vehicle (the total queues length after the arrival of the preceding vehicle)

  # However, filling with the next non-NA value using loops(for&while) has higher complexity and longer running time. —
  # —Therefore, reverse 'df_final' and then fill it with the data from the previous car, which will significantly reduce the complexity
  reversed_df <- df_final[nrow(df_final):1, ]
  for (col in 1:ncol(reversed_df)) {
    for (row in 2:nrow(reversed_df)) {
      if (is.na(reversed_df[row, col])) {
        reversed_df[row, col] <- reversed_df[row - 1, col]
      }
    }
  }
  ### For the initial NA in the ‘reversed_df’, fill them with the first non-empty value(!is.na('value_name'))
  reversed_df$queue_f_sum[is.na(reversed_df$queue_f_sum)] <- reversed_df$queue_f_sum[!is.na(reversed_df$queue_f_sum)][1]
  reversed_df$queue_b_sum[is.na(reversed_df$queue_b_sum)] <- reversed_df$queue_b_sum[!is.na(reversed_df$queue_b_sum)][1]
  df_final <- reversed_df[nrow(reversed_df):1, ] # After filling, restore the reversed ‘df_final’ to its original order

  ## considered that when the total queues length in the British over 100, the cars from the French station will no longer be able to enter the British stations
  ## add the portion of the British station queues length that exceeds 100 back to the French stations
  for (i in 1:length(df_final$time)) {
    if (df_final$queue_b_sum[i] > 100) { # Check if the total queues length at the British station is greater than 100
      df_final$queue_f_sum[i] <- df_final$queue_f_sum[i] + (df_final$queue_b_sum[i] - 100)
      df_final$queue_b_sum[i] <- 100
    }
  }
  ### Calculate the average length of the French and British stations for each seconds
  df_final$queue_f_ave <- (1 / 5) * df_final$queue_f_sum
  df_final$queue_b_ave <- (1 / 5) * df_final$queue_b_sum

  ### Calculate the average expected waiting time for each seconds
  ##  Average expected waiting time =  average length of French stations*average processing time in French
  ##                                   + average length of British stations*average processing time in British
  df_final$expected_time <- df_final$queue_f_ave * ((tmf + tmf + trf) / 2) + df_final$queue_b_ave * ((tmb + tmb + trb) / 2) # processing time modelled as uniform, so the average time equal to (min + max)/2
  result <- list(nf = df_final$queue_f_ave, nb = df_final$queue_b_ave, eq = df_final$expected_time)
  return(result)
}

### Conduct two simulations using the ‘qsim’ function, in simulation2 the value of ‘tmb’ is 40
simulation1 <- qsim(5, 5, .1, 40, 40, 30, 30, 20)
simulation2 <- qsim(5, 5, .1, 40, 40, 40, 30, 20)

### Produce the 4 panel based on the results of the simulation1&2
par(mfrow = c(2, 2)) # set the parameter of the plots to 2*2
## the first plot: how the French (red) and British (blue) queue lengths ('nf'&'nb') are changing over time
plot(simulation1$nf, col = "red", type = "l", xlab = "Time", ylab = "Average Lengths ", main = "Two Types of Stations in Simulation1(tmb=30)", xlim = c(0, 7200), ylim = c(0, max(simulation1$nb) * 1.5))
lines(simulation1$nb, col = "blue") # For the completeness and aesthetics, we set the maximum value of the y-axis to 1.5 times the maximum value of "nb"
legend("topleft", legend = c("French", "British"), col = c("red", "blue"), lty = 1) # Add a legend to the plot
## the second plot: how the expected queuing time ('eq') changes over time
plot(simulation1$eq, col = "black", type = "l", xlab = "Time", ylab = "Average Expected Waiting Time ", xlim = c(0, 7200), ylim = c(0, max(simulation1$eq)))
## the third plot: how the French (red) and British (blue) queue lengths ('nf'&'nb') are changing over time when 'tmb' equal to 40 (in simulation2)
plot(simulation2$nf, col = "red", type = "l", xlab = "Time", ylab = "Average Lengths ", main = "Two Types of Stations in Simulation2(tmb=40)", xlim = c(0, 7200), ylim = c(0, max(simulation2$nb) * 1.5))
lines(simulation2$nb, col = "blue")
legend("topleft", legend = c("French", "British"), col = c("red", "blue"), lty = 1) # Add a legend to the plot
## the forth plot: how the expected queuing time ('eq') changes over time when 'tmb' equal to 40 (in simulation2)
plot(simulation2$eq, col = "black", type = "l", xlab = "Time", ylab = "Average Expected Waiting Time ", xlim = c(0, 7200), ylim = c(0, max(simulation2$eq)))

### Estimate the probability of at least one car missing the ferry departure
## Regarding the running time of these 100 simulation runs: The simulations were completed in approximately 1.14 minutes using the MacBookPro 2021 (32GB/1TB)
missing_number <- c()
for (i in 1:100) {
  simulation_temp <- qsim(5, 5, .1, 40, 40, 30, 30, 20)
  if (simulation_temp$nb[length(simulation_temp$nb)] > 0) {
    missing_number[i] <- 1 # return 1 if the last value of "nb" (the total queue length at the final time) is greater than 0,indicating that a car missed the ferry during this simulation
  } else {
    missing_number[i] <- 0 # Otherwise, return 0
  }
}
probability_missing <- sum(missing_number) / 100 # probability of the car missing the ferry departure = sum of the missing_number / 100
## probability_missing = 0.95 when tmb = 50
## probability_missing = 1 when tmb = 60
print(probability_missing)


### Basic Assumptions
## 1. Cars are only likely to arrive 2 hours before ferry departure and no cars arrive in the last 30 minutes before departure (but can handle those that
# have arrived). At the beginning there were no cars lined up at either the French or British stations.

## 2. The time step of the simulation is 1 second, and the probability of a vehicle arriving at a French station at each second is 0.1, but the
# probability of 2 or more vehicles arriving at the same second is ignored.

## 3. At both French and British stations, cars always choose the shortest queue to line up in upon arrival and do not change queues midway through.

## 4. For both French and British stations, the time taken between the leaving of one car and the start of processing the next is ignored.

## 5. The time taken by the vehicle to move from the French station to the British station is also not taken into account.

## 6. There can only be a maximum of 20 cars queuing at the same time at each British station, and when there is a situation where all the British stations
# are full, the cars that have completed their processing at the French station must wait in place until at least one empty space appears at one
# of the British stations before they can enter the British station to be processed.

## 7. The processing time of the car at the French station and the processing time at the British station is a random number following a uniform
# distribution [tmf, tmf+trf] and a uniform distribution [tmb, tmb+trb], respectively.
