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
