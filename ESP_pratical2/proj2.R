set.seed(123)
qsim <- function(mf,mb,a.rate,trb,trf,tmb,tmf,maxb){
  car_timetable <- sample(c(0, 1), size=2*60*60, replace = TRUE, prob=c(0.9, 0.1))
  french_time <- round(runif(n=2*60*60, min=tmf, max=tmf+trf), 0)
  british_time <- round(runif(n=2*60*60, min=tmb, max=tmb+trb), 0)
  return (cbind(car_timetable, french_time, british_time))
}
qsim(5,5,.1,40,40,30,30,20)