################################################################################
################################ Assignment 1 ##################################
################################################################################

################################# Setup ########################################
set.seed(1234567890)
library(geosphere)
stations <- read.csv("data/stations.csv", fileEncoding = "ISO-8859-1")
temps <- read.csv("data/temps50k.csv")
st <- merge(stations,temps,by="station_number")
################################# Setup ########################################
############################### Functions ######################################
FilterData <- function(data, my.date){
 filtered_data <- (data$date < my.date)
  return(filtered_data)
}

GaussianKernelDay <- function(dates, target.date, gamma, h) {
  day.delta <-(as.Date(target.date)- as.numeric(as.Date(dates), unit="days"))
  day.delta <- as.numeric(day.delta) %% 365
  day.delta[day.delta>(365/2)] = 365 - 
    day.delta[day.delta>(365/2)]
  rbf.val <- exp(-gamma*((as.numeric(day.delta)/h)^2))
  return(rbf.val)
}

GaussianKernelDistance <- function(locations, target.location, gamma, h) {
  loc.delta <-distHaversine(locations,target.location)
  rbf.val <- exp(-gamma*((loc.delta/h)^2))
  return(rbf.val)
}

GaussianKernelHour <- function(times, target.time, gamma, h) {
  time.delta <- difftime(strptime(times, format = "%H:%M:%S"),
    strptime(target.time,
      format = "%H:%M:%S"
    ),
    units = "hours"
  )
time.delta[which(abs(time.delta)>12)] = 24 - 
  abs(time.delta[which(abs(time.delta)>12)])
  rbf.val <- exp(-gamma * ((as.numeric(time.delta) / h)^2))
  return(rbf.val)
}


TempatureEstimate <- function(data, gamma, target.location, target.date, times,
                              h_distance, h_date, h_time) {
  

  distance.kernel <- GaussianKernelDistance(cbind(data$longitude, data$latitude),
                                            target.location,
                                           gamma, h_distance)
  day.kernel <- GaussianKernelDay(data$date, target.date, gamma, h_date) 
  multi.temp <- c()
  multi.temp2 <- c()
  multi.temp3 <- c()
  for (i in 1:length(times)) {
hours.kernel <- GaussianKernelHour(data$time,
                            target.time = times[i], gamma, h_time)
kernel.sum <- (distance.kernel+day.kernel+hours.kernel)
kernel.product <- (distance.kernel*day.kernel*hours.kernel)
print("distance.kernel")
print(distance.kernel[1])
print("day.kernel")
print(day.kernel[1])
print("hours.kernel")
print(hours.kernel[1])

     multi.temp <- c(multi.temp, sum(kernel.sum %*% data$air_temperature)/
                  sum(kernel.sum))
     multi.temp2 <- c(multi.temp2, sum(kernel.product %*% data$air_temperature)/
                       sum(kernel.product))
  }
  print(multi.temp)
#  print(times)
#  print(multi.temp2)
return(cbind(multi.temp, multi.temp2))
  }
############################### Functions ######################################

# Smoothing factors below. In kernel methods, smoothing factors or widths are 
#parameters that control the shape of the kernel function.
h_distance <- 100000
h_date <-10
h_time <-2

# The point to predict (Linkoing, Sweden)
lat <- 58.4274 
long <- 14.826
target.location <- cbind(long,lat)
target.date <- "1985-12-12" # The date to predict 
times <- c("04:00:00", "06:00:00","08:00:00","10:00:00",
           "12:00:00","14:00:00","16:00:00","18:00:00","20:00:00",
           "22:00:00", "24:00:00")
temp <- vector(length=length(times))

ref.index <- which(FilterData(st, target.date));
ref.data <- st[ref.index,]

multi.temp <- TempatureEstimate(ref.data, 1, target.location, target.date,times,
                                h_distance, h_date, h_time)
check <- (as.numeric(as.Date(target.date) - as.Date(st$date),
                                                      unit="days") %% 365) 
close.dates <- which(c(check, 365-check)<4)
close.dates <- st[close.dates,]
means <- tapply(close.dates$air_temperature, as.factor(close.dates$time), mean)
means <- means[times]
plot(multi.temp[,1], ylab="Temperature (C)", xlab="Time", 
     main = "Temperature estimation", col="blue", type="o", 
     ylim=c(min(multi.temp[which.min(multi.temp)], means, na.rm = TRUE) - 1,
            max(multi.temp[which.max(multi.temp)], means, na.rm = TRUE) + 2),
     xaxt = "n")
axis(1, at=1:length(times), labels=substring(times,1,2))
lines(multi.temp[,2], col="green", type="o")
lines(means, col="red", type="o")
legend(x = "topleft", legend = c("Sum Kernel", "Product Kernel","Mean temp history", target.date),
       col = c("blue", "green","red", "Black"), pch = 20, cex = 0.6)








