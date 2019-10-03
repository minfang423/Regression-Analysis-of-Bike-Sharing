

# reading the data files
bike <- read.csv("hour.csv") # oringinal data
bike1 <- read.csv("hourv3.csv")  # modified data
bike2 <- read.csv("day.csv")  #daily data


#-----------------------------factor predictors-------------------------------------------------
#as.date
library(lubridate)
bike$dteday <- as.Date(bike$dteday,'%m-/%d-%Y')

# factoring some variables from numeric
#bike$dteday = as.factor(bike$dteday)
bike$season = as.factor(bike$season)
bike$yr = as.factor(bike$yr)
bike$mnth = as.factor(bike$mnth)
bike$hr = as.factor(bike$hr)
bike$holiday = as.factor(bike$holiday)
bike$weekday = as.factor(bike$weekday)
bike$workingday=as.factor(bike$workingday)
bike$weathersit=as.factor(bike$weathersit)


#-----------------------------scatter plot-------------------------------------------------
pdf("scatter-plot.pdf")

boxplot(bike$cnt ~ bike$season,col=c(2:5),main="Total User ~ Season")
boxplot(bike$cnt ~ bike$yr,col=c(2:3),main="Total User ~ Year")
boxplot(bike$cnt ~ bike$mnth,col=c(2:13),main="Total User ~ Month")
boxplot(bike$cnt ~ bike$hr,col=rep(2:8,size=24),main="Total User ~ Hour")
boxplot(bike$cnt ~ bike$holiday,col = c(2,3),main="User ~ Holiday")
boxplot(bike$cnt ~ bike$weekday,col = c(2:7,2),bike$user,main="User ~ Weekdayd")
boxplot(bike$cnt ~ bike$workingday,col = c(2:3),main="User ~ Workingday")
boxplot(bike$cnt ~ bike$weathersit,col = c(2:5),main="User ~ Weather")
plot(bike$temp,bike$cnt,col = "skyblue",main="User ~ Temperature")
plot(bike$atemp,bike$cnt,col = "pink",main="User ~ Atemperature")
plot(bike$hum,bike$cnt,col = "green",main="User ~ Humidity")
plot(bike$windspeed,bike$cnt,col = "orange",main="User ~ Windspeed")

dev.off()
#-----------------------------ggplot2------------------------------------------------

#----------------------------- plot user ~ season in 2 types-----------------------------
pdf("ggplot.pdf")
library(plotly)
casual <- numeric()
register <- numeric()

for (i in 1:4) {
  casual <- c(casual,sum(bike$casual[bike$season == i]) / 2)
  register <- c(register,sum(bike$registered[bike$season == i]) / 2)
}
print(list(casual,register))
"
[[1]]
[1]  30311.0 101761.0 113045.5  64891.0

[[2]]
[1] 205363.0 357533.5 417519.0 355915.5
"

season <- factor(rep(1:4,1),labels =c("Spring","Summer","Fall","Winter"))
user.season <- data.frame(season=season,casual=casual,register=register)

plot_ly(user.season, x = ~season, y = ~casual, type = 'bar', name = 'Casual') %>%
  add_trace(y = ~register, name = 'Registered') %>%
  layout(title = "Average Rental by season",yaxis = list(title = 'Total bikes Rented'), barmode = 'stack')


#----------------------------- plot user ~yr in 2 types-----------------------------
plot("year.pdf")
library(plotly)
casual <- numeric()
register <- numeric()

for (i in 0:1){
  casual <- c(casual,sum(bike$casual[bike$yr == i]))
  register <- c(register,sum(bike$registered[bike$yr == i]))
}
print(list(casual,register))
"
[[1]]
[1] 247252 372765

[[2]]
[1]  995851 1676811
"

yr <- factor(rep(0:1,1),labels=c("2011","2012"))
user.yr <- data.frame(year=yr,casual=casual,register=register)

plot_ly(user.yr, x = ~yr, y = ~casual, type = 'bar', name = 'Casual') %>%
  add_trace(y = ~register, name = 'Registered') %>%
  layout(title = "Number of rentals by year",yaxis = list(title = 'Total bikes Rented'), barmode = 'stack')


dev.off()

#----------------------------- plot user ~ mnth in 2 types-----------------------------
#From this plot we can see that bike rental counts tends to peak during the summer months and reaches a low during 
#the winter months.#It also seems that the overall count of bike rentals is increasing in general as we move 
#forward in time.
casual <- numeric()
register <- numeric()

for (i in 1:12) {
  casual <- c(casual,sum(bike$casual[which(bike$mnth == i)]) / 2)
  register <- c(register,sum(bike$registered[which(bike$mnth == i)]) / 2)
}
print(list(casual,register))
"[[1]]
[1]  6021.0  7481.5 22222.0 30401.0 37642.5 36953.0 39078.5 36019.5 35161.5 29880.0 18301.5 10846.5  6021.0
[14]  7481.5 22222.0 30401.0 37642.5 36953.0 39078.5 36019.5 35161.5 29880.0 18301.5 10846.5

[[2]]
[1]  61445.5  68194.5  92238.0 104146.0 128200.5 136218.0 133395.5 139577.5 137834.0 131296.0 109114.0  94671.5
[13]  61445.5  68194.5  92238.0 104146.0 128200.5 136218.0 133395.5 139577.5 137834.0 131296.0 109114.0  94671.5
"
month <- factor(rep(1:12,1))
user.month <- data.frame(month=month,casual=casual,register=register)

plot_ly(user.month, x = ~ month, y = ~casual, type = 'bar', name = 'Casual') %>%
  add_trace(y = ~register, name = 'Registered') %>%
  layout(title = "Average Number of rentals by month",yaxis = list(title = 'Total bikes Rented'), barmode = 'stack')





#----------------------------- plot user ~ hr in 2 types-----------------------------
casual <- numeric()
register <- numeric()

for (i in 0:23) {
  casual <- c(casual,sum(bike$casual[which(bike$hr == i)]) / (365+366))
  register <- c(register,sum(bike$registered[which(bike$hr == i)]) / (365+366))
}
print(list(casual,register))
"
[[1]]
 [1] 10.088919  6.441860  4.667579  2.589603  1.195622  1.384405  4.127223 10.994528 21.560876 30.722298
[11] 46.222982 59.214774 68.013680 72.110807 75.361149 74.700410 73.644323 74.172367 60.870041 48.570451
[21] 36.084815 28.139535 22.161423 15.136799

[[2]]
[1]  43.440492  26.614227  17.701778   8.592339   4.861833  18.124487  71.292750 199.909713 335.485636
[10] 187.387141 126.495212 147.789330 184.262654 180.856361 164.928865 175.845417 237.912449 386.648427
[19] 362.894665 261.674419 189.017784 143.467852 108.634747  72.333789
"

hour <- factor(rep(0:23,1))
user.hour <- data.frame(hour=hour,casual=casual,register=register)

plot_ly(user.hour, x = ~ hour, y = ~casual, type = 'bar', name = 'Casual') %>%
  add_trace(y = ~register, name = 'Registered') %>%
  layout(title = "Average Number of rentals by hour",yaxis = list(title = 'Total bikes Rented'), barmode = 'stack')




#----------------------------- plot user ~ holiday in 2 types-----------------------------
casual <- numeric()
register <- numeric()

for (i in 0:1) {
  casual <- c(casual,sum(bike$casual[which(bike$holiday == i)])/2)
  register <- c(register,sum(bike$registered[which(bike$holiday == i)])/2)
}
print(list(casual,register))
"
[[1]]
[1] 298829.0  11179.5

[[2]]
[1] 1308293   28038
"
holiday <- factor(rep(0:1,1),labels=c("Non-Holiday","Holiday"))
user.holiday <- data.frame(holiday=holiday,casual=casual,register=register)

plot_ly(user.holiday, x = ~ holiday, y = ~casual, type = 'bar', name = 'Casual') %>%
  add_trace(y = ~register, name = 'Registered') %>%
  layout(title = "Average Number of rentals by holiday",yaxis = list(title = 'Total bikes Rented'), barmode = 'stack')



#----------------------------- plot user ~ weekday in 2 types-----------------------------
casual <- numeric()
register <- numeric()

for (i in 0:6) {
  casual <- c(casual,sum(bike$casual[bike$weekday == i]) / 2)
  register <- c(register,sum(bike$registered[bike$weekday == i]) / 2)
}
print(list(casual,register))
"
[[1]]
[1] 70260.5 35392.0 28921.5 28659.5 30730.0 39119.0 76926.0

[[2]]
[1] 151753.0 192359.5 205633.0 207864.5 211967.5 204776.0 161977.5
"
weekday <- factor(rep(0:6,1),labels=c("Sun","Mon","Tue","Wed","Thr","Fri","Sat"))
user.weekday <- data.frame(weekday=weekday,casual=casual,register=register)

plot_ly(user.weekday, x = ~ weekday, y = ~casual, type = 'bar', name = 'Casual') %>%
  add_trace(y = ~register, name = 'Registered') %>%
  layout(title = "Average Number of rentals by weekday",yaxis = list(title = 'Total bikes Rented'), barmode = 'stack')



#----------------------------- plot user ~ workingday in 2 types-----------------------------
casual <- numeric()
register <- numeric()
day.wd <- 11865/24 #[1] 494.375   1
daynon.wd <- 5514/24 #[1] 229.75  0
nday <- c(229.75,494.375)

for (i in 0:1) {
  casual <- c(casual,sum(bike$casual[bike$workingday == i])/nday[i+1])
  register <- c(register,sum(bike$registered[bike$workingday == i])/nday[i+1])
}
print(list(casual,register))
"
[[1]]
[1] 1378.5941  613.4716

[[2]]
[1] 2975.134 4023.515

"
holiday <- factor(rep(0:1,1),labels=c("Non-Working Day","Working Day"))
user.holiday <- data.frame(WorkingDay=holiday,casual=casual,register=register)

plot_ly(user.holiday, x = ~ WorkingDay, y = ~casual, type = 'bar', name = 'Casual') %>%
  add_trace(y = ~register, name = 'Registered') %>%
  layout(title = "Average Daily rentals by working day",yaxis = list(title = 'Total bikes Rented'), barmode = 'stack')


#----------------------------- plot user ~ weather in 2 types-----------------------------
casual <- numeric()
register <- numeric()
hr.w1 <- sum(bike$weathersit == 1)  #11413
hr.w2 <- sum(bike$weathersit == 2)  # 4544
hr.w3 <- sum(bike$weathersit == 3)  # 1419
hr.w4 <- sum(bike$weathersit == 4)  # 3
hr.weather <- c(hr.w1,hr.w2,hr.w3,hr.w4)

for (i in 1:4) {
  casual <- c(casual,sum(bike$casual[bike$weathersit == i]) / hr.weather[i])
  register <- c(register,sum(bike$registered[bike$weathersit == i]) / hr.weather[i])
}
print(list(casual,register))
"
[[1]]
[1] 40.545431 29.595290 16.055673  2.666667

[[2]]
[1] 164.32384 145.57020  95.52361  71.66667
"
weather <- factor(rep(1:4,1))
user.weather <- data.frame(weather=weather,casual=casual,register=register)

plot_ly(user.weather, x = ~ weather, y = ~casual, type = 'bar', name = 'Casual') %>%
  add_trace(y = ~register, name = 'Registered') %>%
  layout(title = "Average Hourly rentals by weather",yaxis = list(title = 'Total bikes Rented'), barmode = 'stack')



#----------------------------- plot user ~ temp in 2 types-----------------------------
library(ggplot2)
bike1$temp <- bike1$temp * 41

bike1$type <- factor(bike1$type,labels =c("Registered","Casual"))
ggplot(bike1, aes(temp, user)) + geom_point(aes(color = temp), alpha = 0.5) + 
  scale_color_gradient(low = "#88d8b0", high = "#ff6f69") + ggtitle("Temperature vs Count") + facet_wrap(~type) +
  theme(title=element_text(size=15),axis.title=element_text(size=14))


#----------------------------- plot user ~ atemp in 2 types-----------------------------
bike1$atemp <- bike1$atemp * 50

bike1$type <- factor(bike1$type,labels =c("Registered","Casual"))
ggplot(bike1, aes(atemp, user)) + geom_point(aes(color = atemp), alpha = 0.5) + 
  scale_color_gradient(low = "#88d8b0", high = "#ff6f69") + ggtitle("Atemperature vs Count") + facet_wrap(~type) +
  theme(title=element_text(size=15),axis.title=element_text(size=14))





#----------------------------- plot user ~ hum in 2 types-----------------------------
bike1$hum <- bike1$hum * 100

bike1$type <- factor(bike1$type,labels =c("Registered","Casual"))
ggplot(bike1, aes(hum, user)) + geom_point(aes(color = hum), alpha = 0.5) + 
  scale_color_gradient(low = "#88d8b0", high = "#ff6f69") + ggtitle("humidity vs Count") + facet_wrap(~type) +
  theme(title=element_text(size=15),axis.title=element_text(size=14))



#----------------------------- plot user ~ windspeed in 2 types-----------------------------
bike1$windspeed <- bike1$windspeed * 67

bike1$type <- factor(bike1$type,labels =c("Registered","Casual"))
ggplot(bike1, aes(windspeed, user)) + geom_point(aes(color = windspeed), alpha = 0.5) + 
  scale_color_gradient(low = "#88d8b0", high = "#ff6f69") + ggtitle("Windspeedy vs Count") + facet_wrap(~type)




#-----------------------------Hyperthesis -------------------------------------------------
####### total/day ~ workingday 
#On working days, bike rental acitivty peaks in the morning (~8PM) and in the afternoon (~5PM). 
bike.wd <- subset(bike, workingday == 1)
ggplot(bike.wd, aes(hr, cnt)) + geom_point(position = position_jitter(w = 1, h = 0), aes(color = temp)) + 
  scale_color_gradient(low = "#88d8b0", high = "#ff6f69") + ggtitle("Bike Count on Working Days")
#######On total/day ~ non-working days we see a gradual increase in bike rental activity that peaks between 1PM and 3PM.
bike.nwd <- subset(bike, workingday == 0)
ggplot(bike.nwd, aes(hr, cnt)) + geom_point(position = position_jitter(w = 1, h = 0), aes(color = temp)) + 
  scale_color_gradient(low = "#88d8b0", high = "#ff6f69") + ggtitle("Bike Count on non-Working Days")


####### cnt/day ~ workingday ~ type
colors.tempurature <- c("#5e4fa2", "#3288bd", "#66c2a5", "#abdda4", "#e6f598",
                        "#fee08b", "#fdae61", "#f46d43", "#d53e4f", "#9e0142")

bike1$type <- factor(bike1$type,labels = c("Registered","Casual"))
bike1$workingday <- factor(bike1$workingday,labels=c("Non-working Day", "Working Day"))
gg <- ggplot(bike1, aes(hr, user, color=temp * 41)) +
  facet_grid(type ~ workingday) +
  geom_point() +
  geom_smooth() +
  theme(plot.title = element_text(size = rel(1.5))) +
  labs(title="Daily Bike Rental Demand \nPer Time of Day, Registered/Casual, and Working Day") + 
  labs(x="Hour of Day") + 
  labs(y="Bike Rentals Initiated per Hour") +
  scale_colour_gradientn("Temp (°F)",colours=colors.tempurature)



####### cnt/day ~ weather ~ type
colors.tempurature <- c("#5e4fa2", "#3288bd", "#66c2a5", "#abdda4", "#e6f598",
                        "#fee08b", "#fdae61", "#f46d43", "#d53e4f", "#9e0142")

bike1$type <- factor(bike1$type,labels = c("Registered","Casual"))
bike1$weathersit <- factor(bike1$weathersit,labels=c("Clear Day","Mist/Cloudy Day",
                                                      "Light Snow/Light Rain with Thunderstorm/Clouds Day",
                                                      "Heavy Rain/Ice Pallets with Mist Day"))
gg <- ggplot(bike1, aes(hr, user, color=temp * 41)) +
  facet_grid(type ~ weathersit) +
  geom_point() +
  geom_smooth() +
  theme(plot.title = element_text(size = rel(1.5))) +
  labs(title="Daily Bike Rental Demand \nPer Time of Day, Registered/Casual, and Weather") + 
  labs(x="Hour of Day") + 
  labs(y="Bike Rentals Initiated per Hour") +
  scale_colour_gradientn("Temp (°F)",colours=colors.tempurature)




####### cnt/day ~ season ~ type
colors.tempurature <- c("#5e4fa2", "#3288bd", "#66c2a5", "#abdda4", "#e6f598",
                        "#fee08b", "#fdae61", "#f46d43", "#d53e4f", "#9e0142")

bike1$type <- factor(bike1$type,labels = c("Registered","Casual"))
bike1$season <- factor(bike1$season)
gg <- ggplot(bike1, aes(hr, user, color=temp * 41)) +
  facet_grid(type ~ season) +
  geom_point() +
  geom_smooth() +
  theme(plot.title = element_text(size = rel(1.5))) +
  labs(title="Daily Bike Rental Demand \nPer Time of Day, Registered/Casual, and Season") + 
  labs(x="Hour of Day") + 
  labs(y="Bike Rentals Initiated per Hour") +
  scale_colour_gradientn("Temp (°F)",colours=colors.tempurature)






####### total/mnth ~ yr(2011)
library(ggplot2)
bike.yr0 <- subset(bike, yr == 0)
ggplot(bike.yr0, aes(mnth, cnt)) + geom_point(position = position_jitter(w = 1, h = 0), aes(color = temp)) + 
  scale_color_gradient(low = "#88d8b0", high = "#ff6f69") + ggtitle("Bike Count in 2011")
####### total/mnth ~ yr(2012)
bike.yr1 <- subset(bike, yr == 1)
ggplot(bike.yr1, aes(mnth, cnt)) + geom_point(position = position_jitter(w = 1, h = 0), aes(color = temp)) + 
  scale_color_gradient(low = "#88d8b0", high = "#ff6f69") + ggtitle("Bike Count in 2012")




##### cnt/day ~ type ~ weather (1)
library(ggplot2)
bike.we1 <- subset(bike, weathersit == 1)
ggplot(bike.we1, aes(hr, cnt)) + geom_point(position = position_jitter(w = 1, h = 0), aes(color = temp)) + 
  scale_color_gradient(low = "#88d8b0", high = "#ff6f69") + ggtitle("Bike Count on Clear Day")
##### cnt/day ~ type ~ weather (2)
bike.we2 <- subset(bike, weathersit == 2)
ggplot(bike.we1, aes(hr, cnt)) + geom_point(position = position_jitter(w = 1, h = 0), aes(color = temp)) + 
  scale_color_gradient(low = "#88d8b0", high = "#ff6f69") + ggtitle("Bike Count on Mist + Cloudy Day")
##### cnt/day ~ type ~ weather (3)
bike.we3 <- subset(bike, weathersit == 3)
ggplot(bike.we1, aes(hr, cnt)) + geom_point(position = position_jitter(w = 1, h = 0), aes(color = temp)) + 
  scale_color_gradient(low = "#88d8b0", high = "#ff6f69") + ggtitle("Bike Count on Light Snow, Light Rain + Thunderstorm + Scattered clouds Day")
##### cnt/day ~ type ~ weather (4)
bike.we4 <- subset(bike, weathersit == 4)
ggplot(bike.we1, aes(hr, cnt)) + geom_point(position = position_jitter(w = 1, h = 0), aes(color = temp)) + 
  scale_color_gradient(low = "#88d8b0", high = "#ff6f69") + ggtitle("Bike Count on Heavy Rain + Ice Pallets + Thunderstorm + Mist Day")



#--------------------------------type ~ hr ~ working day table------------------------------
# working-day
bike.wd <- subset(bike, workingday == 1)
casual.wd <- numeric()
register.wd <- numeric()
n.wd <- nrow(bike.wd)  #[1] 11865
day.wd <- 11865/24 #[1] 494.375

for (i in 0:23) {
  casual.wd <- c(casual.wd,sum(bike.wd$casual[which(bike.wd$hr == i)]) / (11865 / 24))
  register.wd <- c(register.wd,sum(bike.wd$registered[which(bike.wd$hr == i)]) / (11865 / 24))
}

result.wd <- list(casual.wd = round(casual.wd,1),register.wd = round(register.wd,1))
print(result.wd)
"
$casual.wd
 [1]  7.2  3.7  2.1  0.9  0.7  1.3  4.3 12.2 22.3 24.4 31.3 37.7 41.6 42.9 46.2 46.7 48.8 57.4 50.8 41.3 31.6 24.8 19.7
[24] 13.5

$register.wd
[1]  29.7  12.8   6.4   3.8   4.4  23.7  98.5 279.3 456.2 217.9 104.5 121.0 160.3 157.0 138.7 156.1 247.1 472.8 445.0
[20] 309.6 220.0 162.9 119.7  75.9

"

#non-working day
bike.nonwd <- subset(bike, workingday == 0)
casual.nonwd <- numeric()
register.nonwd <- numeric()
n.nonwd <- nrow(bike.nonwd)  #[1] 5514
daynon.wd <- 5514/24 #[1] 229.75

for (i in 0:23) {
  casual.nonwd <- c(casual.nonwd,sum(bike.nonwd$casual[which(bike.nonwd$hr == i)]) / (11865 / 24))
  register.nonwd <- c(register.nonwd,sum(bike.nonwd$registered[which(bike.nonwd$hr == i)]) / (11865 / 24))
}
result.nonwd <- list(casual.nonwd = round(casual.nonwd,1),register.nonwd = round(register.nonwd,1))
print(result.nonwd)

"
$casual.nonwd
 [1]  7.7  5.8  4.8  3.0  1.0  0.8  1.8  4.0  9.5 21.0 37.0 49.8 59.0 63.7 65.2 63.8 60.1 52.2 39.2 30.5 21.8 16.8 13.1
[24]  8.9

$register.nonwd
[1]  34.5  26.5  19.7   8.9   2.8   3.1   6.9  16.3  39.8  59.2  82.5  97.5 112.1 110.4 105.2 103.9 104.7  98.9  91.6
[20]  77.3  59.5  49.3  40.9  31.1
"




#------------------------------addition-------------------------------------------



#-----------------------------model fit-------------------------------------------------
#Create X matrix
X <- cbind(eason,yr,mnth,hr,holiday,weekday,workingday,weathersit,temp,atemp,hum 
           + windspeed)

#linear regression
fit.full <- lm(total ~ season + yr + mnth + hr + holiday + weekday + workingday +  weathersit + temp + atemp + hum 
              + windspeed, data = bike)
summary(fit.full)
plot(fitted.values(fit.lin),rstandard(fit.lin))
abline(0,0)
qqnorm(rstandard(fit.lin))
abline(0,1)


fit.lin2 <- lm(cnt ~ season + mnth + hr + holiday + weekday + workingday +  weathersit + I(log(temp)) + I(hum^10)
              + I(windspeed^2), data = bike)
summary(fit.lin2)


#two way interaction of type with all other predictors
bike.fit20 <-lm(log(bike.subset$user) ~ hr*type+atemp*type+yr*type+hum*type+workingday*type+season*type+weathersit*type, data = bike.subset)
summary(bike.fit20)

#two way intereaction of type*hr and type*workingday
#fit a model to look at the differentiated response on casual vs. registered users on the 5 time slots(hr)
#and on workingday vs. non-workingday 
#(two weay interaction which will change the intercept or distance of regression lines)
bike.fit21 <- lm(log(bike.subset$user) ~ atemp+type*hr+type*workingday+season+weathersit+yr+hum, data = bike.subset)
summary(bike.fit21)

#three way interaction (atemp*type*workingday) 
#fit a model to look at the potential different influence of atemp on casual vs. registered users on workingday or non-workingday
#we want to look at the angle of regression lines(slope change)

bike.fit22 <- lm(log(bike.subset$user) ~ atemp*type*workingday+hum+hr+season+weathersit+yr, data = bike.subset)
summary(bike.fit22)
###Interpretation: beta123 is not significant, which means the average count change associated with 1 unit increase of atemp between casual and registered users
###on workingday vs. non-workingday is not significant.
###beta13 is positive, which means the average casual users increase more with every 1 unit increase of atemp compared with 
###registered users on a non-workingday.

#three way interaction (atemp*type*workingday and  atemp*type*hr)
#fit a model to look at the potential different influence of atemp on casual vs. registered users on the 5 time slots(hr)
#and on workingday vs. non-workingday ()
bike.fit23 <- lm(log(bike.subset$user) ~ atemp*type*hr+atemp*type*workingday+season+weathersit+yr+hum, data = bike.subset)
summary(bike.fit23)















