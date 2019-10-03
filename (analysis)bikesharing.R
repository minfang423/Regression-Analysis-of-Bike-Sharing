#### log(count+1) instead of removing zero values
#### when validation, eat.model should use data=data_est
###  VIF is missing




# ---- 1.Data transform and fit the full model ----

#read table
bike <- read.csv("hourv2.csv", header = TRUE)
#factoring variables
bike$season = as.factor(bike$season)
bike$mnth = as.factor(bike$mnth)
bike$weekday = as.factor(bike$weekday)
bike$holiday = as.factor(bike$holiday)
bike$workingday = as.factor(bike$workingday)
bike$weathersit = as.factor(bike$weathersit)
bike$hr = as.factor(bike$hr)
bike$yr = as.factor(bike$yr)
bike$weekday = as.factor(bike$weekday)
bike$type = as.factor(bike$type)



# ---- 1.1 split data ---
#split
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(bike$user,SplitRatio = 0.8)
training_set = subset(bike,split == T)
test_set = subset(bike,split == FALSE)

# ---- 1.2 fit a full multiple linear regression model ---- 
bike.fit1 <- lm(user ~ season + yr + mnth + hr + holiday + weekday +
                  workingday + weathersit + temp + atemp + hum + windspeed + type, data = training_set)
summary(bike.fit1)

# ---- 2.Perform a thorough residual analysis of this model. ----
par(mfrow=c(1,2))
qqnorm(rstandard(bike.fit1))
abline(0,1)
plot(fitted.values(bike.fit1), rstandard(bike.fit1), main = "Residual plot")
abline(0,0)

# ---- 3.Transform Y --- Boxcox method ----
par(mfrow=c(1,1))
bike.fitboxcox <- lm(bike$user1 ~ season + yr + mnth + hr + holiday + weekday +
                       workingday + weathersit + temp + atemp + hum + windspeed + type, data = bike)
library(MASS)
boxcox(bike.fitboxcox)

# ---- 4.new model ----
#remove user = 0
bike.subset <- subset(bike, user > 0)
bike.fit2 <-lm(log(user) ~ season + yr + mnth + hr + holiday + weekday +
                 workingday + weathersit + temp + atemp + hum + windspeed + type, data = bike.subset)
summary(bike.fit2)  
#qqplot and residual plot
par(mfrow=c(1,2))
qqnorm(rstandard(bike.fit2))
abline(0,1)
plot(fitted.values(bike.fit2), rstandard(bike.fit2), main = "Redisual plot")
abline(0,0)

# ---- 5.Variable selection ----
#divide variable hr into 5 category
#level 1 [0-5], level 2 [6-9], level 3 [10-15], level 4 [16-20], level 5 [21-23]
levels(bike.subset$hr) <- c(1,1,1,1,1,1,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,5,5,5)
levels(bike.subset$hr)
##forward selection
library(leaps)
attach(bike.subset)
forward <- regsubsets(x=cbind(bike.subset$season,bike.subset$yr,bike.subset$mnth,bike.subset$hr,bike.subset$holiday,bike.subset$weekday,bike.subset$workingday,
                              bike.subset$weathersit,bike.subset$temp,bike.subset$atemp,bike.subset$hum,bike.subset$windspeed,bike.subset$type), y=log(bike.subset$user),
                      method="forward",nbest=1)
summary(forward)
Cp <- summary(forward)$cp
AdjR2 <- summary(forward)$adjr2
SSRes <- summary(forward)$rss
R2 <- summary(forward)$rsq
Matrix <- summary(forward)$which
p <- apply(Matrix,1, sum)
MSE <- SSRes/(33153-p)
output1 <- cbind(p, Matrix, R2, AdjR2, SSRes, Cp)
colnames(output1)[3:15] <- c("season","yr","mnth","hr","holiday","weekday","workingday","weathersit","temp","atemp","hum","windspeed","type") 
output1
###forward variable selection order: 1.type;2.hr;3.temp;4.hum;5.yr;6.season;7.workingday;8.atemp

##backward selection
library(leaps)
attach(bike.subset)
backward <- regsubsets(x=cbind(bike.subset$season,bike.subset$yr,bike.subset$mnth,bike.subset$hr,bike.subset$holiday,bike.subset$weekday,bike.subset$workingday,
                               bike.subset$weathersit,bike.subset$temp,bike.subset$atemp,bike.subset$hum,bike.subset$windspeed,bike.subset$type), y=log(bike.subset$user),
                       method="backward", nbest = 1)
summary(backward)
Cp <- summary(backward)$cp
AdjR2 <- summary(backward)$adjr2
SSRes <- summary(backward)$rss
R2 <- summary(backward)$rsq
Matrix <- summary(backward)$which
p <- apply(Matrix,1, sum)
MSE <- SSRes/(33153-p)
output2 <- cbind(p, Matrix, R2, AdjR2, SSRes, Cp)
colnames(output2)[3:15] <- c("season","yr","mnth","hr","holiday","weekday","workingday","weathersit","temp","atemp","hum","windspeed","type") 
output2
###backward variable drop order: season+yr+hr+workingday+atemp+hum+windspeed+type   
####drop order:1.windspeed;2.workingday;3.season;4.yr;5.hum;6.atemp;7.hr

##exhaustive selection
library(leaps)
attach(bike.subset)
exhaustive <- regsubsets(x=cbind(bike.subset$season,bike.subset$yr,bike.subset$mnth,bike.subset$hr,bike.subset$holiday,bike.subset$weekday,bike.subset$workingday,
                                 bike.subset$weathersit,bike.subset$temp,bike.subset$atemp,bike.subset$hum,bike.subset$windspeed,bike.subset$type), y=log(bike.subset$user),
                         method="exhaustive", nbest = 3)
summary(exhaustive)
Cp <- summary(exhaustive)$cp
AdjR2 <- summary(exhaustive)$adjr2
SSRes <- summary(exhaustive)$rss
R2 <- summary(exhaustive)$rsq
Matrix <- summary(exhaustive)$which
p <- apply(Matrix,1, sum)
MSE <- SSRes/(33153-p)
output3 <- cbind(p, Matrix, R2, AdjR2, SSRes, Cp)
colnames(output3)[3:15] <- c("season","yr","mnth","hr","holiday","weekday","workingday","weathersit","temp","atemp","hum","windspeed","type") 
output3
###exhaustive selection order: 1.type 2.type+hr3.type+hr+temp;4.type+hr+atemp+hum;
####5.type+hr+atemp+hum+yr;6.type+hr+atemp+hum+yr+season;7.type+hr+atemp+hum+yr+season+workingday;8.type+hr+atemp+hum+yr+season+workingday+windspeed

######conclusion:1.intersting finding:In our prediction, the temp should be an important factor affecting the number of bike user, 
#################but through the variable selection, we found that the temp has not a significant influence the bike user, 
#################while, atemp has more signifcant influence than temp.
#################2.some variables(eg.weathersit)should be add in the model, we suspect the reason why these variables are not selected 
##################may be due to the relationship with response, so, next, we need to draw some scatterplot respectively to analyze.  
#################3. The criteria of R2, AdjR2, SSres, Cp not good, It maybe because of the form of variables. We need do the further analyze

#4.Variable selection---"By-hand"
#named varibales
y <- log(bike.subset$user)
x1 <- bike.subset$season
x2 <- bike.subset$yr
x3 <- bike.subset$mnth
x4 <- bike.subset$hr
x5 <- bike.subset$holiday
x6 <- bike.subset$weekday
x7 <- bike.subset$workingday
x8 <- bike.subset$weathersit
x9 <- bike.subset$temp
x10 <- bike.subset$atemp
x11 <- bike.subset$hum
x12 <- bike.subset$windspeed
x13 <- bike.subset$type

##Forward variable selection
fit.0 <- lm( y ~ 1, data = bike.subset)
add1(fit.0, y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13, test = "F")
fit.1 <- lm(y ~ x13, data = bike.subset)
add1(fit.1, y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13, test = "F")
fit.2 <- lm(y ~ x13 + x4, data = bike.subset)
add1(fit.2, y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13, test = "F")
fit.3 <- lm(y ~ x13 + x4 + x10, data = bike.subset)
add1(fit.3, y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13, test = "F")
fit.4 <- lm(y ~ x13 + x4 + x10 + x2, data = bike.subset)
add1(fit.4, y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13, test = "F")
fit.5 <- lm(y ~ x13 + x4 + x10 + x2 + x11, data = bike.subset)
add1(fit.5, y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13, test = "F")
fit.6 <- lm(y ~ x13 + x4 + x10 + x2 + x11 + x7, data = bike.subset)
add1(fit.6, y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13, test = "F")
fit.7 <- lm(y ~ x13 + x4 + x10 + x2 + x11 + x7 + x1, data = bike.subset)
add1(fit.7, y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13, test = "F")
fit.8 <- lm(y ~ x13 + x4 + x10 + x2 + x11 + x7 + x1 + x8, data = bike.subset)
add1(fit.8, y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13, test = "F")
fit.9 <- lm(y ~ x13 + x4 + x10 + x2 + x11 + x7 + x1 + x8 + x5, data = bike.subset)
add1(fit.9, y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13, test = "F")
fit.10 <- lm(y ~ x13 + x4 + x10 + x2 + x11 + x7 + x1 + x8 + x5 + x9, data = bike.subset)
add1(fit.10, y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13, test = "F")
fit.11 <- lm(y ~ x13 + x4 + x10 + x2 + x11 + x7 + x1 + x8 + x5 + x9 + x6, data = bike.subset)
add1(fit.11, y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13, test = "F")
fit.12 <- lm(y ~ x13 + x4 + x10 + x2 + x11 + x7 + x1 + x8 + x5 + x9 + x6 + x3 , data = bike.subset)
add1(fit.12, y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13, test = "F")
##add order:1.type;2.hr;3.atemp;4.yr;5.hum;6.workingday;7.season;8.weathersit;9.holiday;10.temp;11.weekday;12.mnth;13.windspeed

##Backward variable selection
fit.13 <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13, data = bike.subset)
drop1(fit.13,y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13, test = "F")
#x5 and x7's F value is 0 
fit.12 <- lm(y ~ x1 + x2 + x3 + x4 + x6 + x8 + x9 + x10 + x11 + x12 + x13, data = bike.subset)
drop1(fit.12,y ~ x1 + x2 + x3 + x4 + x6 + x8 + x9 + x10 + x11 + x12 + x13, test = "F")
fit.11 <- lm(y ~ x1 + x2 + x3 + x4 + x6 + x8 + x9 + x10 + x11 + x13, data = bike.subset)
drop1(fit.11,y ~ x1 + x2 + x3 + x4 + x6 + x8 + x9 + x10 + x11 + x13, test = "F")
fit.10 <- lm(y ~ x1 + x2 + x4 + x6 + x8 + x9 + x10 + x11 + x13, data = bike.subset)
drop1(fit.10,y ~ x1 + x2 + x4 + x6 + x8 + x9 + x10 + x11 + x13, test = "F")
fit.9 <- lm(y ~ x1 + x2 + x4 + x6 + x8 + x10 + x11 + x13, data = bike.subset)
drop1(fit.9,y ~ x1 + x2 + x4 + x6 + x8 + x10 + x11 + x13, test = "F")
#Drop out order:holiday, workingday, windspeed, mnth, temp

##Stepwise variable selection
fit.0 <- lm(y ~ 1, data = bike.subset)
add1(fit.0, y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13, test = "F")
fit.1 <- lm(y ~ x13, data = bike.subset)
drop1(fit.1, y ~ x13, test = "F")

add1(fit.1, y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13, test = "F")
fit.2 <- lm(y ~ x13 + x4, data = bike.subset)
drop1(fit.2, y ~ x13 + x4, test = "F")

add1(fit.2, y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13, test = "F")
fit.3 <- lm(y ~ x13 + x4 + x10, data = bike.subset)
drop1(fit.3, y ~ x13 + x4 + x10, test = "F")

add1(fit.3, y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13, test = "F")
fit.4 <- lm(y ~ x13 + x4 + x10 + x2, data = bike.subset)
drop1(fit.4, y ~ x13 + x4 + x10 + x2, test = "F")

add1(fit.4, y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13, test = "F")
fit.5 <- lm(y ~ x13 + x4 + x10 + x11, data = bike.subset)
drop1(fit.5, y ~ x13 + x4 + x10 + x11, test = "F")

add1(fit.5, y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13, test = "F")
fit.6 <- lm(y ~ x13 + x4 + x10 + x11 + x2, data = bike.subset)
drop1(fit.6, y ~ x13 + x4 + x10 + x11 + x2, test = "F")

add1(fit.6, y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13, test = "F")
fit.7 <- lm(y ~ x13 + x4 + x10 + x11 + x2 + x7, data = bike.subset)
drop1(fit.7, y ~ x13 + x4 + x10 + x11 + x2 + x7, test = "F")

add1(fit.7, y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13, test = "F")
fit.8 <- lm(y ~ x13 + x4 + x10 + x11 + x2 + x7 + x1, data = bike.subset)
drop1(fit.8, y ~ x13 + x4 + x10 + x11 + x2 + x7 + x1, test = "F")

add1(fit.8, y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13, test = "F")
fit.9 <- lm(y ~ x13 + x4 + x10 + x11 + x2 + x7 + x1 + x8, data = bike.subset)
drop1(fit.9, y ~ x13 + x4 + x10 + x11 + x2 + x7 + x1 + x8, test = "F")

add1(fit.9, y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13, test = "F")
fit.10 <- lm(y ~ x13 + x4 + x10 + x11 + x2 + x7 + x1 + x8 + x5, data = bike.subset)
drop1(fit.10, y ~ x13 + x4 + x10 + x11 + x2 + x7 + x1 + x8 + x5, test = "F")

add1(fit.10, y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13, test = "F")
fit.11 <- lm(y ~ x13 + x4 + x10 + x11 + x2 + x7 + x1 + x8 + x5 + x9, data = bike.subset)
drop1(fit.11, y ~ x13 + x4 + x10 + x11 + x2 + x7 + x1 + x8 + x5 + x9, test = "F")

add1(fit.11, y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13, test = "F")
fit.12 <- lm(y ~ x13 + x4 + x10 + x11 + x2 + x7 + x1 + x8 + x5 + x9 + x6, data = bike.subset)
drop1(fit.12, y ~ x13 + x4 + x10 + x11 + x2 + x7 + x1 + x8 + x5 + x9 + x6, test = "F")
##conclusion:no drop variable
###1.type;2.hr;3.atemp;4.hum;5.yr;6.workingday;7.season;8.weathersit;9.holiday;10.temp;11.weekday

# ---- 6. Fit model and assumption checking ----
bike.fit3 <- lm(log(user) ~ season + yr + hr + workingday + weathersit + atemp + hum + type, data = bike.subset)
summary(bike.fit3)
#checking
par(mfrow=c(1,2))
qqnorm(rstandard(bike.fit3))
abline(0,1)
plot(fitted.values(bike.fit3), rstandard(bike.fit3), main = "Redisual plot")
abline(0,0)
#testing
anova(bike.fit2,bike.fit3)

#6.add interaction--final model
#according to ggplot, we divide hr into two level(add ggplot code)

bike.fit4 <- lm(log(user) ~ season + yr + workingday*type*hr + weathersit + atemp + hum, data = bike.subset)
summary(bike.fit4)


# ---- 7. model validation ----
set.seed(5)
obs <- c(1:33153)
sample.est <- sort(sample(obs,32000))
sample.val <- (1:33153)[-sample.est]
bike.est <- bike.subset[sample.est,]
bike.val <- bike.subset[sample.val,]
fit.est <- lm(log(user) ~ season + yr + workingday*type*hr + weathersit + atemp + hum, data = bike.subset)    #################### #data = bike.est #######################
summary(fit.est)
coefficients(fit.est)
y_hat <- predict(fit.est, bike.val[,c(3,4,6,9,10,12,13,16)])
pred_error <- log(bike.val[,15]) - y_hat
sum(pred_error^2)/1153   #MSP= 0.4962858  MSE=0.5 
anova(fit.est)
##repeat 100 times
sample.est <- sort(sample(obs,32153))
sample.val <- (1:33153)[-sample.est]
bike.est <- bike.subset[sample.est,]
bike.val <- bike.subset[sample.val,]
fit.est <- lm(log(user) ~ season + yr + workingday*type*hr + weathersit + atemp + hum, data = bike.subset)

coefficients(fit.est)
y_hat <- predict(fit.est, bike.val[,c(3,4,6,9,10,12,13,16)])
pred_error <- log(bike.val[,15]) - y_hat
sum(pred_error^2)/1000

beta0 <- numeric()
beta1 <- numeric()
beta2 <- numeric()
beta3 <- numeric()
beta4 <- numeric()
beta5 <- numeric()
beta6 <- numeric()
beta7 <- numeric()
beta8 <- numeric()
beta9 <- numeric()
beta10 <- numeric()
beta11 <- numeric()
beta12 <- numeric()
beta13 <- numeric()
beta14 <- numeric()
beta15<- numeric()
beta16<- numeric()
beta17<- numeric()
beta18<- numeric()
beta19<- numeric()
beta20<- numeric()
beta21<- numeric()
beta22<- numeric()
beta23<- numeric()
beta24<- numeric()
beta25<- numeric()
beta26<- numeric()
beta27<- numeric()
beta28<- numeric()
MSP <- numeric()
for (i in 1:100){
  sample.est <- sort(sample(obs,32153))
  sample.val <- (1:33153)[-sample.est]
  bike.est <- bike.subset[sample.est,]
  bike.val <- bike.subset[sample.val,]
  
  fit.est <- lm(log(user) ~ season + yr + workingday*type*hr + weathersit + atemp + hum, data = bike.subset)

  coefficients(fit.est)
  y_hat <- predict(fit.est, bike.val[,c(3,4,6,9,10,12,13,16)])
  pred_error <- log(bike.val[,15]) - y_hat
  
  beta0[i] <- coef(fit.est)[1]
  beta1[i] <- coef(fit.est)[2]
  beta2[i] <- coef(fit.est)[3]
  beta3[i] <- coef(fit.est)[4]
  beta4[i] <- coef(fit.est)[5]
  beta5[i] <- coef(fit.est)[6]
  beta6[i] <- coef(fit.est)[7]
  beta7[i] <- coef(fit.est)[8]
  beta8[i] <- coef(fit.est)[9]
  beta9[i] <- coef(fit.est)[10]
  beta10[i] <- coef(fit.est)[11]
  beta11[i] <- coef(fit.est)[12]
  beta12[i] <- coef(fit.est)[13]
  beta13[i] <- coef(fit.est)[14]
  beta14[i] <- coef(fit.est)[15]
  beta15[i] <- coef(fit.est)[16]
  beta16[i] <- coef(fit.est)[17]
  beta17[i] <- coef(fit.est)[18]
  beta18[i] <- coef(fit.est)[19]
  beta19[i] <- coef(fit.est)[20]
  beta20[i] <- coef(fit.est)[21]
  beta21[i] <- coef(fit.est)[22]
  beta22[i] <- coef(fit.est)[23]
  beta23[i] <- coef(fit.est)[24]
  beta24[i] <- coef(fit.est)[25]
  beta25[i] <- coef(fit.est)[26]
  beta26[i] <- coef(fit.est)[27]
  beta27[i] <- coef(fit.est)[28]
  beta28[i] <- coef(fit.est)[29]
  MSP[i] <- sum(pred_error^2)/1000
}
summary(MSP) #  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##############0.4610  0.5082  0.5328  0.5337  0.5552  0.6285 