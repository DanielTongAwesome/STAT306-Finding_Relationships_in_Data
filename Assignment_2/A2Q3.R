#cat("\014")  
rm(list = ls())

# housing price in richmond / 1000
askpr=c(68.8, 25.9, 56.88, 61.5, 49.9, 74.8, 44.8, 58.68, 52.4, 58.8, 54.98, 59.8, 71.99, 68.8, 73.9, 40.9, 53.8, 78.8, 40.8, 48.5, 47.9, 55.2, 79.8, 62.9, 54.8, 108.8, 55.8, 51.99, 51.68, 48.8, 60.8, 73.8, 58.39, 50.5, 53.8, 54.8, 45.99, 65.99, 56.8, 41.99, 46.8, 86.8, 79.99, 40.8, 57.8, 33.7, 26.99, 50.8, 50.8, 53.9)
# area of house / 100
ffarea=c(16.9, 6.1, 15.78, 14.5, 15.6, 17.48, 9.4, 13.96, 16.22, 17.37, 13.06, 17.63, 15.05, 15.95, 15.15, 16.06, 12.22, 19.48, 14, 14.8, 12.1, 15.3, 15.25, 14, 11.26, 23.98, 13.06, 12.09, 15.1, 14.8, 13.2, 17.54, 15.09, 12.26, 10.95, 15.46, 16.01, 22.78, 15.5, 12.9, 16.2, 15.08, 22, 12.26, 12.01, 12, 10.5, 12.27, 16.6, 11.84)
# age of house
age=c(8, 11, 17, 7, 20, 5, 14, 9, 25, 26, 1, 26, 8, 18, 0, 25, 9, 11, 38, 24, 7, 9, 3, 5, 0, 16, 0, 7, 20, 50, 3, 9, 8, 3, 18, 41, 25, 35, 23, 44, 30, 1, 20, 29, 0, 28, 37, 17, 23, 15)
# monthly maintaince fee /10
mfee=c(19.4, 17.1, 17.3, 18.7, 27, 29.7, 23.3, 22, 36.4, 31, 19.6, 32, 22.3, 23.6, 22.2, 24.4, 18.5, 20.4, 23, 16.1, 18, 16.9, 35, 19.6, 24.8, 36.9, 18.6, 18.1, 24.5, 25, 18.9, 18.2, 20.3, 18, 24.7, 31, 33.7, 57.4, 17.4, 23.2, 16, 48.8, 26.7, 19.8, 14.2, 25.9, 28, 25.2, 19.9, 21)
# number of bedrooms
beds=c(4, 1, 4, 3, 3, 4, 2, 3, 3, 3, 3, 5, 3, 3, 4, 2, 3, 3, 3, 3, 3, 3, 2, 3, 2, 3, 3, 3, 3, 3, 3, 4, 4, 3, 2, 3, 3, 2, 3, 3, 4, 3, 3, 3, 3, 2, 2, 2, 4, 2)

# prediction: ffarea=18, age=12, mfee=26, beds=3
# turns into data frame
richmondtownh=data.frame(cbind(askpr,ffarea,age,mfee,beds))

# part a 
askpr  <- richmondtownh$askpr
ffarea <- richmondtownh$ffarea
age    <- richmondtownh$age
mfee   <- richmondtownh$mfee
beds   <- richmondtownh$beds

# one with only 2 variables
# 3 ways of regression model
reg1 <- lm(askpr~ffarea+age, data=richmondtownh)
reg2 <- lm(askpr~ffarea+age+mfee, data=richmondtownh)
reg3 <- lm(askpr~ffarea+age+mfee+beds, data=richmondtownh)

# part c
new<-data.frame(ffarea=18,age=12)
prediction <- predict(reg1, new, interval = "prediction", level = 0.95)

# residual sd 
# reg1 - 7.364  reg2 - 7.156 reg3 - 7.234
# so reg2 has better result, and reg2 - df = 46
c <- confint(reg2, level = 0.95)


# part d
newd<-data.frame(ffarea=18,age=12, mfee=26, beds=3)
prediction_d <- predict(reg3, newd, interval = "prediction", level = 0.95)
t_val = qt(0.975, 46)

# calculate SE
SE = (prediction_d[upr]-prediction_d[fit])/t_val
# SE stands for the standard error of prediction, so reverse the progress of finding prediction interval





