#Time Series Analysis#################eyhb 
##### A sample time series modeling by using existineyhb ets of R##################
###email: yeldikrantthikumarr@gmail.com#############eyhb 
#Make sure you gothrough this link: "https://cran.r-eyhb views/TimeSeries.html"
#the main function "ts", which conv{erts a vector inteyhb ith this TS object use plot function
#forecast package eyhb 
##dataset name "lynx"eyhb 
plot(lynx)eyhb 
length(lynx)eyhb
install.packages("lubridate")
library(lubridate)
ymd(20152222)
ymd(20000101)
dmy(12122012)
mdy(11111100)

selftimepoint <- ymd_hm("2017-07-20 11:23",tz = "Asia/Calcutta")
selftimepoint
OlsonNames()
wday(selftimepoint,label = T, abbr = F)
wday(y)
t1 <- ymd_hm("1993-09-23 11:23", tz = "Europe/Prague")
t2 <- ymd_hm("1995-11-02 15:23", tz = "Europe/Prague")

interval1 <- interval(t1,t2); interval1

#Excercise Date time measurement


r2 <- ymd(19981111, 19830123)
r3 <- hms(c("22 9 34", "11-11-11"))
r4 <- rnorm(n = 2,mean = 5); r4<-round(r4,2)

exdata <- cbind.data.frame(date = r2, time = r3, measurementss <- r4)
exdata

exdata$date[,3]
date(exdata)

#Time component is very critical in understanding.
# TS analysis is statistical effort, that implies statistical traits or characters
#time component is specific and order.
#based on data set the TS analysis varies
#Right plot and tests are important.
#Identify TREND/ SEASONALITY / CYCLICAL / RANDOM. Observe mean and variance.
#Data should be stationarity. if not transpose should be done.
#Variance Mean Autocorrelation
library(tseries)
x  <- rnorm(2000)
kk<- adf.test(x)
plot(x)
plot(decompose(x))
plot(nottem)
plot(decompose(nottem))

adf.test(nottem)
y <- diffinv(x)
plot(y)
adf.test(y)
#install.packages(fUnitRoots)
plot(lynx)
library(lmtest)
length(lynx)
head(lynx)
head(lynx[-1])
length(lynx[-1])
length(lynx[-114])
dwtest(lynx[-114]~lynx[-1])
#computing DW test on random number to identify presence of autocorrelation or not
x=rnorm(600)
x
plot(x)
dwtest(x[-600]~x[-1])
#from the result, the p-value is greater than 0.05, which means 
#stick to null hypthesis:- That is there is no autocorrelation
# if P-value is less than 0.05 then stick to alternate hypothesis, i.e there is autocorrelation
length(AirPassengers)
plot(AirPassengers)
dwtest(AirPassengers[-144] ~ AirPassengers[-1])
#p value lessthan 0.05 and accept alt hypo
#acf in TS, is simply the correlation coefficint between diff time points or lags of that time series:- It is used to identify MA part of ARIMA
#?pacf() correlation coefficient adjusted for shorter lags:- AR; PACF values are used to identifies AR part, i.e auto regressive.

?pacf()
acf(lynx,lag.max = 20,plot = T); 
pacf(lynx,lag.max = 20)
#Lags are known as steps in time series.
?acf()
acf(rnorm(500),lag.max = 20)
?tsd

mydata <- runif(n=50, min = 10,max = 45)
plot(mydata)
length(mydata)
dwtest(mydata[-50] ~ mydata[-1])
#Prior to working with time series analysis in R, we should make sure that R recognises the dataset as time series one.
#it means, we should restrict our dataset to one special class, i.e TS class. Which is termed as Time Series class.
# A time series object is nothing but a vector with labels on it (attached).  These labels are called as time marks.

#lets take our own dataset of ts, with simulation

mydata <- runif(n = 50, min = 10,max = 49)
mydata
plot(mydata)
plot(mydata[-50]~mydata[-1])
#let use TS function, 
myts <- ts(data = mydata,start = 1956,frequency = 4)
plot(myts)
class(myts)
class(EuStockMarkets)
head(EuStockMarkets)

#Simple time series of only one column. #For multivariate we have multiple columns

# Once, we have data, we would be obvious to know what portion (fraction)of it be a trend, seasonality and Whitenoise.
#Three main components of TS.
#In additive model, either of Trend, Seasonlity, White noise can be added or multiplied.
#As general rule of thumb: a multiplicative model, should be used if we have increase in seasonality.
plot(nottem)
length(nottem)
decompose(nottem,"additive")
plot(decompose(nottem,"additive"))

plot(stl(nottem,s.window="periodic"))
#remove seasonality from dataset.
mynottem <- decompose(nottem, "additive")
nottemadjusted <- nottem - mynottem$seasonal
plot(nottemadjusted)
#White noise: exists when there is no seasonal component and when we have random in dataset plot
# visualising time series is imp part.
# Graph is good tools to understand pattern.
plot(nottem)
plot(decompose(nottem)) #plot of components.
#Say for example, if given dataset is not classified as time series, then we can do it by using
plot.ts(cumsum(rnorm(500)))
library(forecast)
seasonplot(nottem)

#Smoothing
library("TTR")
xx = c(1:7)
xx
SMA(xx, n=3)
#understanding ARIMA
# In general ARIMA model has three parameters, P, D and Q.
# It deals with stationary time series.

auto.arima(lynx)

?auto.arima


#Upper case stands for sesaonal parameters
