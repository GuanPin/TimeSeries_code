roadkill <- read.table("C:/Users/user/Desktop/roadkill2.csv", header=TRUE, sep=',')
vehicle <- read.table("C:/Users/user/Desktop/vehicle2.csv", header=TRUE, sep=',')

library(TSA)
library(tseries)

##roadkill
a <- roadkill$roadkill
b <- roadkill$t
Month <- c('J', 'F', 'M', 'A', 'M', 'J', 'J', 'A', 'S', 'O', 'N', 'D')

plot(roadkill, type='l', xlab='year')
points(x=b, y=a, pch=Month)

BoxCox.ar(a)
a1 <- log(a)

adf.test(a1)
kpss.test(a1)


par(mfrow=c(2, 1))
acf(a1, lag=25, main="Original Data")
pacf(a1, lag=25)
eacf(a1)   

plot(diff(a1), type='l', xlab="time", ylab="1st difference of roadkill")
acf(diff(a1), lag=25, main="1st difference")
pacf(diff(a1), lag=25)
eacf(diff(a1))

plot(diff(diff(a1), lag=12), type='l', xlab="time", ylab="seasonal difference")
acf(diff(diff(a1), lag=12), ci.type="ma", main="Seasonal Difference")

model1 <- arima(a1, order=c(1, 0, 0))

model2 <- arima(a1, order=c(4, 1, 0))
model2.1 <- arima(a1, order=c(4, 1, 0), fixed=c(0, NA, 0, NA), transform.pars = FALSE)

model3 <- arima(a1, order=c(0, 1, 2))
model3.1 <- arima(a1, order=c(0, 1, 2), fixed=c(0, NA), transform.pars = FALSE)

model4 <- arima(a1, order=c(1, 0, 0), seasonal=list(order=c(1, 0, 0), period=12))

model5 <- arima(a1, order=c(0, 1, 1), seasonal=list(order=c(0, 1, 0), period=12))


par(mfrow=c(2, 1))
res <- residuals(model4)
plot(res, ylab="residuals");abline(h=0)
acf(res, lag=25, main="residuals")
pacf(res, lag=25)
win.graph()
par(mfrow=c(1, 2))
hist(res, main="residuals", breaks=20)
t.test(res)
qqnorm(res);qqline(res)
shapiro.test(res)

McLeod.Li.test(y=res)
Box.test(res, lag=12, type="Ljung")
Box.test(res, lag=24, type="Ljung")



##vehicle
plot(vehicle, type='l', xlab="year")
m <- vehicle$t
n <- vehicle$vehicle
logn =log(n)
plot(logn, type="l", xlab="time", ylab="log vehicle")

adf.test(logn)
kpss.test(logn)

par(mfrow=c(2, 1))
plot(diff(logn), type="l", xlab="time", ylab="1st difference of log vehicle")
acf(diff(logn), main="1st difference")
pacf(diff(logn))
eacf(diff(logn))

mod1 <- arima(logn, order=c(0, 1, 2))

mod2 <- arima(logn, order=c(1, 1, 0))


res2 <- residuals(mod1)
plot(res2, ylab="residuals");abline(h=0)
acf(res2, main="residuals")
pacf(res2)
Box.test(res2, lag=4, type="Ljung")

win.graph()
par(mfrow=c(1, 2))
hist(res2, breaks=10, main="residuals")
t.test(res2)
qqnorm(res2);qqline(res2)
shapiro.test(res2)
McLeod.Li.test(y=res2)

#correlation
plot(n, a, xlab="vehicle", ylab="roadkill")
cor(n, a)

residual <- ts.intersect(res2, res)
plot(residual, yax.flip=T)
cor(res2, res)

ccf(as.numeric(residual[,1]), as.numeric(residual[,2]), main="residuals of vehicle(x) & roadkill(y)", ylab="CCF")
cor(res2[1:58], res[2:59])
plot(res2[1:58], res[2:59])
