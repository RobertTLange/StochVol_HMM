rm( list=ls() )

path <- "/Users/rtl/Desktop/02_BGSE_2016_2017/03_Financial_Econometrics/code/forecast-competition/"
ts <- "forecast-competition-training.csv"
setwd(path)

#getwd()
library(tseries)
library(moments)
library(sandwich)
library(lmtest)
library(stats)

data <- as.data.frame(read.table(ts, header=TRUE, sep=","))
#head(data)
#dim(data)

######################################
#Train Test Split
######################################
N = dim(data)[1]
H = 3

train_ind <- seq(1, N-H, 1)

N  = N-H

train <- data[train_ind, ]
test <- data[-train_ind, ]

new_names_train <- c()
for (i in 1:(dim(data)[2]-1)){
    new_names_train[i] <- paste("X", i, sep = "_")
}
new_names_train <- c("y", new_names_train)
train <- setNames(train, new_names_train)

new_names_test <- c()
for (i in 1:(dim(data)[2]-1)){
    new_names_test[i] <- paste("X.out", i, sep = "_")
}
new_names_test <- c("y.out", new_names_test)
test <- setNames(test, new_names_test)

attach(train)
attach(test)
#############################################################
#Stationarity of Time Series - Seems stationary!
#############################################################
plot(y, type='l')

round(c(mean(y), sd(y), skewness(y), kurtosis(y)), 3)

PP.test(y)$p.value
kpss.test(y)$p.value
adf.test(y)$p.value

acf(y)[1:5]

jarque.test(y)$statistic
jarque.test(y)$p.value

Box.test(y, lag=6 , type="Ljung-Box" )


#############################################################
#Model Estimation
#############################################################

# Estimate ARMA Models
ar1    = arima(y,order=c(1,0,0))
ar2    = arima(y,order=c(2,0,0))
ma1    = arima(y,order=c(0,0,1))
arma11 = arima(y,order=c(1,0,1))

# GoF
ar1_aic    <- (-2*ar1$loglik+2*3)/N
ar2_aic    <- (-2*ar2$loglik+2*4)/N
ma1_aic    <- (-2*ma1$loglik+2*3)/N
arma11_aic <- (-2*arma11$loglik+2*4)/N

ar1_bic    <- (-2*ar1$loglik+log(N)*3)/N
ar2_bic    <- (-2*ar2$loglik+log(N)*4)/N
ma1_bic    <- (-2*ma1$loglik+log(N)*3)/N
arma11_bic <- (-2*arma11$loglik+log(N)*4)/N

round( rbind( c(ar1$loglik,ar2$loglik,ma1$loglik,arma11$loglik),
              c(ar1_aic,ar2_aic,ma1_aic,arma11_aic) ,
              c(ar1_bic,ar2_bic,ma1_bic,arma11_bic) ) ,  3 )

# FITTED VALUES
ar1_mu     <- y-ar1$residuals
ar2_mu     <- y-ar2$residuals
ma1_mu     <- y-ma1$residuals
arma11_mu  <- y-arma11$residuals
ar1_res    <- as.numeric(ar1$residuals)
ar2_res    <- as.numeric(ar2$residuals)
ma1_res    <- as.numeric(ma1$residuals)
arma11_res <- as.numeric(arma11$residuals)

#############################################################
#Plots - Residual Analysis - Want normality and no autocorr
#############################################################
#ACF + PACF for AR1
par( mar=c(2,2,1,1) , mfrow=c(2,1) )
acf( ar1_res , ylim=c(-0.2,1) , lwd=5 , xlim=c(0,25) , col='darkorange2' , tck=0.02)
legend('topright',c('ACF'),col=c('darkorange2'),lwd=3)
pacf( ar1_res , ylim=c(-0.2,1) , lwd=5 , xlim=c(0,25) , col='darkorange2' , tck=0.02)
legend('topright',c('PACF'),col=c('darkorange2'),lwd=3)

#ACF + PACF for AR2
par( mar=c(2,2,1,1) , mfrow=c(2,1) )
acf( ar2_res , ylim=c(-0.2,1) , lwd=5 , xlim=c(0,25) , col='darkorange2' , tck=0.02)
legend('topright',c('ACF'),col=c('darkorange2'),lwd=3)
pacf( ar2_res , ylim=c(-0.2,1) , lwd=5 , xlim=c(0,25) , col='darkorange2' , tck=0.02)
legend('topright',c('PACF'),col=c('darkorange2'),lwd=3)

#ACF + PACF for MA1
#Clear autocorrelation in residuals - not the correct model!
par( mar=c(2,2,1,1) , mfrow=c(2,1) )
acf( ma1_res , ylim=c(-0.2,1) , lwd=5 , xlim=c(0,25) , col='darkorange2' , tck=0.02)
legend('topright',c('ACF'),col=c('darkorange2'),lwd=3)
pacf( ma1_res , ylim=c(-0.2,1) , lwd=5 , xlim=c(0,25) , col='darkorange2' , tck=0.02)
legend('topright',c('PACF'),col=c('darkorange2'),lwd=3)

#ACF + PACF for ARMA11
#Seems to be performing the best!
par( mar=c(2,2,1,1) , mfrow=c(2,1) )
acf( arma11_res , ylim=c(-0.2,1) , lwd=5 , xlim=c(0,25) , col='darkorange2' , tck=0.02)
legend('topright',c('ACF'),col=c('darkorange2'),lwd=3)
pacf( arma11_res , ylim=c(-0.2,1) , lwd=5 , xlim=c(0,25) , col='darkorange2' , tck=0.02)
legend('topright',c('PACF'),col=c('darkorange2'),lwd=3)

#Residuals very very close to normal!
kernel <- density(arma11_res/sqrt(arma11$sigma2))
plot( kernel , main='ARMA11' )
polygon( kernel , col="tomato" , border='darkred')
abline(h=0,lwd=2)
lines( seq(-10,20,0.1) , dnorm( seq(-10,20,0.1) ) , col='darkblue' ,lwd=2 )

#qqPlot for deviations from normal!
qqnorm(arma11_res,col='tomato',main='ARMA11')
qqline(arma11_res,lwd=2,lty=3)

jarque.test(arma11_res)$p.value #null of normality is not rejected!
Box.test( arma11_res, lag=22 , type="Ljung-Box" )$p.value #null of no autocorrelation is not rejected!


#############################################################
#Plots - Out-of-Sample Forecast Performance
#############################################################
ar1_pred    <- predict(ar1 , n.ahead=H)
ar2_pred    <- predict(ar2 , n.ahead=H)
ma1_pred    <- predict(ma1 , n.ahead=H)
arma11_pred <- predict(arma11 , n.ahead=H)

ar1_mse    = mean((y.out - as.numeric(ar1_pred$pred))**2)
ar2_mse    = mean((y.out - as.numeric(ar2_pred$pred))**2)
ma1_mse    = mean((y.out - as.numeric(ma1_pred$pred))**2)
arma11_mse = mean((y.out - as.numeric(arma11_pred$pred))**2)

par(mar=c(2,2,2,0.5))
plot( c((N-10):(N+H)) , c(y[(N-10):N], y.out) , main=sprintf('AR(1) MSE %3.3f',ar1_mse) , ylim=c(min(y),max(y)) , ylab='',xlab='', tck = 0.02 , pch=16 , col='darkorange' )
abline( v=N , lwd=2 )
abline( h=ar1$coef['intercept'] , lwd=2 )
grid( lwd=1 , col="darkgrey" )
lines( c((N-10):N) , ar1_mu[(N-10):N] , t='l' , lwd=2 , col='blue3' )
lines( c((N+1):(N+H)) , as.numeric(ar1_pred$pred)  , t='b' , lwd=2 , col='blue3' )

par( mar=c(2,2,2,0.5) )
plot( c((N-10):(N+H)) , c(y[(N-10):N], y.out) , main=sprintf('AR(2) MSE %3.3f',ar2_mse) , ylim=c(min(y),max(y)) , ylab='',xlab='', tck = 0.02 , pch=16 , col='darkorange')
abline( v=N , lwd=2 )
abline( h=ar2$coef['intercept'] , lwd=2 )
grid( lwd=1 , col="darkgrey" )
lines( c((N-10):N) , ar2_mu[(N-10):N] , t='l' , lwd=2 , col='blue3' )
lines( c((N+1):(N+H)) , as.numeric(ar2_pred$pred)  , t='b' , lwd=2 , col='blue3' )

par( mar=c(2,2,2,0.5) )
plot( c((N-10):(N+H)) , c(y[(N-10):N], y.out) , main=sprintf('MA(1) MSE %3.3f',ma1_mse) , ylim=c(min(y),max(y)) , ylab='',xlab='', tck = 0.02 , pch=16 , col='darkorange')
abline( v=N , lwd=2 )
abline( h=ma1$coef['intercept'] , lwd=2 )
grid( lwd=1 , col="darkgrey" )
lines( c((N-10):N) , ma1_mu[(N-10):N] , t='l' , lwd=2 , col='blue3' )
lines( c((N+1):(N+H)) , as.numeric(ma1_pred$pred)  , t='b' , lwd=2 , col='blue3' )

par( mar=c(2,2,2,0.5) )
plot( c((N-10):(N+H)) , c(y[(N-10):N], y.out) , main=sprintf('ARMA(1,1) MSE %3.3f',arma11_mse) , ylim=c(min(y),max(y)) , ylab='',xlab='', tck = 0.02 , pch=16  , col='darkorange')
abline( v=N , lwd=2 )
abline( h=ma1$coef['intercept'] , lwd=2 )
grid( lwd=1 , col="darkgrey" )
lines( c((N-10):N) , arma11_mu[(N-10):N] , t='l' , lwd=2 , col='blue3' )
lines( c((N+1):(N+H)) , as.numeric(arma11_pred$pred)  , t='b' , lwd=2 , col='blue3' )

round(c( ar1_mse , ar2_mse , ma1_mse ,  arma11_mse )*100 , 3 )
