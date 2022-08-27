library(quantmod)
library(xts)
library(PerformanceAnalytics)
library(rugarch)
library(fGarch)
library(tseries)
library(forecast)
library(FinTS)
library(aTSA)
library(lmtest)

#######LOAD MICROSOFT DATA#####

MSFT <- getSymbols("MSFT", from = "2011-01-01",
                   to= "2020-12-31", auto.assign = FALSE)

#load Out-of-sample data for forecast for First month of 2021
MSFT.ft <- getSymbols("MSFT", from = "2021-01-01",
                   to= "2021-01-30", auto.assign = FALSE)

#Save data to drive
write.csv(MSFT.ft, 'C:\\Users\\Porl-Timi\\Desktop\\my_project//MSFT_test.csv')
write.csv(MSFT, 'C:\\Users\\Porl-Timi\\Desktop\\my_project//MSFT.csv'

#Assessing data
View(MSFT)
chartSeries(MSFT$MSFT.Close)

#Due to the non--linear shape of the data, we make use of log transform
log.MSFT <- log(MSFT$MSFT.Close)
chartSeries(log.MSFT)

?CalculateReturns
#RETURNS
Return<- CalculateReturns(log.MSFT)
View(Return)
Return <- Return[-1]
hist(Return)
chart.Histogram(Return, methods = c('add.density', 'add.normal'),
                colorset = c('blue', 'black','red'),)

charts.TimeSeries(Return, colors('blue'))
chartSeries(Return)

####Distribution test###
summary(Return)

skewness(Return)
kurtosis(Return)
sd(Return)
rbind(kurtosis(Return), skewness(Return))
jarque.bera.test(Return)

####Unit root test of return###
adf.test(Return)
kpss.test(Return)
###Test of ARCH effect####
ArchTest(Return, lags=1, demean = T)

par(mfrow = c(1,2))
acf(Return) 
pacf(Return)

#ARIMA MODEL

fit_arima <- auto.arima(Return,max.D = 2, ic= "aic", trace= TRUE,stationary = T)
autoplot(fit_arima)
print(summary(fit_arima))

? auto.arima
#The best parsimonious model is the MA model of order 1
arma_model <- arima(Return, order = c(0,0,1))
print(summary(arma_model))

tsdiag(arma_model)
coeftest(arma_model)
#Confirms that residual of ARIMA is a  stationary in the mean
checkresiduals(arma_model)

####Test for ARCH effect of ARIMA MODEL
arch.test(arma_model)

#The arch test show that the series is not stationary in the variance.
#Therefore the GARCH Model is suggested 

#GARCH Model
####Arch(1)
Arch1 <- garchFit(~garch(1,0), data = Return, trace = F)
summary(Arch1)
?garchFit
####Arch(2)
Arch2 <- garchFit(~garch(2,0), data = Return, trace = F)
summary(Arch2)

###GARCH with Normal Dist
Garch_spec1 <- ugarchspec(variance.model = list(model= "sGARCH", garchOrder=c(1,1)),
                         mean.model = list(armaOrder = c(0,0)))#, distribution.model = "std")
Garch_model1 <- ugarchfit(spec= Garch_spec1, data = Return)

Garch_model1

###GARCH with Student-t Dist
Garch_spec2 <- ugarchspec(variance.model = list(model= "sGARCH", garchOrder=c(1,1)),
                           mean.model = list(armaOrder = c(0,0)), distribution.model = "std")
Garch_model2 <- ugarchfit(spec= Garch_spec2, data = Return)

Garch_model2

###EGARCH with Normal Dist
egarch_spec1 <- ugarchspec(variance.model = list(model= "eGARCH", garchOrder=c(1,1)),
                           mean.model = list(armaOrder = c(0,0)))
egarch_model1 <- ugarchfit(spec= egarch_spec1, data = Return)

egarch_model1

###EGARCH with Student-t
egarch_spec2 <- ugarchspec(variance.model = list(model= "eGARCH", garchOrder=c(1,1)),
                         mean.model = list(armaOrder = c(0,0)), distribution.model = "std")
egarch_model2 <- ugarchfit(spec= egarch_spec2, data = Return)

egarch_model2

#Conditional Volatility
###Egarch
plot(egarch_model2)
plot.ts(sigma(egarch_model2), ylab="Conditional volatility", col="red",)
#Forecasting (First month of 2021)
egarch.for = ugarchforecast(egarch_model2, n.ahead = 19)
egarch.for
