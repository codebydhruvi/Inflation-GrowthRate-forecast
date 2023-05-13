# Inflation-GrowthRate-forecast
Forecasting India's GDP growth for the next two fiscal years (2022-23 and 2023-24)
Understanding the relationship between GDP growth rate and inflation rate (CPI based) through vector auto regression (VAR).

rm(list = ls())
library(gdata)
library(readxl)
india<-read_excel("F:\\Users\\DHRUVI SHAH\\OneDrive\\Documents\\Timeseries Ecotrics\\GDP_Dataset.xlsx")
View(india)
par(mfcol = c(2,1), oma = c(0,0,1,0) + 0.2, 
    mar = c(0,1,0,0) + 1, mgp = c(0, 0.2, 0))
plot(india$YEAR,india$`GDP (%)`,type="l",las=1,
     xaxs="i",yaxs="i",xlab="",ylab="",
     tck=.02,main=colnames(india)[3],
     col="steelblue4",ylim=c(-10,30))
plot(india$YEAR,india$`Inflation rate(%)`,type="l",las=1,
     xaxs="i",yaxs="i",xlab="",ylab="",
     tck=.02,main=colnames(india)[2],
     col="red",ylim=c(-10,30))
acf(india$`GDP (%)`)
pacf(india$`GDP (%)`)
acf(india$`Inflation rate(%)`)
pacf(india$`Inflation rate(%)`)

library(tseries)
adf.test(india$`GDP (%)`)
adf.test(india$`Inflation rate(%)`)
library(forecast)
auto.arima(india$`GDP (%)`)
auto.arima(india$`Inflation rate(%)`)
PP.test(india$`GDP (%)`)

y1<-ts(india$`GDP (%)`,start = 1961,frequency = 1)
autoplot(y1)
y2<-ts(india$`Inflation rate(%)`,start = 1961,frequency = 1)
autoplot(y2)
checkresiduals(y1)
finalmodel<-arima(india$`GDP (%)`,order = c(0,1,1))
qqnorm(residuals(finalmodel))
qqline(residuals(finalmodel))
myforcast<-forecast(finalmodel,2)
myforcast
plot(myforcast)


library(vars)
y=cbind(y1,y2)
plot.ts(y, main = "", xlab = "")
fitvar2= VAR(y, p=1, type="both")
summary(fitvar2)
acf(residuals(fitvar2))

lagn<-VARselect(y)
lagn$selection
estim<-VAR(y,p=1,type = "none")
summary(estim)


library(stargazer)
stargazer(estim[["varresult"]],type = 'text')

roots(estim,modulus = TRUE)
#since the values obtained are less than one, the model is stable.

grangery1<-causality(estim,cause = "y2")
grangery1$Granger
#y2 does cause y1
grangery2<-causality(estim,cause = "y1")
grangery2$Granger
#y1 does not cause y2

irf1<-irf(estim, impulse = "y2",response = "y1",n.ahead = 3,boot = TRUE,run=200,ci=0.95)
plot(irf1,ylab="y1",main="y1 response to y2 shock")
irf2<-irf(estim, impulse = "y1",response = "y2",n.ahead = 3,boot = TRUE,run=200,ci=0.95)
plot(irf2,ylab="y2",main="y2 response to y1 shock")
