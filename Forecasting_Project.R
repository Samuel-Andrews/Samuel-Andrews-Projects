#This file performs all of the forecasts as shown in the powerpoint "Forecasting_price_Tin_vehicles".
# The entire process going through this section by section is contained "Forecasting_Methods_Write_up"
# This project was originally done as the final project for FIN 335 Forecasting Methods



library(fpp2)
library(forecast)
library(dplyr)
library(ggplot2)
require(fpp2)
require(GGally)
require(seasonal)
require(tseries)

Fish  <- read.csv("TOTALNSA.csv")

#pre stuff

Fishf <- ts(Fish$TOTALNSA, start = c(2009,3), frequency = 12)

vehicle <- ts(Fish$TOTALNSA, start = c(2009,3), frequency = 12)

Fishf

length(Fishf)

Fish.train = head(Fishf, 97)
Fish.test = tail(Fishf, 24)

autoplot(Fish.test)
autoplot(Fish.train)

#1 B

autoplot(Fishf) + ggtitle("Total Vehicle Sales") + xlab("Years") + ylab("U.S. Dollars per thouasnds of units")

gglagplot(Fishf) + ggtitle("Lagplot of Total Vehicle Sales") + xlab("Years") + ylab("U.S. Dollars per thounsands of units")

ggtsdisplay(Fishf) 

ggseasonplot(Fishf) + ggtitle("Seasnoal plot of Total Vehicle Sales") + xlab("Years") + ylab("U.S. Dollars per thousands of units")

Acf(vehicle)

ggsubseriesplot(Fishf) + ggtitle("Subseries plot of Total Vehicle Sales") + xlab("Years") + ylab("U.S. Dollars per thousands of units")

#2

MFishf <- meanf(Fish.train, h = 24)

summary(MFishf)
accuracy(MFishf, Fish.test)



NFishf <- naive(Fish.train, h = 24)

summary(NFishf)
accuracy(NFishf, Fish.test)


SNFishf <- snaive(Fish.train, h = 24)

summary(SNFishf)
accuracy(SNFishf, Fish.test)
autoplot(SNFishf) + ggtitle("Seasonl Naive forecasts of Total Vehicle Sales") + xlab("Years") + ylab("Thousands of units") 


DFishf <- rwf(Fish.train, h = 24)

summary(DFishf)
accuracy(DFishf, Fish.test)

autoplot(Fishf)  + ggtitle("Basic forecasts of Total Vehicle Sales") + xlab("Years") + ylab("U.S. Dollars per thousands of units") +
  autolayer(meanf(Fishf,h=24)$mean,series="Mean Method") +
  autolayer(naive(Fishf,h=24)$mean,series="Naive Method") +
  autolayer(snaive(Fishf,h=24)$mean,series="Snaive Method") +
  autolayer(rwf(Fishf,h=24,drift=T)$mean,series="Naive with Drift") +
  guides(color=guide_legend(title="Forecast Method"))




#3 B OLS regression

Fishmod1 = tslm(Fish.train~trend+season)
Fishmod2 = tslm(Fish.train~trend + I(trend^2) + season)
Fishmod3 = tslm(Fish.train~trend + I(trend^2) + I(trend^3) + season)
Fishmod4 = tslm(Fish.train~trend + I(trend^2) + I(trend^3) + season)
Fishmod5 = tslm(Fish.train~trend + I(trend^2) + I(trend^3) + I(trend^4) + season)
Fishmod6 =  tslm(Fish.train~trend + fourier(Fish.train,K=2) + season)
Fishmod7 =  tslm(Fish.train~trend + fourier(Fish.train,K=2) + I(trend^2) + I(trend^3) + season)
Fishmod8 = tslm(Fish.train~trend + fourier(Fish.train,K=2))
Fishmod9 = tslm(Fish.train~trend + fourier(Fish.train,K=2) + I(trend^2))
Fishmod10 = tslm(Fish.train~poly(trend, 3) + fourier(Fish.train,K=2) + I(trend^2))
Fishmod11 = tslm(Fish.train~poly(trend, 3) + fourier(Fish.train,K=2))
Fishmod12 = tslm(Fish.train~poly(trend, 3) +  fourier(Fish.train,K=3))
Fishmod13 = tslm(Fish.train~trend +  fourier(Fish.train,K=3))


summary(Fishmod1) 
summary(Fishmod2)
summary(Fishmod3)
summary(Fishmod4)
summary(Fishmod5)
summary(Fishmod6)
summary(Fishmod7)
summary(Fishmod8)
summary(Fishmod9)
summary(Fishmod10)
summary(Fishmod11)
summary(Fishmod12)
summary(Fishmod13)

CV(Fishmod1)
CV(Fishmod2)
CV(Fishmod3)
CV(Fishmod4)
CV(Fishmod5)
CV(Fishmod6)
CV(Fishmod7)
CV(Fishmod8)
CV(Fishmod9)
CV(Fishmod10)
CV(Fishmod11)
CV(Fishmod12)
CV(Fishmod13)

checkresiduals(Fishmod12)
checkresiduals(Fishmod13)
checkresiduals(Fishmod10)
checkresiduals(Fishmod9)

checkresiduals(Fishmod13)

accuracy(mod1.fc,Fish.test)

fc1 = forecast(Fishmod1,h=24, robust = T)
mod2.fc = forecast(Fishmod13,h=24, robust = T)
mod3.fc = forecast(Fishmod10,h=24)
mod4.fc = forecast(Fishmod9,h=24)


mod1.fc <- forecast(Fishmod13,
                          data.frame(fourier(Fish.train,3,24)))


mod2.fc <- forecast(Fishmod13,
                    data.frame(fourier(Fish.train,3,24)))

autoplot(mod2.fc) + ggtitle("OLS regression forecast of Total Vehicle Sales") + xlab("Years") + ylab("Thousands of vehicles sales")
mod2.fc

#subset

Fish.train2 = window(Fishf, start = 2014, end = 2017, frequency = 12)
autoplot(Fish.train2)



Fishmod1s = tslm(Fish.train2~trend+season)
Fishmod2s = tslm(Fish.train2~trend + I(trend^2) + season)
Fishmod3s = tslm(Fish.train2~trend + I(trend^2) + I(trend^3) + season)
Fishmod4s = tslm(Fish.train2~trend + I(trend^2) + I(trend^3) + season)
Fishmod5s = tslm(Fish.train2~trend + I(trend^2) + I(trend^3) + I(trend^4) + season)
Fishmod6s =  tslm(Fish.train2~trend + fourier(Fish.train2,K=3) + season)
Fishmod7s =  tslm(Fish.train2~trend + fourier(Fish.train2,K=2) + I(trend^2) + I(trend^3) + season)
Fishmod8s = tslm(Fish.train2~trend + fourier(Fish.train2,K=2))
Fishmod9s = tslm(Fish.train2~trend + fourier(Fish.train2,K=2) + I(trend^2))
Fishmod10s = tslm(Fish.train2~poly(trend, 3) + fourier(Fish.train2,K=2) + I(trend^2))
Fishmod11s = tslm(Fish.train2~poly(trend, 3) + fourier(Fish.train2,K=2))
Fishmod12s = tslm(Fish.train2~poly(trend, 3) +  fourier(Fish.train2,K=3))
Fishmod13s = tslm(Fish.train2~trend +  fourier(Fish.train2,K=3))
Fishmod14s = tslm(Fish.train2~trend +  fourier(Fish.train2,K=5))
Fishmod15s = tslm(Fish.train2~trend +  fourier(Fish.train2,K=4))

summary(Fishmod1s) 
summary(Fishmod2s)
summary(Fishmod3s)
summary(Fishmod4s)
summary(Fishmod5s)
summary(Fishmod6s)
summary(Fishmod7s)
summary(Fishmod8s)
summary(Fishmod9s)
summary(Fishmod10s)
summary(Fishmod11s)
summary(Fishmod12s)
summary(Fishmod13s)
summary(Fishmod14s)
summary(Fishmod15s)

CV(Fishmod1s)
CV(Fishmod2s)
CV(Fishmod3s)
CV(Fishmod4s)
CV(Fishmod5s)
CV(Fishmod6s)
CV(Fishmod7s)
CV(Fishmod8s)
CV(Fishmod9s)
CV(Fishmod10s)
CV(Fishmod11s)
CV(Fishmod12s)
CV(Fishmod13s)
CV(Fishmod14s)
CV(Fishmod15s)

checkresiduals(Fishmod15s)

mod1s.fc <- forecast(Fishmod15s,
                    data.frame(fourier(Fish.train,4,24)))


accuracy(mod1s.fc, Fish.test)



mod2.fcs <- forecast(Fishmod15s,
                    data.frame(fourier(Fish.train2,4,24)))

autoplot(mod2.fcs) + ggtitle("OLS regression forecast (Subsetted training data) of Global Price of Fish") + xlab("Years") + ylab("U.S. Dollars per Metric Ton")

accuracy(mod2.fcs,Fish.test)

mod2.fc

autoplot(mod1s.fc) + ggtitle("OLS regression forecast (Subsetted training data) of Global Price of Fish") + xlab("Years") + ylab("U.S. Dollars per Metric Ton")


mod2.fc <- forecast(Fishmod13,
                    data.frame(fourier(Fish.train,3,24)))

autoplot(mod2.fc) + ggtitle("OLS regression forecast of Global Price of Fish") + xlab("Years") + ylab("U.S. Dollars per Metric Ton")






#4 STL


Fish.stl = stl(Fishf, t.window = 13, s.window = 13, robust = T)
autoplot(Fish.stl) + ggtitle("stl decomp of Global Price of Fish") + xlab("Years") + ylab("U.S. Dollars per Metric Ton")


Fish.stl2 = stl(Fishf, t.window = 13, s.window = 13)
autoplot(Fish.stl2) + ggtitle("stl decomp (no robust) of Global Price of Fish") + xlab("Years") + ylab("U.S. Dollars per Metric Ton")


#with robust
stl1.fc = stlf(Fish.train,t.window=13,s.window=13,method="naive",h=24, robust = T)
stl2.fc = stlf(Fish.train,t.window=13,s.window=13,method="rwdrift",h=24, robust = T)
stl3.fc = stlf(Fish.train,t.window=13,s.window=13,method="ets",h=24, robust = T)
stl4.fc = stlf(Fish.train,t.window=13,s.window=13,method="arima",h=24, robust = T)

accuracy(stl1.fc,Fish.test)
accuracy(stl2.fc,Fish.test)
accuracy(stl3.fc,Fish.test)
accuracy(stl4.fc,Fish.test)

checkresiduals(stl4.fc)

stl4.fc.fin = stlf(Fish.train,t.window=13,s.window=13,method="ets",h=24, robust = T)

autoplot(stl4.fc.fin) + ggtitle("stl decomp ETS Robust forecast of Global Price of Fish") + xlab("Years") + ylab("U.S. Dollars per Metric Ton")

#subsetted



#with robust
stl1.fc2 = stlf(Fish.train2,t.window=13,s.window=13,method="naive",h=24, robust = T)
stl2.fc2 = stlf(Fish.train2,t.window=13,s.window=13,method="rwdrift",h=24, robust = T)
stl3.fc2 = stlf(Fish.train2,t.window=13,s.window=13,method="ets",h=24, robust = T)
stl4.fc2 = stlf(Fish.train2,t.window=13,s.window=13,method="arima",h=24, robust = T)

accuracy(stl1.fc2,Fish.test)
accuracy(stl2.fc2,Fish.test)
accuracy(stl3.fc2,Fish.test)
accuracy(stl4.fc2,Fish.test)

checkresiduals(stl1.fc2)

stl4.fc.fin2 = stlf(Fish.train,t.window=13,s.window=13,method="ets",h=24, robust = T)

autoplot(stl4.fc.fin2) + ggtitle("STL decomp ETS Robust forecast of Total Vehicle Sales") + xlab("Years") + ylab("Thousands of units")

#6 h

hw.Fishf.addt = hw(Fish.train, seasonal = "additive", h = 24)
summary(hw.Fishf.addt)

accuracy(hw.Fishf.addt, Fish.test)


hw.Fishf.addt2 = hw(Fish.train, seasonal = "additive", h = 36)
summary(hw.Fishf.addt2)

accuracy(hw.Fishf.addt2, Fish.test)


hw.Fishf.addt3 = hw(Fish.train, seasonal = "additive", h = 12)
summary(hw.Fishf.addt3)

accuracy(hw.Fishf.addt3, Fish.test)



#6
hw.Fishf.add = hw(Fish.train, seasonal = "additive", h = 24)
summary(hw.Fishf.add)


hw.Fishf.mult = hw(Fish.train, seasonal = "multiplicative", h = 24)
summary(hw.Fishf.mult)

#damped versions

hw.Fishf.add.dp = hw(Fish.train, seasonal = "additive", damped = T, h = 24)
summary(hw.Fishf.add.dp)


hw.Fishf.mult.dp = hw(Fish.train, seasonal = "multiplicative", damped = T, h = 24)
summary(hw.Fishf.mult.dp)

accuracy(hw.Fishf.add, Fish.test)
accuracy(hw.Fishf.mult, Fish.test)
accuracy(hw.Fishf.add.dp, Fish.test)
accuracy(hw.Fishf.mult.dp, Fish.test)

checkresiduals(hw.Fishf.mult.dp)


#subset

hw.Fishf.add2 = hw(Fish.train2, seasonal = "additive", h = 24)
summary(hw.Fishf.add2)


hw.Fishf.mult2 = hw(Fish.train2, seasonal = "multiplicative", h = 24)
summary(hw.Fishf.mult2)

#damped versions

hw.Fishf.add.dp2 = hw(Fish.train2, seasonal = "additive", damped = T, h = 24)
summary(hw.Fishf.add.dp2)


hw.Fishf.mult.dp2 = hw(Fish.train2, seasonal = "multiplicative", damped = T, h = 24)
summary(hw.Fishf.mult.dp2)

accuracy(hw.Fishf.add2, Fish.test)
accuracy(hw.Fishf.mult2, Fish.test)
accuracy(hw.Fishf.add.dp2, Fish.test)
accuracy(hw.Fishf.mult.dp2, Fish.test)

checkresiduals(hw.Fishf.mult2)

hw.Fishf.mult2.fin =hw(Fish.train2, seasonal = "mult", h = 24)

autoplot(hw.Fishf.mult2.fin) + ggtitle("Holt Winters Multiplicative (Subset Training) forecast of Total vehicle Sales") + xlab("Years") + ylab("Thousands of units")


#ETS

#determine h

Fishf.ets1t = ets(Fish.train)
summary(Fishf.ets1t)

Fishf.ets1.fct = forecast(Fishf.ets1, h=24)
accuracy(Fishf.ets1.fct,Fish.test)


Fishf.ets1t2 = ets(Fish.train)
summary(Fishf.ets1t2)

Fishf.ets1.fct2 = forecast(Fishf.ets1, h=12)
accuracy(Fishf.ets1.fct2,Fish.test)


Fishf.ets1t3 = ets(Fish.train)
summary(Fishf.ets1t3)

Fishf.ets1.fct3 = forecast(Fishf.ets1, h=36)
accuracy(Fishf.ets1.fct3,Fish.test)



#normal
Fishf.ets1 = ets(Fish.train)
summary(Fishf.ets1)

Fishf.ets2 = ets(Fish.train, damped = T)
summary(Fishf.ets2)

Fishf.ets3 = ets(Fish.train2)
summary(Fishf.ets3)

Fishf.ets4 = ets(Fish.train2, damped = T)
summary(Fishf.ets4)




Fishf.ets1.fc = forecast(Fishf.ets1, h=24)
accuracy(Fishf.ets1.fc,Fish.test)

Fishf.ets2.fc = forecast(Fishf.ets2, h=24)
accuracy(Fishf.ets2.fc,Fish.test)

Fishf.ets3.fc = forecast(Fishf.ets3, h=24)
accuracy(Fishf.ets3.fc,Fish.test)

Fishf.ets4.fc = forecast(Fishf.ets4, h=24)
accuracy(Fishf.ets4.fc,Fish.test)

checkresiduals(Fishf.ets3.fc)

autoplot(Fishf.ets3.fc) + ggtitle("ETS (Subset Training) forecast of Total Vehicle Sales") + xlab("Years") + ylab("Thousands of Units")

accuracy(Fishf.ets3.fc, Fish.test)


#7 Arima

#untruncated
Fishfseasdiff = diff(Fish.train, 12)
ggtsdisplay(Fishfseasdiff)

ndiffs(Fishfseasdiff)

Fishfdiffs = diff(Fishfseasdiff)

ggtsdisplay(Fishfdiffs)


fmod1 = Arima(Fish.train,order=c(1,1,1),seasonal=c(0,1,0))
fmod2 = Arima(Fish.train,order=c(1,1,0),seasonal=c(0,1,0))
fmod3 = Arima(Fish.train,order=c(0,1,1),seasonal=c(0,1,0))
fmod4 = Arima(Fish.train,order=c(0,1,0),seasonal=c(0,1,0))
fmod5 = Arima(Fish.train,order=c(1,1,0),seasonal=c(0,1,0))
fmod6 = Arima(Fish.train,order=c(2,1,1),seasonal=c(0,1,0))
fmod7 = Arima(Fish.train,order=c(1,1,2),seasonal=c(0,1,0))


fmod1
fmod2
fmod3
fmod4
fmod5
fmod6
fmod7


fmod1.fc = forecast(fmod1,h=24)
accuracy(fmod1.fc,Fish.test)

fmod3.fc = forecast(fmod3,h=24)
accuracy(fmod3.fc,Fish.test)


AAfmod1 = auto.arima(Fish.train,approximation=F,stepwise=F)
AAfmod1

AAfmod1.fc = forecast(AAfmod1,h=24)
accuracy(AAfmod1.fc,Fish.test)


#truncated

tFishfseasdiff = diff(Fish.train2, 12)
ggtsdisplay(tFishfseasdiff)


ndiffs(tFishfseasdiff)

tFishfdiffs = diff(tFishfseasdiff)

ggtsdisplay(tFishfdiffs)


tfmod1 = Arima(Fish.train,order=c(0,1,3),seasonal=c(0,1,0))
tfmod2 = Arima(Fish.train,order=c(1,1,3),seasonal=c(0,1,0))
tfmod3 = Arima(Fish.train,order=c(0,1,2),seasonal=c(0,1,0))
tfmod4 = Arima(Fish.train,order=c(1,1,2),seasonal=c(0,1,0))
tfmod5 = Arima(Fish.train,order=c(0,1,4),seasonal=c(0,1,0))
tfmod6 = Arima(Fish.train,order=c(1,1,4),seasonal=c(0,1,0))
tfmod7 = Arima(Fish.train,order=c(1,1,2),seasonal=c(0,1,0))


tfmod1
tfmod2
tfmod3
tfmod4
tfmod5
tfmod6
tfmod7

tfmod3.fc = forecast(tfmod3,h=24)
accuracy(tfmod3.fc,Fish.test)

autoplot(tfmod3.fc) + ggtitle("ARIMA(0,1,2)(0,1,0) forecast of Total Vehicle Sales") + xlab("Years") + ylab("Thousands of Units")


checkresiduals(AAfmod1.fc)

autoplot()




fmod.final.fc = Arima(Fishf,order=c(0,1,1),seasonal=c(2,1,2))

finalfmod.fc = forecast(fmod.final.fc,h=24)

autoplot(finalfmod.fc) + ggtitle("ARIMA(0,1,1)(2,1,2)[12] forecast of Global Price of Fish") + xlab("Years") + ylab("U.S. Dollars per Metric Ton")


