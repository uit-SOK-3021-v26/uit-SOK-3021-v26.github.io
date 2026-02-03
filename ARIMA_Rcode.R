

# Forecasting using ARIMA model 

rm(list = ls())
library(tidyverse)
#browseURL("http://www.principlesofeconometrics.com/poe5/data/def/usmacro.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/usmacro.rdata"))

head(usmacro)

# Forecasting 
library(forecast)

# Forecasting using AR(2) model using arima()

#?arima()

u <- ts(usmacro$u, frequency = 4, start = c(1948,1))
#u

fit_ar <- arima(u, order = c(2,0,0))  # AR(2)
summary(fit_ar)


#Use the model to forecast the unemployment rate in the next 10 quarters 
# forcast next 10 time points 
ar_forecast  <- forecast(fit_ar,h=10)
ar_forecast


#plot the data and forecast
plot(ar_forecast)

# Alternatively 
arima(usmacro$u, order = c(2,0,0)) %>% forecast(h=10) %>% autoplot + theme_bw()


# Forcasting using MA(2)
arima(usmacro$u, order = c(0,0,2))  %>% forecast(h=10) %>% autoplot + theme_bw()


# Forecasting using ARMA(2,0,2)
arima(usmacro$u, order = c(2,0,2))  %>% forecast(h=10) %>% autoplot + theme_bw()


# notice the difference between the following two lines of codes
arima(usmacro$u, order = c(2,0,0)) %>% forecast(h=10)   # nothing comes, why?

# defining u as ts() object 
u <- ts(usmacro$u, frequency = 4, start = c(1948,1))
arima(u, order = c(2,0,0)) %>% forecast(h=10) 

#arima(u, order = c(2,0,0)) %>% forecast(h=10) %>% autoplot





# Example 9.7 Forecasting unemployment with an ARDL(2,1) model

usmacro.lag <- cbind( u = usmacro[,"u"],
                      g = usmacro[,"g"],
                      gLag1 = dplyr::lag(usmacro[,"g"],1))

head(usmacro.lag)

#
Arima(usmacro.lag[,"u"], order=c(2,0,0), xreg = usmacro.lag[,"gLag1"])

library(broom)
Arima(usmacro.lag[,"u"], order=c(2,0,0), xreg = usmacro.lag[,"gLag1"]) %>% tidy()

#  remove tidy() at the end and  save
fit2 <- Arima(usmacro.lag[,"u"], order=c(2,0,0), xreg = usmacro.lag[,"gLag1"]) 


# Forecasting 
fc2 <- forecast(fit2, h=3,xreg=cbind(xreg = c(usmacro[,"g"][273],0.869,1.069))) 

fc2

# plot the forecast value and interval 
autoplot(fc2) + ylab("Unemployment") +
  ggtitle("Forecast unemployment with future GDP growth")+
  theme_bw()






