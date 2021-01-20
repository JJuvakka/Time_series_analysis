# SET-UP 
library("forecast");
library("ggplot2");
library("readxl");
library("tseries");


# Data download
Lappeenrantaweather <- read.csv("~/R/Lappeenrantaweatherdata.csv")
head(Lappeenrantaweather)

# Time series
Lprweather <- ts(Lappeenrantaweather[,7], start = c(2000, 12), frequency = 12)

# Identifying key factors of data with visualization, Dickey-Fuller test and ACF and PACF plots
autoplot(Lprweather)
dftest = adf.test(Lprweather)
ggAcf(Lprweather)
ggPacf(Lprweather)
# Dickey-fuller test suggests that the time series is stationary
# Autocorrelation shows seasonality



## First model, self-built

# Seasonal differencing
SDLprweather <- diff(Lprweather, lag = 12)

autoplot(SDLprweather)
ggAcf(SDLprweather)
ggPacf(SDLprweather)
# Acf and Pacf still show peaks every 12th month but in overall time series seems stationary
# Suggestion for Arima model: (1,0,1), (1,1,1)

# Building the suggested Arima model
Arimaown <- Arima(Lprweather, order = c(2, 0, 1), seasonal = list(order = c(1, 1, 1), period=12))
# Summary 
Arimaown
# AIC = 1123, BIC = 1144

# Residuals inspection
checkresiduals(Arimaown)
# Ljung-Box p-value 0.24, residual figures ok



## Second model, auto.arima

auto.arima(Lprweather)
Arimaauto <- Arima(Lprweather, order = c(1, 0, 1), seasonal = list(order = c(1, 1, 1)))
# Summary
Arimaauto
# AIC = 1121, BIC = 1139

checkresiduals(Arimaauto)
# Ljung-Box p-value 0.27, residual figures ok



## Comparison of the models

# Residuals almost identical
# Autoarima has slightly lower AIC and BIC

# Choosing Arimaauto as the model to go with (lower complexity and slightly lower AIC and BIC)



## Forecasting average temperature for the next 12 months

Forecast2021 = forecast(Arimaauto, h = 12)
autoplot(Forecast2021, ylab = 'Temperature (°C)')


