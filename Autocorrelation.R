

# Autocorrelation

rm(list=ls())
library(broom)
library(tidyverse)

#' Example 9.1

#browseURL("http://www.principlesofeconometrics.com/poe5/data/def/usmacro.def")
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/usmacro.rdata"))

str(usmacro)

head(usmacro)

usmacro %>% 
  ggplot(aes(x=dateid01, y=u)) + 
  geom_line() + ylab("Unemployment Rate") + 
  xlab("Year") +
  labs(title = "Figure 9.2a: U.S. Quarterly unemployment rate 1948:Q1 to 2016:Q1")

ggplot(usmacro, aes(x=dateid01, y=g)) + geom_line() + ylab("Growth Rate") + xlab("Year") +
  labs(title = "Figure 9.2b: U.S. Quarterly GDP growth rate 1948:Q1 to 2016:Q1")


#' Example 9.2
#' The autocorrelation function (acf) is a set of correlation coefficients
#' between a time series and various lags of itself.

#' The default base r function:
acf(usmacro$u) 
#' Note that the autocorrelation for lag 0 is always 1,so there is no need to plot it.

#' We can print the autocorrelation coefficients.
acf(usmacro$u)$acf


#' The forecast package gives a nicer ggplot, with the ar(0)=1 removed
#' library(forecast)
usmacro %>% select(u) %>% ggAcf() +
  labs(title = "Figure 9.4: Correlogram for U.S. Quarterly unemployment rate")

usmacro %>% select(g) %>% ggAcf() +
  labs(title = "Figure 9.5: Correlogram for growth rate in U.S. GDP")

