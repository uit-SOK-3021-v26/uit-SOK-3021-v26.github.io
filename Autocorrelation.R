

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




# autocorrelation function (acf)

