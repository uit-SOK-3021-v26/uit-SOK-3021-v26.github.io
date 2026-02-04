

#' Lag selection criteria  
#' ARDL(p, q)  p, q = ?


rm(list=ls())
load(url("http://www.principlesofeconometrics.com/poe5/data/rdata/usmacro.rdata"))

head(usmacro)

# Example 9.8
library(tidyverse)
library(broom)
library(forecast)

usmacro %>% dplyr::select(u) %>% Arima(., order=c(0,0,0)) %>% glance()   
usmacro %>% dplyr::select(u) %>% Arima(., order=c(1,0,0)) %>% glance()

# auto.arima() = the auto.arima() function from the forecast package 
usmacro %>% dplyr::select(u) %>% auto.arima()
auto.arima(usmacro[,"u"], xreg = usmacro[,"g"]) 

## selects best model based on information criteria , aic and bic
auto.arima(usmacro[,"u"], xreg = usmacro[,"g"], ic = "aic") 
auto.arima(usmacro[,"u"], xreg = usmacro[,"g"], ic = "bic")

# 
library(dynlm)
fit <- dynlm(u~L(u,1)+L(g,0:8),data=ts(usmacro))

BIC(fit)
AIC(fit)


# loop 'BIC()' over multiple ADL models 
order <- 1:8

BICs <- sapply(order, function(x) 
  BIC(dynlm(u ~ L(u, 1:x),data = ts(usmacro,frequency = 4,start=c(1948,1)))))

BICs

# select the AR model with the smallest BIC
BICs[ which.min(BICs)]

#
BICs <- sapply(order, function(x) 
  BIC(dynlm(u~ L(u, x) + L(g, 1:x),data = ts(usmacro,frequency = 4,start=c(1948,1))))) 

BICs[ which.min(BICs)] 


################################################
# Select optimal lag length in ARDL(p,q), more advanced  
####################################

# Initialize variables
p_max <- 12  # Maximum lag for dependent variable
q_max <- 12  # Maximum lag for independent variable


# Initialize storage for model summaries
aic_values <- matrix(NA, nrow = p_max, ncol = q_max)
bic_values <- matrix(NA, nrow = p_max, ncol = q_max)


# Loop through possible lag lengths
for (p in 1:p_max) {
  for (q in 1:q_max) {
    # Define the model formula
    model_formula <- as.formula(paste(
      "u ~", 
      paste(paste0("L(u, ", 1:p, ")"), collapse = " + "), "+",
      paste(paste0("L(g, ", 0:q, ")"), collapse = " + ")
    ))
    
    # Fit the model
    model <- dynlm(model_formula, data = ts(usmacro))
    
    # Calculate AIC and BIC
    aic_values[p, q] <- AIC(model)  # Correctly using AIC
    bic_values[p, q] <- BIC(model)  # Correctly using BIC
  }
}

# The optimal lag lengths based on AIC
optimal_aic_index <- which(aic_values == min(aic_values, na.rm = TRUE), arr.ind = TRUE)
optimal_bic_index <- which(bic_values == min(bic_values, na.rm = TRUE), arr.ind = TRUE)

# optimal lag, based on AIC
cat("Optimal lag length based on AIC: p =", optimal_aic_index[1], ", q =", optimal_aic_index[2], "\n")

# optimal lag, based on BIC
cat("Optimal lag length based on BIC: p =", optimal_bic_index[1], ", q =", optimal_bic_index[2], "\n")



best_model_SC <- dynlm(u ~ L(u,1:2 )+L(g,0:4), data = ts(usmacro))
summary(best_model_SC)


# Testing for Serial Correlation

# H0: no correlation/serial correlation 
# H1: there is correlation 

require(lmtest)
bgtest(best_model_SC ,1) 
bgtest(best_model_SC , order=2) 
bgtest(best_model_SC , order=3)
bgtest(best_model_SC , order=4) 

# extract the residual 
res <- resid(best_model_SC )

# corrorogram 
ggAcf(res) +
  labs(title = "the residul from the best model")


# Testing for Serial Correlation 

# Example 9.10, page 439 
require(dynlm)
require(mosaic)

#' Create ts data
g <- usmacro %>% dplyr::select(g) %>% ts(., start = c(1948,1), frequency = 4)
u <- usmacro %>% dplyr::select(u) %>% ts(., start = c(1948,1), frequency = 4)

#using ts() function will help us to let R know  we are working with 
# time series data 

#'Estimate ARDL(2,1)
fit1 <- dynlm(u~L(u,1)+L(u,2)+L(g,1)) 
summary(fit1)

library(forecast)
residuals(fit1) %>% ggAcf() +
  labs(title = "Figure 9.7: Correlogram from ARDL(2,1) model")

#The correlations for its residuals are generally small and insignificant. 
#There are some correlation at lag 7 , 8 and 17
#however, these correlations are at long lags and barely insignificant.


#' Lagrange multiplier(Lm)/Breusch-Godfrey test for serial correlation
#' H0: No autocorrelation vs H1: there is autocorrelation/Serial correlation 
require(lmtest)
bgtest(fit1) 
bgtest(fit1, order=2) 
bgtest(fit1, order=3)
bgtest(fit1, order=4) 

#' The Durbin-Watson test(the test statistic does not rely on large samples like that of LM test)
#' , HO:no serial correlation 
dwtest(fit1)   



#' Estimate ARDL(1,1)

fit2 <- dynlm(u~L(u,1)+L(g,1))
summary(fit2)


residuals(fit2) %>% ggAcf() +
  labs(title = "Figure 9.8: Correlogram from ARDL(1,1) model")+ theme_bw()

#The first two autocorrelations are significant.
#We conclude that that the errors are serially correlated. 
#More lags are needed to improve the forecasting specifcation, 
#and the least squares standard errors. 



#Lagrange multiplier(Lm)/Breusch-Godfrey test for seral correlation
# Example 9.12 LM test
# H0: No autocorrelation
bgtest(fit2) #order one by deafualt, null is rejected
bgtest(fit2, order=2) 
bgtest(fit2, order=3) 
bgtest(fit2, order=4) 
bgtest(fit2, order=40) 

#' The Durbin-Watson test
#' , HO:no serial correlation 
dwtest(fit2)  #Ho is rejected 
#conclusion: Model 1 is preferred than model 2.



# Example 9.9 Testing for Granger causality

#H0: x doesn't granger cause y (i.e., x doesn't contribute to the forecast of y)

library(lmtest)
grangertest(u ~ g, order = 1, data = usmacro)
grangertest(u ~ g, order = 2, data = usmacro)

#' g does granger cause u



