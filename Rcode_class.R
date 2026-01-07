
# Decomposition of time series data


# The quarterly beer production in Australia

#install.packages("fpp2")
library(fpp2)
ausbeer

beer2 <- window(ausbeer, start=1992)
beer2

# plot the data 
ggplot2::autoplot(beer2) + xlab("Year") + ylab("Megalitres")+theme_bw()

# Decomposition of the quarterly beer production in Australia: 
decompose(beer2)

ggplot2::autoplot(decompose(beer2))+theme_bw()



# generating lags of a time series variable 

library(dplyr)
library(tidyr)
library(tibble)

set.seed(123)

years <- 2020:2025   

data <- tibble(
  year = years,
  y    = rnorm(length(years), mean = 10, sd = 2)
)

data

data %>% 
  mutate(y1=lag(y,1),  # the first lag/ values in the previous period 
         y2=lag(y,2),  # the second lag 
         y_2 = lag(y1)) 

# remove NA's 
data_clean <- na.omit(data)



# Update the data 
years <- 2000:2025   
data <- tibble(
  year = years,
  y    = rnorm(length(years), mean = 10, sd = 2)
)

data

data <- data %>% 
  mutate(y1=lag(y,1),  # the first lag/ values in the previous period 
         y2=lag(y,2),  # the second lag 
         y_2 = lag(y1)) #%>% View()


data_clean <- na.omit(data)

# regression 
fit <- lm(y~ y1+y2, data = data_clean)
summary(fit)
