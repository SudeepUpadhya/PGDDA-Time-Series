#
#####Retail-Giant Sales Forecasting Case Study Group Submission by:
# 1. Sudeep Upadhya
# 2. Rishab Nigam
# 3. Amit Mankikar
# 4. Vikash Prasad

# Date: 29-10-2017

##Install and Load the required packages

#install.packages("dplyr")
#install.packages("plotrix")
library(forecast)
library(tseries)
require(graphics)
library(dplyr)
library(plotrix)
library(ggplot2)
library(zoo)

# Set the working directory 
setwd("C:\\Users\\sudha\\Downloads")

# Loading Given Dataset

Global_Superstore_data<-read.csv("Global Superstore.csv",stringsAsFactors = F)

##Data Understanding
str(Global_Superstore_data)

## We will need only a few columns for Time Series analysis - the ones requested from Business Requirements
## Those are: Order Date, Market, Segment, Sales, Quantity, Profit
## Let us keep only these
Global_Superstore_data<-Global_Superstore_data[, c(3, 8, 13, 19, 20, 22) ]

tmp <- lapply(Global_Superstore_data, function(x) sum(is.na(x)))
tmp
# There are no na/ missing values in the data required to do time series analysis

#Convert the date format in system read
Global_Superstore_data$Order.Date<-as.yearmon(as.Date(Global_Superstore_data$Order.Date,format="%d-%m-%Y"))

ggplot(Global_Superstore_data, 
       aes(x = Global_Superstore_data$Order.Date)) + geom_bar()
#
# Data is from 2011 to 2014. Clearly every holiday season - June, November and December of every year, the no. of sales are high
# we can see "Seasonality". Also there is an upward "Trend"
#

#Total 24 attributes are there in which below two are most important for Data Preparation Part
#
#Market attribute has 7 factor "US" "APAC" "APAC" "EU" "canada" "Africa" "EMEA"
#Cusomer Segment attribute has 3 factors "Consumer" "Corporate" "Home Office"
##

#
#
# EDA 
#
tmp <- Global_Superstore_data %>% 
  group_by(Market, Segment) %>% 
  summarise(., std_profit = sd(Profit), avg_profit = mean(Profit), sales_count = n(), total_profit = sum(Profit), CV = sd(Profit)/mean(Profit) )


ggplot(tmp, 
       aes(x = tmp$total_profit, y = tmp$CV, color = tmp$Market, shape = tmp$Segment
       )) + geom_point(size = 5) 
#
# From the plot it is evident that APAC.Consumer & EU.Consumer are best suited as the 2 most profitable Market, Segment 
# combination. Let us confirm if we get the same 2 using the CV method
#

#
# Split the data frane by Market and Segment
#
tmp2 <- split(Global_Superstore_data, list(Global_Superstore_data$Market, Global_Superstore_data$Segment))

length(tmp2)
# 21. we have 7 * 3 = 21 combinations of data frames.

#
# Create a blank data frame with 2 columns and set its column names
#
list_of_time_series <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("TimeSeriesName","CV", "TotalProfit"))

#
# Iterate over all Market, Segment combinations, group by month-year and calculate its CV
#
for (name in names(tmp2)) {
  
  df <- tmp2[[name]] # Fetch the current dataframe into df variable
  print(paste(name, ": ", nrow(df))) # For debugging, comment once done
  
  # Group by Order.Date, aggregate on Sales, Quantity & Profit
  tmp3 <- df %>% 
    group_by(Order.Date) %>%
    summarise(total_sales = sum(Sales), total_quantity = sum(Quantity), total_profit = sum(Profit))
  
  # Compute the CV 
  CV <- sd(tmp3$total_profit)/ mean(tmp3$total_profit)*100
  TP <- sum(tmp3$total_profit)
  
  # Store the computed CV against the Market, Segment combination
  list_of_time_series <- rbind(list_of_time_series, data.frame(name, CV, TP) )
  
}


# Now we have a list of time series (size = 21) with CV and max profit per month basis
# Let us get the 2 best Market, Segment combination with least CV

best <- head(arrange(list_of_time_series, CV, desc(TP)), 2)
best

#     name        CV       TP
# 1   EU.Consumer 62.43052 188687.7
# 2   APAC.Consumer 63.21323 222817.6

ggplot(list_of_time_series, 
       aes(y = list_of_time_series$CV, x = list_of_time_series$TP, color = list_of_time_series$name)) + 
  geom_point(size = 5)


#
# In https://learn.upgrad.com/v/course/29/session/3438/segment/14946
# Data preparation Step is completed, df1 and df2 variables now contain data frames for 
# EU.Consumer and APAC.Consumer respectively. Now proceed with next step i.e. Model building
#

#
# Function to retrieve the i-th best Market-Segment combination
# to use for Time Series analysis
#
createTimeSeries<- function(idx) {
  
  df <- as.data.frame(tmp2[best$name[idx]])  
  colnames(df) <- gsub(".*\\..*\\.", "", colnames(df))
  df <- df %>% 
    group_by(Date) %>%
    summarise(total_sales = sum(Sales), total_quantity = sum(Quantity), total_profit = sum(Profit))
  
  # sort by date
  df <- arrange(df, Date)
  
  return(df)
}

EU_df <- createTimeSeries(1)
APAC_df <- createTimeSeries(2)

# the date 6 months before
max_date_EU <- max(EU_df$Date) - 0.5
max_date_APAC <- max(APAC_df$Date)- 0.5

# create 4 time series for complete data - 
total_timeser_EU_sales <-ts(EU_df$total_sales)
total_timeser_APAC_sales <- ts(APAC_df$total_sales)
total_timeser_EU_quant <- ts(EU_df$total_quantity)
total_timeser_APAC_quant <- ts(APAC_df$total_quantity)

indata_EU <- EU_df[EU_df$Date <= max_date_EU, ]
indata_APAC <- APAC_df[APAC_df$Date <=max_date_APAC,]

drawHoltWintersSmoothen <- function(title, df, dependentVarIdx, alphas) {
  
  timeser <- ts(df[,dependentVarIdx])
  plot(timeser, main = title, xlab = "Time", ylab = colnames(indata_APAC)[dependentVarIdx], ylim=c(0, max(timeser)*2))
  cols <- c("red", "blue", "green", "black")
  labels <- c(paste("alpha =", alphas), "Original")
  
  for (i in seq(1,length(alphas))) {
    smoothedseries <- HoltWinters(timeser, alpha=alphas[i], beta=FALSE, gamma=FALSE)
    lines(fitted(smoothedseries)[,1], col=cols[i], lwd=2)
  }
  legend("topleft", labels, col=cols, lwd=1)
}
#
# TBD: Verify if the fine tuned alpha values for all 4 time series are OK
#
drawHoltWintersSmoothen("EU-Consumer Sales TS [HoltWinters Smoothing]", indata_EU, 2, c(0.01, 0.25, 0.8))
# After experimentation, alpha of 0.25 looks fair for EU Sales ts
smoothedseries_EU_sales <- HoltWinters(ts(indata_EU[,2]), alpha=0.25, beta=FALSE, gamma=FALSE)

drawHoltWintersSmoothen("EU-Consumer Quantity TS [HoltWinters Smoothing]", indata_EU, 3, c(0.01, 0.25, 0.9))
# After experimentation, alpha of 0.25 looks fair for EU Quantity ts
smoothedseries_EU_quant <- HoltWinters(ts(indata_EU[,3]), alpha=0.25, beta=FALSE, gamma=FALSE)

drawHoltWintersSmoothen("APAC-Consumer Sales TS [HoltWinters Smoothing]", indata_APAC, 2, c(0.05, 0.25, 0.6))
# After experimentation, alpha of 0.25 looks fair for APAC Sales ts
smoothedseries_APAC_sales <- HoltWinters(ts(indata_APAC[,2]), alpha=0.25, beta=FALSE, gamma=FALSE)

drawHoltWintersSmoothen("APAC-Consumer Quantity TS [HoltWinters Smoothing]", indata_APAC, 3, c(0.1, 0.25, 0.8))
# After experimentation, alpha of 0.25 looks fair for APAC Quantity ts
smoothedseries_APAC_quant <- HoltWinters(ts(indata_APAC[,3]), alpha=0.25, beta=FALSE, gamma=FALSE)

timevals_in_EU_sales <- seq(1:length(smoothedseries_EU_sales$x))
timevals_in_EU_quant <- seq(1:length(smoothedseries_EU_quant$x))
timevals_in_APAC_sales <- seq(1:length(smoothedseries_APAC_sales$x))
timevals_in_APAC_quant <- seq(1:length(smoothedseries_APAC_quant$x))

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe
smootheddf_EU_sales <- as.data.frame(cbind(timevals_in_EU_sales, as.vector(smoothedseries_EU_sales$x)))
colnames(smootheddf_EU_sales) <- c('Month', 'Sales')

smootheddf_EU_quant <- as.data.frame(cbind(timevals_in_EU_quant, as.vector(smoothedseries_EU_quant$x)))
colnames(smootheddf_EU_quant) <- c('Month', 'Quantity')

smootheddf_APAC_sales <- as.data.frame(cbind(timevals_in_APAC_sales, as.vector(smoothedseries_APAC_sales$x)))
colnames(smootheddf_APAC_sales) <- c('Month', 'Sales')

smootheddf_APAC_quant <- as.data.frame(cbind(timevals_in_APAC_quant, as.vector(smoothedseries_APAC_quant$x)))
colnames(smootheddf_APAC_quant) <- c('Month', 'Quantity')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

################################ EU - Consumer Sales Time Series Analysis ####################################
lmfit_EU_sales <- lm(Sales ~ sin(0.6*Month) * poly(Month,2) + cos(0.6*Month) * poly(Month,2) + Month,
                     data=smootheddf_EU_sales)
global_pred_EU_sales <- predict(lmfit_EU_sales, Month=timevals_in_EU_sales)
summary(global_pred_EU_sales)
drawHoltWintersSmoothen("EU-Consumer Sales TS", indata_EU, 2, c(0.25))
lines(timevals_in_EU_sales, global_pred_EU_sales, col='blue', lwd=1)
#Now, let's look at the locally predictable series
#We will model it as an ARMA series
local_pred_EU_Sales <- ts(indata_EU[,2])-global_pred_EU_sales
plot(local_pred_EU_Sales, col='red', type = "l")
acf(local_pred_EU_Sales)
acf(local_pred_EU_Sales, type="partial")
armafit_EU_sales <- auto.arima(local_pred_EU_Sales)
tsdiag(armafit_EU_sales)
#We'll check if the residual series is white noise
resi_EU_sales <- local_pred_EU_Sales-fitted(armafit_EU_sales)
adf.test(resi_EU_sales,alternative = "stationary")
kpss.test(resi_EU_sales)
#for dickey-fuller test, null hypothesis is that the series is not stationary
#from the above dickey-fuller test we see that p value is 0.36 which is less than 0.05 therefore we cannot accept the null hypothesis
#hence we can conclude that residual series is stationary
#for Kpss test, null hypothesis is that the series is stationary
#from the above kpss test we find that p-value is 0.1 which is greater than 0.05 therefore we cannot reject the null hypothesis
#hence we can conclude that the series is stationary

#Now, let's evaluate the models using MAPE
#First, let's make a prediction for the last 6 months
outdata_EU_sales <- EU_df[EU_df$Date > max_date_EU,"total_sales"]
timevals_out_EU_sales <- seq(1:nrow(outdata_EU_sales)) + length(timevals_in_EU_sales)
global_pred_out_EU_sales <- predict(lmfit_EU_sales,data.frame(Month =timevals_out_EU_sales))
fcast_EU_sales <- global_pred_out_EU_sales
#Now, let's compare our prediction with the actual values, using MAPE
MAPE_class_dec_EU_sales <- accuracy(fcast_EU_sales,t(outdata_EU_sales))[5]
MAPE_class_dec_EU_sales
#Let's also plot the predictions along with original values, to
#get a visual feel of the fit
class_dec_pred_EU_sales <- c(ts(global_pred_EU_sales),ts(global_pred_out_EU_sales))
plot(total_timeser_EU_sales, col = "black")
lines(class_dec_pred_EU_sales, col = "red")


################################ EU - Consumer Quantity Time Series Analysis ####################################
lmfit_EU_quant <- lm(Quantity ~ sin(0.6*Month) * poly(Month,3) + cos(0.6*Month) * poly(Month,3) + Month, 
                     data=smootheddf_EU_quant)
global_pred_EU_quant <- predict(lmfit_EU_quant, Month=timevals_in_EU_quant)
summary(global_pred_EU_quant)
drawHoltWintersSmoothen("EU-Consumer Quantity TS", indata_EU, 3, c(0.25))
lines(timevals_in_EU_quant, global_pred_EU_quant, col='blue', lwd=1)
#Now, let's look at the locally predictable series
#We will model it as an ARMA series
local_pred_EU_quant <- ts(indata_EU[,3])-global_pred_EU_quant
plot(local_pred_EU_quant, col='blue', type = "l")
acf(local_pred_EU_quant)
acf(local_pred_EU_quant, type="partial")
armafit_EU_quant <- auto.arima(local_pred_EU_quant)
tsdiag(armafit_EU_quant)
armafit_EU_quant
#We'll check if the residual series is white noise
resi_EU_quant <- local_pred_EU_quant-fitted(armafit_EU_quant)
adf.test(resi_EU_quant,alternative = "stationary")
kpss.test(resi_EU_quant)
#for dickey-fuller test, null hypothesis is that the series is not stationary
#from the above dickey-fuller test we see that p value is 0.09 which is > than 0.05 therefore we cannot reject the null hypothesis
#hence we can conclude that residual series is not stationary
#for Kpss test, null hypothesis is that the series is stationary
#from the above kpss test we find that p-value is 0.1 which is greater than 0.05 therefore we cannot reject the null hypothesis
#hence we can conclude that the series is stationary

#Now, let's evaluate the models using MAPE
#First, let's make a prediction for the last 6 months
outdata_EU_quant <- EU_df[EU_df$Date > max_date_EU,"total_quantity"]
timevals_out_EU_quant <- seq(1:nrow(outdata_EU_quant)) + length(timevals_in_EU_quant)
global_pred_out_EU_quant <- predict(lmfit_EU_quant,data.frame(Month =timevals_out_EU_quant))
fcast_EU_quant <- global_pred_out_EU_quant
#Now, let's compare our prediction with the actual values, using MAPE
MAPE_class_dec_EU_quant <- accuracy(fcast_EU_quant,t(outdata_EU_quant))[5]
MAPE_class_dec_EU_quant
#Let's also plot the predictions along with original values, to
#get a visual feel of the fit
class_dec_pred_EU_quant <- c(ts(global_pred_EU_quant),ts(global_pred_out_EU_quant))
plot(total_timeser_EU_quant, col = "black")
lines(class_dec_pred_EU_quant, col = "red")

################################ APAC - Consumer Sales Time Series Analysis ####################################
lmfit_APAC_sales <- lm(Sales ~ sin(0.6*Month) * poly(Month,2) + cos(0.6*Month) * poly(Month,2) + Month, 
                       data=smootheddf_APAC_sales)
global_pred_APAC_sales <- predict(lmfit_APAC_sales, Month=timevals_in_APAC_sales)
summary(global_pred_APAC_sales)
drawHoltWintersSmoothen("APAC-Consumer Sales TS", indata_APAC, 2, c(0.25))
lines(timevals_in_APAC_sales, global_pred_APAC_sales, col='blue', lwd=1)
#Now, let's look at the locally predictable series
#We will model it as an ARMA series
local_pred_APAC_Sales <- ts(indata_APAC[,2])-global_pred_APAC_sales
plot(local_pred_APAC_Sales, col='red', type = "l")
acf(local_pred_APAC_Sales)
acf(local_pred_APAC_Sales, type="partial")
armafit_APAC_sales <- auto.arima(local_pred_APAC_Sales)
tsdiag(armafit_APAC_sales)
armafit_APAC_sales
#We'll check if the residual series is white noise
resi_APAC_sales <- local_pred_APAC_Sales-fitted(armafit_APAC_sales)
adf.test(resi_APAC_sales,alternative = "stationary")
kpss.test(resi_APAC_sales)
#for dickey-fuller test, null hypothesis is that the series is not stationary
#from the above dickey-fuller test we see that p value is 0.01 which is less than 0.05 therefore we cannot accept the null hypothesis
#hence we can conclude that residual series is stationary
#for Kpss test, null hypothesis is that the series is stationary
#from the above kpss test we find that p-value is 0.1 which is greater than 0.05 therefore we cannot reject the null hypothesis
#hence we can conclude that the series is stationary

#Now, let's evaluate the models using MAPE
#First, let's make a prediction for the last 6 months
outdata_APAC_sales <- APAC_df[APAC_df$Date > max_date_APAC,"total_sales"]
timevals_out_APAC_sales <- seq(1:nrow(outdata_APAC_sales)) + length(timevals_in_APAC_sales)
global_pred_out_APAC_sales <- predict(lmfit_APAC_sales,data.frame(Month =timevals_out_APAC_sales))
fcast_APAC_sales <- global_pred_out_APAC_sales
#Now, let's compare our prediction with the actual values, using MAPE
MAPE_class_dec_APAC_sales <- accuracy(fcast_APAC_sales,t(outdata_APAC_sales))[5]
MAPE_class_dec_APAC_sales
#Let's also plot the predictions along with original values, to
#get a visual feel of the fit
class_dec_pred_APAC_sales <- c(ts(global_pred_APAC_sales),ts(global_pred_out_APAC_sales))
plot(total_timeser_APAC_sales, col = "black")
lines(class_dec_pred_APAC_sales, col = "red")


################################ APAC - Consumer Quantity Time Series Analysis ####################################
lmfit_APAC_quant <- lm(Quantity ~ sin(0.6*Month) * poly(Month,2) + cos(0.6*Month) * poly(Month,2)
                       + Month, data=smootheddf_APAC_quant)
global_pred_APAC_quant <- predict(lmfit_APAC_quant, Month=timevals_in_APAC_quant)
summary(global_pred_APAC_quant)
drawHoltWintersSmoothen("APAC-Consumer Quantity TS", indata_APAC, 3, c(0.25))
lines(timevals_in_APAC_quant, global_pred_APAC_quant, col='blue', lwd=1)
#Now, let's look at the locally predictable series
#We will model it as an ARMA series
local_pred_APAC_quant <- ts(indata_APAC[,3])-global_pred_APAC_quant
plot(local_pred_APAC_quant, col='blue', type = "l")
acf(local_pred_APAC_quant)
acf(local_pred_APAC_quant, type="partial")
armafit_APAC_quant <- auto.arima(local_pred_APAC_quant)
tsdiag(armafit_APAC_quant)
armafit_APAC_quant
#We'll check if the residual series is white noise
resi_APAC_quant <- local_pred_APAC_quant-fitted(armafit_APAC_quant)
adf.test(resi_APAC_quant,alternative = "stationary")
kpss.test(resi_APAC_quant)
#for dickey-fuller test, null hypothesis is that the series is not stationary
#from the above dickey-fuller test we see that p value is 0.01 which is less than 0.05 therefore we cannot accept the null hypothesis
#hence we can conclude that residual series is stationary
#for Kpss test, null hypothesis is that the series is stationary
#from the above kpss test we find that p-value is 0.1 which is greater than 0.05 therefore we cannot reject the null hypothesis
#hence we can conclude that the series is stationary

#Now, let's evaluate the models using MAPE
#First, let's make a prediction for the last 6 months
outdata_APAC_quant <- APAC_df[APAC_df$Date > max_date_EU,"total_quantity"]
timevals_out_APAC_quant <- seq(1:nrow(outdata_APAC_quant)) + length(timevals_in_APAC_quant)
global_pred_out_APAC_quant <- predict(lmfit_APAC_quant,data.frame(Month =timevals_out_APAC_quant))
fcast_APAC_quant <- global_pred_out_APAC_quant
#Now, let's compare our prediction with the actual values, using MAPE
MAPE_class_dec_APAC_quant <- accuracy(fcast_APAC_quant,t(outdata_APAC_quant))[5]
MAPE_class_dec_APAC_quant
#Let's also plot the predictions along with original values, to
#get a visual feel of the fit
class_dec_pred_APAC_quant <- c(ts(global_pred_APAC_quant),ts(global_pred_out_APAC_quant))
plot(total_timeser_APAC_quant, col = "black")
lines(class_dec_pred_APAC_quant, col = "red")

##################################################################################################3
#So, that was classical decomposition, now let's do an ARIMA fit

################################ EU - Consumer Sales Time Series Analysis (AUto-ARIMA)####################################
timeser_EU_sales <- ts(indata_EU$total_sales)
autoarima_EU_sales <- auto.arima(timeser_EU_sales)
autoarima_EU_sales
tsdiag(autoarima_EU_sales)
plot(autoarima_EU_sales$x, col="black")
lines(fitted(autoarima_EU_sales), col="red")
#Again, let's check if the residual series is white noise
resi_auto_arima_EU_sales <- timeser_EU_sales - fitted(autoarima_EU_sales)
adf.test(resi_auto_arima_EU_sales,alternative = "stationary")
kpss.test(resi_auto_arima_EU_sales)
#for dickey-fuller test, null hypothesis is that the series is not stationary
#from the above dickey-fuller test we see that p value is 0.01 which is less than 0.05 therefore we cannot accept the null hypothesis
#hence we can conclude that residual series is stationary
#for Kpss test, null hypothesis is that the series is stationary
#from the above kpss test we find that p-value is 0.1 which is greater than 0.05 therefore we cannot reject the null hypothesis
#hence we can conclude that the series is stationary

#Also, let's evaluate the model using MAPE
fcast_auto_arima_EU_sales <- predict(autoarima_EU_sales, n.ahead = 6)
MAPE_auto_arima_EU_sales  <- accuracy(fcast_auto_arima_EU_sales$pred,t(outdata_EU_sales))[5]
MAPE_auto_arima_EU_sales
#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit
auto_arima_pred_EU_sales <- c(fitted(autoarima_EU_sales),ts(fcast_auto_arima_EU_sales$pred))
plot(total_timeser_EU_sales, col = "black")
lines(auto_arima_pred_EU_sales, col = "red")

################################ EU - Consumer Quantity Time Series Analysis (AUto-ARIMA)####################################
timeser_EU_quant <- ts(indata_EU$total_quantity)
autoarima_EU_quant <- auto.arima(timeser_EU_quant)
autoarima_EU_quant
tsdiag(autoarima_EU_quant)
plot(autoarima_EU_quant$x, col="black")
lines(fitted(autoarima_EU_quant), col="red")
#Again, let's check if the residual series is white noise
resi_auto_arima_EU_quant <- timeser_EU_quant - fitted(autoarima_EU_quant)
adf.test(resi_auto_arima_EU_quant,alternative = "stationary")
kpss.test(resi_auto_arima_EU_quant)
#for dickey-fuller test, null hypothesis is that the series is not stationary
#from the above dickey-fuller test we see that p value is 0.04 which is less than 0.05 therefore we cannot accept the null hypothesis
#hence we can conclude that residual series is stationary
#for Kpss test, null hypothesis is that the series is stationary
#from the above kpss test we find that p-value is 0.1 which is greater than 0.05 therefore we cannot reject the null hypothesis
#hence we can conclude that the series is stationary

#Also, let's evaluate the model using MAPE
fcast_auto_arima_EU_quant <- predict(autoarima_EU_quant, n.ahead = 6)
MAPE_auto_arima_EU_quant  <- accuracy(fcast_auto_arima_EU_quant$pred,t(outdata_EU_quant))[5]
MAPE_auto_arima_EU_quant
#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit
auto_arima_pred_EU_quant <- c(fitted(autoarima_EU_quant),ts(fcast_auto_arima_EU_quant$pred))
plot(total_timeser_EU_quant, col = "black")
lines(auto_arima_pred_EU_quant, col = "red")

################################ APAC - Consumer Sales Time Series Analysis (AUto-ARIMA)####################################
timeser_APAC_sales <- ts(indata_APAC$total_sales)
autoarima_APAC_sales <- auto.arima(timeser_APAC_sales, allowdrift = TRUE)
autoarima_APAC_sales
tsdiag(autoarima_APAC_sales)
plot(autoarima_APAC_sales$x, col="black")
lines(fitted(autoarima_APAC_sales), col="red")
#Again, let's check if the residual series is white noise
resi_auto_arima_APAC_sales <- timeser_APAC_sales - fitted(autoarima_APAC_sales)
adf.test(resi_auto_arima_APAC_sales,alternative = "stationary")
kpss.test(resi_auto_arima_APAC_sales)
#for dickey-fuller test, null hypothesis is that the series is not stationary
#from the above dickey-fuller test we see that p value is 0.01 which is less than 0.05 therefore we cannot accept the null hypothesis
#hence we can conclude that residual series is stationary
#for Kpss test, null hypothesis is that the series is stationary
#from the above kpss test we find that p-value is 0.1 which is greater than 0.05 therefore we cannot reject the null hypothesis
#hence we can conclude that the series is stationary

#Also, let's evaluate the model using MAPE
fcast_auto_arima_APAC_sales <- predict(autoarima_APAC_sales, n.ahead = 6)
MAPE_auto_arima_APAC_sales  <- accuracy(fcast_auto_arima_APAC_sales$pred,t(outdata_APAC_sales))[5]
MAPE_auto_arima_APAC_sales
#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit
auto_arima_pred_APAC_sales <- c(fitted(autoarima_APAC_sales),ts(fcast_auto_arima_APAC_sales$pred))
plot(total_timeser_APAC_sales, col = "black")
lines(auto_arima_pred_APAC_sales, col = "red")

################################ APAC - Consumer Quantity Time Series Analysis (AUto-ARIMA)####################################
timeser_APAC_quant <- ts(indata_APAC$total_quantity)
autoarima_APAC_quant <- auto.arima(timeser_APAC_quant)
autoarima_APAC_quant
tsdiag(autoarima_APAC_quant)
plot(autoarima_APAC_quant$x, col="black")
lines(fitted(autoarima_APAC_quant), col="red")
#Again, let's check if the residual series is white noise
resi_auto_arima_APAC_quant <- timeser_APAC_quant - fitted(autoarima_APAC_quant)
adf.test(resi_auto_arima_APAC_quant,alternative = "stationary")
kpss.test(resi_auto_arima_APAC_quant)
#for dickey-fuller test, null hypothesis is that the series is not stationary
#from the above dickey-fuller test we see that p value is 0.01 which is less than 0.05 therefore we cannot accept the null hypothesis
#hence we can conclude that residual series is stationary
#for Kpss test, null hypothesis is that the series is stationary
#from the above kpss test we find that p-value is 0.1 which is greater than 0.05 therefore we cannot reject the null hypothesis
#hence we can conclude that the series is stationary

#Also, let's evaluate the model using MAPE
fcast_auto_arima_APAC_quant <- predict(autoarima_APAC_quant, n.ahead = 6)
MAPE_auto_arima_APAC_quant  <- accuracy(fcast_auto_arima_APAC_quant$pred,t(outdata_APAC_quant))[5]
MAPE_auto_arima_APAC_quant
#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit
auto_arima_pred_APAC_quant <- c(fitted(autoarima_APAC_quant),ts(fcast_auto_arima_APAC_quant$pred))
plot(total_timeser_APAC_quant, col = "black")
lines(auto_arima_pred_APAC_quant, col = "red")

##=============================================================================================================##
## Plot the comparitive MAPE values to decide which model to use for the 4 Market/ Segment combination predictions
##=============================================================================================================##

MarketSegment <- c("EU Sales", "EU Quantity", "APAC Sales", "APAC Quantity")
MAPE <- c(MAPE_auto_arima_EU_sales, MAPE_auto_arima_EU_quant, MAPE_auto_arima_APAC_sales, MAPE_auto_arima_APAC_quant,
          MAPE_class_dec_EU_sales, MAPE_class_dec_EU_quant, MAPE_class_dec_APAC_sales, MAPE_class_dec_APAC_quant)
type1 <- c( rep("AA: ",4), rep("CD: ", 4))
plt <- data.frame(MarketSegment, MAPE, type1) 
plt$type1 <- as.factor(plt$type1)

ggplot(plt, aes(x = MarketSegment, y = MAPE, group = type1)) + geom_col(position = "dodge", fill = "gray", colour = "black") +
  geom_text(aes(label=paste(type1, round(MAPE,2))),position=position_dodge(width=0.9), vjust=-0.25)

##=======================================================================================================================##
##                                                FINAL PREDICTION 
##=======================================================================================================================##
# Let us make use of Auto Arima model for Prediction for the following Market-Segment combinations: EU-Sales, EU-Quantity
# APAC-Sales. Whereas the model built using classical decomposition can be used for predicting the timeseries for 
# APAC-Quantity
##========================================================================================================================##


### EU Sales ####
timeser_EU_sales <- ts(EU_df$total_sales)
autoarima_EU_sales <- auto.arima(timeser_EU_sales)
fcast_auto_arima_EU_sales <- predict(autoarima_EU_sales, n.ahead = 6)
auto_arima_pred_EU_sales <- c(fitted(autoarima_EU_sales),ts(fcast_auto_arima_EU_sales$pred))
plot(auto_arima_pred_EU_sales, col = "red", type = "l")
lines(auto_arima_pred_EU_sales[1:48], col = "black")

sum(auto_arima_pred_EU_sales[49:54])
mean(auto_arima_pred_EU_sales[49:54])

### EU Quantity ####
timeser_EU_quant <- ts(EU_df$total_quantity)
autoarima_EU_quant <- auto.arima(timeser_EU_quant)
fcast_auto_arima_EU_quant <- predict(autoarima_EU_quant, n.ahead = 6)
auto_arima_pred_EU_quant <- c(fitted(autoarima_EU_quant),ts(fcast_auto_arima_EU_quant$pred))
plot(auto_arima_pred_EU_quant, col = "red", type = "l")
lines(auto_arima_pred_EU_quant[1:48], col = "black")
lines(head(plt, 42), col = "blue", lwd = 2)

sum(auto_arima_pred_EU_quant[49:54])
mean(auto_arima_pred_EU_quant[49:54])


### APAC Sales ####
timeser_APAC_sales <- ts(APAC_df$total_sales)
autoarima_APAC_sales <- auto.arima(timeser_APAC_sales)
fcast_auto_arima_APAC_sales <- predict(autoarima_APAC_sales, n.ahead = 6)
auto_arima_pred_APAC_sales <- c(fitted(autoarima_APAC_sales),ts(fcast_auto_arima_APAC_sales$pred))
plot(auto_arima_pred_APAC_sales, col = "red", type = "l")
lines(auto_arima_pred_APAC_sales[1:48], col = "black")

sum(auto_arima_pred_APAC_sales[49:54])
mean(auto_arima_pred_APAC_sales[49:54])


### APAC Quantity ####
timeser_APAC_quant <- ts(APAC_df$total_quantity)
autoarima_APAC_quant <- auto.arima(timeser_APAC_quant)
fcast_auto_arima_APAC_quant <- predict(autoarima_APAC_quant, n.ahead = 6)
auto_arima_pred_APAC_quant <- c(fitted(autoarima_APAC_quant),ts(fcast_auto_arima_APAC_quant$pred))
plot(auto_arima_pred_APAC_quant, col = "red", type = "l")
lines(auto_arima_pred_APAC_quant[1:48], col = "black")

sum(auto_arima_pred_APAC_quant[49:54])
mean(auto_arima_pred_APAC_quant[49:54])

