
#################### RETAIL CASE STUDY ####################


#################### SYPNOSIS ####################

## The data is related to a retailer Glen which contains the sales of 
## food & beverages .

## We are going to use a TIME SERIES MODEL.
## We are going to see the trends & patterns in the data
## & prepare a S(Seasonal)-ARIMA model to forecast 
## the sales of the food & beverages


## BUSINESS PROBLEM : : Forecasting the sales
## BUSINESS SOLUTION: : Building a SARIMA Model

#################### IMPORTANT PACKAGES ####################

install.packages("astsa")

library(dplyr)
library(ggplot2)
library(astsa)
library(gridExtra)
library(forecast)

#################### EXPLORING DATASET ####################

getwd()
setwd("E:/R Programming/Data Analysis using R CASE STUDIES/R scripts")

Before_Converting <- read.csv("E:/R Programming/Data Analysis using R CASE STUDIES/Dataset/Chapter_03_final_sales_dataset.csv",
                              header = T, sep = ",")

View(Before_Converting)

## Converting it to TIME SERIES
Retail_Data <- ts(Before_Converting[,2], start = c(1992,1), 
                  frequency = 12)
Retail_Data

## Now, as you can the data is converted into form where 
## Year is on Obs side & Months is on Fields side
## We have converted into MONTHLY FORM USING FREQ = 12

start(Retail_Data)
## Returns Start values in form ( YYYY : MM)

end(Retail_Data)
## Returns End values in form ( YYYY : MM)

glimpse(Retail_Data)
class(Retail_Data)

summary(Retail_Data)
## It shows the MIN SALES & MAXIMUM SALES AMOUNT

## FOR MISSING VALUES ::

sum(is.na(Retail_Data))
## It shows we have cleaned data in hand


#################### VISUALIZING DATA ####################

plot(Retail_Data, xlab = "Years", ylab = "Sales" ,
     col = "red" , main = "Time Series Data from 1992 - 2017")

plot(Retail_Data, xlab = "Years", ylab = "Sales", type = "b" ,
     col = "darkgreen" , main = "Time Series Data from 1992 - 2017")

## The graphs shows that there is a UPWARD-TREND in data
## Meaning that the data is not stationary..

## We know when the data is not stationary we must convert it to one]
## A stationary series has a ZERO MEAN & CONSTANT VARIANCE..


#################### DEFINING ACF & PACF PLOTS ####################

## ACF Plots shows the corrl. betweeen obs of current time
## spots & obs of previous time spots..

## PACF Plot shows the corrl. between obs. of two time spots
## & takes into account the corrl. bewteen the time spot 
## before the previous time spot as well.. 



#### TO DEPICT ACF & PACF IN THE DATA ::
acf2(Retail_Data, max.lag = 24)

## The series graphs shows that even with a lag of 24 the ACF & PACF
## withers after a long time representing the fact of series being 
## NON-STATIONARY..



## SEASONALITY PATTERNS ::

?diff
## DIFF Returns suitabley lagged differences..
Differenced <- diff(Retail_Data, 12)

## Plotting
plot.ts(Differenced, main = "Seasonality Differenced")

## As its monthly sales so we have differenced it with 12..

## Calculting ACF
acf2(Differenced, max.lag = 24)


##### NOW TO SEE TREND PATTERNS

Trends_Differenced <- diff(Differenced, 1)

## Plotting
plot(Trends_Differenced, main = "Trends & Seasonality Differenced")
## IT SHOWS TRENDS ALONGWITH SEASOANLLY DIFFERENCED RETAIL SALES..

## Calculting ACF
acf2(Trends_Differenced, max.lag = 36)


### WE NOW HAVE TO IDENTIFY APPROPRIATE "SARIMA" MODEL
### USING THESE ACF & PACF PLOT ABOVE AFTER DIFFERENCING


## Looking at the spikes of ACF, 
## Significant spikes at lag 1 in ACF show
## non-seasonal MA (1) component &
## Significant spikes at lag 12 and 24 show
## seasonal MA (2) component. 

## Similarly, looking at the spikes of PACF,
## Significant spikes at lag 2 in PACF show
## non-seasonal AR (2) component &
## Significant spikes at lag 12 and 24 show 
## seasonal AR (2) component. 

## We chose an ARIMA(2,1, 1)
## wherein, AR = 2, lag = 1, MA = 1 (NON - SEASONAL)

## & ARIMA(2, 1, 2)12 model, 
## wherein, AR = 2, lag = 1, MA = 2 (SEASONAL)

## ALONGWITH first and seasonal difference.


#################### BUIDLING THE FORECAST MODEL ##############
options(scipen = 999)
Retail_Model_1 <- arima(Retail_Data, order = c(2,1,1),
                        seasonal = list(order = c(2,1,2),period = 12))

summary(Retail_Model_1)
## THERE ARE MANY ERROR MEASURES, HOWEVER OUR MAIN CONCERN IS 
## AIC, which is 4607.83


### HYPOTHESIS ::
### H0 : NO AUTOCORRELATION
### Ha : AUTOCORRELATION

## NOW TO CHECK WHITE NOISE, WE USE BOX-LJUNG TEST
Acf(residuals(Retail_Model_1))
?Box.test
Box.test(residuals(Retail_Model_1), lag = 24, fitdf = 1,
         type = "Ljung")
## The spike at lag 5 shows significance of the residuals
## Moreover, the p-value < 0.05 also depicts the fact that 
## the residuals have AUTOCORRLETION, hence NO WHITE NOISE..
## WE MUST ADD SOME NON-SEASONAL TERMS in our model..

##### LET"S USE TRIAL & ERROR WITH DIFFERENT NON-SEASONAL TERMS


######## ARIMA(3,1,2)(2,1,2)--12
Retail_Model_2 <- arima(Retail_Data, order = c(3,1,2),
                        seasonal = list(order = c(2,1,2),period = 12))

summary(Retail_Model_2)
## THERE ARE MANY ERROR MEASURES, HOWEVER OUR MAIN CONCERN IS 
## AIC, which is 4616.8.

## AIC HAS INCREASED SO WE REJECT IT STRAIGHT AWAY


######## ARIMA(4,1,2)(2,1,2)--12
Retail_Model_3 <- arima(Retail_Data, order = c(4,1,2),
                        seasonal = list(order = c(2,1,2),period = 12))

summary(Retail_Model_3)
## THERE ARE MANY ERROR MEASURES, HOWEVER OUR MAIN CONCERN IS 
## AIC, which is 4595.11

Acf(residuals(Retail_Model_3))
## ZERO TO NONE SPIKES PRESENT, THE DATA IS STATIONARY

Box.test(residuals(Retail_Model_3), lag = 24, fitdf = 1,
         type = "Ljung")
## AIC still high, however, 
## insigf. of p-value (NO-AUTOCORRL PRESENT)



######## ARIMA(5,1,2)(2,1,2)--12
Retail_Model_4 <- arima(Retail_Data, order = c(5,1,2),
                        seasonal = list(order = c(2,1,2),period = 12))

summary(Retail_Model_4)
## THERE ARE MANY ERROR MEASURES, HOWEVER OUR MAIN CONCERN IS 
## AIC, which is 4604.49

Acf(residuals(Retail_Model_4))
## ZERO TO NONE SPIKES PRESENT, THE DATA IS STATIONARY

Box.test(residuals(Retail_Model_4), lag = 24, fitdf = 1,
         type = "Ljung")
## AIC still high, however, 
## insigf. of p-value, (NO - AUTOCORRL)



######## ARIMA(3,1,1)(2,1,2)--12
Retail_Model_5 <- arima(Retail_Data, order = c(3,1,1),
                        seasonal = list(order = c(2,1,2),period = 12))

summary(Retail_Model_5)
## THERE ARE MANY ERROR MEASURES, HOWEVER OUR MAIN CONCERN IS 
## AIC, which is 4608.8

Acf(residuals(Retail_Model_5))
## ONE SPIKE PRESENT..

Box.test(residuals(Retail_Model_5), lag = 24, fitdf = 1,
         type = "Ljung")
## P-Value is sigf., hence WHITE NOISE, AUTOCORRL PRESENT
## Moreover,
## THE AIC is still high..


######## ARIMA(6,1,1)(2,1,2)--12
Retail_Model_6 <- arima(Retail_Data, order = c(6,1,1),
                        seasonal = list(order = c(2,1,2),period = 12))

summary(Retail_Model_6)
## THERE ARE MANY ERROR MEASURES, HOWEVER OUR MAIN CONCERN IS 
## AIC, which is 4600.89

Acf(residuals(Retail_Model_6))
## ZERO TO NONE SPIKES PRESENT, THE DATA IS STATIONARY

Box.test(residuals(Retail_Model_6), lag = 24, fitdf = 1,
         type = "Ljung")
## AIC & ACF are convincing, making it 
## THE BEST MODEL OUT THERE..



#################### FORECASTING ####################

## FOR THE NEXT 30 MONTHS
Forecast <- forecast(Retail_Model_6, h = 30)
Forecast

plot(Forecast, xlab = "Years", ylab = "Sales (In $ Million)",
     main = "FORECASTS FROM RETAIL MODEL")



#################### SUMMARY ####################

## The sales are forecasted using the SARIMA Modelling
## techniques which show patterns similar to the 
## Previous sales made, however, in the upward direction..


################### END OF CASE STUDY ###################