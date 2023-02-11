library(odbc)
library(DBI)
library(lubridate)
library(tidyverse)
library(dplyr)
library(data.table)
library(readxl)
library("R.utils")
library(forecast)
#Import data into Rstudio
Borough <- read.csv("/Users/jhanavibysani/Desktop/BOROUGH.csv")
View(Borough)

Neighborhood <- read.csv("/Users/jhanavibysani/Desktop/NEIGHBORHOOD.csv")
View(Neighborhood)

Building_class <- read.csv("/Users/jhanavibysani/Desktop//BUILDING_CLASS.csv")
View(Building_class)

NYC_Historical <- read.csv("/Users/jhanavibysani/Desktop/NYC_HISTORICAL (1).csv",sep = ";")
View(NYC_Historical)

NYC_Future <- read.csv("/Users/jhanavibysani/Desktop/NYC_FUTURE.csv",sep = ";")
View(NYC_Future)

task1 <- NYC_Historical %>%
  #Combining both the tables NYC_historical and Building_Class into table task1
  left_join(Building_class, by=c("BUILDING_CLASS_FINAL_ROLL" = "BUILDING_CODE_ID")) %>%
  left_join(Neighborhood) %>% 
  #Adding Sale_Year and Sale_Quar by preserving sale date
  mutate(Sale_Year = year(SALE_DATE), Sale_Quar = quarter(SALE_DATE))%>% 
  #Applying filter to Belle Harbor Neighborhood with residential properties from 2009
  filter(SALE_PRICE>0, GROSS_SQUARE_FEET>0, NEIGHBORHOOD_NAME == 'BELLE HARBOR', TYPE == 'RESIDENTIAL', Sale_Year>=2009)%>% 
  #Calculating the required time index for section 2
  mutate(ti = Sale_Year * 4 + Sale_Quar - 2009*4)%>% 
  select('SALE_DATE', 'Sale_Year', 'Sale_Quar', 'ti', 'ADDRESS', 'BUILDING_CLASS_FINAL_ROLL', 'YEAR_BUILT', 'GROSS_SQUARE_FEET', 'RESIDENTIAL_UNITS', 'SALE_PRICE')
view(task1)
#Section 1: Time Series
#Data is grouped by quarter and year wise
task1_g <- task1 %>%
group_by(Sale_Year, Sale_Quar)%>%
#Total sales price for individual time period has been calculated.
summarise(Total_Sales = sum(SALE_PRICE))

#Total sales price by individual period is converted into time series object  
total_Sales <- ts(task1_g['Total_Sales'][[1]], start = 2009, frequency = 4)
#Displaying total sales
total_Sales

#decomposition of a time series into seasonal, trend and irregular components.
STL_d <- stl(total_Sales, "periodic")
#Plotting of seasonal decomposition of time series
plot(STL_d)

#Time series model being created
times_model <- ets(y = total_Sales, model='ANA')
#Sumarry of time series model ANA
summary(times_model)

#Time series model being used to forecast sales for future 8 periods
times_forecast <- as.data.frame(forecast(times_model, 8))

#plotting of forecasted sales data for next 8 periods
plot(forecast(times_model, 8))

#Section 2: Regression Forecast
#Sales_Quar data being converted into a factor to perform regression analysis
task1_g$Sale_Quar <- factor(task1_g$Sale_Quar)

#Obtaining ti values from task1 and set the names to rows
task1_g$ti = rownames(task1_g)
#Converting into numeric
task1_g$ti <- as.numeric(task1_g$ti)

#Regression model with time period as predictor
RM_time <- lm(data = task1_g, formula = Total_Sales~ti)
#Sumarry of regression model with time period-predictor
summary(RM_time)

#Regression model with time period-ti, season-Sales_Quar as predictor
RM_time_season <- lm(data = task1_g, formula = Total_Sales~ti+Sale_Quar)
#Sumarry of regression model  with time period-ti, season-Sales_Quar as predictor
summary(RM_time_season)


#Section 3: Regression Prediction
#Multiple linear regression model using sale rpice,year,gross_sqft from table task1
RM_multiple<-lm(formula=SALE_PRICE~ti + YEAR_BUILT + GROSS_SQUARE_FEET + RESIDENTIAL_UNITS + BUILDING_CLASS_FINAL_ROLL, data=task1)
#Sumarry of Multiple regression model
summary(RM_multiple)

#Adding residuals to the data frame df from RM_multiple
df["residuals"] <- RM_multiple$residuals

#Plotting of gross square feet from data frame df using boxplots to identify the outliers
boxplot(df$GROSS_SQUARE_FEET)
#Plotting of Residential units from data frame df using boxplots to identify the outliers
boxplot(df$RESIDENTIAL_UNITS)

#Variable analysis for all the numeric data ti,year bulit,gross_sqft and residential units used in regression
cols <- c('ti', 'YEAR_BUILT', 'GROSS_SQUARE_FEET', 'RESIDENTIAL_UNITS')
vars <- select(task1, cols)
corr <- cor(vars)
pairs(vars)

#Multiple linear regression model including Gross Square feet
RM_multiple1<-lm(formula=SALE_PRICE~ti + YEAR_BUILT + RESIDENTIAL_UNITS + BUILDING_CLASS_FINAL_ROLL, data=task1)
summary(RM_multiple1)

#Multiple linear regression model without Residential Units
RM_multiple2<-lm(formula=SALE_PRICE~ti + YEAR_BUILT + GROSS_SQUARE_FEET + BUILDING_CLASS_FINAL_ROLL, data=task1)
summary(RM_multiple2)

