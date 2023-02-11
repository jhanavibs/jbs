library(odbc)
library(DBI)
library(tidyverse)
library(stringr)


#Import data into Rstudio
library(data.table)
library(readxl)
library("R.utils")

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


library(dplyr)
library(lubridate)
library(stringr)

# Task1 and Task 2
# Residential properties of Belle harbor encapsulating average price per square foot by year
task1 <-  NYC_Historical %>%
  #Combining both the tables NYC_historical and Building_Class into table task1
  left_join(Building_class, by=c("BUILDING_CLASS_FINAL_ROLL" = "BUILDING_CODE_ID")) %>%
  left_join(Neighborhood) %>% 
  #Applying filter to Belle Harbor Neighborhood with residential properties
  filter(NEIGHBORHOOD_NAME == "BELLE HARBOR", TYPE == "RESIDENTIAL") %>%
  #Sorting by year
  group_by(sale_year = year(SALE_DATE)) %>%
  #To sum up Sale_Price and Gross_square_Feet
  summarise(total_sale_price = sum(SALE_PRICE), total_square_feet = sum(GROSS_SQUARE_FEET)) %>%
  #To create a new column price_per_sqft
  mutate(price_per_sqft = total_sale_price/total_square_feet)

ggplot(task1, aes(sale_year, price_per_sqft)) + geom_point()
ggplot()+geom_line(data=task1,aes(x=sale_year,y=price_per_sqft,color="BELLE HARBOR"))+ggtitle("Avg. Price per Sq.ft/of Residential area by year")



#Task 3 
#Filtering data by removing 0 for sale_price and Gross_square_feet fields of Belle Harbor Neighborhood for residential properties
task3 <- NYC_Historical %>%
  left_join(Building_class, by=c("BUILDING_CLASS_FINAL_ROLL" = "BUILDING_CODE_ID")) %>%
  left_join(Neighborhood) %>% 
  filter(NEIGHBORHOOD_NAME == "BELLE HARBOR", TYPE == "RESIDENTIAL", SALE_PRICE>0, GROSS_SQUARE_FEET>0) %>%
  group_by(sale_year = year(SALE_DATE)) %>%
  summarise(total_sale_price = sum(SALE_PRICE), total_square_feet = sum(GROSS_SQUARE_FEET)) %>%
  mutate(price_per_sqft = total_sale_price/total_square_feet)
#Task4
#Plotting sale_year vs price_per sqft for Belle Harbor Neighborhood
ggplot(task1, aes(sale_year, price_per_sqft)) + geom_point()
ggplot()+geom_line(data=task3,aes(x=sale_year,y=price_per_sqft,color="BELLE HARBOR"))+ggtitle("Avg. Price per Sq.ft/of Residential area by year")


#Task 5: Compare with two nearby neighborhoods
#Filtering data by removing 0 for sale_price and Gross_square_feet fields of Belle Rose Neighborhood for residential properties
task5a <- NYC_Historical %>%
  left_join(Building_class, by=c("BUILDING_CLASS_FINAL_ROLL" = "BUILDING_CODE_ID")) %>%
  left_join(Neighborhood) %>% 
  filter(NEIGHBORHOOD_NAME == "BELLEROSE", TYPE == "RESIDENTIAL", SALE_PRICE>0, GROSS_SQUARE_FEET>0) %>%
  group_by(sale_year = year(SALE_DATE)) %>%
  summarise(total_sale_price = sum(SALE_PRICE), total_square_feet = sum(GROSS_SQUARE_FEET)) %>%
  mutate(price_per_sqft = total_sale_price/total_square_feet)
#Plotting sale_year vs price_per sqft for Belle rose Neighborhood
ggplot(task5a, aes(sale_year, price_per_sqft)) + geom_point()
ggplot()+geom_line(data=task5a,aes(x=sale_year,y=price_per_sqft,color="BELLROSE"))+ggtitle("Avg. Price per Sq.ft/of Residential area by year")

#Filtering data by removing 0 for sale_price and Gross_square_feet fields of Belmont Neighborhood for residential properties
task5b <- NYC_Historical %>%
  left_join(Building_class, by=c("BUILDING_CLASS_FINAL_ROLL" = "BUILDING_CODE_ID")) %>%
  left_join(Neighborhood) %>% 
  filter(NEIGHBORHOOD_NAME == "BELMONT", TYPE == "RESIDENTIAL", SALE_PRICE>0, GROSS_SQUARE_FEET>0) %>%
  group_by(sale_year = year(SALE_DATE)) %>%
  summarise(total_sale_price = sum(SALE_PRICE), total_square_feet = sum(GROSS_SQUARE_FEET)) %>%
  mutate(price_per_sqft = total_sale_price/total_square_feet)
#Plotting sale_year vs price_per sqft for Belmont Neighborhood
ggplot(task5b, aes(sale_year, price_per_sqft)) + geom_point()
ggplot()+geom_line(data=task5b,aes(x=sale_year,y=price_per_sqft,color="BELMONT"))+ggtitle("Avg. Price per Sq.ft/of Residential area by year")


# Function to obtain yearly sales by neighborhood
yearlysalesbyneighborhood <-  function(inputNAME){
  
  task5c <-  NYC_Historical %>%
    left_join(Building_class, by=c("BUILDING_CLASS_FINAL_ROLL" = "BUILDING_CODE_ID")) %>%
    left_join(Neighborhood, by=c("NEIGHBORHOOD_ID" = "NEIGHBORHOOD_ID")) %>% 
    filter(NEIGHBORHOOD_NAME == inputNAME, TYPE == "RESIDENTIAL", SALE_PRICE>0, GROSS_SQUARE_FEET>0) %>%
    group_by(sale_year = year(SALE_DATE)) %>%
    summarise(total_sale_price = sum(SALE_PRICE), total_square_feet = sum(GROSS_SQUARE_FEET)) %>%
    mutate(price_per_sqft = total_sale_price/total_square_feet,neighborhood_name=inputNAME)
}

#Assigning the results of the function to following neighborhood variables
belleharbor <- yearlysalesbyneighborhood("BELLE HARBOR") 
bellerose <- yearlysalesbyneighborhood("BELLEROSE") 
belmont <- yearlysalesbyneighborhood("BELMONT")

install.packages("ggplot")
library(ggplot2)

#Task6
#Comparing Belle harbor with Belle rose and Belmont neighborhoods
gg <- rbind(belleharbor,bellerose,belmont)
ggplot()+geom_line(data=gg,aes(x=sale_year,y=price_per_sqft,color=neighborhood_name))+ggtitle("Avg. Price per Sq.ft/of Residential area by year")
ggplot(gg, aes(sale_year,price_per_sqft, color=neighborhood_name)) +  geom_point(shape=1) +  geom_smooth(method=lm,se=FALSE)


summary(gg)
View(gg)
