library(tidyverse)
library(lubridate)
library(ggplot2)
library(readr)
library(datetime)
library(dplyr)
library(forcats)
library(MASS)

#1. Read the dataset spotify_top_charts_22.csv into your environment.Scroll through this dataset, using any method that you prefer, and select any song. If
#you find a song that you know and like, then select it.

data<- read.csv("spotify_top_charts_22.csv")
data

#a. What song did you pick?
#Shivers

#b. In a sentence or two, why did you pick this song?
#I Keep listening to this song while I workout. It is also in my liked playlist.

#c. What values does your song have for the following categories:
  #danceability: 0.788
  #energy: 0.859
  #loudness: -2.724
  #speechiness: 0.0856 
  #acousticness: 0.281 
  #instrumentalness: 0
  #liveness: 0.0424
  #tempo: 141.02
  #duration_ms: 207853

#2. Extract the row that contains info for your song from spotify_top_charts_22.csv. Verify
#that your song is now stored as its own dataframe 

fav<- filter(data, track_name=="Shivers")

#3. Now, read spotify.csv dataset into your environment. Call the str() function on your dataset and show the results.

data1<- read.csv("spotify.csv")
str(data1)

#a. What type of variable is target? If target is not currently a factor, convert it into
#a factor. It will be our response variable in this model. Target tells us whether
#George, the person who uploaded this dataset, liked the song. “1” means that
#George liked it, and “0” means that he did not.

#on running the str() function we can determine that target variable is integer.Hence, converting into factor
data1$target <- as.factor(data1$target)
str(data1)


#b. What unique values does the target variable have? For each of these outcome
#values, find out how many records in the dataset have that value, and state it here.

#Target variable has two unique values 0 and 1.

table(data1$target)

#0 - did not like - 997 songs
#1 - liked it - 1020 songs

#4. Are there any NAs in this dataset? Show the code that you used to find this out. If there
#are any NA values in any particular column, replace them with the median value for that column.

anyNA(data1)
any(is.na(data1))

#5. Remove these columns from the dataframe: X, key, mode, and time signature. We won’t use them here.

data1<- subset(data1, select = -c(X, key, mode,time_signature))

#6. Using your assigned seed value (from Assignment 2), partition the spotify dataset into training (60%) and validation (40%) sets.

set.seed(40)
train.index <- sample(c(1:nrow(data1)), nrow(data1)*0.6)
train.df <- data1[train.index, ]
valid.df <- data1[-train.index, ]

View(train.df)

#7. Next, we will do some variable selection.
#a. Using your training set, and either a grouping or filtering function to separate the songs that George liked from the ones that he did not like, examine the
#percentage difference in mean value between danceability, energy, loudness,speechiness, acousticness, instrumentalness, liveness, tempo,
#duration_ms.

data_1<- filter(train.df, target==1)
data_0<- filter(train.df, target==0)

Glike <- data_1[c(1,2,3,4,5,6,7,8,9)]
Gdislike<- data_0[c(1,2,3,4,5,6,7,8,9)]

percentage_difference<- ((colMeans(Glike)-colMeans(Gdislike))/colMeans(Glike))*100
percentage_difference


#b. Which variables show a percentage difference of 10% or more? If any variables show less than 10% difference in mean value between the two groups (songs that
#George likes, and songs that George doesn’t like), then remove those variables entirely. 

#variables more than 10%
#instrumentalness,speechiness

#variables less than 10%
#acousticness, danceability,duration,energy, liveness,loudness,tempo

train.df<- subset(train.df, select = -c(acousticness, danceability,duration_ms,energy, liveness,loudness,tempo))
valid.df<- subset(valid.df, select = -c(acousticness, danceability,duration_ms,energy, liveness,loudness,tempo))
data1<- subset(data1, select = -c(acousticness, danceability,duration_ms,energy, liveness,loudness,tempo))


#c. In a sentence or two, why might it make sense to remove variables from a k-nn
#model when those variables’ values are very similar for both outcome classes?

#With similar values in data it might result in wrong results. To avoid that its better to remove the 
#variables so that the grouping is performed in fair manner without any influence.As this results in better accuracy of the model.
#Similar values might make the model to see the variables as same leading to unnecessary complications.


#8. Normalize your data using the preProcess() function from the caret package. Use
#Table 7.2 from the book as a guide for this.

library(caret)

train.norm.df <- train.df
valid.norm.df <- valid.df
data1.norm.df<- data1

norm.values <- preProcess(train.df[, 1:2], method=c("center", "scale"))
train.norm.df[, 1:2] <- predict(norm.values, train.df[, 1:2])
valid.norm.df[, 1:2] <- predict(norm.values, valid.df[, 1:2])
data1.norm.df[, 1:2] <- predict(norm.values, data1[, 1:2])


#9. Using the knn() function from the FNN package, and using a k-value of 7, generate a
#predicted classification for your song -- Will George like it or not? What outcome did
#the model predict? Also, what were your song’s 7 nearest neighbors? List their titles,
#artists, and outcome classes. Be sure to show their outcome classes in your write-up.

library(FNN)
 
fav1 <- as.data.frame(c(instrumentalness=0, speechiness=0.0856))%>% t()

fav1.df <- predict(norm.values, fav1)

nn <- knn(train = train.norm.df[, c(1:2)], test = fav1.df, 
          cl = train.norm.df[, 4], k = 7)

nn 

row.names(train.df)[attr(nn,"nn.index")]

nm <- train.df[ c(1037,1085,961,51,752,532,812), c(4,5,6)]
nm

#George did not like most of its neighbor  songs.7 nearest neighbors are 

#1060-Family Table-Zac Brown Band-0
#1415-Litty (feat. Tory Lanez)- Meek Mill-0
#233- Never gonna give you up - Baary white - 1
#1178-Issues-Julia Michaels-0
#1157-Rollin-Calvin Harris-0
#1094-Hey Dj-CNCO-0
#102-Leave a Trace- Chvrches-1

#10. Use your validation set to help you determine an optimal k-value. Use Table 7.3 from the textbook as a guide here.

accuracy.df <- data.frame(k = seq(1, 14, 1), accuracy = rep(0, 14))

# compute knn for different k on validation.
for(i in 1:14) {
  knn.pred <- knn(train.norm.df[, 1:2], valid.norm.df[, 1:2],
                  cl = train.norm.df[, 4], k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, valid.norm.df[, 4])$overall[1]
}


#11. Using either the base graphics package or ggplot, make a scatterplot with the various k
#values that you used in the previous step on your x-axis, and the accuracy metrics on the y-axis.

ggplot(accuracy.df, aes(x = k , y = accuracy))+geom_point()+theme_classic()


#12. Re-run your knn() function with the optimal k-value that you found previously. What
#result did you obtain? Was it different from the result you saw when you first ran the
#k-nn function? Also, what were the outcome classes for each of your song’s k-nearest
#neighbors? Be sure to show their outcome classes in your write-up.


knn.new <- knn(train = train.norm.df[, c(1:2)], test = fav1.df, 
                          cl = train.norm.df[, 4], k = 13)
row.names(train.df)[attr(nn, "nn.index")]
knn.new
nm1 <- train.df[ c(1037,1085,961,51,752,532,812,452,162,1044,475,1116,128), c(4,5,6)]
nm1

#I have performed Knn modeling using the k value - 13 as it had the maximum accuracy.For K values of 8 and 13 the accuracy is the same.
#Result is pretty much similar to the previous one but it has several other neighbors added to it.First few nearest 
#neighbors remain same.

## Naive Bayes

library(e1071)

#1. After downloading the file from Blackboard, bring hotel_bookings.csv into your R environment.

Hotel<- read.csv("hotel_bookings.csv")

#2. Exploring the dataset and preparing the variables
#a. Remove the reservation status and reservation status variables from the dataset.
#Why will it be essential for us to not use reservation status as an input in this
#model? (To answer this, you may want to perform some data exploration)

str(Hotel)
Hotel<- subset(Hotel, select = -c(reservation_status,reservation_status_date))

#since our prediction is whether the guest will cancel a hotel reservation or not. Including the reservation status or
#any variable related to it makes no sense as it basically indicates the result. Here the model is trying to predict about the 
#cancellation.

#b. Right now, we have some NAs in the dataset, but they are being sneaky – they are described as NULL.
#i. Convert any NULL values in the dataset to instead become NA.

Hotel[Hotel=="NULL"]<- NA

#ii. Check to see if you have any variables with more than 10% missingness (NA values).

hotel<-sum(is.na(Hotel$hotel))
perc_hotel<- hotel/count(Hotel[1])

iscan<-sum(is.na(Hotel$is_canceled))
perc_iscan<- iscan/count(Hotel[2])

isleadtime<-sum(is.na(Hotel$lead_time))
perc_isleadtime<- isleadtime/count(Hotel[3])

arrdyr<-sum(is.na(Hotel$arrival_date_year))
perc_arrdyr<- arrdyr/count(Hotel[4])

arrdmnth<-sum(is.na(Hotel$arrival_date_month))
perc_arrdmnth<- arrdmnth/count(Hotel[5])

arrdwk<-sum(is.na(Hotel$arrival_date_week_number))
perc_arrdwk<- arrdwk/count(Hotel[6])

arrddm<-sum(is.na(Hotel$arrival_date_day_of_month))
perc_arrddm<- arrddm/count(Hotel[7])

sweekend<-sum(is.na(Hotel$stays_in_weekend_nights))
perc_sweekend<- sweekend/count(Hotel[8])

sweek<-sum(is.na(Hotel$stays_in_week_nights))
perc_sweek<- sweek/count(Hotel[9])

adults<-sum(is.na(Hotel$adults))
perc_adults<- adults/count(Hotel[10])

children<-sum(is.na(Hotel$children))
perc_children<- children/count(Hotel[11])
perc_children

babies<-sum(is.na(Hotel$babies))
perc_babies<- babies/count(Hotel[12])

meal<-sum(is.na(Hotel$meal))
perc_meal<- meal/count(Hotel[13])

country<-sum(is.na(Hotel$country))
perc_country<- country/count(Hotel[14])

market_seg<- sum(is.na(Hotel$market_segment))
perc_market_seg<-market_seg/count(Hotel[15])

dchann<- sum(is.na(Hotel$distribution_channel))
perc_dchann<-dchann/count(Hotel[16])

repg<- sum(is.na(Hotel$is_repeated_guest))
perc_repg<-repg/count(Hotel[17])

prevc<- sum(is.na(Hotel$previous_cancellations))
perc_prevc<-prevc/count(Hotel[18])

prevnc<- sum(is.na(Hotel$previous_bookings_not_canceled))
perc_prevnc<-prevnc/count(Hotel[19])

rroom<- sum(is.na(Hotel$reserved_room_type))
perc_rroom<-rroom/count(Hotel[20])

aroom<- sum(is.na(Hotel$assigned_room_type))
perc_aroom<-aroom/count(Hotel[21])

bookc<- sum(is.na(Hotel$booking_changes))
perc_bookc<-bookc/count(Hotel[22])

depoty<- sum(is.na(Hotel$deposit_type))
perc_depoty<-depoty/count(Hotel[23])

agent<- sum(is.na(Hotel$agent))
perc_agent<-agent/count(Hotel[24])

company<- sum(is.na(Hotel$company))
perc_company<-company/count(Hotel[25])

dayswl<- sum(is.na(Hotel$days_in_waiting_list))
perc_dayswl<-dayswl/count(Hotel[26])

cust<- sum(is.na(Hotel$customer_type))
perc_cust<-cust/count(Hotel[27])

adrr<- sum(is.na(Hotel$adr))
perc_adrr<-adrr/count(Hotel[28])

reqcp<- sum(is.na(Hotel$required_car_parking_spaces))
perc_reqcp<-reqcp/count(Hotel[29])

totalsr<- sum(is.na(Hotel$total_of_special_requests))
perc_totalsr<-totalsr/count(Hotel[30])


#agent-13%
#company-94%
#country- 0.004087445 #children-3.350364e-05

#1. Are there any such variables here? If so, remove them entirely.
#hence eliminating agent and company variables

Hotel<- subset(Hotel, select = -c(agent,company))

#c. How many variables begin with ‘arrival_date’?

#4 variables: arrival_date_year,arrival_date_month,arrival_date_week_number,arrival_date_day_of_month

#i. Keep arrival_date_month, but remove the other variables that begin
#with arrival_date.

Hotel<- subset(Hotel, select = -c(arrival_date_year,arrival_date_week_number,arrival_date_day_of_month))

#3. Are any variables in this data set currently seen as characters? If so, convert them to factors now.
str(Hotel)

#variables seen as characters- hotel,arrival_date_month,meal,market_segment,distribution_channel,assigned_room_type 
#reserved_room_type,deposit_type,agent,company,customer_type

Hotel$hotel <- as.factor(Hotel$hotel)
Hotel$arrival_date_month<- as.factor(Hotel$arrival_date_month)
Hotel$meal<- as.factor(Hotel$meal)
Hotel$country<- as.factor(Hotel$country)
Hotel$market_segment<- as.factor(Hotel$market_segment)
Hotel$distribution_channel<- as.factor(Hotel$distribution_channel)
Hotel$assigned_room_type<- as.factor(Hotel$assigned_room_type)
Hotel$reserved_room_type<- as.factor(Hotel$reserved_room_type)
Hotel$deposit_type<- as.factor(Hotel$deposit_type)
Hotel$customer_type<- as.factor(Hotel$customer_type)

str(Hotel)

#a. Also, convert the response variable to a factor now.

Hotel$is_canceled <- as.factor(Hotel$is_canceled)

str(Hotel)

#b. Convert any other categorical variable(s) that are represented in 0/1 format into factors, too

#is_repeated_guest- Categorical variable indicating if the booking name was from a repeated guest (1) or not (0)

Hotel$is_repeated_guest <- as.factor(Hotel$is_repeated_guest)

str(Hotel)

#4. Right now, arrival_date_month has 12 levels. We’ll leave it alone. However, run the str()
#function on the data frame now to check the number of levels for the other categorical
#variables here that we might use as inputs. If any contain more than 5 levels, do a
#filtering operation (just as you did in HW #1 and HW #2) to reduce the data so that only the 5 most common levels remain in the data.

#categorical variable: arrival_date_month(12 levels), meal(5 levels), distribution channel(5 levels),is_repeated_guest(2 levels),market_segment(8 levels),
#reserved_room_type(10 levels),assigned_room_type(12 levels),deposit_type(3 levels),agent(333 levels),company(352 levels),customer_type(4 levels),hotel(2 levels),is_canceled(2 levels)

str(Hotel)

qa<-data.frame(table(Hotel$market_segment))
q1a<-qa[order(qa$Freq,decreasing=TRUE),]
Hotel1a<-q1a %>% slice(1:5)

Hotel<- filter(Hotel,market_segment %in% c('Online TA','Online TA/TO','Groups',' TDirect','Corporate'))

qb<-data.frame(table(Hotel$reserved_room_type))
q1b<-qb[order(qb$Freq,decreasing=TRUE),]
Hotel1b<-q1b %>% slice(1:5)

Hotel<- filter(Hotel,reserved_room_type %in% c('A','D','E','F','G'))

qc<-data.frame(table(Hotel$assigned_room_type))
q1c<-qc[order(qc$Freq,decreasing=TRUE),]
Hotel1c<-q1c %>% slice(1:5)

Hotel<- filter(Hotel,assigned_room_type %in% c('A','D','E','F','G'))

qd<-data.frame(table(Hotel$country))
q1d<-qd[order(qd$Freq,decreasing=TRUE),]
Hotel1d<-q1d %>% slice(1:5)

Hotel<- filter(Hotel,country %in% c('PRT','GBR','FRA','ESP','DEU'))


#5. After performing these filtering operations, run the droplevels() function on your
#dataframe. You will do something like this:
  
Hotel <- droplevels(Hotel)
  
#6. For any numeric variables in your data, bin them into factors. Bin them using equal
#frequency binning. Be sure to give a label to each bin. Select a bin number of your choice.
  
 # Hotel$lead_time <- (discretize(mydf$myvar, method = "fixed", breaks = c(-Inf, 6, Inf),  labels = c("small", "large"))
  
  str(Hotel)
  library(arules)
  
    Hotel$lead_time <- (discretize(Hotel$lead_time, method = "fixed", breaks = c(-Inf, 6, Inf),  labels = c("small", "large")))
    
    Hotel$stays_in_weekend_nights <- (discretize(Hotel$stays_in_weekend_nights, method = "fixed", breaks = c(-Inf, 6, Inf),  labels = c("short_stay", "long_stay")))
    
    Hotel$stays_in_week_nights <- (discretize(Hotel$stays_in_week_nights, method = "fixed", breaks = c(-Inf, 6, Inf),  labels = c("short_stay", "long_stay")))
    
    Hotel$adults <- (discretize(Hotel$adults, method = "fixed", breaks = c(-Inf, 6, Inf),  labels = c("Yes_adults", "No_adults")))
    
    Hotel$children <- (discretize(Hotel$children, method = "fixed", breaks = c(-Inf, 6, Inf),  labels = c("Yes_children", "No_children")))
    
    Hotel$babies <- (discretize(Hotel$babies, method = "fixed", breaks = c(-Inf, 6, Inf),  labels = c("Yes_babies", "No_babies")))
    
    Hotel$previous_cancellations <- (discretize(Hotel$previous_cancellations, method = "fixed", breaks = c(-Inf, 6, Inf),  labels = c("PC_found", "PC_not_found")))
    
    Hotel$previous_bookings_not_canceled <- (discretize(Hotel$previous_bookings_not_canceled, method = "fixed", breaks = c(-Inf, 6, Inf),  labels = c("PB_found", "PB_not_found")))
    
    Hotel$booking_changes <- (discretize(Hotel$booking_changes, method = "fixed", breaks = c(-Inf, 6, Inf),  labels = c("BC_made", "BC_not_made")))
    
    Hotel$days_in_waiting_list <- (discretize(Hotel$days_in_waiting_list, method = "fixed", breaks = c(-Inf, 6, Inf),  labels = c("DWL_found", "DWL_not_found")))
    
    Hotel$required_car_parking_spaces <- (discretize(Hotel$required_car_parking_spaces, method = "fixed", breaks = c(-Inf, 6, Inf),  labels = c("require_Space", "donot_requirespace")))
    
    Hotel$total_of_special_requests <- (discretize(Hotel$total_of_special_requests, method = "fixed", breaks = c(-Inf, 6, Inf),  labels = c("spec_req_made", "spec_req_not_made")))
    
 # Hotel$lead_time <- factor(round(Hotel$lead_time/100), labels = c("D1","D2","D3","D4","D5","D6","D7"))
  #Hotel$stays_in_weekend_nights <- factor(round(Hotel$stays_in_week_nights/3), labels = c("WN1","WN2","WN3","WN4","WN5","WN6","WN7","WN8","WN9","WN10"))
  #Hotel$stays_in_week_nights <- factor(round(Hotel$stays_in_week_nights/3), labels = c("WN1","WN2","WN3","WN4","WN5","WN6","WN7","WN8","WN9","WN10"))
  #Hotel$adults <- factor(round(Hotel$adults/2),labels = c("A1","A2","A3"))
  #Hotel$children <- factor(round(Hotel$children/2),labels = c("C1","C2","C3"))
  #Hotel$babies <- factor(round(Hotel$babies/2),labels = c("B1","B2","B3"))
  #Hotel$previous_cancellations <- factor(round(Hotel$previous_cancellations/2),labels = c("PC1","PC2","PC3","PC4","PC5","PC6","PC7"))
  #Hotel$previous_bookings_not_canceled <- factor(round(Hotel$previous_bookings_not_canceled/10),labels = c("BN1","BN2","BN3","BN4","BN5","BN6","BN7","BN8"))
  #Hotel$booking_changes <- factor(round(Hotel$booking_changes/2),labels = c("CH1","CH2","CH3","CH4","CH5","CH6","CH7","CH8"))
  #Hotel$days_in_waiting_list <- factor(round(Hotel$days_in_waiting_list/100),labels = c("L1","L2","L3","L4","L5"))
  #Hotel$required_car_parking_spaces <- factor(round(Hotel$required_car_parking_spaces/2),labels = c("CP1","CP2","CP3"))
  #Hotel$total_of_special_requests <- factor(round(Hotel$total_of_special_requests/2),labels = c("SR1","SR2","SR3"))
  
  #a. Show the results of this process, using the table() function.
  
  table(Hotel$lead_time)
  table(Hotel$stays_in_weekend_nights)
  table(Hotel$stays_in_week_nights)
  table(Hotel$adults)
  table(Hotel$children)
  table(Hotel$babies)
  table(Hotel$previous_cancellations)
  table(Hotel$previous_bookings_not_canceled)
  table(Hotel$booking_changes)
  table(Hotel$days_in_waiting_list)
  table(Hotel$required_car_parking_spaces)
  table(Hotel$total_of_special_requests)
  
  #b. What is the difference between equal width binning and equal frequency
  #binning? Why might equal frequency binning be preferable in some scenarios?
  
  #In equal width binning all the bins have equal width and represent equal range irrespective of number of values in each bin
  #Whereas equal frequency binning the dataset is divided into group of equal samples.Whereas bins have same frequency
  #As each bin has same amount of data it useful for any kind of model as the bins will be more significative in representing 
  #underlying distribution.
  
  
  #7. Using your seed value (the same one from Assignment #2) , partition your data into
  #training (60%) and validation (40%) sets.
  
  set.seed(40)
  train.index <- sample(c(1:nrow(Hotel)), nrow(Hotel)*0.6)
  train.df <- Hotel[train.index, ]
  valid.df <- Hotel[-train.index, ]

  
  #8. Let’s take a look at the variables from the dataset, and explore the way that they might
  #impact is_canceled. Using your training set data only, make a proportional barplot for
  #each one of your prospective input variables. Each barplot should show one of your
  #input variables as a category on the x-axis, with is_canceled as the fill variable. You
  #should build proportional barplots 
  
  ggplot(train.df, aes(fill=is_canceled, x=hotel)) + geom_bar(position="fill")
  ggplot(train.df, aes(fill=is_canceled, x=lead_time)) + geom_bar(position="fill")
  ggplot(train.df, aes(fill=is_canceled, x=arrival_date_month)) + geom_bar(position="fill")   
  ggplot(train.df, aes(fill=is_canceled, x=stays_in_week_nights)) + geom_bar(position="fill")
  ggplot(train.df, aes(fill=is_canceled, x=stays_in_weekend_nights)) + geom_bar(position="fill")
  ggplot(train.df, aes(fill=is_canceled, x=adults)) + geom_bar(position="fill")
  ggplot(train.df, aes(fill=is_canceled, x=children)) + geom_bar(position="fill")
  ggplot(train.df, aes(fill=is_canceled, x=babies)) + geom_bar(position="fill")
  ggplot(train.df, aes(fill=is_canceled, x=meal)) + geom_bar(position="fill")
  ggplot(train.df, aes(fill=is_canceled, x=country)) + geom_bar(position="fill")
  ggplot(train.df, aes(fill=is_canceled, x=market_segment)) + geom_bar(position="fill")
  ggplot(train.df, aes(fill=is_canceled, x=distribution_channel)) + geom_bar(position="fill")
  ggplot(train.df, aes(fill=is_canceled, x=is_repeated_guest)) + geom_bar(position="fill")
  ggplot(train.df, aes(fill=is_canceled, x=previous_bookings_not_canceled)) + geom_bar(position="fill")
  ggplot(train.df, aes(fill=is_canceled, x=previous_cancellations)) + geom_bar(position="fill")
  ggplot(train.df, aes(fill=is_canceled, x=reserved_room_type)) + geom_bar(position="fill")
  ggplot(train.df, aes(fill=is_canceled, x=assigned_room_type)) + geom_bar(position="fill")
  ggplot(train.df, aes(fill=is_canceled, x=booking_changes)) + geom_bar(position="fill")
  ggplot(train.df, aes(fill=is_canceled, x=deposit_type)) + geom_bar(position="fill")
  ggplot(train.df, aes(fill=is_canceled, x=days_in_waiting_list)) + geom_bar(position="fill")
  ggplot(train.df, aes(fill=is_canceled, x=customer_type)) + geom_bar(position="fill")
  ggplot(train.df, aes(fill=is_canceled, x=adr)) + geom_bar(position="fill")
  ggplot(train.df, aes(fill=is_canceled, x=required_car_parking_spaces)) + geom_bar(position="fill")
  ggplot(train.df, aes(fill=is_canceled, x=total_of_special_requests)) + geom_bar(position="fill")
  
  #a. Based on the barplots that you see here, select any variable(s) that seem like they
  #will not have a strong amount of predictive power in a naive Bayes model. Drop
  #any such variable.
  
  str(train.df)
  
  train.df<- subset(train.df, select = -c(adr,children,babies))

  #The variable I have decided to remove is adr- which indicates average transactions with respect to stay and I 
  #think this has nothing to do with booking cancellations.Its a numeric value and doesn't help in any prediction.
  # I have kept variables as they do not show similar proportions at each level.As the proportion plot for children and babies 
  #are similar it makes no sense to include for prediction purpose.Hence eliminated them.
  
  #9. Build a naive bayes model, with the response variable is_canceled. Use all of the other
  #remaining variables in your training set as inputs. Show your model results.
  
  
  model <- naiveBayes(is_canceled~., data=train.df)
  model
  
  
  #10. Show a confusion matrix that compares the performance of your model against the
  #training data, and another that shows its performance against the validation data (just
  #use the accuracy metric for this analysis). How did your training set’s performance
  #compare with your validation set’s performance?
  
  library(caret)
  #creating a confusion matrix
  train_Pred <- predict(model, newdata = train.df)
  confusionMatrix(train_Pred, train.df$is_canceled)
  
  # validation
  valid_Pred <- predict(model, newdata = valid.df)
  confusionMatrix(valid_Pred, valid.df$is_canceled)
  
  #training set accuracy-0.7327
  #validation set accuracy-0.7362
  #The accuracy is found to be similar. The model is able to predict similar results for both the datasets indicating
  #that the model is not over fitted.
  
  #11. In classification, what is the naive rule? If you had used the naive rule as an approach
  #to classification, how would you have classified all the records in your training set?
    
  #Naive rule in classification is useful in identifying the highest probability of occurrence.As Naive rule identifies 
  #each variable as independent. Model wouldnot be able to make a fair prediction or it might stick to only one outcome class.
  #My model's accuracy is 74% where as naive rule accuracy is 45% which is slightly lesser. By using naive rule, the predictions might not be 
  # that relevant and it might be prone to some error or misclassifications as well.Since the accuracy is less than 50%.
  
  #a. How did your model’s accuracy compare with the naive rule accuracy, in
  #percentage terms? (Answer in percentage difference, not in percentage points difference).
 
  prop.table(table(train.df$is_canceled))
  #perc_diff<-(train accuracy - naive accuracy)/ naive accuracy
  #Booking was cancelled 
  percent_diff1<-(0.7327-0.4641)/0.7327
  percent_diff1
  #percent_diff1-36.65%
  
  #Booking was not cancelled
  percent_diff2<-(0.7327-0.5358359 )/0.7327
  percent_diff2
 #percent_diff2-26.86%
  
#12. Next, take a subset of the 200 records in your validation set that your model predicted
  #to be most likely to cancel their reservations. (Table 8.6 in the textbook will be a very
#good thing to look at in order to build this)
  
  
  pred.prob <- predict(model, newdata = valid.df, type = "raw")
  pred.prob
  
  pred_valid_pred <- as.data.frame(pred.prob)
  valid_with_prob <- cbind(valid.df, pred_valid_pred)
  
  most_likely <- valid_with_prob %>% 
   #arrange(desc(valid_with_prob$`1`)) %>% 
    slice(1:200)
  most_likely
  
  table(most_likely$is_canceled)
  prop.table(table(most_likely$is_canceled))
  
 # a. Among those 200 records, how many of the guests actually canceled? How does
 # the accuracy for these predictions compare to the overall model?
  
  #Actually cancelled- 99-49.5% accuracy
  #didnot cancel - 101-50.5% accuracy 
  
  #model accuracy : Cancelled- 49.5%, Didnot cancel- 50.5% 
  # accuracy are extremely close for the model as well as the subset of data
  
  # b. How could a hotel use the result shown above? In a few sentences, indicate
  #what it would mean for a hotel to be able to identify this particular subset of
  #records, and the way the hotel could act on this information.
  
  #From a particular subset of data the hotel will be able to draw conclusions if the customer is actually going to cancel the booking or not.
  #its not necessary to go through the entire dataset but fair partioning of the data will help us arrive at the same result. A small set of data can 
  #also be used by the hotel to make necessary predictions.
  
  #13. Pick any ONE record from your training set. It can be any row in the training set – it
  #doesn’t matter.
  
  #a. Did the guest in your chosen row cancel his or her reservation?
  #Guest  cancelled the reservation
  
  df<- data.frame(hotel='Resort Hotel',is_canceled=1, lead_time='large', arrival_date_month='July', stays_in_weekend_nights='short_stay', stays_in_week_nights='short_stay', adults='Yes_adults', meal='HB', country='PRT', market_segment='Corporate', distribution_channel='Corporate', is_repeated_guest=0, previous_cancellations='PC_found',previous_bookings_not_canceled='PB_found', reserved_room_type='D', assigned_room_type='D', booking_changes='BC_made', deposit_type='No Deposit',days_in_waiting_list='DWL_found',customer_type='Transient', required_car_parking_spaces='require_Space', total_of_special_requests='spec_req_made')
  
  #b. Use the predict() function to see whether your model would predict that this
  #guest would cancel? What did it predict? (Table 8.6 in the textbook might be
  
  predict(model, newdata = df)                        #helpful here)
  
  #c. Now, use the predict() function again but with a slight modification, in order to
  #have it generate the probability that your guest would cancel. What was the probability?
  
  predict(model, newdata = df, type='raw')
  
  #d. As a last step, demonstrate the way that the probability associated with this
  #cancellation prediction was generated. Do this the way we did the flight delay
  #examples in class -- use R to do the math, but don’t use any special functions orpackages.
  
  #A-priori probabilities:
  
  For0 <- 0.5358359 
  For1<- 0.4641641 
  
  #Componenet score= A-priori probabilities* conditional_probability
  CS0<- For0*0.3933906*0.036986376*0.07227663*5.255786e-01*5.255786e-01*0.24916562*0.107895169*0.38852109*0.18805056*0.8330688844*0.93636811*9.951852e-01*9.819445e-01*0.73846911*0.5722492750*9.455053e-01*0.0030639602*9.864857e-01*0.7065711003*0.9994528643*0.80423483
    
  CS1<- For1* 0.2897496*0.151385643*0.08560935*6.044741e-01*6.044741e-01*0.14290484*0.103906511*0.71171953*0.47913189*0.9261435726*0.98484140*9.954591e-01*9.984641e-01*0.79412354*0.7785642738*9.807012e-01*0.3784974958*9.895159e-01*0.8041402337*1.0000000000*0.92200334
  
  SumCS<- CS0+CS1
  
  CS1/SumCS
  #99.99% prediction similar to the one the model predicted the outcome to be.  
  