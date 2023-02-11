library(tidyverse)
library(lubridate)
library(ggplot2)
library(readr)
library(datetime)
library(dplyr)
library(forcats)

#2. Read this file into your R environment.Be sure to use the read.csv() function 

data<- read.csv("NOPD_Service_Calls_2021(1).csv")
data

#a. Call the str() function on your dataset, and show the results.
str(data)

#b. What does this function accomplish? How many rows and how many columns does your dataframe 
    #contain?
dim(data)
 
#3. Filter your dataset, so that it only contains records with your assigned ZIP code (a list
#of all ZIP code assignments can be found on Blackboard).

data1<- filter(data, Zip== "70130")
data1

#a. How many records does your dataframe contain now?
dim(data1)

count(data1)

#4. Dealing with NA data.
#a. Are there any NA values in your dataframe? How do you know this? What is the
#total number of NAs in the dataframe?

anyNA(data1)

sum(is.na(data1))

#b. Convert any blank cells in the dataframe into NAs.

data2<- data1

data2[data2==""]<- NA

#c. How many NAs are in the dataframe now?
sum(is.na(data2))

#d. Generate a table that shows the number of missing values and the percentage of
#missing values for each variable. Which variables have missing values? Why
#might these columns have missing values? 

NOPD<-sum(is.na(data2$NOPD_Item))
perc_NOPD<- NOPD/count(data2[1])

type<-sum(is.na(data2$Type))
perc_type<- type/count(data2[2])

typetext<-sum(is.na(data2$TypeText))
perc_typetext<- typetext/count(data2[3])

priority<-sum(is.na(data2$Priority))
perc_priority<- priority/count(data2[4])

initial<-sum(is.na(data2$InitialType))
perc_initial<- initial/count(data2[5])

initial_text<-sum(is.na(data2$InitialTypeText))
perc_itext<- initial_text/count(data2[6])

initial_p<-sum(is.na(data2$InitialPriority))
perc_initialp<- initial_p/count(data2[7])

mapx<-sum(is.na(data2$MapX))
perc_mapx<- mapx/count(data2[8])

mapy<-sum(is.na(data2$MapY))
perc_mapy<- mapy/count(data2[9])

timec<-sum(is.na(data2$TimeCreate))
perc_timec<- timec/count(data2[10])

timed<-sum(is.na(data2$TimeDispatch))
perc_timed<- timed/count(data2[11])
perc_timed

timea<-sum(is.na(data2$TimeArrive))
perc_timea<- timea/count(data2[12])
perc_timea

timecl<-sum(is.na(data2$TimeClosed))
perc_timecl<- timecl/count(data2[13])
perc_timecl

disp<-sum(is.na(data2$Disposition))
perc_disp<- disp/count(data2[14])

dispt<-sum(is.na(data2$DispositionText))
perc_dispt<- dispt/count(data2[15])

selfI<-sum(is.na(data2$SelfInitiated))
perc_selfI<- selfI/count(data2[16])

beat<-sum(is.na(data2$Beat))
perc_beat<- beat/count(data2[17])

blockad<-sum(is.na(data2$BLOCK_ADDRESS))
perc_blockad<- blockad/count(data2[18])

zip1<-sum(is.na(data2$Zip))
perc_zip1<- zip1/count(data2[19])

policed<-sum(is.na(data2$PoliceDistrict))
perc_policed<- policed/count(data2[20])

loc<-sum(is.na(data2$Location))
perc_loc<- loc/count(data2[21])

table1<- matrix(c(0,0,0,0,0,0,0,0,0,0,6619L,4905L,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,21,15,0,0,0,0,0,0,0,0,0), ncol=2)
colnames(table1) <- c("no. of Missing Values","Percentage(%) of Missing Values")
rownames(table1) <- c("NOPD_Item", "Type","TypeText","Priority","InitialType","InitialTypeText","InitialPriority","MapX","MapY","TimeCreate","TimeDispatch","TimeArrive","TimeClosed","Disposition","DispositionText","SelfInitiated","Beat","Block_address","Zip","PoliceDistrict","Location")
table1<- as.table(table1)
table1

#5. Handling dates
#a. How many time-related variables in the dataset are there? Run the str()
#function to see how R views these variables. What data type are they?

str(data2)

#b. Using any method, convert each of these variables to a ‘Date’ data type, and
#show that their type has been successfully converted. 

data2$TimeCreate <- as.POSIXct(data2$TimeCreate, format = "%m/%d/%Y %H:%M:%S") 
data2$TimeDispatch <-as.POSIXct(data2$TimeDispatch, format ="%m/%d/%Y %H:%M:%S") 
data2$TimeArrive <- as.POSIXct(data2$TimeArrive, format = "%m/%d/%Y %H:%M:%S") 
data2$TimeClosed <- as.POSIXct(data2$TimeClosed, format = "%m/%d/%Y %H:%M:%S") 

str(data2)

#c. Now, add a new variable to the dataframe called delay. Duration should be
#based on the difference between TimeArrive and TimeDispatch.

Delay <-difftime(data2$TimeArrive,data2$TimeDispatch, units = "secs")
data2$Delay<- Delay

#d. Next, create another new variable called handled. Handled should be the
#difference between TimeClosed and TimeDispatch.

Handled <-difftime(data2$TimeClosed,data2$TimeDispatch, units = "secs")
data2$Handled<- Handled

#e. What is your birthday? (just use the month and day for this, not the year)

#birthday month-> 10, birthday date-> 20

#i. How many calls to the New Orleans Police were made in your ZIP code
#on your birthday? What was the most common Description.Text for that day? 

data3 <-filter(data2, TimeClosed=="2021-10-20")
data3

#6. Exploring the dataset

#a. Should ZIP Code be considered a numeric or categorical variable? Why?

# Categorical- as it provides unique identification

#b. What percentage of all the service calls in your ZIP code were self-initiated?

selfIn <-filter(data2, SelfInitiated =="Y")
v<- count(selfIn)

perc_selfIn<- v/count(data2[16])
perc_selfIn

#c. What percentage of all the service calls in your ZIP code were marked “Gone on Arrival?”

GoA <-filter(data2, DispositionText =="GONE ON ARRIVAL")
w<- count(GoA)

perc_GoA<- w/count(data2[16])
perc_GoA

#d. What percentage of all the service calls in your ZIP code have a disposition text of “False Alarm”?

FA <-filter(data2, DispositionText =="FALSE ALARM")
m<- count(FA)

perc_FA<- m/count(data2[16])
perc_FA

#7. Remove the following column from the dataframe: Beat

data4<- subset(data2, select = -c(Beat))

str(data4)

#8. Using the quarter() function from lubridate, create a new column called season. Season
#should be created from the TimeDispatch variable. Next, rename the quarters so that
#Quarter 1 becomes “Winter”, Quarter 2 becomes “Spring”, Quarter 3 becomes “Summer”
#and Quarter 4 becomes “Fall.”

data4$season<-lubridate::quarter(data4$TimeDispatch)

data4$season[data4$season == '1'] <- 'Winter'
data4$season[data4$season == '2'] <- 'Spring'
data4$season[data4$season == '3'] <- 'Summer'
data4$season[data4$season == '4'] <- 'Fall'

#9. Using ggplot, construct a barplot showing the counts of police service requests during
#each of the four seasons. Fill your bars with any color of your choice.

#a. What do you notice about your plot? Why might it look the way it does? 

data5<- na.omit(data4$season)
data5<- as.data.frame(data5)

data5<- na.omit(data4)
data5<- as.data.frame(data5)

ggplot(data5,aes(x=fct_infreq(season)))+geom_bar(fill=c("#56ddc5", "#ff3db7", "#4699dd","red"))+
ggtitle("Police service requests during each of the four seasons")+theme_minimal()+xlab("Season") + ylab("Count")


#10. Perform another filtering step. This time, filter your dataset so that only rows with the
#6 most common Type.Text values remain.

#a. What are your six most common TypeText values?

q<-data.frame(table(data4$TypeText))
q1<-q[order(q$Freq,decreasing=TRUE),]
data6<-q1 %>% slice(1:6)

data5%>%count(TypeText)%>%arrange(desc(n))%>%head(6)
data_t<- filter(data5,TypeText %in% c('COMPLAINT OTHER','AREA CHECK','DISTURBANCE (OTHER)',' TRAFFIC STOP','BURGLAR ALARM, SILENT','RETURN FOR ADDITIONAL INFO'))


#b. How many rows does your dataframe contain now?
dim(data_t)

#c. Using ggplot, make a barplot that depicts the counts for these 6 most common
#TypeText values. Color your bars. Make sure that the axis labels are readable,


ggplot(data6, aes(x = reorder(Var1,Freq), y= Freq,fill = Var1))+
  geom_bar(stat = "identity", alpha = 1, width = 0.9)+
  xlab("TypeText")+
  ylab("Count")+
  scale_x_discrete(labels = function(x) 
    stringr::str_wrap(x, width = 10))+
  theme(legend.position = "none")
  ggtitle("Counts for the 6 most common TypeText values")+theme_minimal()


#d. In a sentence or two, describe your plot – what does it show?
#answered in pdf document

#11. Now, generate faceted histograms. These histograms should depict the distribution of
#the duration variable, faceted on the ‘TypeText’ variable. Fill your histograms with any
#color of your choice. To improve readability, you may wish to adjust the axes sizes with xlim() and ylim().

data_t$Handled<- as.numeric(data_t$Handled)
ps<-ggplot(data=subset(data_t,!is.na(Handled)),aes(x=Handled))+geom_histogram()
ps+facet_grid(TypeText~.,scale="free")+
xlab("Handled")+
  ylab("Count-TypeText")+
  scale_x_discrete(labels = function(x) 
    stringr::str_wrap(x, width = 10))+
  theme(legend.position = "none") +
ggtitle("Histogram for duration variable")+theme_minimal()

  

#i. What do you see here? In a few sentences, describe what these faceted histograms show, and point out anything that is noteworthy or unusual.
#Why might some types of calls show a different pattern than others?

#answered in pdf document

#12. Okay, so it’s time for one last filter operation. Filter the dataframe so that only the rows with the six most Disposition types remain.

dispo<-data.frame(table(data_t$Disposition))
dispo[order(dispo$Freq,decreasing=TRUE),]
datadispo<- filter(data5,Disposition %in% c("NAT","RTF","GOA","VOI","DUP","UNF"))


#a. Now, you will make a proportional fill barplot. Use Disposition as the variable to
#on one axis, and use TypeText as the “fill” variable. Inside your geom_bar()
#layer, write: position=”fill” to generate a proportional fill barplot.

data_t%>%count(Disposition)%>%arrange(desc(n))%>%head(6)
data_disp<- filter(data_t,Disposition %in% c('NAT','RTF','GOA','VOI','DUP','UNF'))

ggplot(data_disp,aes(x=Disposition, fill=TypeText))+geom_bar(position = "fill")


#b. What do you see now? What stands out here as interesting or unusual? Again,
#no domain knowledge is required – but write a couple of sentences of
#reasonable speculation that might explain some of the differences that you see here. 

#answered in pdf document

#Question13

library(leaflet)

datal<- na.omit(data5)
coords <- strsplit(unlist(datal$Location), split = " ")
df <- as.data.frame(do.call(rbind, coords)) 

library(tidyr)
library(magrittr)

xx<- as.data.frame(datal$Location)

df1 <- gsub('[\\(\\)]', '', datal$Location)
df1 <- strsplit(df1, ' ')
df1 <- do.call(rbind, lapply(df1, as.numeric))
colnames(df1) <- c('Null','Longitude', 'Latitude')
df2<- as.data.frame(df1)

m<- leaflet(df1) %>% addTiles()%>% addCircles(df2$Longitude,df2$Latitude)
m

#Question14

m1 <- leaflet() %>% addTiles() %>% addCircles(lng=df2$Longitude, lat= df2$Latitude) %>%
  addProviderTiles("Stamen.Watercolor") %>% 
  addProviderTiles("Stamen.TonerHybrid")
m1 


  
