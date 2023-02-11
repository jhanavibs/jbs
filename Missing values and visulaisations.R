#Loading Packages
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library (naniar)
library(tidytext)
library(wordcloud2)
library(tidyr)
library(magrittr)
library(leaflet)
library(tmap)

#Reading data into local platform
Barcelona<- read_csv("barcelona.csv")
View(Barcelona)

#Filterng data to Gracia neighbourhood
Gracia<- filter(Barcelona, neighbourhood_group_cleansed== "Gràcia")

#I. Missing Values

#A. Does your data contain any missing values and/or blank cells? If so, what
#can you do about this? Show the R code that you used to handle your
#missing values.

anyNA(Gracia)
sum(is.na(Gracia))

#No blank cells
Gracia[Gracia==""]<- NA

names(which(colSums(is.na(Gracia)) > 0))

miss_var_summary(Gracia)

#Handling missing data

# Drop the columns with 100% missing values as well as the other unnecessary columns
Gracia <-  subset(Gracia,select= -c(bathrooms, calendar_updated, id, listing_url, scrape_id,
                               neighbourhood, description, last_scraped, source, picture_url,
                               host_url, host_name, host_thumbnail_url, host_picture_url,
                               host_about, calendar_last_scraped, license))

# create the 'bathrooms_n' variable next to the 'bathrooms_text' variable
bathrooms_n <- as.numeric(sapply(strsplit(Gracia$bathrooms_text, " "), "[", 1))
Gracia <- add_column(Gracia, bathrooms_n, .after = "bathrooms_text")

# create the 'amenities_n' variable next to the 'amenities' variable
amenities_n <- (lengths(regmatches(Gracia$amenities, gregexpr(",", Gracia$amenities))) + 1)
Gracia <- add_column(Gracia, amenities_n, .after = "amenities")

# Replace NAs in the bathrooms_n variable with 0.5
Gracia$bathrooms_n <- ifelse(Gracia$bathrooms_text == "Private half-bath" & is.na(Gracia$bathrooms_n), 0.5, bathrooms_n)
Gracia$bathrooms_n <- ifelse(Gracia$bathrooms_text == "Shared half-bath" & is.na(Gracia$bathrooms_n), 0.5, bathrooms_n)
Gracia$bathrooms_n <- ifelse(Gracia$bathrooms_text == "Half-bath" & is.na(Gracia$bathrooms_n), 0.5, bathrooms_n)
Gracia$bathrooms_n[is.na(Gracia$bathrooms_n)] <- median(Gracia$bathrooms_n, na.rm=TRUE)
Gracia$bathrooms_text <- ifelse(is.na(Gracia$bathrooms_text), Gracia$bathrooms_n, Gracia$bathrooms_text)

# replace NAs in the host_location variable to Unknown
Gracia$host_location <- replace(Gracia$host_location, is.na(Gracia$host_location), "Unknown")

# replace NAs in the 'review_scores' related columns with the mean values
Gracia$review_scores_accuracy[is.na(Gracia$review_scores_accuracy)] <- mean(Gracia$review_scores_accuracy, na.rm=TRUE)
Gracia$review_scores_cleanliness[is.na(Gracia$review_scores_cleanliness)] <- mean(Gracia$review_scores_cleanliness, na.rm=TRUE)
Gracia$review_scores_checkin[is.na(Gracia$review_scores_checkin)] <- mean(Gracia$review_scores_checkin, na.rm=TRUE)
Gracia$review_scores_communication[is.na(Gracia$review_scores_communication)] <- mean(Gracia$review_scores_communication, na.rm=TRUE)
Gracia$review_scores_location[is.na(Gracia$review_scores_location)] <- mean(Gracia$review_scores_location, na.rm=TRUE)
Gracia$review_scores_value[is.na(Gracia$review_scores_value)] <- mean(Gracia$review_scores_value, na.rm=TRUE)
Gracia$review_scores_rating[is.na(Gracia$review_scores_rating)] <- mean(Gracia$review_scores_rating, na.rm=TRUE)
Gracia$reviews_per_month[is.na(Gracia$reviews_per_month)] <- mean(Gracia$reviews_per_month, na.rm=TRUE)

# replace NAs in the bedrooms and beds variables with median
Gracia$bedrooms[is.na(Gracia$bedrooms)] <- median(Gracia$bedrooms, na.rm=TRUE)
Gracia$beds[is.na(Gracia$beds)] <- median(Gracia$beds, na.rm=TRUE)


#B. Write one paragraph describing what you did, and why you did it. (Note: You
#may wish to deal with missing values differently for different tasks. You are not
#‘locked in’ to a decision regarding missing values).

#The neighbourhood Gracia has a total of 8971 missing values. A total of 22 Variables out of 75 contains NA values. 
#We have dropped variables with 100% missing values such as bathrooms and calendar updated. We have also eliminated the variables such as
#id, listing_url, scrape_id, neighbourhood, description, last_scraped, source, picture_url, host_url, host_name, host_thumbnail_url, 
#host_picture_url, host_about, calendar_last_scraped, license. We have not used any id’s or URL’s for further processing of the data as it
#didn’t seem to be useful. Hence, decided to eliminate them completely. Now the data frame contains a total of 58 variables. A new 
#variable ‘bathroom_n’ is introduced next to bathroom text which seperates the number of bathrooms into the varaible. A variables for amenities 
#is created in a similar manner. Variables for bathrooms and amenities in dataframe was originally associated with text. Hence, making it
#difficult to make use of them for analysis. So we decided to create a separate variable for the numeric values of each. The NA values in bathroom_n 
#variable has been replaced by 0.5 value and median values based on the bathroom text details. This is decided based on the bathroom_text variables. As these
#variables tend to indicate A shared bath we have added 0.5 in its place. If the host_location contains NA values, it has been replaced by ‘Unknown’. Missing values for the
#‘review scores’ columns has been replaced by mean values for the individual column. Replacing NAs with 0 for reviews wouldn’t be fair hence we decided to go ahead with mean 
#values. The missing values for beds and bedrooms has been replaced by median values of the variable. Mean values for these variables included decimals such as 3.2 and 4.1. As its not
#feasible to represent the beds and bedrooms with decimals we have decided to go ahead with median values which returned whole numbers.


#II. Summary Statistics

#A. Take a peek at your data, and then brainstorm a bit about some questions
#that you’d like to answer with summary statistics. To answer these questions
#choose any five of the summary statistics functions shown in the textbook,
#class slides, or anywhere else to learn a little bit about your data set.

summary(Gracia)

#Host Neighborhoods
count(unique(Gracia["host_neighbourhood"]))
table(Gracia$host_neighbourhood)

#Response time
table(Gracia$host_response_time)

#verification of host identitiy
table(Gracia$host_identity_verified)

#Room type
table(Gracia$room_type)

#Acceptance rate top 10 values
a<-data.frame(table(Gracia$host_acceptance_rate))
a<-a[order(a$Freq,decreasing=TRUE),]
a<-a %>% slice(1:10)
a

#Top price ranges
b<-data.frame(table(Gracia$price))
b<-b[order(b$Freq,decreasing=TRUE),]
b<-b %>% slice(1:10)
b


#B. Show screenshots of the results. Describe your findings in 1-2 paragraphs.

#On executing the summary() function, there are airbnb’s available in the Gracia neighbourhood which hosts a minimum of 1
#guest to maximum of 16. Median value for bedrooms in the neighbourhood is found to be 1 with a mean value for bathroom to be 1.5
# A total of 38 unique host_neighbourhood is found, out of which a majority of 138 hosts are from Camp d'en Grassot i Gràcia Nova .
#A whooping majority of 807 hosts provide a response within an hour. There are few missing values for response time indicating insufficient data
#collected reasoning being a new host in the neighbourhood. The airbnb’s found in Gracia is Entire home/apt ,Hotel room , Private room and Shared room. Most of them is either Home or a private room. 
#Acceptance rate in this particular neighbourhood is found to be 100% for 429 hosts, followed by 115 and 67 for 99% and 97%. Most common price for an Airbnb in Gracia neighbourhood is found to be $20, followed by $40 and $50.
#There are around 80 airbnb’s with a similar price range in the neighbourhood.


#III. Data Visualization

#A. Using ggplot, create any five plots that help to describe your data. Use five
#unique types of plots to do this. As you do, remember to think about the
#types of variables that you are representing with a particular plot. Think of
#these plots as expository (not exploratory) so be sure to include clear axis
#labels and plot titles.
 

#Plot1
ggplot(Gracia, aes(x = fct_infreq(Gracia$host_neighbourhood))) +
  geom_bar(fill="#99CCFF", color="#0072B2") + 
  ggtitle("Host Neighbourhoods of airbnb's owners in Gracia") +
  xlab("Host Neighbourhood") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Plot2
f<- sample_n(Gracia,20)
ggplot(data = f, aes(x = host_acceptance_rate , y = host_response_rate,colour=host_identity_verified)) + geom_smooth()+theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  geom_point()+labs(title="Scatter plot for host identity verification ",x="Acceptence rate", y="Resonse Rate")

#Plot 3
boxplot(accommodates ~ bedrooms, data = Gracia,col=rainbow(length(unique(Gracia$bedrooms))),  xlab = "Bedrooms",
        ylab = "Accommodates",main = "Accomodation with respect to Number of bedrooms")

#Plot4
table(Gracia$host_is_superhost)

df <- data.frame(
  Host = c("Super Host", "Not a Super Host"),
  value = c(241, 1179)
)

bp<-ggplot(df, aes(x="", y=value, fill=Host))+
  geom_bar(width = 1, stat = "identity")

pie <- bp + coord_polar("y", start=0)+ggtitle("Super host or not?")
pie


#Plot5
Gracia$price = gsub("\\$", "", Gracia$price)
p<-data.frame(table(Gracia$price))
p<-p[order(p$Freq,decreasing=TRUE),]
p<-p %>% slice(1:15)
p

ggplot(data=p, aes(x=Var1, y=Freq, group=1)) +
  geom_line(color="red", linetype = "dashed")+
  geom_point()+theme_bw()+xlab("Price")+ylab("count")+ggtitle("Most common prices of the airbnb in Gracia Neighbourhood")


#Plot6
ggplot(Gracia, aes(x = neighbourhood_cleansed, fill= property_type)) +
  geom_bar() + 
  ggtitle("Property types in Gracia") +
  xlab(" Neighbourhood_cleansed") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#B. Write a two-paragraph description that explains the choices that you made,
#and what the resulting plots show.

#As seen in the above bar plot, majority of the Airbnb owners have their neighbourhood to be Vila de Gràcia, Dreta de l'Eixample ,
#Camp d'en Grassot i Gràcia Nova. These are found to be low -key districts with narrow roads and a ton of co-working spaces in them. 
#Neighbourhoods such as Cihangir ,Cortes, El Camp de l'Arpa del Clot,  El Clot have just one hosts in each of them make them a rare listing in Gracia neighbourhood.
#For a random sample of 20 records, it can be seen in the scatter plot that  hosts with higher acceptance rate have a good response rate.
#Hosts accounts for such records have been verified and seem to be trust worthy . However, we can a single record with 90% acceptance rate and response rate with a 
#profile where the verification is not done.The box plot shown above indicates that maximum number of bedrooms in Gracia neighbourhood is 
#found to be 6. It can accommodate a maximum of 15, minimum of 8  and a median of 12.Whereas, 3 bedrooms can accommodate 1 to 10 people. Interesting
#fact to notice is that a single bedroom can accommodate up to 8 people. I believe there are extra beds to be available/ convertible sofa.
#The above pie chart indicates that, there are approximately 250 hosts to be super host and the rest 1000 something hosts  to be not a super host.
#¼ of the hosts are only verified and are super hosts in the neighbourhood.The above line chart indicates, the most common prices for the Gracia neighbourhood is found to be $20,$40, $50 and $29. 
#The proportional barplot shown above indicates that most of the property types found in la vila de Gracia is found to be rental units and a very few hostings to be a room in a botique hotel.
#Neighbourhood such as el coll contains the property type to be rental unit and private room in a home.


#IV. Mapping

#A. Generate a map of your neighborhood using any R mapping tool. Do any key
#features here seem to stand out? What are a few of the things your map
#shows you about the neighborhood?

x<- c(Gracia$latitude)
y<- c(Gracia$longitude)
df <-data.frame(x,y)

colnames(df) <- c('Latitude', 'Longitude')

#Map1
m<- leaflet(df) %>% addTiles()%>% addCircles(df$Longitude,df$Latitude)
m

#Map2

m1 <- leaflet() %>% addTiles() %>% addCircles(lng=df$Longitude, lat= df$Latitude) %>%
  addProviderTiles("Stamen.Watercolor") %>% 
  addProviderTiles("Stamen.TonerHybrid")
m1 

#On referring to the map above, the airbnb’s are crowded within the Lesseps, Fontana, Gracia, Via Augustia and Joanic. Joanic is a location for metro station
#in the district of Gracia. The Airbnb’s are located around them with an intention to provide easy commute to its guests. Fontana is where magic fountain shows happen in
#Barcelona making it a major tourist spot. There are lot of cafes and restaurants available in the neighbourhood making it perfect to host tourists.

#V. Wordcloud

#A. Using the neighborhood overview column in your dataset, generate a
#wordcloud. What are some terms that seem to be emphasized here?

Gracia1<- Gracia%>%
  dplyr::select(neighborhood_overview)


#Wordcloud for neighbourhood_overview
tidy_s<- Gracia1 %>% unnest_tokens(word, neighborhood_overview) %>% anti_join(stop_words)
l<- tidy_s %>% count(word, sort=TRUE) %>% top_n(200) %>% slice(1:200)
wordcloud2(data=l,size =1.5, color='random-light',backgroundColor="black",shape = 'star')
  
#The neighbourhood name is highlighted many times in the overview column . Gracia and Barcelona seems to stand out along with de and br. 
#It means brother and de means something belongs to in Spanish. I believe this has been used to address the people in the neighbourhood. 
#There seems to be a lot of charm, and culture and joyful mood in the neighbourhood with a lot of families residing there. All the places seems to be easily accessible by walk or metro. 
#All the listings are found to be in the centre of the neighbourhood with several attractions such as boutiques, bars, shops, parks and restaurants. 

