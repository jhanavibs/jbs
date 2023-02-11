library(tidyverse)
library(lubridate)
library(ggplot2)
library(readr)
library(datetime)
library(dplyr)
library(forcats)
library(MASS)
library(arules)
library (naniar)

#Task 1: Hierarchical Clustering

#1. Read the dataset top-500-movies.csv into your R environment.

top_movies<- read_csv("top-500-movies.csv")
View(top_movies)

#2. What are your dataset’s dimensions?

dim(top_movies)

#There are 500 records and 13 variables.

#3. Using any method in R for this purpose, randomly sample 20 rows from the entire
#group. Those are the rows that you’ll use for this clustering. You may set any seed
#value before sampling the data to get these 20 

set.seed(60)
df_sampled <- sample_n(top_movies, 20)

#4. If any rows in your data sample contain NAs, just drop those rows entirely.
anyNA(df_sampled)
sum(is.na(df_sampled))

#5. Create a new variable called age. This new variable should be found by taking 2022 and
#subtracting the year variable from your dataset.

df_sampled <- df_sampled %>%
  add_column(age = 2022- df_sampled$year)

#6. After reading the dataset description, take a look at your data, either with the head()
#function or the View() function. Should your numeric variables be scaled? Why or why
#not? If so, then scale your data’s numeric variables.

head(df_sampled)
View(df_sampled)

list <- c('production_cost', 'domestic_gross', 'worldwide_gross', 'opening_weekend')
new_df <- df_sampled
new_df[list] <- sapply(new_df[list], scale)

#Numeric variables has to be scaled because there is wide range of numeric variables in the dataset. From Domestic gross,production cost,
#worldwide gross and opening weekend to be in millions of dollars where as number of theaters,age and run time to be in hundreds to thousands.
#If the dataset is used without scaling then the clustering model will not be accurate as the larger values will tend to dominate and result in wrong allocation of records to
#clusters.

#7. Build a hierarchical clustering model for the dataset, using any method for
#inter-cluster dissimilarity

new_df<-new_df[,-c(1,3,4,9,10,13)]
distance <- dist(new_df, method = "euclidean")

cluster <- hclust(distance, method = 'complete')

#a. Create and display a dendrogram for your model.
plot(cluster,hang=-1,ann=FALSE)

#b. By looking at your dendrogram, how many clusters do you see here?

#I have tried to identify 5 clusters here. 

#c. Use the cutree function to cut the records into clusters. Specify your desired
#number of clusters, and show the resulting cluster assignments for each movie.

c_tree <- cutree(cluster, k = 5)
c_tree

plot(cluster)
rect.hclust(cluster , k = 5, border = 3:5)

#d. Attach the assigned cluster numbers back to the original dataset. Use groupby()
#and summarize_all() from dplyr to generate per-cluster summary stats, and
#write 2-3 sentences about what you find. What stands out here? What do you 
#notice about any unusual variables or clusters?

 #new_df1 <- cbind(new_df, c_tree)
#colnames(new_df1)[colnames(new_df1) == 'c_tree'] <- 'Cluster'

new_df2 <- cbind(df_sampled, c_tree)
colnames(new_df2)[colnames(new_df2) == 'c_tree'] <- 'Clusters'
new_df2[c('title','Clusters')]

final<- new_df2 %>%
    group_by(Clusters) %>%
    summarise(avg_pcost= mean(production_cost), avg_dgross= mean(domestic_gross),avg_wgross= mean(worldwide_gross),avg_oweekend= mean(opening_weekend))
  final
  
#From the summary stats we can see mean values for all the variables as per cluster. Cluster 4 has largest mean value for production cost,domestic gross, weekly and weekend gross.
#Looks like Transformers, Furious 7,coco,spectre, The peanuts movie and Men in black seem to over power the rest with respect to weekend and weekday collection.
#Cluster 3 has the second highest mean value for production cost because it contains titles like Legend of the Tarzan, BFG.   
  
#e. Make any three simple visualizations to display the results of your clustering
#model. Be sure that the variables depicted in your visualizations are actual
#variables from your dataset. Simple visualizations can include things like
#scatterplots, barplots, histograms, boxplots, etc.

  options(scipen=999)
  ggplot(aes(x = Clusters, y = production_cost), data = new_df2) + geom_bar(stat = "identity", fill="blue",width=.5) + ggtitle('Production_cost of Clusters') + ylab("Production_cost")
  
  plot(new_df2$Clusters,new_df2$domestic_gross, main=" Domestic gross for each cluster", xlab="Clusters", ylab="Domestic gross", col="red", cex=2)
  
  boxplot(worldwide_gross ~ Clusters, data = new_df2, xlab = "Clusters",
          ylab = "worldwide gross", main = "Worldwide gross for each cluster")
  
#f. Choose any movie from among your sample. What cluster did it fall into? Write
#2-3 sentences about the other members of its cluster.

#I have chosen the movie 'Hugo' from the sample. It belongs to cluster 5. Other and the only member of its cluster 
#is 'The Alamo'. I believe both the movies belong to the same cluster as they are historically true.

 # 8. In a previous step, you made the case for standardizing the variables. Now they’re all
# on equal footing… but why might it be problematic to view these variables with equal
#weight? 
  
#The rating/Ranking provided for each movie is bases on several variables in the data frame and not just one.I believe,
#runtime and opening weekened sales have a higher influence or weight age on the rating/ranking than the year , title or other variables.It is mostly because, the production cost has no relation with the rating.
  #Even the  movies of higher rank/Production cost might not have an expected rating.If all the variables are treated equally then the model might get skewed and not provide 
  #appropriate results.

  
#9. Now it’s time to fix that problem! Come up with your own weighting system for these
#variables, and apply it here. Be sure that you are working with a dataframe 
  
  weight_df<- new_df
  weight_df$production_cost<- weight_df$production_cost*150
  weight_df$domestic_gross<- weight_df$domestic_gross*150
  weight_df$worldwide_gross<-weight_df$worldwide_gross*300
  weight_df$opening_weekend<- weight_df$opening_weekend*400
  
  #a. Explain the weighting system in a short paragraph. There is no single *right* or
  #*wrong* way to do this, but your answer to this question should demonstrate
  #that you’ve taken some time to put some thought into it. One sentence per
  #variable is enough to explain the weighting system.
  
#The following assumptions has been made for the weighting system. The total is considered to be 1000.

#Opening weekend<- This variable is assigned with highest value of 400 as its result has a major influence to attract more 
#audience to watch the movie.

#Worldwide gross<- This variable is assigned with a value of 300 considered to be of second highest influence as its result has a influence on larger crowd
#and it provides a wider perspective about the crowd it has attracted in the entire world.

#Domestic gross<- This variable is assigned with the lowest value of 150 as its result doesnot have a huge influence as much as worldwide gross as it provides data on the crowd the movie has attracted in 
  #a particular locality.

#Production cost<- This variable is assigned with the lowest  value of 150 as its result doesnot have a huge influence or provides least insight on the 
  #movie when compared to other variables. Production cost seems to have no direct connection to attract the audience for the movie.
  #Maybe when the audience is aware that its a high production cost movie they might be interested to experience it in theatres. Seems to be tricky to provide a weightage to it.As it mostly reflects the money used to produce a movie and other expenses.
  
#10. Now, generate one more dendrogram, using your newly-rescaled set of variables (be
#sure that you’re not accidentally using the cluster assignments from a previous step as
#  a clustering variable here).
  
  distance1 <- dist(weight_df, method = 'euclidean')
  cluster1 <- hclust(distance1, method = 'complete')
  plot(cluster1)
  

  #a. Once more, provide some description of what you see, and whether there are
#any noteworthy changes between this and the other dendrogram.
  
# I can see that the titles have been shuffled into different clusters now.Record 5 was previously grouped into 
# cluster 3. Now the record alone belongs to a cluster. I can see only the members of last cluster to be somewhat similar with the 
  #previous one.


  #b. Just as you did after the first hierarchical clustering, use the cutree() function to
#cut the records to clusters. Specify your desired number of clusters, and show
#the resulting cluster assignments for each state.

  c_tree1 <- cutree(cluster1, k = 5)
  c_tree1
  plot(cluster1)
  rect.hclust(cluster1 , k = 5, border = 3:5)
  
#I have identified 5 clusters here.
  
  #c. Attach the cluster assignments back to the original dataset. Use groupby() and
#summarize_all() from dplyr to generate per-cluster summary stats, and write
#2-3 sentences about what you find.
  
  new_df3 <- cbind(df_sampled, c_tree1)
  colnames(new_df3)[colnames(new_df3) == 'c_tree1'] <- 'Clusters'
  new_df3[c('title','Clusters')]
  
  final1<- new_df3 %>%
    group_by(Clusters) %>%
    summarise(avg_pcost= mean(production_cost), avg_dgross= mean(domestic_gross),avg_wgross= mean(worldwide_gross),avg_oweekend= mean(opening_weekend))
  final1
  
  
#Considering the new summary stats, we see cluster 3 has the largest value for production cost whereas, previously cluster 4 had the highest value.
#It has the second highest average domestic gross, weekend and weekday gross. As only movies such as Transformers, coco, spectre and aquaman. Three titles has been missing from this cluster when compared to 
#last time. This resulting in reduction of weekend and weekly gross. Second highest value for production cost is seen in cluster 4 and highest domestic,weekend and weekly gross is evident in cluster 4. Furious 7 is the only movie belonging to cluster 4 and seem 
#to overpower the rest. In the previous summary stats Furious 7 was part of cluster 4 and here its stands alone in an individual cluster.
  
  #d. Show the same three types of visualizations that you made in a previous step,
#but with this model version, rather than the original one. What changes do you
#notice? (Please note: the cluster numbers are arbitrary -- to answer this, you need
#         to be willing to look into your data a bit to see what’s going on).

  options(scipen=999)
  ggplot(aes(x = Clusters, y = production_cost), data = new_df3) + geom_bar(stat = "identity", fill="blue",width=.5) + ggtitle('Production_cost of Clusters') + ylab("Production_cost")
  
  plot(new_df3$Clusters,new_df3$domestic_gross, main=" Domestic gross for each cluster", xlab="Clusters", ylab="Domestic gross", col="red", cex=2)
  
  boxplot(worldwide_gross ~ Clusters, data = new_df3, xlab = "Clusters",
          ylab = "worldwide gross", main = "Worldwide gross for each cluster")


#Production cost for Cluster 2 has a significant increase in it and gradual decrease in cluster 4. Cluster 3 Production cost has increased from 600000000 to 800000000.
#Significant increase in domestic gross can be found in Cluster 3 with just one of its record.Worldwide gross for cluster 4 from previous plots and now seems to be extreme. 
#Previously it contained low worldwide gross. Whereas, maximum value can be seen in the current plot.
  
  
  #e. Let’s check back in on that movie that you selected during a previous step.
#Where is that person now, with this new model? Who else is in the same
#cluster? In a few sentences, talk about what changed, and why, regarding this
#star’s cluster assignment
 
#The movie I selected previously is 'The Alamo' and it belongs to cluster 5 now. Cluster 5 also contain Hugo movie.
#Cluster allocation has remained same from last time as 5 also the members allocated in the clusters remain same.
  
  
#Task 2: Text Mining
  
#1. Load the Simpsons dataset into your R environment. Filter the dataset so that it only
#contains the episode that corresponds to 1/10th of your seed value from the previous
#two assignments (i.e. if your seed value was 20, use episode 2; if your seed value was
#400, use episode 40).
  
  
  simpsons<- read_csv("simpsons_script_lines.csv")
  View(simpsons)
  
  #As my seed value for previous assignments was 40 hence episode 4 is used.
  simpsons<- filter(simpsons,episode_id==4)
  

 # 2. What were the most common locations in your episode? Create a barplot that depicts
  #the count values for each of your raw_location_text values. Orient this barplot
  #horizontally, and be sure to order your bars, either from highest to lowest or lowest to
  #highest
  
table(simpsons$raw_location_text)

#Most common locations - Backyard,Moe's Tavern,  Dr. Monroe's Office,Simpson Living Room 


simpsons %>% 
  group_by(raw_location_text) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = reorder(raw_location_text,(-count)), y = count, fill="Red",width=.5)) + 
  theme(plot.title=element_text(face="bold"))+
  geom_bar(stat = 'identity')+ ggtitle("Count values for each location")+ xlab("Location_text") + ylab(" count")+ theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))

#3. Using the select() function from dplyr, generate a new dataframe for your episode that
#only contains the spoken_words column from your episode.

library(dplyr)
library(MASS)
library(tidytext)
library(wordcloud)

simpsons1<- simpsons%>%
dplyr::select(spoken_words)

#a. Next, create a tidy version of your episode text. Using unnest_tokens(word,
#spoken_words) should help you to convert your text into a dataframe in which
#each word occupies its own row.           

tidy_s<- simpsons1 %>% unnest_tokens(word, spoken_words) 
tidy_s

#4. What were the 10 most frequently used words in your episode? Show the code that you
#used to answer this question, along with your results.

tidy_s %>% count(word, sort=TRUE) %>% top_n(10) 

#a. Why is this list of very limited value for any kind of analysis?

#This data can be used to understand the nature of the episode and to come up with a title, the top 10 words can also be 
#used for mpaa rating.
 
# b. Now, use the anti_join() function to remove stopwords. Show the code that you
#used to do this. With the stopwords removed, what are the 10 most common
#words in your episode? Show them here.

tidy_s1 <- simpsons1 %>% unnest_tokens(word, spoken_words) %>% anti_join(stop_words) 
tidy_s1

tidy_s1 %>% count(word, sort=TRUE) %>% top_n(10) %>% slice(1:10)

#i. Do these offer useful clues about your episode?

#Yes, this is helpful as it provides insights of the particular episode. The current one looks like family based.
 
#c. Do this again, but instead, do it with bigrams instead of unigrams.

tidy_s2 <- simpsons1 %>% unnest_tokens(bigram, spoken_words, token="ngrams", n=2) %>% drop_na() 
tidy_s2

#i. How are bigrams different from unigrams?

#Unigrams considers each and every individual word to be independent. Whereas, Bigrams every occurence of a word 
#is dependent on the previous one, therefore it is seen in paits.

#ii. How might bigram analysis yield different results than unigram analysis?
 
#Consider 'sit down' when the words are considered separately it tends to effect the analysis. 'sit' and 'together' tend to intend the dictionary meaning 
#of the word. Whereas, when it is considered together it means seating closely.Hence, to draw meaningful conclusions
#bigram analysis is better.

# d. Write 1-2 sentences that speculate about why it might be useful/interesting to
#see this list of the most frequently-used words from your episode. What could
#someone do with it? Use your imagination and creativity to answer this.

#On analysing the top 10 words from the episode, it looks like a family episode. With all of them at home and watching tv.
#Dad seems to love the tv show and there seems to be some conversations going on.

#5. Generate a wordcloud based on your episode. You may use any wordcloud package in
#R, and you may set this up any way you wish to.

library(reshape2)
raw_text<- simpsons1
tidy_text<- raw_text%>%
  unnest_tokens(output=tokens, input=spoken_words) %>% 
  inner_join(get_sentiments("bing"),by = c("tokens"="word"))%>%
               count(tokens,sentiment, sort=TRUE)%>%
  acast(tokens~sentiment,value.var="n",fill=0)%>%
  comparison.cloud(colors=c("red","green"),max.words = 50)
  
  
#a. What does your wordcloud show you? Describe it in a sentence or two
#The wordcloud that I have generated here tends to identify both positve and negative words in the episode. 
#Positve words tend to outweigh the negative ones. Words such as 'Well, right and good' were frequently used as the font 
#of such words is larger when comapred to others indicating its frequent usage. Negative words such as 'sorry, burn and punch' were also 
# a part of current episode.

#6. Next, let’s do some sentiment analysis. We will use the bing lexicon for this purpose.
#a. What 10 words made the biggest sentiment contributions in your episode?
#Show the code that you used to find this, along with your results.

senti1 <- tidy_s1 %>% inner_join(get_sentiments("bing")) 
senti <- senti1 %>% count(word, sentiment,sort=TRUE) %>% top_n(10)%>% slice(1:10)
senti


#b. Of these top 10 words, how many were positive? How many were negative?

#Out of the 10 words generated, 6 words were positive and 4 words were negative.

#c. In a sentence or two, speculate about what this list suggests about your episode.

#There seems to be an argument going on and seems to have ended in a positive manner. As the words love,nice,excellent and glad are present.
#I assumed a fight to be going on as the words such as punch,burn and hurt are visible.

#7. Create a barplot that shows both the 10 negative words and 10 positive words that
#contributed the most to the sentiment of your episode.

#simpsons2 <- senti1%>% count(word, sentiment, sort=TRUE) %>% top_n(20)
#simpsons3 <- simpsons2 %>% group_by(sentiment) %>% top_n(10) %>% ungroup() %>% mutate(word = reorder(word, n))
#simpsons3
#ggplot(simpsons3, aes(word, n, fill = sentiment))+ geom_col(show.legend = FALSE) + facet_wrap(~sentiment, scales = "free") +  coord_flip() + ggtitle("Positive and Negative Sentiments")+ ylab("Sentiment Count")

word_counts <- tidy_s1 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  slice(1:10)%>%
  ungroup()
word_counts


word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)


#a. Use 2-3 sentences to describe what you see in your barplot
#In the above barplot, the positive words tend to outweigh the negative ones. It means that even though the words such as 
#burns,punch,shock and hurt are present but the postive words such as love,nice,whoa,ready are more frequent in the episode.

#8. Now let’s take a look at how a different sentiment lexicon would view your episode.
#Bring the afinn lexicon into your environment, and join it with the text from your
#episode. Show the step(s) you used to do this.

library(textdata)
s_score <- tidy_s1 %>% inner_join(get_sentiments("afinn"))
View(s_score)

sum(s_score$value)

aggregate(s_score$value, by=list(Category=s_score$value), FUN=sum)
#a. What were the three ‘worst’ words in your episode? What were the three ‘best’
#words in your episode?

#Three worst words- damn,torturing and worst
#Three best words- wonderful,wow and fabulous
 
# b. Sum all the values for your episode. What was the total?
#The total value was 71.
 
# c. What does this sum suggest about your episode? Why might this be helpful...but
#why might it also be incomplete or even misleading?

#The total value is 71. It also shows that the positive values outweigh the negative ones here. Even though there are words with -2 values 
#more than the words with +2 value it doesnot tend to provide a negative outlook on the episode. This can be incomplete because even though the positive outweighs the negative here 
#there are some very harsh words used - torturing and worst it might mislead the user while trying to understand the nature of the episode.


