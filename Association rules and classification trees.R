library(tidyverse)
library(lubridate)
library(ggplot2)
library(readr)
library(datetime)
library(dplyr)
library(forcats)
library(MASS)
library(arules)

data(Groceries)

#1. Describe “Groceries” by answering following questions:
  # What is the class of “Groceries”?
  # How many rows and columns does Groceries contain?

class(Groceries)
dim(Groceries)
summary(Groceries)

#Class- "transactions"
#Dimensions- 9835 rows and 169 columns

#2. Generate an item frequency barplot for the grocery items with support rate greater
#than 7%. Include a screenshot of your results, along with the code you used to do this.
#Orient this plot horizontally, and fill the bars with any color of your choice.

library(RColorBrewer)

frequentItems <- eclat (Groceries, parameter = list(supp = 0.07, maxlen = 15)) 
inspect(frequentItems)

itemFrequencyPlot(Groceries, topN=15, type="absolute", main="Item Frequency Plot for Top Groceries", 
                  horiz = TRUE, col=brewer.pal(6,'Greens'))

#3. Now, create a subset of rules that contain your grocery item . Select any one rule with your item on the left-hand
#side, and any one rule with your item on the right-hand side, and explain them in the
#way you would explain them to your roommate. Remember, every rule has four
#components: support, coverage, confidence, and lift.

#Right hand side
rules_yogurt_rhs <- apriori (data=Groceries, parameter=list (supp=0.001,conf = 0.15,target= "rules", 
                                                             minlen=2), appearance = list (default="lhs",rhs="yogurt"), 
                                                              control = list (verbose=F)) 

rules_yogurt_rhs <- sort (rules_yogurt_rhs, by="confidence", decreasing=TRUE) 

inspect(head(rules_yogurt_rhs))
inspect(head(rules_yogurt_rhs[5]))

#I chose {sausage, pip fruit, sliced cheese} as the left hand side rule.The Support= 0.001220132, means that 0.12% of total products data had
#sausage, pip fruit, sliced cheese in them. Whereas, confidence= 0.8571429,  means that the records containing sausage, pip fruit and sliced cheese
#has a  85.71% chance of having Yogurt too. The coverage of  0.001423488,  means that 0.142% data had both sausage, pip fruit, sliced cheese.
#lift=6.144315 , this means whoever purchases sausage, pip fruit, sliced cheese, they are 6.14 times more likely to purchase Yogurt more than others.

#Left hand side
rules_yogurt_lhs <- apriori (data=Groceries, parameter=list (supp=0.01,conf = 0.15,target= "rules",  minlen=2),
                             appearance = list (default="rhs",lhs="yogurt"), 
                             control = list (verbose=F)) 

rules_yogurt_lhs <- sort (rules_yogurt_lhs, by="confidence", decreasing=TRUE) 

inspect(head(rules_yogurt_lhs))
inspect(head(rules_yogurt_lhs[4]))

#I chose tropical fruit} as the right hand side rule.The Support= 0.02928317, means that 2.92% of total products data had
#tropical fruit in them. Whereas, confidence= 0.2099125,  means that the records containing tropical fruit
#has a  20.99% chance of having Yogurt too. The coverage of  0.1395018,  means that 13.95% data had tropical fruit in them.
#lift=2.000475 , this means whoever purchases tropical fruit, they are 2 times more likely to purchase Yogurt more than others.


#4. In a sentence or two, explain what meaning these rules might have for a store like Star
#Market. What could it do with this information?

#Rules like coverage,support,lift and support can be extremely helpful for stores like star market as they help in coming up with a strategy
#of arranging the groceries next to each other to maximize the sales. This data actually helps the store to understand what the individual 
#actually came to buy at the store Vs what he/she ended up buying.It helps in identifying the strategy that actually helped in increasing the 
#sales.


#5. Using the plot() function in the arulesViz package, generate a scatter plot of any three
#rules involving your grocery item. Include a screenshot of your plot, along with the
#code you used to generate the plot. Describe your results in a sentence or two.

library(arulesViz)

plot(rules_yogurt_lhs [1:3, ], shading="lift",col=rainbow(5))

#The above plot shows us confidence, lift and support of 3 association rules (other vegetables, whole milk, rolls/buns). 
#It shows a upward increase in positive slope.

#6. Again using the plot() function in the arulesViz package, generate a plot for any three of
#your rules. This time, add two more arguments to the function: method="graph",
#engine="htmlwidget". What do you see now? Include a screenshot of your plot, along
#with the code you used to generate the plot. Describe your results in a sentence or two

plot(rules_yogurt_lhs  [1:3], method = 'graph', engine = 'htmlwidget')

#This plot provides a visualization of the three rules I chose in the plot(other vegetables, whole milk, rolls/buns) in the form of a network graph.
#Yogurt is found in the center of the plot and it is the grocery item assigned to me.
#All the three rules are depicted in the plot based on shades of red indicating its priority.The darker color of red ,
#indicates that it is more likely for a person choosing yogurt would also pick up the item assigned in the rule.
#The size of the circle indicates the lift of the rule. shows the lift of the rule.Bigger circle indicating the high value of lift and viseversa.


#Task 2: Classification Tree
#1. Bring the dataset College from the ISLR package into your R environment. Use the ? or
#help() function to learn more about its variables. What does perc.alumni mean?

library(ISLR)
library (naniar)
library (dplyr)
library (ggplot2)
library(fivethirtyeight)
library(caret)
library(e1071)
library(tidyverse)
library(rpart.plot)

help(College)

#perc.alumni indicate the percentage of alumni who donate

#2. Convert perc.alumni into a factor. Set this up so that any college whose perc.alumni is
#above the dataset median is labeled “High” while all others are labeled “Low.”

x<- as.data.frame(College)
median(x$perc.alumni)

x$perc.alumni[x$perc.alumni< 21] <- "Low"
x$perc.alumni[x$perc.alumni != "Low"] <- "High"

unique(x$perc.alumni)

#3. Using your assigned seed value (from Assignment 2), partition your data into training
#(60%) and validation (40%) sets. Show the step(s) that you used to do this.

set.seed(100)
train.index <- sample(c(1:nrow(x)), nrow(x)*0.6)
train.df <- x[train.index, ]
valid.df <- x[-train.index, ]

#4. Build a tree model with this dataset, using perc.alumni as your outcome variable.

model<-rpart(perc.alumni~.,method='class', data=train.df)

#5. Use rpart.plot to display a classification tree that depicts your model.

rpart.plot(model)

#a. Then, adjust the way your model looks. Don’t change anything about the model
#itself, but use a new combination of values for ‘type’ and/or ‘extra’ in rpart.plot
#to change the appearance of the tree.

rpart.plot(model, extra =1, fallen.leaves = TRUE, varlen = 0, cex = 0.6)

#b. Try yet another alternative way of viewing your model. Show your results.

rpart.plot(model, fallen.leaves = FALSE, varlen =0, cex = NULL)

rpart.plot(model, type=2, fallen.leaves = TRUE, varlen = 0, cex = 0.6)

#c. Now, write a couple of sentences about what you saw with each of the three
#graphical versions of your model. Which one do you like best, and why?

#The first plot seems to be very crowded and compact making it difficult to read. On adding fallen leaves to it seems to be more 
#legigible and its easy to read. In the second plot however I have added varlen as 0 to provide full names. Whereas cex helps with 
#size of the text.In third plot I have used fallen leaves as false as it changes the shape of the plot as it helps in visuaising the 
#leaves without confusion.I have also, added an extra plot in the end with type=2 which helps in naming the each leaf node and increasing the node 
#size by little and I like the last plot as it helps me to read the tree diagram easily.

#6. Describe the split that’s created at your tree’s root node. Why is the root node significant?

#The split is on Outstate option at root node, indicating if Outstate>= 12e+3.It has a probability of 49%.
#It basically indicates if the outstate tuition fee is greater than or equal to 12000. It shows 49% of the records had 
#outstate tuition fee to be greater than or equal to 12000. Root node is significant as it indicates the starting 
#point of the tree model. It is important because it acts as the deciding variable that basically splits the tree model.
#Root node is the highest node which doesn't have a Parent node.


#7. Did all the input variables from the dataset appear in your model diagram? If not, why not?

#No, not all the variables appeared on the model diagram as the decision trees choose the best variable for classification 
#based on the Gini index with lower value. This indicates the reason for the variable not to be available on the diagram.

#8. Describe any one rule that your tree generates regarding whether a college will have a
#high perc.alumni rate or a low one. To describe a rule, just trace any path along your
#tree from the root node to a terminal node.

#Assuming the Outstate tution fee is not greater than 12000 now the tree will check if the university was private or public.
#Considering it is a private university the tree model is now going to check if the graduation rate-Grad.Rate >=63.If yes, then 
#it later checks if the student to Faculty ration- S.F ratio <12.If yes, then the perc.alumni is high indicating its highly possible for 
#the pct.alumni to donate.

#9. Now, build another tree model. This time, set a complexity parameter of 0, and use
#minsplit =2, to make the tree as large as possible. Show what your overfit tree looks
#like, using rpart.plot. Don’t worry about interpreting this tree – just show it.

set.seed(100)
model_overfit<-rpart(perc.alumni~.,method='class', data=train.df, cp=0, minsplit=2)
rpart.plot(model_overfit)

#10. Using five-fold cross-validation, determine the optimal complexity parameter (cp) for
#a tree model built with your training data. Demonstrate this by showing your cptable
#and stating which cp value you chose.

set.seed(100)
model_cp<-rpart(perc.alumni~.,method='class',xval=5, data=train.df, cp=0.00)
options(scipen=999)
table<-printcp(model_cp)

class(table)
table<-data.frame(table)
which.min(table$xerror)
which.min(table$xstd)
plotcp(model_cp)

# The complex parameter value that I have chosen the value to be 0.013100437. As it has the lowest error rate.

#11. Generate a new tree model, with the cp value that you found previously.

model_new<- rpart(perc.alumni~.,method='class', data=train.df, cp=0.013100437)

#12. Use rpart.plot to show your new tree model (the pruned tree). Show this with your
#preferred “type” and “extra” settings in rpart.plot.

rpart.plot(model_new, fallen.leaves = FALSE,digits=2, type = 2, box.palette = "Browns")

#12a. Create confusion matrices in R to assess the performance of your huge tree
#against your training and validation sets. How did it perform?

pred_train <- factor(predict(model_overfit, train.df, type="class"))
confusionMatrix(factor(train.df$perc.alumni), factor(pred_train))

pred_valid <- factor(predict(model_overfit, valid.df, type="class"))
confusionMatrix(factor(valid.df$perc.alumni), factor(pred_valid))


#The accuracy of training set is 100%. Whereas for validation set its 65.92%. This indicates that the training set may be overfitting the
#data in the huge tree.

#b. Now, create confusion matrices to assess your optimally-sized tree model.How was this optimally-sized model’s performance
#against the training and validation sets? What happened to the difference between the
#two accuracy values as you went from the huge tree to the optimal one?
 
cp_train <- predict(model_new, train.df, type="class")
confusionMatrix(factor(train.df$perc.alumni), factor(cp_train))

cp_valid <- predict(model_new, valid.df, type="class")
confusionMatrix(factor(valid.df$perc.alumni), factor(cp_valid))

#In the optimized tree, the accuracy of training set is : 79.83% and the validation set is :70.74% which seems to be a decent rate of accuracy.
#The change or difference in accuracy rates for training and validation set has reduced,indicating that the model is not overfitted 
#and it performs well for new data.In optimal one, there is a slight difference in accuracy for training and validation set  from 79% to 70% but for 
#huge tree there is large difference in accuracy from 100% to 65.92% making it not reliable for handling new data.

# c. Why would it be reasonable to expect that the difference between training set accuracy
#and validation set accuracy would decrease when using a pruned tree?

# The pruned tree contains less splits as shown in the plot when compared to the huge tree. It is clearly seen 
#that the data is overfitted and classified at each level in huge tree, it will not be able to make any predictions
#if any new data is added to it. But with increase in splits, its accuracy increases at it is classified at every level.




