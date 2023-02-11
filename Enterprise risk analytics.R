library(extraDistr)
library(FinancialMath)
library(fitdistrplus)
library(ggplot2)
library(psych)

library(readxl)
library(readr)
library(stats)
library(tidyverse)


#OFFICE PROJECT REGRESSION MODEL------------------------------------------------------
#---------------------------DATA PREPARATION---------------------------
#import office buildings' historical data
office<-read_excel("C:/Users/Olivia Cai/Desktop/BU MET-ABA/AD616/Assignments/Final Project/office_historical_data.xlsx")

#rename columns
names(office)[names(office) == 'leading_indicator_of_economic_outlook']<-'indicator'
names(office)[names(office) == 'stated_expected_sales']<-'expected'
names(office)[names(office) == 'realized_sales']<-'actual'

#check the data types
str(office)

#replace the values 
office$indicator[which(office$indicator =='pessimistic')]=-1
office$indicator[which(office$indicator =='neutral')]    = 0
office$indicator[which(office$indicator =='positive')]   = 1

#convert the char to numeric
office$indicator<-as.numeric(office$indicator)

#regression data overview
summary(office)

#fit distributions to the expected sales of office
d_list<-c("lnorm", "norm", "weibull")
m_list<-lapply(d_list, FUN=fitdist, data=office$expected)
gofstat(m_list)
##choose lognormal


#---------------------------VISUALIZATION: CORRELATION---------------------------
#correlation between variables
cor(office[c("expected", "actual")])

#visualization of variables, including distribution of x & y (independent & dependent)
pairs.panels(office[c("expected", "actual")])


#---------------------------MODEL: BUILDING & OPTIMIZING---------------------------
#creating a multi regression model
office_model<-lm(actual ~ expected+
                   factor(indicator), 
                 data=office)

#get the result of our initial model
summary(office_model)


#adjust the model: interaction?
office_model2<-lm(actual~expected+
                         factor(indicator)+
                         expected*factor(indicator),
                  data=office)

#get the result of our adjusted model
summary(office_model2)
##Here we didn't see that expected & indicator will interact to impact the actual sales, so we discard this adjusted model


#try to identify leverage points through hat statistics
hat.plot <- function(x){
  p <- length(coefficients(x))
  n <- length(fitted(x))
  plot(hatvalues(x), main = "Index Plot of Hat Values")
  abline(h = c(2, 3) * p/n, col = "red", lty = 2)
  identify(1:n, hatvalues(x), names(hatvalues(x)))
}
hat.plot(office_model)
##only one point is above 0.25

#try to identify the influence points
cutoff<-4/(nrow(office$actual) - length(office_model$coefficients) - 2)

plot(office_model, which=4, cook.levels=cutoff)
##points of 5, 28, and 31 are influence points

#refit the model after removing 
#the leverage points and influence points
office_model3<-lm(actual ~ expected+
                    factor(indicator), 
                  data = office[-c(5,28,30,31), ])

summary(office_model3) 
##Adjusted R-squared is increased from 0.8485 0.892
##p-values <0.05 (statistic significant)
##The refitted model performs better




#CONDITION SET-UP FOR BOTH PROJECTS' NPV MODEL------------------------------------------------------
#Note: ALL units are in thousands of dollars

#number of trials
n<-10000

#CERTAIN inputs
ir<-.07

##costs
land_1=500
land_2=4375

constr_3=9000
constr_4=10200

sal_mar_4=2300

interest=8636.03

loan_in=38375
loan_out=38375

#UNCERTAIN inputs
constr_1<-rnorm(n, mean=20000, sd=102000)


#---------------------------OFFICE PROJECT NPV MODEL---------------------------
#use lognormal distribution to simulate the expected office sales
rlnorm2<-function(n, mean, sd){
  rlnorm(n, log(mean*(1+sd^2/mean^2)^-0.5), log(1+sd^2/mean^2)^0.5)
}

#set the probability of pessimistic/neutral/positive
prob<-runif(n, 0, 1)

#use the regression model to predict the office sales
for (i in 1:n){
  office_sales<-predict(office_model3, 
                        data.frame(expected =rlnorm2(n, mean(office$expected), sd(office$expected)), 
                                   indicator=case_when(prob<9/32 ~ 0,
                                                       prob<20/32~ 1,
                                                       prob<1    ~-1)))
  
  OP<-data.frame(
    rev_office<-office_sales,
    FCF_1=-land_1+loan_in,
    FCF_2=-land_2-constr_1,
    FCF_3=-constr_3,
    FCF_4=-constr_4-sal_mar_4,
    FCF_5=rev_office-interest-loan_out) %>% 
    mutate(
      C_FCF=FCF_1+FCF_2+FCF_3+FCF_4+FCF_5)
}


#calculate NPV for office project
time<- 1:3
OP$NPV_office<-OP$C_FCF / (1+0.06)^time

#distribution of office project's NPV
hist(OP$NPV_office)

summary(OP$NPV_office)

#mean profit of office project
m1<-mean(OP$NPV_office)
m1

#the percentage that office project will lose money
mean(OP$NPV_office<0)

#95% confidence interval
mean(OP$NPV_office)-qnorm(0.95)*sd(OP$NPV_office)/sqrt(n)
mean(OP$NPV_office)+qnorm(0.95)*sd(OP$NPV_office)/sqrt(n)

#residential NPV visualization
ggplot(data=OP) + 
  geom_histogram(aes(x = NPV_office, y = ..count../n)) + 
  xlab("NPV") + 
  ylab("Probability") + 
  labs(title = "Histogram of office's NPV")


##---------------------------RESIDENTIAL PROJECT NPV MODEL---------------------------
#number of trials, the certain inputs & period 1 construction cost remain unchanged
#a new UNCERTAIN input
res_sales<-rtriang(n, 20000, 130000, 42300)

for (k in 1:n){
  res_sales<-rtriang(n, 20000, 130000, 42300)
  
  RP<-data.frame(
    rev_res<-res_sales,
    FCF_1=-land_1+loan_in,
    FCF_2=-land_2-constr_1,
    FCF_3=-constr_3,
    FCF_4=-constr_4-sal_mar_4,
    FCF_5=rev_res-interest-loan_out) %>%
    mutate(
      C_FCF=FCF_1+FCF_2+FCF_3+FCF_4+FCF_5
    )
}


#calculate NPV for residential project
time<- 1:3
RP$NPV_res<-RP$C_FCF / (1+0.06)^time

#distribution of residential project's NPV
hist(RP$NPV_res)

summary(RP$NPV_res)

#mean profit of residential project
m2<-mean(RP$NPV_res)
m2

#the percentage that residential project will lose money
mean(RP$NPV_res<0)

#95% confidence interval
mean(RP$NPV_res)-qnorm(0.95)*sd(RP$NPV_res)/sqrt(n)
mean(RP$NPV_res)+qnorm(0.95)*sd(RP$NPV_res)/sqrt(n)

#residential NPV visualization
ggplot(data=RP) + 
  geom_histogram(aes(x = NPV_res, y = ..count../n)) + 
  xlab("NPV") + 
  ylab("Probability") + 
  labs(title = "Histogram of residential's NPV")


##---------------------------SOUTHEAST BANK OF TEXAS NPV MODEL---------------------------
#inputs
Rep_principal<- 38375
Interest     <- 8636.03
money_lent   <- 38375

bank<- data.frame(
  FCF_1=-money_lent,
  FCF_2=0,
  FCF_3=0,
  FCF_4=0,
  FCF_5=Rep_principal+Interest) %>% 
  mutate(
    C_FCF=FCF_1+FCF_2+FCF_3+FCF_4+FCF_5)



time<- 1:3
NPV_bank<- bank$C_FCF / (1+0.06)^time
NPV_bank

hist(NPV_bank,breaks= 20)




##---------------------------COMPARISON: OFFICE & RESIDENTIAL---------------------------
#probability that NPV of office project greater than residential project
mean(OP$NPV_office-RP$NPV_res >0)




#SENSITIVITY ANALYSIS: BANK & CDC------------------------------------------------------
#calculate profits through 7% to 11% 
for(ir in c(.07, .08, .09, .10, .11)){
  IT <- 38375*ir+38375*(1+ir)*ir+38375*(1+ir)**2*ir
  
  CDC_office_FCF <- OP$FCF_1+OP$FCF_2+OP$FCF_3+OP$FCF_4+(OP$rev_office....office_sales-IT-38375)
  CDC_res_FCF    <- RP$FCF_1+RP$FCF_2+RP$FCF_3+RP$FCF_4+(RP$rev_res....res_sales-IT-38375)
  bank_FCF       <- IT
  
  
  cat("When interest rate is", ir,", interest will be $", IT, "thousand dollars, 
  cause FCF of $", mean(CDC_office_profit), "thousand dollars in the office project, 
      FCF of $", mean(CDC_res_profit), "thousand dollars in the residential project, and FCF of $",
      mean(bank_profit), "for the bank.", "\n")
}





