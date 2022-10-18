library(ggplot2)
library(dplyr)
library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)
library(plyr)
library(readr)
library(GGally)
library(mlbench)
library(caTools)
library(ROCR) 


# dataset:
df<-read.csv("C:/Users/HP/Downloads/FoodChoices.csv")
df

# EXPLORATORY DATA ANALYSIS

# plot1
# Basic histogram
p2<- ggplot(df, aes(x= data$cuisine)) +
geom_histogram()
p2


# plot2
p3<-ggplot(df, aes(x=df$eating_changes_coded1)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  ggtitle("density plot")
p3


# plot3
ggplot(df, aes(x=exercise, y=healthy_feeling )) +geom_col() +
  scale_x_discrete(limits = df$exercise) +
  geom_bar(stat = "identity")


#plot 4
# Histogram
hist(df$calories_day,xlab='calories per day',main = 'histogram'  )

### How often does one cook and relationship with GPA

### Do you take any supplements or vitamins?
# Check for missing values in the "vitamins" column
# There are no missing values in the "vitamins" column
# Count the number of occurences of "yes" and "no"
table(df$vitamins)
# "yes" or 1 occurs 61 times and "no" or 2 occurs 64 times

labels<-c("Yes","No")
x3<-c(63,64)

# Plot the chart
pie(x3,labels)
pie(x3, labels, main = "Supplements taken by college students")

# Add slice percentage
piepercent<- round(100*x3/sum(x3), 1)

#Plot
pie(x3, labels = piepercent, main = "Supplements Taken by College Students",col = rainbow(length(x3)))

# Add a  chart legend by creating additional chart variables.
legend("topright", c("Yes","No"), cex = 0.8,
       fill = rainbow(length(x3)))  ### Final correct version


# 3-D pie chart
# Get the library
library(plotrix)

# Create data for the graph.
x4 <-  c(63,64)
lbl <-  c("Yes","No")


# Plot the chart.
pie3D(x4,labels = lbl,explode = 0.1, main = "Supplements Taken by College Students")
### Final 3-D pie chart

## What are the reasons college students turn to their comfort food?

# Check for missing values in the "comfort_food_reasons_coded" column
# There are two columns present with the same name and there are ambiguities
# in one of them(renamed the column with ambiguities as "comfort_food_reasons_coded11")
# Read the csv file again and run the whole code again

# Now consider the column "comfort_food_reasons_coded"
# Check for missing values in the "comfort_food_reasons_coded" column
colSums(is.na(df))

# There are no missing values in this column

# Count the number of occurences of different answers
table(df$comfort_food_reasons_coded)
# 1: 28,  2: 53,  3: 23, 4: 3, 5: 7,  6: 1,  7: 5, 9: 5


# Create data for the graph.
x5 <-  c(28,53,23,3,7,1,5,5)
lbl5 <-  c("stress","boredom","depression/sadness","hunger","laziness",
           "cold weather","happiness","none")

# Add slice percentage
piepercent<- round(100*x5/sum(x5), 1)

#Plot
pie(x5, labels = piepercent, main = "Reasons that College Students Turn to Their
    Comfort Food",
    col = rainbow(length(x5)))

# Add a  chart legend by creating additional chart variables.
legend("topright", c("stress","boredom","depression/sadness","hunger","laziness",
                     "cold weather","happiness","none"), cex = 0.8,
       fill = rainbow(length(x5)))  ### Final correct version

# 3-D pie chart
# Get the library
library(plotrix)

# Create data for the graph.
x6 <-  c(28,53,23,3,7,1,5,5)
lbl6 <-  c("stress","boredom","depression/sadness","hunger","laziness",
           "cold weather","happiness","none")


# Plot the chart.
pie3D(x6,labels = lbl6,explode = 0.1, main = "Reasons that College Students
      Turn to Their Comford Food")
### Final 3-D pie chart


# Understanding the dataset

#first 6 responses

head(df, 6)

View(df)

describe(df)

glimpse(df)

is.null(df)  #finding missing values of 0 

is.na(df)   #finding missing values of na

sum(is.na(df))  # sum of missing na values in the data set

mean(is.na(df))  # mean of na values in each of the columns

colSums(is.na(df))  # sum of na vlues in each coulmns

str(df)  # description of data set

mean(df$GPA)

# Correlation between Calories_day and weight

cor(x="weight", y="calories_day", method = c("pearson", "kendall", "spearman"))
cor.test(x, y, method=c("pearson", "kendall", "spearman"))

cor(x, y,  method = "pearson", use = "complete.obs")


# Plotting the correlation of the variables minus the outcome variable

ggcorr(df[,-14], name = "corr", label = TRUE)+
  
  theme(legend.position="none")+
  
  labs(title="Correlation Plot")+
  
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))
ggcorr(df)

cordata = df[,c(1,2,5,7,8,9,10,11,13)]
corr <- round(cor(cordata), 1)
corr

cordata = df[,c(14,16,21,2327,31,33,34,35,39)]  
corr <- round(cor(cordata), 1)
corr

#The output above shows the presence of slight linear correlation between the variables Cook and Gender.

#Correlation between GPA and calories_day
cor(df$GPA, df$calories_day)        
cor.test(df$GPA, df$calories_day) 


#Correlation between GPA and calories_day
cor(df$GPA, df$calories_chicken)        
cor.test(df$GPA, df$calories_chicken) 

# Correlation Plot
ggcorr(df[,c(1,2,5,7,8,9,10,11,13)], name = "corr", label = TRUE)+
  
  theme(legend.position="none")+
  
  labs(title="Correlation Plot")+
  
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))


ggcorr(df[,c(14,15,16,17,18,19,20,21,22)], name = "corr", label = TRUE)+
  
  theme(legend.position="none")+
  
  labs(title="Correlation Plot")+
  
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))


ggcorr(df[,c(23,24,25,26,27,28,29,30,31)], name = "corr", label = TRUE)+
  
  theme(legend.position="none")+
  
  labs(title="Correlation Plot")+
  
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))




ggcorr(df[,c(32,33,34,35,36,37,38,39)], name = "corr", label = TRUE)+
  
  theme(legend.position="none")+
  
  labs(title="Correlation Plot")+
  
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))


ggcorr(df[,c(14,16,19,20,21,22,23,24,25,26,27,28)], name = "corr", label = TRUE)+
  
  theme(legend.position="none")+
  
  labs(title="Correlation Plot")+
  
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))


ggcorr(df[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39)], name = "corr", label = TRUE)+
  
  theme(legend.position="none")+
  
  labs(title="Correlation Plot")+
  
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))


ggcorr(df[,c(15,22,23,26,35)], name = "corr", label = TRUE)+
  
  theme(legend.position="none")+
  
  labs(title="Correlation Plot")+
  
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))



ggcorr(df[,c(5,6,7,8,15,23,25,26,35,37,38)], name = "corr", label = TRUE)+
  
  theme(legend.position="none")+
  
  labs(title="Correlation Plot")+
  
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))

#label encoding

table(converted_df$sports)


df$sports<- factor(df$sports, labels = c("sports", "no sports"))
df <- df %>% select(everything()) 
# change sports to numeric between 0 & 1
df$sports <- as.numeric(df$sports)-1
str(df$sports)
head(df,10)
table(df$sports)
str(data)


#Logistic regression

# Installing the package
install.packages("caTools")    # For Logistic regression
install.packages("ROCR")       # For ROC curve to evaluate model

# Loading package
library(caTools)
library(ROCR) 

#make this example reproducible
set.seed(1)

#Use 70% of dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.7,0.3))
train <- df[!sample, ]
test <- df[sample, ] 

#fit logistic regression model
model <- glm(df$sports~df$Gender+df$vitamins, family="binomial", data=test)
summary(model)



