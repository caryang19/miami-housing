#set directory setwd()
#get directory getwd()

#install tidyverse
install.packages('tidyverse')
library('tidyverse')

#import dataset--- make sure you move dataset inside project directory
df <- read_csv("miami-housing.csv")

#view dataset
view(df)

#view head
head(df)
colnames(df)
glimpse(df)
str(df)
#missing data
is.na(df)
sum(is.na(df))


#clean dataset
library(janitor)
library(tidyr)
library(dplyr)
cleaned_df <- df
colnames(cleaned_df)
cleaned_df = cleaned_df %>% clean_names()
colnames(cleaned_df)

count(subset(df, avno60plus >=1))#check and count if there are values greater than 0
count(subset(df, avno60plus < 1))#count values less than 1
check_df <- subset(df, avno60plus >=1)
view(check_df)

#calculate standard deviation, mean, etc 
summary(cleaned_df)

#correlation
cor(cleaned_df)
install.packages("corrplot")
install.packages("ggcorrplot")
library("corrplot")
library("ggcorrplot")
corrplot(cor(cleaned_df), method = "circle")
ggcorrplot(cor(cleaned_df))
cor.test(cleaned_df)
cor(cleaned_df$sale_prc, cleaned_df$spec_feat_val)
plot(cleaned_df, main = "Sample")


#Selecting a sample subset of data
location_df <- cleaned_df %>% select(longitude, latitude)
view(location_df)
cost_df <- cleaned_df %>% select(parcelno, sale_prc, tot_lvg_area, spec_feat_val, age, month_sold)
view(cost_df)

#using subset to select from sample subset
valued_df <- subset(cost_df, sale_prc >= 250000 & age <= 20,select=sale_prc:month_sold)
view(valued_df)

#Merging data sets and/or records
property_df <- merge(valued_df,cost_df)
view(property_df %>% arrange(sale_prc))

#Aggregating records
agg_df <- cleaned_df
attach(agg_df)
aggdf <-aggregate(agg_df, by=list(sale_prc,spec_feat_val),
                  FUN=mean, na.rm=TRUE)
view(aggdf)

#Deriving new attributes
new_df <- data.frame(Date = c("1/3/2021", "2/5/2021"))
new_df <- new_df %>% separate(Date, into = c("Month", "Day", "Year"))
summary(new_df)
new_df <- cleaned_df %>% mutate(month_sold = month.abb[as.numeric(month_sold)])
cleaned_df <- new_df
view(cleaned_df)

#Sorting
cleaned_df %>% arrange(sale_prc)
#sort in ascending order
view(cleaned_df %>% arrange(sale_prc))
#sort in descending order
view(cleaned_df %>% arrange(-sale_prc))

#Deleting and removing 
property_df$parcelno <- NULL
or
remove_df <- subset(property_df, select = -parcelno)
#removing multiple variblaes
remove_df <- subset(property_df, select = -c(age,parcelno))
#delete entire dataset
rm(property_df)
remove(property_df)

#Splitting into training and test data sets
h <- runif(nrow(cleaned_df))
d_df <- cleaned_df[order(h), ]
train_df <- d_df[1:6966, ]#Training dataset to train data
test_df <- d_df[6967:13932, ]#Test dataset to test data performance
str(train_df)
str(test_df)


#price distribution: histplot-sale_price
price_distr <- df$SALE_PRC / 10^6
hist(price_distr, main = paste("Price Distribution"),xlab = "SALE_PRC", ylab = "count" ,col = 'green', border = NULL)

#sort data with sale_prc >2.5*10^6
#plot graph 1 with y-Sale Price x-Land Square Foot
view(train_df)
ggplot(data = train_df, mapping = aes(x = spec_feat_val ,y = sale_prc)) + geom_point(alpha = 0.5, aes(color = month_sold)) #+ facet_wrap(~sale_prc)

#using structure_quality as hue
#plot graph 2 y-sale price x-total living area
ggplot(data = train_df, aes(x = tot_lvg_area ,y = sale_prc)) + geom_point(alpha = 0.5, aes(color = structure_quality))

#using age
#plot graph 3 with y-Sale Price x-Value of
ggplot(data = train_df, aes(x = spec_feat_val ,y = sale_prc)) + geom_point(alpha = 0.5, aes(color = age))

#special features using Tot_lvg_area
#plot graph 4 with y-sale price x-Rail Distance
ggplot(data = train_df, aes(x = rail_dist ,y = sale_prc)) + geom_point(alpha = 0.5, aes(color = tot_lvg_area))


#using Ocean_dist
#plot graph 5 with y-Sale Price x-Distance to Ocean
ggplot(data = train_df, aes(x = ocean_dist ,y = sale_prc)) + geom_point(alpha = 0.5, aes(color = ocean_dist))


#using structure_quality
#plot graph 6 y-Sale Price x-Cntr_Distance,
ggplot(data = train_df, aes(x = cntr_dist ,y = sale_prc)) + geom_point(alpha = 0.5, aes(color = structure_quality))



#cntr_Dist
#plot graph 7 x-Distance to miami central business
ggplot(data = train_df, aes(x = subcntr_di ,y = sale_prc)) + geom_point(alpha = 0.5, aes(color = cntr_dist))

#age, sale_prc and month_sold
ggplot(data = train_df, aes(x = age ,y = sale_prc)) + geom_point(alpha = 0.5, aes(color = month_sold))

#linegraph
ggplot(data = train_df, aes(x = spec_feat_val, y = sale_prc, color = ocean_dist)) +
  geom_line()

ggplot(data = train_df, aes(x = spec_feat_val, y = sale_prc)) +
  geom_line() +
  facet_wrap(facets = vars(month_sold))



#boxplot
ggplot(data = train_df, mapping = aes(x = month_sold , y = sale_prc)) +
  geom_boxplot() 



#linear regression
plot(train_df$sale_prc, train_df$tot_lvg_area)
train.regression <- lm(sale_prc ~ tot_lvg_area, data=train_df)
summary(train.regression)
abline(train.regression, col="blue")

plot(test_df$sale_prc, test_df$tot_lvg_area)
test.regress <- lm(sale_prc ~ tot_lvg_area, data = test_df)
summary(test.regress)
abline(test.regress, col="red")