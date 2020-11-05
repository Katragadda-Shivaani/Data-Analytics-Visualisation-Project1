#installing all the required packages.

install.packages("readr")
install.packages("DataExplorer")
install.packages("tidyverse")
install.packages("imputeTS")
install.packages("DT")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("dplyr")
install.packages("stringr")
install.packages("viridis")
install.packages("tidyr")
install.packages("plotly")
install.packages("gganimate")
install.packages("ggmap")
install.packages("ggalt")
install.packages("ggthemes")
install.packages("googleVis")
install.packages("leaflet")
install.packages("maps")
install.packages("scales")
install.packages("treemap")
install.packages("tm")
install.packages("viridisLite")
install.packages("highcharter")
install.packages("ggridges")
install.packages("tidytext")
install.packages("ggjoy")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("RColorBrewer")
istall.packages("shiny")
istall.packages("shinydashboard")

#loads and attaches the add-on packages.
library(readr)
library(DataExplorer)
library(tidyverse)
library(imputeTS)
library(DT)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(stringr)
library(viridis)
library(tidyr)
library(plotly)
library(gganimate)
library(ggmap)
library(ggalt)
library(ggthemes)
library(googleVis)
library(leaflet)
library(maps)
library(scales)
library(treemap)
library(tm)
library(viridisLite)
library(highcharter)
library(ggridges)
library(tidytext)
library(ggjoy)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(shiny)
library(shinydashboard)

#************************************************************************************
#Reading the wine130.csv file by using read.csv() function and it is named as wine

wine<-read.csv("F:/semester2/DV1/wine130.csv")

View(wine)#viewing the wine dataset

str(wine)#structure of wine dataset
summary(wine)#summary of wine dataset
dim(wine)#dimensions of the dataset
class(wine)#checking the class of the dataset
head(wine)#head(displays the first few lines of data) of the wine dataset

#Removing the unnecessary column 
wine$X<-NULL
#viewing the new updated dataset
View(wine)

#**************************************************************************************
#DATA EXPLORER Package provides some good functions to know about the data
#Introduce() function will give the outline of the data 
#it tells about the number of rows,columns,missin
introduce(wine)

#plot_str() will plot a graph with all the columns present in the data set and also it will tell how many observations and columns present in the dataset
plot_str(wine)

#plot_intro() will plot a graph which will give the percentage of  discrete columns,continous columns,All missing columns,complete rows and missing observations
plot_intro(wine)

#plot_missing() function shows the percentage of missing values of each column present in the dataset
plot_missing(wine)

#profile_missing() function Analyze missing value profile
profile_missing(wine)

#plot_histogram() function visualize the distributions for all continuous features
plot_histogram(wine)

#plot_correlation() function is used to visualize correlation heatmap for all non-missing features
plot_correlation(wine)
plot_correlation(na.omit(wine), maxcat = 5L)

#create_report(Wine)

#*************************************************************************************


colnames(wine)#column names of the wine dataset

#sapply returns vector or matrix instead of list object
sapply(wine, class)
#glimpse is like a transposed version of print()it makes to see every column in a data frame.
wine %>% glimpse()

#printing the complete cases in the data
cat('The Complete cases in the dataset are : ', sum(complete.cases(wine)))

#printing the total number of null values present in the dataset
cat('Total null values in the wine dataset are : ' , sum(is.na(wine)))

#printing the name of the column with null values
list_na <- colnames(wine)[apply(wine, 2, anyNA)]
cat('Columns with null values : ',list_na)

#finding the column with null values along with the count of null values
nulls <- sapply(wine, function(x) sum(is.na(x)))
cat('null count for each column : ')
nulls

cat('Using imputeTS to fill NA values with mean')

#filling NA values with mean
mean_fill <- wine%>%na_mean()

cat('The Missing values after filling with mean : ', sum(is.na(mean_fill)))
#filling NA with mean
wine <- mean_fill 
cat('Total null values after imputing data with mean :' , sum(is.na(wine)))

#All the missing data is imputated or not  check again whether any missimg data is present in the datase by using plot_missing()
plot_missing(wine)

#******************************************************************************
#EXTRACTING THE YEAR FROM THE TITLE COLUMN

numbers <- lapply(wine$title, function(x) as.numeric(str_extract_all(x, "[0-9]+")[[1]]))

# finding the  year
wine_year <- sapply(numbers, function(x) max(x[x > 1933 & x < 2019]))

# change -Inf to NA
wine_year[wine_year == -Inf & !is.na(wine_year)] <- NA

# histogram of the years
ggplot(data.frame(year = wine_year[wine_year > 1990]), aes(x = year)) +
   geom_histogram(bins = 100) +
   ggtitle('Histogram of vintage') +
   scale_y_continuous('count', labels = scales::dollar_format(prefix = '')) +
   scale_x_continuous('vintage', labels = seq(1991, 2017, 2), breaks = seq(1991, 2017, 2))

# add column the year column to the dataset 
wine$year <- wine_year
View(wine)#Viewing the dataset

# deleting the title column from the dataset 
wine$title <- NULL
View(wine)

#plotting the graph to wheck whether there is any missing values after adding the new column year
plot_missing(wine)

#We do have NA values in the year column so we have to replace them
#printing the total number of null values present in the dataset
nulls <- sapply(wine, function(x) sum(is.na(x)))
cat('null count for each column: ')
nulls

cat('Using imputeTS to fill NA values with mean ')
#fillinf NA values with mean
mean_fill <- wine%>%na_mean()
cat('The Missing values after filling with mean: ', sum(is.na(mean_fill)))
wine <- mean_fill 
cat('Total null values after imputing data with mean: ' , sum(is.na(wine)))

plot_missing(wine)

#****************************
#plotting a scatter plot between points and price
ggplot(wine, aes(x = points, y = price)) + geom_point(stat="identity", color="red") + geom_smooth(method = "lm") + theme_classic()+ggtitle('Points and price')

#plotting scatter plot between points and price by using country as facet_wrap  
ggplot(wine[complete.cases(wine),], aes(x = price, y = points)) + geom_point() + geom_smooth(method = "lm") + facet_wrap(~country)

#plotting boxplots for the prices in different countries
a<-ggplot(sample_n(wine, 10000),aes(country, price, col = country)) + 
   geom_boxplot()+ 
   coord_cartesian(ylim = c(1,100))+ ggtitle('Prices in Different Countries')+
   theme(legend.position="none",
         axis.text.x = element_text(angle = 90))
a
#Applying animation for the plot a
a+transition_time(year) +
   labs(title = "year: {frame_time}")

#plotting boxplots for the prices in different countries along with outliers
b<-ggplot(sample_n(wine, 10000),aes(country, price, col = country)) + 
   geom_boxplot()+ 
   ggtitle('Prices in Different Countries (Outliers Included)')+
   theme(legend.position="none",
         axis.text.x = element_text(angle = 90))
b
#Applying animation for the plot b

b+transition_time(year) +
   labs(title = "year: {frame_time}")

#plotting boxplots for the points in different countries 
c<-ggplot(sample_n(wine, 10000),aes(country, points, col = country)) + 
   geom_boxplot()+ 
   ggtitle('Points in Different Countries')+
   theme(legend.position="none",
         axis.text.x = element_text(angle = 90))
c
#Applying animation for the plot c

c+transition_time(year) +labs(title = "year: {frame_time}")
c+transition_time(price)+labs(title = "price: {frame_time}")


#**********************************************************************
#plotting scatter plot for points and country
d <- ggplot(
   wine, 
   aes(x = points, y=country, size = price, colour = country)) +
   geom_point(show.legend = FALSE, alpha = 0.7) +
   scale_color_viridis_d() +
   scale_size(range = c(2, 12)) +
   scale_x_log10() +
   labs(x = "points", y = "country",title = "Points Vs country")
d
#Applying animation for the plot d

d + transition_time(year)+labs(title = "year: {frame_time}")

# Price and Points correlation for wines 
e<-ggplot(wine, aes(price, points, color = country)) + 
   geom_point(alpha = 0.01) +
   geom_count() +
   theme_minimal() + 
   labs(x = 'Price', y = 'Points', title = "Price Vs points")+
   coord_flip()+
   scale_size_area(max_size = 10)
e
#Applying animation for the plot e

e+transition_time(year)

#*************************************************************

#Price and Points correlation for wines with rating of over 95
#subsetting the data with the point range greater than 95 and assigning to wine2
wine2 <- subset(wine, points > 95)

#scatter plot for the price and points with age greater than 95
f<-ggplot(wine2, aes(price, points, color = country)) + 
   geom_point(alpha = 0.05) +
   geom_count() +
   theme_minimal() + 
   labs(x = 'Price', y = 'Points', title = "Price Vs Ratings")+
   coord_flip()+
   scale_size_area(max_size = 10)
f
#Applying animation for the plot f

f + transition_time(year) +
   labs(title = "Year: {frame_time}")


#wines

#***********which wine***********************
# Best Wine Producing Country - Point  and price.
Best_wine<-wine %>%
   filter(points >=90 & price>=90) %>%
   group_by(country)%>%
   summarise(n = n()) %>%
   arrange(desc(n)) %>%
   head(n = 10)

#printing the data in the form of table
datatable(Best_wine)

#lollipop graph of best Wine Producing Country - Point  and price
Best_wine%>%
   ggplot( aes(x=reorder(country,n), y=n)) + 
      geom_point(size=5, color="red", fill="orange", alpha=0.7, shape=21, stroke=2) +geom_text(aes(label = sprintf("%.1f %%",country))) +
      geom_segment(aes(x=country, 
                       xend=country, 
                       y=0, 
                       yend=n),col="red1") +labs(x = 'country', y = 'Count',subtitle='  Best Wine Producing Country - Point  and price wise') +
      theme(panel.grid.major.x = element_blank(),
            panel.border = element_blank(),
            axis.ticks.x = element_blank())+
   labs(x = 'country', y = 'Count', title = 'Lollipop chart') 
#we can see that US is the best wine producing country followed by France, Italy.
#**************************
#Which Country Makes Coslty Wines.
# Selecting top 20 from table based on Price
Costly_wine_country<-wine %>%
   arrange(desc(price)) %>%
   head(n = 20)%>%
   group_by(country)%>%
   summarise(n = n())%>%
   arrange(desc(n)) 

#printing the data in the form of table
datatable(Costly_wine_country)

#Barplot plot for the country which makes the costly wines.
Costly_wine_country %>%    
   ggplot(aes(x =reorder(country,n), y =  n )) +
   geom_bar(stat='identity',colour="black", aes(fill =country),linetype="dashed", size=0.5) +
   labs(x = 'Country', y = 'Count', title = 'Which Country Makes Coslty Wines') +
   coord_flip() +geom_text(aes(label = sprintf("%.1f %%",country)))
#France is on top producing costly wines.

#*************************************
#Which Country Produce Economic wine.
# Selecting bottom 20 from table based on Price

Economic_wine<-wine %>%
   arrange(price) %>%
   head(n = 20)%>%
   group_by(country)%>%
   summarise(n = n())%>%
   arrange(desc(n)) 

#printing the data in the form of table
datatable(Economic_wine)

#BAR plot with coord_polar for the countries which produce the economic wine
Economic_wine %>%    
   ggplot(aes(x =reorder(country,n), y =  n )) +
   geom_bar(stat='identity',colour="red", aes(fill =country),linetype="dashed",size=1.0) +
   labs(x = 'Country', y = 'Count ', title = 'Which Country Produce Economic wine') +
   coord_polar() +geom_text(aes(label = sprintf("%.1f %%",country)))+
   theme_bw()
#US is where the least cost wine produced followed by Spain, portugal.
#*******************

# Average Price of Wine - Country wise
Average_price_wine<-wine %>%
   select(country,points,price,province,winery) %>%
   group_by(country)%>%
   drop_na(price,country )%>%
   summarize(avg_price = mean(price))%>% 
   arrange(desc(avg_price)) 

#printing the data in the form of table
datatable(Average_price_wine)
#jitter plot for the average price of wine-Country wise
Average_price_wine %>%    
   ggplot(aes(x =reorder(country,avg_price), y =  avg_price )) +
   geom_jitter(width = .5, size=1,col="#FA6900") +
   labs(x = 'country', y = 'Average Price of Wine', title = 'Average Price of Wine - Country wise') +
   coord_polar() + 
   theme_bw()
#*********************************************************************
#varities of wines
varaties_of_wine <- wine %>%
   select(variety) %>%
   filter(!is.na(variety)) %>%
   group_by(variety) %>%
   summarise(count = n())
#counts chart for the varities of wines with coord_polar()
ggplot(varaties_of_wine[varaties_of_wine$count > 500, ], aes(x = reorder(variety, count), y = count)) +
   geom_count(col="tomato3", show.legend=F) +
   labs(x = "varieties") +
   coord_polar()

#counts chart for the varities of wines with coord_flip()
ggplot(varaties_of_wine[varaties_of_wine$count > 500, ], aes(x = reorder(variety, count), y = count)) +
   geom_count(col="tomato3", show.legend=F) +
   labs(x = "varieties") +
   coord_flip()
#******************************************************
#Top Wineries by Count
#filtering the top wineries by count and plotting barplot 
wine %>%
   group_by(winery) %>% 
   filter(n() > 120) %>% 
   mutate(count = n()) %>%
   mutate(avg = mean(points)) %>% 
   ggplot(aes(x = reorder(winery, count))) + 
   geom_bar(aes(fill =winery))+
   coord_polar() + 
   theme_minimal() + 
   labs(x = 'winery', y = 'Count', title = "Most Popular Wineries")
#*******************************************

#which countries get the best reviews on average?
#filtering the countries based on best review of the points
best_review<-wine %>%
   group_by(country) %>%
   summarise(n=n(), AverageReview=mean(points)) %>%
   mutate(rank=min_rank(desc(AverageReview))) %>%
   select(rank, everything()) %>%
   arrange(rank)

##printing the data in the form of table
datatable(best_review)

#jitter plot for the countries with the best reviews on average
best_review %>%    
   ggplot(aes(x =country, y =  rank )) +
   geom_jitter(aes(col=country, size=rank)) + 
   labs(x = 'Country', y = 'Count', title = 'Best review') +
   coord_flip() + 
   theme_bw()


#************************
#*****************************************

#Distribution of Wine Reviews by Top 10 Countries
Top_10_country <- wine %>% 
   group_by(country) %>% 
   summarise(total = n()) %>% 
   arrange(desc(total)) %>% 
   mutate(totpcnt = round(total/ sum(total), digits=7), accum = cumsum(totpcnt))
#barplot for the top 10 countries based on distribution of wine reviews
Top_10_country %>% head(10) %>%
   ggplot( aes(x= factor(country, levels = Top_10_country$country[order(desc(Top_10_country$totpcnt))]), y = total)) +
   geom_col(aes(fill =country)) + 
   geom_text(aes(label = sprintf("%.1f %%", 100*totpcnt), y = total + 1500)) +
   labs(x="Country", y="Total Reviews", title="Distribution of Wine Reviews by Top 10 Countries")

#**************************

##########################################

#which countries produce the best wines with 100 reviews
Wines<-wine %>%
   group_by(country) %>%
   summarise(n=n(), AverageReview=mean(points)) %>%
   filter(n>=100) %>%
   mutate(rank=min_rank(desc(AverageReview))) %>%
   select(rank, everything()) %>%
   arrange(rank)

#printing the data in the form of table
datatable(Wines)

#ggplot with best wine producing country with point and price with 100 reviews
Wines%>%    
   ggplot(aes(x =reorder(country,n), y =  rank )) +
   geom_bar(stat='identity',colour="white",aes(fill =country)) +
   geom_text(aes(label = sprintf("%.1f %%",rank))) +
   labs(x = 'country', y = 'Count', title = 'Best Wine Producing Country - Point  and price wise') +
   coord_flip() +
   theme_bw()

#****************************************************************************************
#wine review score
#the distribution of reviews for countries
wine %>%
   ggplot() +
   geom_freqpoly(aes(points, color=country), binwidth=3) + #y=..density.. is ugly
   labs(x="Wine review score", title="Distribution of wine review scores by country") +
   theme(legend.position="none")+facet_wrap(~country)+scale_color_viridis_d() 


#calculating median number of reviews by country
med_n_rev <- wine %>%
   count(country) %>%.$n %>%median

#after calculating the meadian number of reviews the distribution reviews of the cpuntries are plotted
wine %>%
   group_by(country) %>%
   mutate(n_reviews=n()) %>%
   filter(n_reviews < med_n_rev) %>%
   ggplot() +
   geom_freqpoly(aes(points, color=country), binwidth=3) +
   theme(legend.position="none")+facet_wrap(~country)

#******************************************************************
#plot price VS points for Italy wines
Italy<-wine %>% 
   filter(country=="Italy", !is.na(price)) %>%
   ggplot() +
   geom_point(aes(points, price, color=province), size=1) +
   coord_cartesian(ylim=c(1, 500)) +
   facet_wrap(~province)
Italy
#Applying animation for Italy
Italy+transition_time(points)+labs(title = "points: {frame_time}")

#plot price VS points for french wines
France<-wine %>% 
   filter(country=="France", !is.na(price)) %>%
   ggplot() +
   geom_point(aes(points, price, color=province), size=1) +
   coord_cartesian(ylim=c(1, 500)) +
   facet_wrap(~province)
France
#Applying animation for France
France+transition_time(points)+labs(title = "points: {frame_time}")

#plot price VS points for US wines
US<-wine %>% 
   filter(country=="US", !is.na(price)) %>%
   ggplot() +
   geom_point(aes(points, price, color=province), size=1) +
   coord_cartesian(ylim=c(1, 500)) +
   facet_wrap(~province)
US
#Applying animation for US
US+transition_time(points)+labs(title = "points: {frame_time}")
#******************************************************

#***************Taster analysis*******************
#Grouping by taster and sorting by total reviews and analyzing the reviews distribution
wineTaster <- wine %>% group_by(taster_name) %>% summarise(total=n()) %>% 
   arrange(desc(total)) %>% 
   mutate(totpcnt = round(total/ sum(total), 7), accum = cumsum(totpcnt))

wineTaster

#Factoring the taster name on desc order for organizing the bars on the next plot
wineTaster$taster_name <- factor(wineTaster$taster_name, levels = wineTaster$taster_name[order(-wineTaster$total)])

#print a plot with the tasters and number of reviews
wineTaster %>% ggplot(aes(x= taster_name, y=total)) + geom_col(aes(fill=taster_name)) +
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
   geom_text(aes(label = sprintf("%.f%%", 100*totpcnt), y = total+2000)) +
   labs(x="Wine Taster Name", y="Total Wine Reviews", title="Total Reviews by Wine Taster")
#the graph clearly shows that 20% of our records come from anonymous tasters.

#***********************************

#Countries Review by Top 5 Tasters
temp <- wineTaster %>% filter(taster_name != "") %>% head(5)

#filtering the top five tasters and grouping them by counry
TopTasterCountry <- wine %>% 
   filter(taster_name %in% temp$taster_name) %>%
   group_by(taster_name, country) %>%
   summarise(total = n())

#scatter plot for the top five tasters with respect to the countries
TopTasterCountry %>% 
   ggplot( aes(x=factor(taster_name, levels = wineTaster$taster_name[order(-wineTaster$total)]), 
               y=factor(country, levels= Top_10_country$country[order(Top_10_country$total)]), 
               size = total)) +
   geom_point(col="purple") +
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
   labs(x="Taster", y="Country of Wine Reviewed",title="Countries Reviewed by Top 5 Tasters")
#****************


#******************
#filtering the wine with price less than or equa to 200 and assigned to wine2
wine2 = wine %>% filter(!is.na(price), price <=200)
#A quantile, or percentile, tells you how much of your data lies below a certain value
#Assigning the wine2 price to q1
Q1 = quantile(wine2$price)

#Assingning the prices to each quantile with subclasses such as Very Expensive,Expensive,Average,Inexpensive
wine2 = wine2 %>%
   mutate(pricerange = ifelse(price >= Q1[4], "Very Expensive", 
                              ifelse(price >= Q1[3] & price < Q1[4],"Expensive", 
                                     ifelse(price >= Q1[2] & price < Q1[3], "Average", "Inexpensive"))))
#Assigning the wine2 points to q2
Q2 = quantile(wine2$points)
#Assingning the prices to each quantile with subclasses such as A,B,C,D
wine2 = wine2 %>%
   mutate(rank = ifelse(points >= Q2[4], "A", 
                        ifelse(points >= Q2[3] & points < Q2[4],"B", 
                              ifelse(points >= Q2[2] & points < Q2[3], "C", "D"))))
#assigning the barplot with point category to the variable points
points = ggplot(data=wine2, aes(x=rank)) + 
   geom_bar(fill="Tomato") + 
   labs(x="Assigned Point Category", y="Wine Count", title="Wine Count by Assigned points")
#assigning the barplot with price category to the variable price
price = ggplot(data=wine2, aes(x=pricerange)) + 
   geom_bar(fill="yellow") +
   scale_x_discrete(limits=c("Inexpensive","Average","Expensive","Very Expensive")) +
   labs(x="Assigned Price Category", y="Wine Count", title="Wine Count by Assigned Price")
#library(gridExtra)

#The grid package provides  to create graphical objects ( grobs ), and position them on a page in specific viewports 
#arrange() pair of functions, gridExtra builds upon gtable to arrange multiple grobs on a page.
grid.arrange(points, price, nrow=2)
#***********************

#*******************************************************************************************
#Points Histogram Showing How Wines have been Catagorized by Grade

wines_grades = wine %>%
   mutate(grade = ifelse(points > 91,"Good",ifelse(points >86,"Average","Bad")))

#changing the wines_grades$grade to factor
wines_grades$grade = as.factor(wines_grades$grade)
#Assigning the levels to wines_grades$grade
wines_grades$grade = factor(wines_grades$grade,levels(wines_grades$grade)[c(3,1,2)])

#plotting the histogram for how the wines are categorized b the grades
ggplot(wines_grades, aes(x=points, fill = grade)) + geom_histogram(binwidth = 1, color= 'white') + 
   coord_cartesian(xlim = c(75, 100))+
   labs(title ="Points Histogram Showing How Wines have been Categorized by Grade", x = "Points Given by Reviewer", y = "Number  of Reviews") +
   scale_x_continuous(breaks=seq(75,100, by = 1))
#*******************************************************************

#plotting the Histogram for Wines Under $100
ggplot(wines_grades, aes(x=price, fill = grade)) + geom_histogram(binwidth = 5, color= 'white') + 
   coord_cartesian(xlim = c(0, 100))+
   labs(title ="Price Histogram for WInes Under $100", x = "Price in Dollars", y = "Number  of Reviews") +
   scale_x_continuous(breaks=seq(0,100, by = 10))

#**************************************************************

#Points Given by Reviewer
#grouping the taster_name and assigning to the reviewers
reviewers = wine %>%
   group_by(taster_name) %>%
   count() 
#filtering the reviewers where count greater than 5
frequent_reviewers= reviewers %>%
   filter(n>5)

#filtering the taster name in frequent reviewers and selecting the taster name and points and storing the whole data in Reviewers_Data
Reviewer_Data = wine %>%
   filter(taster_name %in% frequent_reviewers$taster_name) %>%
   select(taster_name,points)
#In Reviewer_Data I group by taster name and the find the mean of the points and arrange them in descending order and store in Reviewer_Standards
Reviewer_Standards = Reviewer_Data %>%
   group_by(taster_name) %>%
   summarise(Mean_Score = mean(points)) %>%
   arrange(desc(Mean_Score)) %>%
   mutate(standard = ntile(Mean_Score,3))

#changing the Reviewer_Standards$standard  as factor
Reviewer_Standards$standard = as.factor(as.character(Reviewer_Standards$standard))
#Assigning the levels to Reviewer_Standards$standard
levels(Reviewer_Standards$standard) = c("High","Medium", "Low")

#by applying left join to wines_grades and Reviewers_Standards by the taster_name and storing in wines_grades
wines_grades = left_join(wines_grades,Reviewer_Standards, by = "taster_name")
#NOw plotting histogram for the points given by reviewer
filter(wines_grades, !is.na(taster_name))%>%
   ggplot(aes(x=points, fill = standard)) + geom_histogram(binwidth = 1, color= 'white') + 
   coord_cartesian(xlim = c(75, 100))+
   labs(title ="Points Histogram", x = "Points Given by Reviewer", y = "Number  of Reviews") +
   scale_x_continuous(breaks=seq(75,100, by = 1))
#*********************************************************************************************

#Distribution of Wine Grade by Price

filter(wines_grades, !is.na(grade))%>%
   ggplot(aes(x=price, fill = grade)) + geom_bar( position = "fill") + 
   coord_cartesian(xlim = c(0, 100))+
   labs(title ="Distribution of Wine Grade by Price", x = "Price of Wine in Dollars", y = "Proportion") +
   scale_x_continuous(breaks=seq(0,100, by = 10))

#*********************************************************************************************
#Distribution of Reviewer Standard by Points Given
filter(wines_grades, !is.na(taster_name))%>%
   ggplot(aes(x=points, fill = standard)) + geom_bar( position = "fill") + 
   coord_cartesian(xlim = c(80, 100))+
   labs(title ="Distribution of Reviewer Standard by Points Given", x = "Score Given by Reviewr", y = "Proportion") +
   scale_x_continuous(breaks=seq(80,100, by = 1))
#************************************************************************
#Grouping the countries in the dataset and storing in to countries variable
countries = wine %>%
   group_by(country) %>%
   count() 
#filtering the countries where count is greater than 500
top_countries = countries %>%
   filter(n>500)
#Countries with the Best and Worst Wine
Best_Wines = wine %>%
   filter(country %in% top_countries$country) %>%
   select(country,points)
#grouping by country and then finding the mean of the points and arrange them in descending order and store in Best_wines

Best_Wines %>%
   group_by(country) %>%
   summarise(Mean_Score = mean(points)) %>%
   arrange(desc(Mean_Score))%>%
   datatable()

#Filtering the best and worst wine countries
Best_Worst = Best_Wines %>%
   filter(country %in% c("Austria","Chile"))

#plotting the best and worst wine countries
ggplot(Best_Worst, aes(points, colour = country, fill = country)) + geom_density(alpha = .4) +
   labs(title ="Point Densities of Top and Bottom Countries with at Least 500 Reviews", x = "Points Given", y = "Density") 
#*****************************

#Top and Bottom Wineries
#Grouping the winery in the dataset and storing in to wineries variable

wineries = wine %>%
   group_by(winery) %>%
   count() 
#filtering the wineries where count is greater than 100
Biggest_Wineries = wineries %>%
   filter(n>100)

#grouping by biggest wineries and then finding then selecting winery and points and then storing in Best_Wineries

Best_Wineries = wine %>%
   filter(winery %in% Biggest_Wineries$winery) %>%
   select(winery,points)

#grouping by winery and then finding the mean of the points and arrange them in descending order and store in Best_wineries

Best_Wineries %>%
   group_by(winery) %>%
   summarise(Mean_Score = mean(points)) %>%
   arrange(desc(Mean_Score))%>%
   datatable()
#Filtering the best and worst wineries

Best_Worst = Best_Wineries %>%
   filter(winery %in% c("Lynmar","Santa Ema"))

#plotting the best and worst wineries

ggplot(Best_Worst, aes(points, colour = winery, fill = winery)) + geom_density(alpha = .4) +
   labs(title ="Point Densities of Top and Bottom Wineries with at least 100 Reviews", x = "Points Given", y = "Density") 
#******************************************

#Top and Bottom Varieties of Wine
#Grouping the varieties in the dataset and storing in to varieties variable

varieties = wine %>%
   group_by(variety) %>%
   count() 

#filtering the varieties where count is greater than 500

Popular_Varieties = varieties %>%
   filter(n>500)

#grouping by popular_varieties and then finding then selecting variety,points and then storing in Variety_Data 

Variety_Data = wine %>%
   filter(variety %in% Popular_Varieties$variety) %>%
   select(variety,points)
#grouping by variety and then finding the mean of the points and arrange them in descending order and store in Variety_Data

Variety_Data %>%
   group_by(variety) %>%
   summarise(Mean_Score = mean(points)) %>%
   arrange(desc(Mean_Score))%>%
   datatable()
#filtering the best and worst variety_Data
Best_Worst = Variety_Data %>%
   filter(variety %in% c("Sangiovese Grosso","Pinot Grigio"))
#plotting the best and worst variety_Data
ggplot(Best_Worst, aes(points, colour = variety, fill = variety)) + geom_density(alpha = .4) +
   labs(title ="Point Densities of Top and Bottom Wine Varieties with at least 500 Reviews", x = "Points Given", y = "Density") 
#*****************************************************
#Tasters with Highest and Lowests Standards
#Grouping the taster_name in the dataset and storing in to reviewers variable

reviewers = wine %>%
   group_by(taster_name) %>%
   count() 

#filtering the reviewers where count is greater than 100

frequent_reviewers= reviewers %>%
   filter(n>100)

#grouping by frequent_reviewers and then finding then selecting taster_name,points and then storing in Reviewer_Data 

Reviewer_Data = wine %>%
   filter(taster_name %in% frequent_reviewers$taster_name) %>%
   select(taster_name,points)
#grouping by taster_name and then finding the mean of the points and arrange them in descending order and store in Reviewer_Data

Reviewer_Data %>%
   group_by(taster_name) %>%
   summarise(Mean_Score = mean(points)) %>%
   arrange(desc(Mean_Score))%>%
   datatable()

#filtering the best and worst Reviewer_Data
Best_Worst = Reviewer_Data %>%
   filter(taster_name %in% c("Alexander Peartree","Matt Kettmann"))
#plotting the best and worst Reviewer_Data

ggplot(Best_Worst, aes(points, colour = taster_name, fill = taster_name)) + geom_density(alpha = .4) +
   labs(title ="Point Densities of Most and Least Critical Reviews with at least 100 Reviews", x = "Points Given", y = "Density") 
#*************************************************
#***********************
#Country wise Graphs
#Best Wine Producing States
wine_producing_states <- wine %>%            
   group_by(province, country)%>%
   summarise(n = n()) %>%
   arrange(desc(n)) %>%
   head(n = 30)%>%
   ggplot(aes(x = reorder(province, n), y = n, ))+
   geom_bar(stat = 'identity',aes(fill=country))+geom_text(aes(label = sprintf("                  %.1f %%",n)))+
   labs(x = 'states', y ='Count', title ='Best Wine Producing States')+
   coord_flip()+
   theme_minimal() 

wine_producing_states
#*************
#Countries make Affordable wines and highly rated 
low_priced_states <- wine%>%
   filter(points > 90)%>%
   arrange(price) %>%
   head(n = 50) %>%
   group_by(country) %>%
   summarise(n = n())%>%
   arrange(desc(n))%>%
   ggplot(aes(x = reorder(country, n), y = n, fill = country))+
   geom_bar(stat = 'identity')+geom_text(aes(label = sprintf("              %.1f %%",n)))+
   labs(x = 'country', y ='Count', title ='Countries make Affordable wines and highly rated ')+
   coord_flip()+
   theme_minimal()
low_priced_states

#***********

#Countries make Costly wines and highly rated 
high_priced_states <- wine%>%
   filter(points > 90)%>%
   arrange(desc(price))%>%
   head(n = 40) %>%
   group_by(country)%>%
   summarise(n = n())%>%
   arrange(desc(n))%>%
   ggplot(aes(x = reorder(country, n), y = n, fill = country))+
   geom_bar(stat = 'identity')+geom_text(aes(label = sprintf("              %.1f %%",n)))+
   labs(x = 'country', y ='Count', title ='Countries make Costly wines and highly rated ')+
   coord_flip()+
   theme_minimal()
#high_priced+transition_states(n)
high_priced_states
##**************************************************

#Exploring product
#variety

#selecting and grouping the variety and then summarizing and filtering the count greater than 90 and storing the value_data
variety_data = wine %>% 
   select(variety) %>%
   group_by(variety) %>% 
   summarise(Count = n())%>%
   filter(Count > 90)
#printing data in form of table
datatable(variety_data)


#building the treemap for the variety_data by using the index variety
tm <- treemap(variety_data, index = "variety",
              vSize = "Count", palette =  plasma(4))
#winery

#selecting and grouping the winery and then summarizing and arranging the count in descending order and storing the variety_data

winery_data = wine %>% 
   select(winery) %>%
   group_by(winery) %>% 
   summarise(Count = n())%>%
   arrange(desc(Count))%>%
   head(100)
#printing data in form of table

datatable(winery_data ) 
#building the treemap for the variety_data by using the index winery

tm <- treemap(winery_data, index = "winery",
              vSize = "Count", palette =  magma(4))
#designation

#selecting and grouping the designation and then summarizing and arranging the count in descending order and storing the variety_data

designation_data = wine %>% 
   select(designation) %>%
   group_by(designation) %>% 
   summarise(Count = n())%>%
   arrange(desc(Count))%>%
   head(100)
#filtering the designation in the variety_data and stored in variety_data1
designation_data1 <- designation_data %>%
   filter(designation != '')
#printing the data in the form of table
datatable(designation_data1)   
#building the treemap for the variety_data by using the index designation

tm <- treemap(designation_data1, index = "designation",
              vSize = "Count", palette =  viridis(4))
#************************************************************************
#COSTLY WINES
#selecting the winery and price and arranging them in descending order and storing in variety_data
costly_data = wine%>% 
   select(winery, price) %>%
   arrange(desc(price))%>%
   head(10)

#library(highcharter)
#high character chart of the winery and price
hchart(costly_data, "column", hcaes(x = winery, y = price, group = winery))


#costly grape designations
#selecting the designation and price and arranging them in descending order and storing in variety_data

costly_designation_data = wine%>% 
   select(designation, price) %>%
   arrange(desc(price))%>%
   filter(designation != '')%>%
   head(10)
#high character chart of the designation and price

hchart(costly_designation_data, "column", hcaes(x = designation, y = price, group = designation))
#*****************************************************

#Top  Wine variety usage
top30wines <- wine %>% 
   select(province,variety)%>%
   group_by(province,variety) %>%
   summarise(count = n()) %>%
   arrange(desc(count))

#Top 30 Wine variety usage
ggplot(top30wines[1:30,], aes(x = province,y = variety)) +
   geom_tile(aes(fill = count)) +
   labs(x ="variety",y="province",title="Top 30 Wine variety usage")+
   theme_bw()+
   theme(axis.text.x = element_text(angle=45,hjust = 1))

# NEXT Top 30 Wine variety usage
ggplot(top30wines[30:60,], aes(x = province,y = variety)) +
   geom_tile(aes(fill = count)) +
   labs(x ="variety",y="province",title="Next top 30 Wine variety usage")+
   theme_bw()+
   theme(axis.text.x = element_text(angle=45,hjust = 1))

#**************************************************
#considering the top 15 variety and storing in the v_vec
v_vec<- wine %>% count(variety, sort = T) %>% head(15) %>% pull(variety)
#plotting the boxplot for the top 15 variety
wine %>% 
   filter(variety %in% v_vec) %>% 
   ggplot(aes(x = reorder(variety, points), y = points))+
   geom_boxplot(aes(fill = variety), show.legend = F, alpha = .7)+
   coord_flip()+
   theme_bw()+
   labs(x = '',
        title = 'Wine Ratings by Varietal')+
   theme(plot.title = element_text(hjust = 0.5, vjust = 2.5))
#density plot for the wine by the tasters
wine %>% 
   ggplot(aes(x =  points, y = , reorder(taster_name, points)))+
   geom_density_ridges( fill = 'dodgerblue', alpha = .4, rel_min_height = 0.005, scale = 1.5)+
   theme_minimal()+
   labs(title = 'Reviewing for the Wine by the tasters',
        x = 'Points',
        y = '')+
   theme_minimal()

#************
#correlation matrix for the countries and variety
wine %>% 
   mutate(country = fct_lump(country, 13),
          variety = fct_lump(variety, 10)) %>% 
   filter(country != 'Germany',
          variety != "Other") %>% 
   count(country, variety) %>%
   ggplot(aes(x = country, y = variety))+
   geom_tile(aes(fill = n), show.legend = F)+
   geom_text(aes(label = n))+
   scale_fill_gradient2(low = 'pink1',  high = 'firebrick3', trans = "log1p")+
   theme_minimal()+
   labs(x = 'Country',
        y = '',
        title = "Which Countries Are Varietals Coming From")+
   theme(axis.text.x = element_text(angle = 45, vjust = .7),
         plot.title = element_text(hjust = 0.5, vjust = 2.5),
         panel.grid = element_blank())

#**********
#filtering that by price
wine<- wine %>% filter(!is.na(price))
#tokenizing the data
wine_unnested<- wine %>% 
   mutate(description = str_to_lower(description)) %>% 
   unnest_tokens(word, description) %>% 
   anti_join(stop_words, by = 'word')
#filtering the data  y word
wine_unnested<- wine_unnested %>% filter(word != 'wine')

#finding the mean point
mean_point<- mean(wine$points) %>% round(2)
#assignng the ordered mean data of points to tib1
tib1<- wine_unnested %>% 
   filter(str_detect(word, "[:alpha:]+")) %>% 
   group_by(word) %>% 
   summarize(mean = mean(points), num = n(), sd = sd(points)) %>% 
   filter(num > 50) %>%
   arrange(desc(mean)) %>% head(14)
#assignng the  mean data of points to tib2

tib2<- wine_unnested %>% 
   filter(str_detect(word, "[:alpha:]+")) %>% 
   group_by(word) %>% 
   summarize(mean = mean(points), num = n(), sd = sd(points)) %>% 
   filter(num > 50) %>%
   arrange(mean) %>% head(14)
#binding both tib1,tib2
full<- rbind(tib1, tib2)
#diverging chart for the words show up in good or poor wine reviews
full %>% 
   mutate( num_words = str_to_title(paste0(word ," ", '(',num,')'))) %>% 
   ggplot(aes(x = reorder(num_words,mean), y = mean))+
   geom_point(aes(colour = mean > mean_point), show.legend = F, size = 3)+
   geom_hline(yintercept = mean_point, lty = 3)+
   geom_segment(aes(y = mean_point, x = num_words, yend = mean, xend = num_words, colour = mean> mean_point), 
                show.legend = F, linetype = 1, size = 1.3)+
   coord_flip()+
   scale_colour_manual(values = c('grey40','slateblue1'))+
   theme_minimal()+
   labs(x = '',
        y= "Average Points",
        title = 'What Words Show up In Good/Poor Wine Reviews',
        subtitle = '# Of times word is used in brackets'
   )

#************
#tokenizing the data
wine_bigrams<- wine %>% 
   mutate(description = str_to_lower(description)) %>% 
   unnest_tokens(word, description, token = 'ngrams', n = 2) %>%
   separate(word, into = c('word1', 'word2'), sep = ' ') %>% 
   filter(!word1 %in% stop_words$word, 
          !word2 %in% stop_words$word) %>% 
   mutate(bigram = paste(word1, word2))

#assignng the  mean data of points to tib3

tib3<- wine_bigrams %>%
   filter(str_detect(bigram, "[:alpha:]+")) %>% 
   group_by(bigram) %>% 
   summarize(mean = mean(points), num = n()) %>% 
   filter(num > 50) %>% 
   arrange(mean) %>% head(14)
#assignng the ordered mean data of points to tib4

tib4<- wine_bigrams %>%
   filter(str_detect(bigram, "[:alpha:]+")) %>% 
   group_by(bigram) %>% 
   summarize(mean = mean(points), num = n()) %>% 
   filter(num > 50) %>% 
   arrange(desc(mean)) %>% head(14)
#storing some keyword in variable funny
funny<- c('green bean', 'apple juice', 'banana flavors', 'cherry cough', 'bath soap', 'cough drop', 'bubble gum', 'pineapple jam')
#assignng the funny data and mean of points to tib5

tib5<-  wine_bigrams %>%
   filter(str_detect(bigram, "[:alpha:]+")) %>% 
   group_by(bigram) %>% 
   summarize(mean = mean(points), num = n()) %>% 
   filter(bigram %in% funny)


#binding the tib3,tib4,tib5 to full2
full2<- rbind(tib3, tib4, tib5)
#diverging chart for the Simple/Sweet Wines Dont Go Over Well With Reviewers
full2 %>% 
   mutate( num_words = str_to_title(paste0(bigram ," ", '(',num,')'))) %>% 
   ggplot(aes(x = reorder(num_words, mean), y = mean))+
   geom_point(aes(colour = mean > mean_point), show.legend = F, size = 3)+
   geom_hline(yintercept = mean_point, lty = 3)+
   geom_segment(aes(y = mean_point, x = num_words, yend = mean, xend = num_words, colour = mean> mean_point), 
                show.legend = F, linetype = 1, size = 1.3)+
   coord_flip()+
   scale_colour_manual(values = c('grey40','slateblue1'))+
   theme_minimal()+
   labs(x = '',
        y= "Average Points",
        title = 'Simple/Sweet Wines Dont Go Over Well With Reviewers.'
   )
#****************************
#assigning some words and v_vec to v_vec2

v_vec2<- c(v_vec, 'merlots', 'zin','zins','zinfandels', 'sb', 'cab', 'cabs', 'chards', 'chard', 'chardonnays', 'barolo', 'pinot','noirs',
           'reislings', 'syrahs', 'blanc' )
#filtering the data and creating bar plots for the unique words in different wine variety using tf-ldf
wine_unnested %>% 
   filter(variety %in% v_vec) %>% 
   count(variety, word) %>% 
   bind_tf_idf(variety, word, n) %>%
   filter(n > 25,
          !word %in% str_to_lower(v_vec2)) %>% 
   arrange(desc(tf_idf)) %>% 
   group_by(variety) %>% 
   top_n(10, tf_idf) %>% 
   ungroup() %>% 
   mutate(word = reorder_within(word, tf_idf, variety)) %>%  
   ggplot(aes(x = word, y = tf_idf, fill = variety))+
   geom_col(show.legend = F, colour ='black', size = .4)+
   scale_x_reordered()+
   scale_fill_brewer(palette = 'Paired')+
   coord_flip()+
   facet_wrap(~ variety,scales = 'free', ncol = 4)+
   labs(title= 'Looking at Unique Words in Different Wine Varietals Using Tf-Idf',
        subtitle = 'Word used minimum 25 Times',
        x = "Word",
        y = "Tf-Idf Value")
#************************************
#assigning some words and v_vec to v_vec2

v_vec2<- c(v_vec, 'merlots', 'zin','zins','zinfandels', 'sb', 'cab', 'cabs', 'chards', 'chard', 'chardonnays', 'barolo', 'pinot','noirs',
           'reislings', 'syrahs', 'blanc' )
#filtering the data and creating bar plots for the unique words in different wine variety using tf-ldf

wine_unnested %>% 
   filter(variety %in% v_vec) %>% 
   count(variety, word) %>% 
   bind_tf_idf(variety, word, n) %>%
   filter(n > 25,
          !word %in% str_to_lower(v_vec2)) %>% 
   arrange(desc(tf_idf)) %>% 
   group_by(variety) %>% 
   top_n(10, tf_idf) %>% 
   ungroup() %>% 
   mutate(word = reorder_within(word, tf_idf, variety)) %>%  
   ggplot(aes(x = word, y = tf_idf, fill = variety))+
   geom_col(show.legend = F, colour ='black', size = .4)+
   scale_x_reordered()+
   scale_fill_brewer(palette = 'Paired')+
   coord_flip()+
   facet_wrap(~ variety,scales = 'free', ncol = 4)+
   labs(title= 'Looking at Unique Words in Different Wine Varietals Using Tf-Idf',
        subtitle = 'Word used minimum 25 Times',
        x = "Word",
        y = "Tf-Idf Value")


#filtering unknown names of taster_name
top_tasters<- wine %>% filter(taster_name != 'Unknown') %>% 
   count(taster_name, sort = T) %>% head(9) %>% pull(taster_name)
#filtering the data and creating bar plots for the  words the tasters like using
wine_unnested %>% 
   filter(taster_name %in% top_tasters) %>% 
   count(taster_name, word) %>% 
   bind_tf_idf(taster_name, word, n) %>%
   filter(n > 100) %>% 
   arrange(desc(tf_idf)) %>% 
   group_by(taster_name) %>% 
   top_n(8, tf_idf) %>% 
   ungroup() %>% 
   mutate(word = str_to_title(word),
          word = (reorder_within(word, tf_idf, taster_name))) %>%  
   ggplot(aes(x = word, y = tf_idf, fill = taster_name))+
   geom_col(show.legend = F, colour ='black', size = .4)+
   scale_x_reordered()+
   scale_fill_brewer(palette = 'Paired')+
   coord_flip()+
   facet_wrap(~ taster_name ,scales = 'free', ncol = 3)+
   labs(title= 'What Words Do the Tasters Like Using?',
        subtitle = 'Word used Minimum 100 Times',
        x ='Word',
        y= 'Tf-Idf Value')  
#***************************************


########################################## WINERY STAT ######################################
# mode function
get_mode <- function(vec) {
   uniq_vec = unique(vec)
   uniq_vec[which.max(tabulate(match(vec, uniq_vec)))]
}

# mean prices for each class
wine_class <- wine %>%
   group_by(points) %>%
   summarise(
      segment_price = mean(price, na.rm = T))

# merging
wine <- merge(wine, wine_class, by = 'points')

# scaled prices and points
wine <- wine %>%
   mutate(
      scaled_points = as.numeric(scale(points)),
      scaled_price = as.numeric(scale(segment_price - price))
   )

# winery statistics
winery_stat <- wine %>%
   group_by(winery) %>%
   summarise(
      no_of_wines = length(winery),
      country = get_mode(country),
      best_designation = designation[min(which.max(scaled_points + scaled_price))],
      points_mean = mean(points),
      points_sd = sd(points),
      points_min = min(points),
      points_max = max(points),
      points_weighted_price = mean(
         scaled_points +
            scaled_price,
         na.rm = T),
      no_of_wines_with_price = sum(!is.na(price)),
      price_mean = mean(price, na.rm = T),
      province = get_mode(province[country == get_mode(country)]),
      region_1 = get_mode(
         region_1[province == get_mode(province[country == get_mode(country)])]),
      region_2 = get_mode(
         region_2[region_1 == get_mode(
            region_1[province == get_mode(province[country == get_mode(country)])])]),
      best_title = title[min(which.max(scaled_points + scaled_price))],
      best_variety = variety[min(which.max(scaled_points + scaled_price))],
      best_points = points[min(which.max(scaled_points + scaled_price))],
      best_price = price[min(which.max(scaled_points + scaled_price))]
   ) %>%
   filter(no_of_wines > 9 & no_of_wines_with_price > 9) %>%
   arrange(desc(points_weighted_price)) %>%
   top_n(10, points_weighted_price)

# sorting
winery_stat$winery <- factor(winery_stat$winery, levels = rev(winery_stat$winery))
# plot 1
ggplot(winery_stat, aes(x = winery, y = points_weighted_price, fill = points_weighted_price)) +
   geom_bar(stat = 'identity') +
   coord_flip() +
   ggtitle('Best winaries (by normalized prices and points)')

# plot 2
ggplot(winery_stat, aes(x = winery, y = no_of_wines, fill = no_of_wines)) +
   geom_bar(stat = 'identity') +
   coord_flip() +
   ggtitle('Best winaries - number of different wines') +
   scale_fill_continuous(type = 2)

# plot 3
ggplot(winery_stat, aes(x = winery, y = price_mean, fill = country)) +
   geom_bar(stat = 'identity') +
   coord_flip() +
   ggtitle('Best winaries - mean prices') +
   scale_fill_discrete()

# plot 4
ggplot(winery_stat, aes(x = winery, y = points_mean - 80, fill = country)) +
   geom_bar(stat = 'identity') +
   coord_flip() +
   ggtitle('Best winaries - mean points') +
   scale_fill_discrete() +
   scale_y_continuous('points_mean', labels = seq(80, 100, by = 2), breaks = seq(0, 20, by = 2))


########################################## REGION 1 #########################################

# summarise
regions <- wine %>%
   group_by(region_1) %>%
   summarise(cnt = length(points)) %>%
   arrange(desc(cnt))

# arrange
regions$region_1 <-
   factor(
      regions$region_1,
      levels = rev(regions$region_1))

# plot
ggplot(regions[2:11, ], aes(x = region_1, y = cnt)) +
   geom_bar(stat = 'identity',fill="yellow") +
   coord_flip() +
   ggtitle('Number of different wines by region') +
   scale_y_continuous('count', labels = scales::dollar_format(prefix = ''))


########################################## REGION 2 #########################################

# summarise
regions <- wine %>%
   group_by(region_2) %>%
   summarise(cnt = length(points)) %>%
   arrange(desc(cnt))

# arrange
regions$region_2 <-
   factor(
      regions$region_2,
      levels = rev(regions$region_2))

# plot
ggplot(regions, aes(x = region_2, y = cnt)) +
   geom_bar(stat = 'identity',fill="purple") +
   coord_flip()

######################################### TASTER NAME #######################################

# summarise
tasters <- wine %>%
   group_by(taster_name) %>%
   summarise(cnt = length(points)) %>%
   arrange(desc(cnt))

# arrange
tasters$taster_name <-
   factor(
      tasters$taster_name,
      levels = rev(tasters$taster_name))

# plot
ggplot(tasters[2:11, ], aes(x = taster_name, y = cnt)) +
   geom_bar(stat = 'identity',fill="orange") +
   coord_flip() +
   ggtitle('Number of wine reviews by tasters') +
   scale_y_continuous('count', labels = scales::dollar_format(prefix = ''))



#*****************************************
#grouping the top 30 variety

top_30df <- wine %>%
   group_by(variety) %>%
   summarise(count = n())%>%
   arrange(desc(count))

top_30df <- top_30df[1:30,1:2]

top_30 <- top_30df$variety  

#subsetting the the top 30 variety
new_data <- subset(wine, variety %in% top_30)
# Create a new variable to indicate whether wine is white or red
new_data$wine_type <- ifelse(new_data$variety == "Chardonnay" | new_data$variety == "Riesling" | new_data$variety == "Sauvignon Blanc" | new_data$variety == "White Blend" | new_data$variety == "Sparkling Blend" | new_data$variety == "Pinot Gris" | new_data$variety == "Champagne Blend" | new_data$variety == "GrÃ¼ner Veltliner" | new_data$variety == "Pinot Grigio" | new_data$variety == "Portuguese White" | new_data$variety == "Viognier" | new_data$variety == "GewÃ¼rztraminer" | new_data$variety == "GewÃ¼rztraminer", "White Wine", "Red Wine")

new_data$wordcount <- sapply(gregexpr("\\S+", new_data$description), length)

#scatter plot for the price and poits for red and white wines

new_data %>%
   group_by(variety, wine_type) %>%
   summarise(n=n(),
             avg_score = mean(points),
             avg_price = mean(price)) %>%
   ggplot(aes(x=avg_price, y= avg_score, size = n, colour = wine_type))+
   geom_point()+
   scale_color_manual(values = c("#CC3300", "#FFCC00"))

#density plot for red and white wines
ggplot(data = new_data, aes(x= wordcount, y= wine_type, fill = wine_type))+
   geom_density_ridges ()+
   labs(x = "Word Count", title = "Distribution of word count of description")+
   scale_fill_cyclical(values = c("#CC3300", "#FFCC00"))


#boxplot for wordcount by variety
ggplot(data = new_data, aes(x=variety, y=wordcount))+
   geom_boxplot(col="red")+
   coord_flip()+
   labs(title = "Wordcount Distribution by Variety", x= "Variety", y= "Word Count")


#Distribution of points awarded for the most reviewed
#How are the points a wine receives distributed for each of the varieties?

p1 <- ggplot(data = subset(new_data, wine_type == "Red Wine"), aes(x=points, y=variety))+
   geom_joy2(bandwidth = 0.539, fill = "#CC3300")

p2 <- ggplot(data = subset(new_data, wine_type == "White Wine"), aes(x=points, y=variety))+
   geom_joy2(bandwidth = 0.539, fill = "#FFCC00")

grid.arrange(p1, p2, nrow = 1)


#Distribution of price awarded for the most reviewed
#How does the price of a wine vary given its variety?

p3 <- ggplot(data = subset(new_data, wine_type == "Red Wine"), aes(x=log(price), y=variety))+
   geom_joy2(bandwidth = 0.103, fill = "#CC3300")

p4 <- ggplot(data = subset(new_data, wine_type == "White Wine"), aes(x=log(price), y=variety))+
   geom_joy2(bandwidth = 0.103, fill = "#FFCC00")

grid.arrange(p3, p4, nrow=1)



#relationship between the points given and the wine's price?
p5 <- ggplot(data = subset(new_data, wine_type == "Red Wine"), aes(x=points, y= price))+
   geom_point(colour="#CC3300")+
   scale_y_log10()+
   geom_smooth()

p6 <- ggplot(data = subset(new_data, wine_type == "White Wine"), aes(x=points, y= price))+
   geom_point(colour="#FFCC00")+
   scale_y_log10()+
   geom_smooth()

grid.arrange(p5, p6, nrow=1)


#WORD ANALYSIS

#Pinot Noir

#---------- Pinot Noir Word cloud ----------#

pinot <- subset(new_data, variety == "Pinot Noir")

descriptors <- Corpus(VectorSource(pinot$description))
head(descriptors)

# Convert the text to lower case
descriptors <- tm_map(descriptors, content_transformer(tolower))
# Remove numbers
descriptors <- tm_map(descriptors, removeNumbers)
# Remove english common stopwords
descriptors <- tm_map(descriptors, removeWords, stopwords("english"))
# Remove own stop word
# specify stopwords as a character vector
descriptors <- tm_map(descriptors, removeWords, c("wine", "pinot", "noir", "drink", "flavors")) 
# Remove punctuations
descriptors <- tm_map(descriptors, removePunctuation)
# Eliminate extra white spaces
descriptors <- tm_map(descriptors, stripWhitespace)


# Build a term-document matrix
dtm <- TermDocumentMatrix(descriptors)
dtm_mat <- as.matrix(dtm)
v <- sort(rowSums(dtm_mat),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# Generate the Word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))



#---------- Cabernet Sauvignon Word cloud ----------#

cabsav <- subset(new_data, variety == "Cabernet Sauvignon")

descriptors <- Corpus(VectorSource(cabsav$description))


# Convert the text to lower case
descriptors <- tm_map(descriptors, content_transformer(tolower))
# Remove numbers
descriptors <- tm_map(descriptors, removeNumbers)
# Remove english common stopwords
descriptors <- tm_map(descriptors, removeWords, stopwords("english"))
# Remove own stop word
# specify stopwords as a character vector
descriptors <- tm_map(descriptors, removeWords, c("wine", "drink", "cabernet", "flavors")) 
# Remove punctuations
descriptors <- tm_map(descriptors, removePunctuation)
# Eliminate extra white spaces
descriptors <- tm_map(descriptors, stripWhitespace)

# Build a term-document matrix
dtm <- TermDocumentMatrix(descriptors)
dtm_mat <- as.matrix(dtm)
v <- sort(rowSums(dtm_mat),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)


# Generate the Word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))



#---------- Chardonnay Noir Word cloud ----------#

chardy <- subset(new_data, variety == "Chardonnay")

descriptors <- Corpus(VectorSource(chardy$description))

# Convert the text to lower case
descriptors <- tm_map(descriptors, content_transformer(tolower))
# Remove numbers
descriptors <- tm_map(descriptors, removeNumbers)
# Remove english common stopwords
descriptors <- tm_map(descriptors, removeWords, stopwords("english"))
# Remove own stop word
# specify stopwords as a character vector
descriptors <- tm_map(descriptors, removeWords, c("wine", "chardonnay", "drink", "flavors")) 
# Remove punctuations
descriptors <- tm_map(descriptors, removePunctuation)
# Eliminate extra white spaces
descriptors <- tm_map(descriptors, stripWhitespace)


# Build a term-document matrix
dtm <- TermDocumentMatrix(descriptors)
dtm_mat <- as.matrix(dtm)
v <- sort(rowSums(dtm_mat),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)


# Generate the Word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
##Extract features based on description and bag of words

wine$fruity <- str_detect(wine$description, 'fruit|fruity')
wine$cherry <- str_detect(wine$description, 'cherry')
wine$chocolate <- str_detect(wine$description, 'chocolate')
wine$blackberry <- str_detect(wine$description, 'blackberry')
wine$plum <- str_detect(wine$description, 'plum')
wine$tannins <- str_detect(wine$description, 'tannins')
wine$black <- str_detect(wine$description, 'black')
wine$pepper <- str_detect(wine$description, 'pepper|spice')
wine$delicious <- str_detect(wine$description, 'delicious')
wine$sweet <- str_detect(wine$description, 'sweet')
wine$smoke <- str_detect(wine$description, 'smoke')
wine$best <- str_detect(wine$description, 'notable|notably|elegant|wellmade|great|structured|delicious')
wine$best2 <- str_detect(wine$description, 'Impeccable|morelsh|dazzling|incisive|dazzles|exquisitely|superbly|glorious|thrilling|stunner')
wine$bad <- str_detect(wine$description, 'dilute|watery|bland|pickled|weedy|diluted|burgers|quaffer|raspy|forced')
wine$acidity <- str_detect(wine$description, 'acidity')


#Important features in description related to Points
p1 <- ggplot(wine, aes(fruity, points, fill = fruity))+
   geom_boxplot()

p2 <- ggplot(wine, aes(cherry, points, fill = cherry))+
   geom_boxplot()

p3 <- ggplot(wine, aes(chocolate, points, fill = chocolate))+
   geom_boxplot()

p4 <- ggplot(wine, aes(blackberry, points, fill = blackberry))+
   geom_boxplot()

p5 <- ggplot(wine, aes(plum, points, fill = plum))+
   geom_boxplot()

p6 <- ggplot(wine, aes(tannins, points, fill = tannins))+
   geom_boxplot()

p7 <- ggplot(wine, aes(black, points, fill = black))+
   geom_boxplot()

p8 <- ggplot(wine, aes(pepper, points, fill = pepper))+
   geom_boxplot()

p9 <- ggplot(wine, aes(delicious, points, fill = delicious))+
   geom_boxplot()

p10 <- ggplot(wine, aes(sweet, points, fill = sweet))+
   geom_boxplot()

p11 <- ggplot(wine, aes(smoke, points, fill = smoke))+
   geom_boxplot()

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11)


#california review

wine %>%
   dplyr::filter(price <= 100) %>% # remove price outliers
   dplyr::select(points,variety,province) %>%
   na.omit() ->
   wine_under_100

#  picking up california as province
california <- wine_under_100$province == "California"
#picking up pinot Noir as variety
pinot <- wine_under_100$variety == "Pinot Noir"

#Assigining the subclasses
wine_under_100$california <- "Everywhere Else"
wine_under_100$california[california] <- "California"
wine_under_100$pinot <- "Other Wines"
wine_under_100$pinot[pinot] <- "Pinot Noir"
#histogram for california and elsewhere
ggplot2::ggplot(data = wine_under_100) + 
   ggplot2::geom_histogram(mapping = ggplot2::aes(x = points), binwidth=1,col="blue",fill="red1") + 
   ggplot2::facet_wrap(~ california) +
   ggplot2::ggtitle("Histogram of Points for California vs Elsewhere")

#histogram for California x Pinot Noir vs not those
ggplot2::ggplot(data = wine_under_100) + 
   ggplot2::geom_histogram(mapping = ggplot2::aes(x = points), binwidth=1,col="red1",fill="blue") + 
   ggplot2::facet_wrap(~ pinot + california, nrow = 2) +
   ggplot2::ggtitle("Histogram of Points for California x Pinot Noir vs not those") 

#******************************

#*************************MICHIGAN REVIEW
#filtering the country ad province
Michigan_Wines <- wine %>%
   filter(country == "US") %>%
   filter(province == "Michigan") %>%
   filter(!is.na(price)) 

#grouping by region_1 and arranfing in descending order

Michigan_Wines_Province <- Michigan_Wines %>%
   group_by(region_1) %>%
   summarise(total = n()) %>%
   arrange(desc(total)) %>%
   mutate(pcnt = round(100*(total/sum(total)), digits = 5), accum = cumsum(pcnt))

print(Michigan_Wines_Province)
head


#Visualizing the top 3 regions in Michigan rated.
Michigan_Wines_Province %>%
   head(3) %>%
   ggplot(aes(x = reorder(region_1, -pcnt), y = total, FILL = region_1)) + 
   geom_bar(aes(fill = region_1), stat = "identity") + 
   labs(x = "\n wines, Top 3 \n") + labs(y = "\n Total \n") + 
   labs(title = "\n Variety of top 3 regions in Michigan rated") + 
   geom_text(aes(label = sprintf("%.1f %%", pcnt))) + 
   theme_solarized()


#Most common wine varieties in Michigan (top 3)
#What are the most prominent wines being reviewed here

Michigan_Varieties <- Michigan_Wines %>%    
   group_by(variety) %>%
   summarise(total = n()) %>%
   arrange(desc(total)) %>%
   mutate(pcnt = round(100*(total/sum(total)), digits = 5), accum = cumsum(pcnt))

Michigan_Varieties %>%
   head(3) %>%
   ggplot(aes(x = reorder(variety, -pcnt), y = total, FILL = variety)) + 
   geom_bar(aes(fill = variety), stat = "identity") + 
   labs(x = "\n wines, Top 3 \n") + labs(y = "\n Total \n") + 
   labs(title = "\n variety of wines that are Most common wine  in Michigan") + 
   geom_text(aes(label = sprintf("%.1f %%", pcnt))) + 
   theme_solarized()


#Tiering the us(Michigan) wines by ratings.
#What i will be doing is dividing it into 2 categories:

#  Excellent (90 - 100)
#Good (80-89)
Excellent <- Michigan_Wines %>%
   group_by(points) %>%
   filter(points >= 90)

Good <- Michigan_Wines %>%
   group_by(points) %>%
   filter(points <= 89)

#varieties of Michigan wines rated
#Starting off with the Excellent varieties.

Excellent_varieties <- Excellent %>%
   group_by(variety) %>%
   summarise(total = n()) %>%
   arrange(desc(total)) %>%
   mutate(pcnt = round(100*(total/sum(total)), digits = 5), accum = cumsum(pcnt))

Excellent_varieties %>%
   head(3) %>%
   ggplot(aes(x = reorder(variety, -pcnt), y = total, FILL = variety)) + 
   geom_bar(aes(fill = variety), stat = "identity") + 
   labs(x = "\n Excellent variety wines Top 3 \n") + labs(y = "\n Total \n") + 
   labs(title = "\n Variety") + 
   geom_text(aes(label = sprintf("%.1f %%", pcnt))) + 
   theme_solarized()
#then getting into the rest of the varieties

Good_varieties <- Good %>%
   group_by(variety) %>%
   summarise(total = n()) %>%
   arrange(desc(total)) %>%
   mutate(pcnt = round(100*(total/sum(total)), digits = 5), accum = cumsum(pcnt))

Good_varieties %>%
   head(3) %>%
   ggplot(aes(x = reorder(variety, -pcnt), y = total, FILL = variety)) + 
   geom_bar(aes(fill = variety), stat = "identity") + 
   labs(x = "\n good variety wines per region, Top 3 \n") + labs(y = "\n Total \n") + 
   labs(title = "\n variety") + 
   geom_text(aes(label = sprintf("%.1f %%", pcnt))) + 
   theme_solarized()

#*********************************************************************

#MAPS


#registering with the API key from google cloud platform
register_google(key = "AIzaSyAszyF9rqhpPa5d8h1qsp3PHwBlHbOBPsU",write=TRUE)

#loading the world map
world <- map_data("world")

#getting the geo code of the locations 
locations <- c("Tuscany","central Italy","piedmont","Sicily & Sardinia","Veneto","Lombardy") %>%
   geocode()


#plotting the terrain map for locations 
map1 <- get_map(location = locations,
                maptype = "terrain",
                source = "google",
                crop = FALSE,
                zoom = 5)
#getting the coordinates  and encircling the locations
plot_map1 <- ggmap(map1) + 
   xlab("Longitude") + ylab("Latitude") +
   theme(legend.position="none")+geom_point(aes(x=lon, y=lat),
                                            data = locations, 
                                            alpha = 0.7, 
                                            size = 7, 
                                            color = "red")+ geom_encircle(aes(x=lon, y=lat),
                                                                             data = locations, size = 2, color = "blue")
#plotting the map
plot_map1
#plotting the hybrid map for locations 

map2 <- get_map(location = locations,
                maptype = "hybrid",
                source = "google",
                crop = FALSE,
                zoom = 5)
#getting the coordinates  and encircling the locations
plot_map2 <- ggmap(map2) +
   xlab("Longitude") + ylab("Latitude") +
   theme(legend.position="none")+geom_point(aes(x=lon, y=lat),
                                            data = locations, 
                                            alpha = 0.7, 
                                            size = 7, 
                                            color = "tomato")+ geom_encircle(aes(x=lon, y=lat),
                                                                             data = locations, size = 2, color = "blue")
#plotting the map
plot_map2

#getting the map for the locations
map3 <- get_map(location = locations,
                maptype = "terrain",
                source = "google",
                crop = FALSE,
                zoom = 5)
#getting the coordinates and drawing a line along the locations
plot_map3 <- ggmap(map3) + 
   geom_point(data = locations, aes(x = lon, y = lat)) +
   geom_path(data = locations,aes(x = lon, y = lat), size = 1, lineend = "round") +
   xlab("Longitude") + ylab("Latitude") +
   ggtitle("Plot of Women's March Time Series Data") +
   theme(legend.position="none",
         plot.title = element_text(hjust = 0.5)) 

plot_map3



#*********************************************************
#for spain
world <- map_data("world")

locations <- c("Levante","Catalonia","Central Spain","Galicia","Andalucia","Nothern Spain") %>%
   geocode()


map1 <- get_map(location = locations,
                maptype = "terrain",
                source = "google",
                crop = FALSE,
                zoom = 5)
plot_map1 <- ggmap(map1) + 
   xlab("Longitude") + ylab("Latitude") +
   theme(legend.position="none")+geom_point(aes(x=lon, y=lat),
                                            data = locations, 
                                            alpha = 0.7, 
                                            size = 7, 
                                            color = "red")
plot_map1

map2 <- get_map(location = locations,
                maptype = "terrain",
                source = "google",
                crop = FALSE,
                zoom = 5)

plot_map2 <- ggmap(map2) +
   xlab("Longitude") + ylab("Latitude") +
   theme(legend.position="none")+geom_point(aes(x=lon, y=lat),
                                            data = locations, 
                                            alpha = 0.7, 
                                            size = 7, 
                                            color = "tomato")+ geom_encircle(aes(x=lon, y=lat),
                                                                             data = locations, size = 2, color = "blue")
plot_map2


map3 <- get_map(location = locations,
                maptype = "terrain",
                source = "google",
                crop = FALSE,
                zoom = 5)


plot_map3 <- ggmap(map3) + 
   geom_point(data = locations, aes(x = lon, y = lat)) +
   geom_path(data = locations,aes(x = lon, y = lat), size = 1, lineend = "round") +
   xlab("Longitude") + ylab("Latitude") +
   ggtitle("Plot of Women's March Time Series Data") +
   theme(legend.position="none",
         plot.title = element_text(hjust = 0.5)) 

plot_map3
#******************************
#for countries in EUROPE

world <- map_data("world")
locations <- c('Germany', 'France', 'England', 'Spain', 'Portugal', 'Italy', 'Austria') %>%
   geocode()


map1 <- get_map(location = locations,
                maptype = "terrain",
                source = "google",
                crop = FALSE,
                zoom = 5)
plot_map1 <- ggmap(map1) + 
   xlab("Longitude") + ylab("Latitude") +
   theme(legend.position="none")+geom_point(aes(x=lon, y=lat),
                                            data = locations, 
                                            alpha = 0.7, 
                                            size = 7, 
                                            color = "red")
plot_map1

map2 <- get_map(location = locations,
                maptype = "terrain",
                source = "google",
                crop = FALSE,
                zoom = 5)


plot_map3 <- ggmap(map2) + 
   geom_point(data = locations, aes(x = lon, y = lat)) +
   geom_path(data = locations,aes(x = lon, y = lat), size = 1, lineend = "round") +
   xlab("Longitude") + ylab("Latitude") +
   ggtitle("country") +
   theme(legend.position="none",
         plot.title = element_text(hjust = 0.5)) 

plot_map3

#*******************************************************************


#exploring location variables
options(scipen=999) #to prevent scientific notation
value_data = wine %>% 
   select(price, country) %>%               
   group_by(country) %>% 
   summarise(Count = n(),
             Avg_Price = mean(price))  

MAP <- list('US' = "USA")

value_data$country <- dplyr::recode(value_data$country, !!!MAP)

datatable(value_data)

#loading the world map
worldmap = map_data("world")
#merging
merged_data <- merge(x = worldmap, y = value_data, by.x = "region", by.y = "country", all.x = TRUE) %>% arrange(desc(order))
#plotting the map
country <- ggplot(data = merged_data, aes(x = long, y = lat, group = group, color = region)) +
   scale_fill_viridis_c(option = "plasma")+
   theme_minimal()+               
   geom_polygon(aes(fill = Avg_Price)) +
   labs(fill='Average price')+labs(title = "Average price Rating by Country")+
   theme(legend.position = 'none')              

ggplotly(country)

#****************************************
#Using the map world package 
map.world <- map_data('world')
#Change US country to USA for map.world match
wine$country <- gsub('US', 'USA', wine$country)

#Create dfworld dataframe, group by country, show total reviews and average rating
wineworld <- wine%>% 
   group_by(country) %>%
   summarise(AverageRating = mean(points), Total = n()) %>% 
   arrange(desc(Total)) %>% 
   ungroup()

#Join dfworld and map.world
wineworld <- left_join(map.world, wineworld, by = c('region' = 'country'))

#Plot average rating by country on world map
p2 <- ggplot(data = wineworld, aes(x = long, y = lat, group = group, text=region)) +
   geom_polygon(aes(fill = AverageRating)) + 
   scale_fill_viridis(option = 'plasma') + 
   labs(title = "Average Rating by Country") + 
   theme_economist()

p2 <- ggplotly(p2)
p2


###################################################################################

#Aranging the wines accordingly to the cataegory
whites="addoraca|aidani|aidini|airen|airén|alarije|albalonga|albana|albanella|albanello|albanello bianco|albaranzeuli bianco|albarello|albarín|albariño|albarola|albillo|aligoté|alionza|altesse|alvarinho|amigne|angevine|ansonica|antão vaz|arbane|arbois|arilla|arinto|arneis|arnsburger|arrufiac|arvesiniadu|asprinio|asprinio bianco|assyrtico|assyrtiko|athiri|aubin blanc|aubin vert|aurore|auxerrois|auxerrois blanc|avesso|azal branco|bacchus|baco blanc|balzac blanc|baratuciat|barbera bianca|bariadorgia|baroque|bayan shirey|beba|besgano bianco|bia blanc|bianca|biancame|bianchetta trevigiana|bianco d\'alessano|biancolella|biancone di portoferraio|biancu gentile|bical|bigolona|blanc|blatterle|boal|bogdanusa|bombino bianco|borba blanca|bosco|bourboulenc|bouvier|breidecker|bronner|brustiano bianco|brustiano faux|bual|bukettraube|burger|cabernet blanc|caíño blanco|camaralet|caprettone|carricante|cascarolo bianco|cassady|catalanesca|catarratto|cayetana|cayuga|cayuga white|cerceal|cercial|cereza|chambourcin|chardonel|chardonnay|chasan|chasselas|chenel|chenin blanc|chinuri|clairette|clairette blanche|claret|claverie|cococciola|coda di pecora|coda di volpe|códega do larinho|colombard|completer|cortese|courbu|crouchen|cserszegi fuszeres|dafni|debina|debit|diamond|dimiat|dinka|doña blanca|donzelinho branco|doradillo|drupeggio|durella|durello|edelzwicker|ehrenbreitsteiner|ehrenfelser|elbling|emir|encruzado|erbaluce|escanyavella|ezerjó|faberrebe|falanghina|favorita|fernão pires|feteasca|feteasca alba|feteasca regala|feteasca regala|fiano|findling|folle blanche|forastera|freisamer|friulano|furmint|gamay blanc gloriod|garganega|garrido fino|gelber muskateller|gelber traminer|gewürztraminer|giró blanc|glera|godello|goldburger|goldriesling|gouais blanc|gouveio|graisse|grasa de cotnari|grasevina|grauburgunder|grecanico|grechetto|greco|green hungarian|grenache blanc|grillo|gringet|grk bijeli|gros manseng|gros plant|grüner veltliner|gutenborner|hárslevelü|hárslevelu|hebén|hibernal|hondarrabi zuri|humagne blanche|huxelrebe|incrocio manzoni|incrocio manzoni 1.50|insolia|inzolia|irsai oliver|irsai olivér|jacquère|jaen|jampal|juhfark|juwel|kabar|kangoun|kanzler|kéknyelu|kerner|kinali yapincak|királyleányka|kisi|knipperlé|koshu|kövérszolo|krstac|la crescent|la crosse|l\'acadie blanc|lagarino bianco|lagorthi|lauzet|len de l\'el|listán de huelva|loin de l\'oeil|loureira|loureiro|luglienga|macabeo|maceratino|madeira blend|madeleine angevine|malagousia|malagouzia|malmsey|malvar|malvasia|malvazija|malvoisie|mantonico|mantonico bianco|manzoni|manzoni bianco|marastina|marawi|maria gomes|marsanne|marzemina bianca|mauzac|melody|melon|melon de bourgogne|merlot blanc|merseguera|meseguera|meslier-saint-françois|minella bianca|misket|misket cherven|molette|mondeuse blanche|montepila|montonico bianco|montù|morava|morillon|morio muscat|morio muskat|moscadello|moscatel de alejandría|moscatel graúdo|moscatel roxo|moscato di noto|moscato giallo|moscato rosa|moschofilero|moscofilero|mtsvane|müller-thurgau|muscadel|muscadelle|muscadet|muscat|muscat blanc à petits grains|muscat d\'eisenstadt|muscat of alexandria|muscat ottonel|muscat rose à petits grains|muscat rouge à petits grains|muskat ottonel|narince|nascetta|nasco|neuburger|neyret|noah|nobling|nosiola|nuragus|ondenc|optima|orangetraube|orléans|ortega|ortrugo|österreichisch-weiß|pallagrello bianco|palomino|pampanuto|paralleda|pardillo|pardina|parellada|pascal blanc|passerina|pearl of csaba|pecorino|pedro giménez|pedro ximénez|perle|petit manseng|petit meslier|petite arvine|petroulianos|peurion|picapoll|picardan|picolit|picpoul|pied de perdrix|pigato|pignoletto|pinela|pinot auxerrois|pinot bianco|pinot blanc|pinot grigio|pinot gris|piquepoul|plavay|plyto|posip|premsal|prié blanc|prosecco|rabigato|rabo de ovelha|räuschling|ravat blanc|rebula|regner|reichensteiner|retagliado bianco|rèze|rhoditis|ribolla gialla|rieslaner|riesling|rivaner|rkatsiteli|robola|roditis|rolle|romorantin|roscetto|rosenmuskateller|rossese bianco|roter traminer|roter veltliner|rotgipfler|roublot|roupeiro|roussanne|rovello bianco|roviello"

whites2="ryzlink rýnský|sacy|saint-pierre doré|sämling|sarba|sarfeher|sauvignon|sauvignon blanc|sauvignon gris|sauvignon musqué|sauvignon vert|sauvignonasse|savagnin|savagnin rose|savatiano|scheurebe|sémillon|sercial|seyval blanc|sherry|sideritis|siegerrebe|silvaner|siria|smederevka|souvignier gris|st. pepin|steen|sylvaner|symphony|tamâioasa româneasca|tamarez|tamianka|taminga|tamjanika|tempranillo blanco|terrantez|terret blanc|terret gris|thrapsathiri|timorasso|tocai|tocai friulano|tokaji|tokay|torbato|torontel|torrontés|tourbat|trajadura|traminer|traminette|trebbiano|treixadura|trousseau gris|tsolikouri|turbiana|valvin muscat|vega|veltliner|verdea|verdeca|verdejo|verdelho|verdello|verdesse|verdicchio|verdil|verdiso|verdosilla|verduzzo|verduzzo trevigiano|vermentino|vernaccia|vernaccia di oristano|versoaln|vespaiola|vespaiolo|vidal|vidal blanc|vigiriega|vignoles|vilana|viognier|viosinho|vital|vitovska|viura|vugava|weissburgunder|weldra|welschriesling|white|white blend|würzer|xarel·lo|xarel-lo|xinisteri|xynisteri|zalema|zelen|zéta|zibibbo|zierfandler|zilavka|zilavka|zlahtin|posip"


#list of all red wine varieties that apply to this dataset. Split list in two as list seemed to max out.
reds="abbuoto|abouriou|abrusco|acitana|acolon|adakarasi|agh shani|agiorgitiko|aglianico|aglianicone|albaranzeuli nero|albarossa|aleatico|aleksandrouli|alfrocheiro|alfrocheiro preto|alicante|alicante bouschet|alicante ganzin|alvarelhão|ancellotta|aragonês|aragonez|aramon|argaman|argant|arrouya noir|aspiran|aubun|avanà|avarengo|azal tinto|babeasca neagra|babic|babosa negro|bachet noir|baco noir|baga|barbarossa|barbaroux|barbera|barbera del sannio|barbera sarda|barsaglina|bastardo|beaunoir|bellone|béquignol noir|black monukka|black muscat|blatina|blauburger|blauburgunder|blauer portugieser|blaufränkisch|bobal|bogazkere|bombino nero|bonamico|bonarda|bonarda piemontese|bonda|bondola|bouchalès|bouteillan noir|bovale|bracciola nera|brachetto|braquet|braucol|brugnola|brun argenté|brun fourca|bubbierasco|busuioaca de bohotin|cabernet|cabernet blend|cabernet dorsa|cabernet franc|cabernet gernischt|cabernet mitos|cabernet moravia|cabernet sauvignon|cabernet-shiraz|caiño tinto|calabrese montenuovo|caladoc|calitor|çalkarasi|camaraou noir|canaiolo|canari noir|cannonau|carcajolu|carignan|carineña|cariñena-garnacha|carmenère|carménère|carnelian|casavecchia|cascade|castelão|castets|catanese nero|catawba|centesimino|cesanese|cesanese comune|cesanese d'affile|césar|chancellor|charbono|chatus (wine grape)|chelois|cienna|ciliegiolo|cinsault|cinsaut|clinton|colombana nera|colorino|complexa|cornalin d'aoste|cornifesto|corot noir|corvina|corvinone|couderc noir|counoise|crespiello|criolla grande|croatina|cygne blanc|dameron|de chaunac|delaware|diolinoir|dobricic|dolcetto|domina|dornfelder|douce noir|douce noire grise|drnekusa|dunkelfelder|duras|dureza|durif|ederena|ekigaïna|emperor|enantio|enfariné noir|espadeiro|étraire de la dui|fer|feteasca neagra|flora|flot rouge|forcallà|forcallat tinta|fortana|francisa|franconia|frankovka|frappato|freisa|frontenac|früburgunder|frühroter veltliner|fuella|fumin|gaglioppo|gamaret|gamay|gamay beaujolais|gamza|garanoir|garnacha|garnacha blend|garnacha tintorera|garnacha-cabernet|garnacha-cariñena|garnacha-monastrell|garró|girò|gouget noir|graciano|gragnano|grand noir de la calmette|grenache|grignolino|grisa nera|grolleau|groppello|gros verdot|g-s-m|gueuche noir|helfensteiner|heroldrebe|hondarribi beltza|hron|incrocio manzoni 2.14|incrocio manzoni 2.15|isabella|ives noir|jacquez|jaén tinto|joubertin|juan garcía|kadarka|kalecik karasi|kallmet|karalahna|karasakiz|kekfrankos|kotsifali|krasnostop zolotovsky|kratosija|kuntra|lacrima|lagrein|lambrusco|landal noir|landot noir|lemberger|léon millot|liatiko|limnio|listán negro|madrasa|magarach ruby|magliocco|magliocco canino|magliocco dolce|malbec|mammolo|mandilaria|manseng noir|mansois|manto negro|maratheftiko|marechal foch|maréchal foch|marechal joffre|marquette|marselan|marzemino|mataro|maturana|mauzac noir|mavro|mavrodafni|mavrud|mazuelo|mencia|mencía|meoru|merille|merlot|milgranet|mission|molinara|monastrell|mondeuse|mondeuse noire|monica|montepulciano|montepulciano d'abruzzo|moreto|moristel|mornen noir|morrastel bouschet|mourisco tinto|mourvèdre|mtevandidi|mujuretuli|mureto|muscardin|muscat bleu|nebbiolo|negoska|negrara|negrette|négrette|negroamaro|nerello|nero d'avola|nero di troia|nielluccio|nielluciu|nocera|noiret|norton|oeillade noire|okuzgozu|öküzgözü|pais|país|pallagrello nero|pamid|papaskarasi|papazkarasi|parraleta|pascale di cagliari|pelaverga|peloursin|perricone|persan|petit bouschet|petit rouge|petit verdot|petite sirah|petite verdot|piccola nera|piedirosso|pignolo|pineau d'aunis|pinot meunier|pinot nero|pinot noir|pinot noir précoce|pinotage|pione|plantet|plassa|plavac mali|pollera nera|port|portan|portuguiser|poulsard|prieto picudo|primitivo|prokupac|prugnolo gentile|prunelard|prunesta|pugnitello|raboso|ramisco|rara neagra|rebo|red|red blend|refosco|refosco dal peduncolo rosso|rimava|roesler|romé|romeiko|rondinella|rosette|rossignola|rossola nera|rossolino nero|rotberger|rouge du pays|royalty|ruby cabernet|ruché|ruen|rufete|sagrantino|salvador|san giuseppe nero|sangiovese|saperavi|schiava|schioppettino|schönburger|sciacarello|sciaccerellu"

reds2= "sciascinoso|segalin|ségalin|servanin|severny|seyval noir|shiraz|shiraz-cabernet|sirica|siroka melniska|sjriak|sousao|sousão|souson|sousón|souzao|souzão|spätburgunder|st. george|st. laurent|st. vincent|stanusina crna|sumoll|susac crni|susumaniello|swenson red|syrah|taferielt|tai|tannat|tarrango|tazzelenghe|teinturier|tempranillo|téoulier|teran|termarina rossa|teroldego|terrano|terret noir|tibouren|tinta amarela|tinta barroca|tinta cao|tinta cão|tinta carvalha|tinta de toro|tinta del toro|tinta fina|tinta francisca|tinta madeira|tinta miuda|tinta miúda|tinta negra mole|tinta roriz|tintilia|tinto fino|tinto velasco|tocai rosso|touriga|touriga franca|touriga francesa|touriga nacional|trepat|tressot|trevisana nera|trincadeira|triomphe d'alsace|trollinger|trousseau|tsardana|uva di troia|uva rara|uva tosca|uvalino|valdiguié|valentino nero|vermentino nero|vespolina|vidadillo|vien de nus|vinhão|vitis rotundifolia|vranac|vranec|vuillermin|wildbacher|xinomavro|yapincak|zametovka|zarya severa|zinfandel|zweigelt|bordeaux-style red blend|rhÃ´ne-style red blend|austrian red blend|provence red blend|portuguese red"



rose <- "rosado|rosato|rosé|tavel|white zinfandel"
sparkling <- "blanquette-de-limoux|brachetto-d'acqui|cava|champagne|champagne-blend|cremant-de loire|franciacorta|lambrusco|moscato-d'asti|portuguese-sparkling|prosecco|sekt|sparkling|sparkling-blend"
#For those interested, needed to do an exact start/end word match with port since they were overided by 'port'uguese white and red varieties and placed in red/white types.
dessert <- "banyuls|chenin-blanc|madeira|marsala|^port$|riesling|rutherglen-muscat|sauternes|sherry|tokaji"
red_and_white <- "azal|meritage|moscatel|moscato|muscadine|muskat|muskateller|pallagrello"


wine$variety <- tolower(wine$variety)
#Create new column: type, that classifies red, white etc. based on word in string
wine$type = ifelse(grepl(rose,wine$variety),"Rosé","Unknown")
wine$type = ifelse(grepl(sparkling,wine$variety),"Sparkling",wine$type)
wine$type = ifelse(grepl(red_and_white,wine$variety),"Red and White",wine$type)
wine$type = ifelse(grepl(whites,wine$variety),"White",wine$type)
wine$type = ifelse(grepl(whites2,wine$variety),"White",wine$type)
wine$type = ifelse(grepl(reds,wine$variety),"Red",wine$type)
wine$type = ifelse(grepl(reds2,wine$variety),"Red",wine$type)
wine$type = ifelse(grepl(dessert,wine$variety),"Dessert",wine$type)

#. There are approx. 8 varieties that make up 27 rows of reviews that we are deleting here that are present in our values lists but for foreign character purposes do not compute. 
wine <-wine[wine$type!= "Unknown", ]

head(wine[c(11,12)])

#View(wine)

#map for the red whine locationsred wines 
dfworldred <- wine %>% 
   filter(wine$type == 'Red') %>% 
   group_by(country) %>%
   summarise(AverageRating = mean(points), Total = n()) %>% 
   arrange(desc(Total)) %>% 
   ungroup()

dfworldred <- left_join(map.world, dfworldred, by = c('region' = 'country'))

p3 <- ggplot(data = dfworldred, aes(x = long, y = lat, group = group, text=region)) +
   geom_polygon(aes(fill = AverageRating)) + 
   scale_fill_viridis(option = 'plasma') + 
   labs(title = "Average Rating for Red Wines by Country") + 
   theme_economist()

p3 <- ggplotly(p3)

p3


#map for the white wines

dfworldwhite <- wine %>% 
   filter(wine$type == 'White') %>% 
   group_by(country) %>%
   summarise(AverageRating = mean(points), Total = n()) %>% 
   arrange(desc(Total)) %>% 
   ungroup()

dfworldwhite <- left_join(map.world, dfworldwhite, by = c('region' = 'country'))

p4 <- ggplot(data = dfworldwhite, aes(x = long, y = lat, group = group, text=region)) +
   geom_polygon(aes(fill = AverageRating)) + 
   scale_fill_viridis(option = 'viridis') + 
   labs(title = "Average Rating for White Wines by Country") + 
   theme_economist()

p4 <- ggplotly(p4)

p4

#############################################################

#provience

province <- wine%>%
   arrange(price) %>%
   group_by(country, province)%>%
   summarise(n = n())%>%
   arrange(desc(n))

datatable(t(province %>% filter(n > 50)))

#plotting a graph for the province
n_companies_candy <- province %>%
   ggplot(aes(x = country, fill = province, color = province, size = province, alpha = province))+
   geom_bar(width = 0.9,  position = "fill")+
   coord_polar(theta = 'x')+
   theme_minimal()+
   labs(x = 'Country', y = 'Province', title = 'number of province, country_wise')+
   theme(plot.title = element_text(hjust=0.5),legend.position = 'none')
n_companies_candy





#________________________________
#Analysising by continent
#dividing the countries according to the continents
NAmerica <- c('US', 'Canada', 'Mexico')
SAmerica <- c('Chile', 'Argentina', 'Brazil', 'Colombia', 'Uruguay', 'Paraguay', 'Bolivia', 'Peru', 'Venezuala')
Europe <- c('Germany', 'France', 'England', 'Spain', 'Portugal', 'Italy', 'Austria')
#Adding the column continent  in to he dataset
wine$continent <- ifelse(wine$country %in% NAmerica, 'NA', ifelse(wine$country %in% SAmerica, 'SA', ifelse(wine$country %in% Europe, 'EU', 'Other')))
wine$price <- ifelse(wine$price > 250, NA, wine$price)

ggplot(wine, aes(x=points, y=price, color=continent)) + geom_point() +ggtitle('continents')
#adding the new column i.e continent to the dataset
NA_Wine <- wine[wine$continent == 'NA',]
SA_Wine <- wine[wine$continent == 'SA',]
EU_Wine <- wine[wine$continent == 'EU',]
Other_Wine <- wine[wine$continent == 'Other',]

#plotting the scatter plots for each continent
ggplot(NA_Wine, aes(x=points, y=price, color=country)) + geom_point() +ggtitle('North American Wine')
ggplot(SA_Wine, aes(x=points, y=price, color=country)) + geom_point() +ggtitle('South American Wine')
ggplot(EU_Wine, aes(x=points, y=price, color=country)) + geom_point() +ggtitle('European Wine')
ggplot(Other_Wine, aes(x=points, y=price, color=country)) + geom_point() +ggtitle('Other Wine')


#SHINY APP*********************************************************************


#Filtering for Price  Range Selectors:
wine = wine %>%
   mutate(price_range = case_when(price <= 10 ~ 1,
                                  price <=25 & price > 10 ~ 2,
                                  price <=50 & price > 25 ~ 3,
                                  price <=100 & price > 50 ~ 4,
                                  price <=500 & price >100 ~ 5,
                                  price > 500 ~ 6))
#considering y and m 
y_int = 78.978654
m = 6.556461

#filtering for points range selectors
wine = wine %>%
   mutate(Rating = case_when(points <= 82 ~ "Acceptable", 
                             points <= 86 & points >= 83 ~ "Good",
                             points <= 89 & points >= 87 ~ "Very Good",
                             points <= 93 & points >= 90 ~ "Excellent",
                             points <= 97 & points >= 94 ~ "Superb",
                             points >= 98 ~ "Classic")) %>%
   mutate(model_score = y_int + m * log10(price)) %>%
   mutate(Value = case_when(points > (+model_score + 2.5) ~ "Good Value"))

#create lists for selectors/filters

top_varieties = wine %>%   
   group_by(variety) %>% 
   distinct(title) %>%
   summarise(count = n()) %>% 
   arrange(desc(count))

top_varieties = as.character(top_varieties$variety)
top_var = c("All",sort(top_varieties[1:25]))


wine_countries = wine %>% 
   group_by(country) %>% 
   drop_na(country) %>%
   summarise(count = n()) %>% 
   arrange(desc(count))
wine_countries = as.character(wine_countries$country)




#Reduced data set - for distinct wines, average rating from multiple reviews
wine_df2 = wine[c("winery","variety","country","province","price","points")] %>%
   distinct(price,points, variety, country) 



#Sampled data set for graphing (Rating vs. Price)
set.seed(1)
wine_sampdf2 = wine_df2[sample(nrow(wine),5000),]


###############################
#---------MAP DATA------------#
#Map data for mean points for the world map
wine_df3 = wine_df2 %>% 
   group_by(country,variety) %>%
   summarise(count = n(), "Ave.Rating" = mean(points)) %>%
   arrange(country, desc(count)) %>% 
   filter(count == max(count)) %>% select(country,variety,"Ave.Rating")
#map data for mean points for US
wine_df4 = wine %>%
   filter(country == "US", province != "America") %>%
   group_by(province,variety) %>%
   summarise(count = n(),"Ave.Rating" = mean(points)) %>%
   arrange(province, desc(count)) %>%
   filter(count == max(count)) %>% select(province, variety, "Ave.Rating")

wine$designation<-NULL
wine$region_1<-NULL
wine$region_2<-NULL
wine$taster_twitter_handle<-NULL


ui1<-dashboardPage(skin = "purple",
                   dashboardHeader(title = "Wine Ratings - Visualized", ###########
                                   titleWidth = 275
                   ), #end header  
                   ## -- SIDEBAR: ######################
                   dashboardSidebar(   
                      width = 275,
                      sidebarMenu(
                         
                         menuItem(" Countries & Varietals", tabName = "barPlot"),
                        
                         menuItem("Top Varietal by Region", tabName = "wineRegions")
                      ) #end sidebarMenu
                   ),
                   dashboardBody(
                      tabItems(
                         #for barplots to compare countries and varietals
                         tabItem(tabName = "barPlot", 
                                 fluidRow(
                                    box(
                                       checkboxGroupInput("pickVar",
                                                          label = "Choose varietal(s) to compare:",
                                                          choices = sort(top_varieties[1:10]),
                                                          selected = sort(top_varieties[1:10])), width = 2
                                    ),
                                    box(
                                       plotOutput("barGraph1", height = 550),width = 5),
                                    box(
                                       plotOutput("barGraph2", height = 550),width = 5
                                    ))
                         ),
                         #for wine regions
                         tabItem(tabName = "wineRegions", 
                                 h3("Average Rating for the Most Popular Varietal Produced by Each Region:"),
                                 fluidRow(
                                    box(htmlOutput("map"), width = 12 )
                                 ),
                                 fluidRow(
                                    box(htmlOutput("map2"), width = 12)
                                 )
                         )
                      ),
                      titlePanel("Find Best Wines by Country & Price"),
                      
                      
                      # Sidebar with a select input and slider input
                      sidebarLayout(
                         sidebarPanel(
                            helpText("Please select a country and a maximum price to find TOP10 wines."),
                            selectInput("countryInput", "Country",
                                        choices = c("Argentina","Armenia","Australia","Austria","Bosnia and Herzegovina","Brazil","Bulgaria","Canada","Chile","Croatia","Cyprus","Czech Republic","Egypt",
                                                    "England","France","Georgia","Germany","Greece","Hungary","India","Israel", "Italy","Lebanon","Luxembourg","Macedonia","Mexico","Moldova","Morocco", "New Zealand",
                                                    "Peru","Portugal","Romania","Serbia","Slovakia","Slovenia","South Africa","Spain","Switzerland","Turkey","Ukraine","Uruguay", "US"),
                                        selected = ""),
                            sliderInput("priceInput", "Price", min = 10, max = 500, value = c(50), pre = "$")
                         ),
                         
                         # Show a results 
                         mainPanel(
                            tableOutput("results")
                         )
                      )
                   ) #end Body 
) #end Page


server1<-function(input,output) {
   #for barplot1
   output$barGraph1 <- renderPlot ({
     
      
      wbar1 = wine_df2 %>%
         filter(country %in% wine_countries[1:5],
                variety %in% input$pickVar) %>%
         group_by(country,variety) %>%
         summarise(median_price = median(price))
      
      ggplot(wbar1, aes(x=variety, y=median_price)) + 
         geom_col(position="dodge", aes(fill=country)) + 
         scale_y_continuous(limits=c(5,65), oob = rescale_none) +
         coord_flip() +
         labs(title = "Median Varietal Prices by Country")
   })  
   #for barplot2
   output$barGraph2 <- renderPlot ({
      
      wbar2 = wine_df2 %>% 
         filter(country %in% wine_countries[1:5], 
                variety %in% input$pickVar) %>% 
         group_by(country,variety) %>%
         summarise(mean_rating = mean(points))
      
      ggplot(wbar2, aes(x=variety, y=mean_rating)) + 
         geom_col(position="dodge", aes(fill=country)) + 
         scale_y_continuous(limits=c(80,95), oob = rescale_none) +
         coord_flip() + 
         labs(title = "Average Varietal Ratings by Country")
      #    }
   })
   #for selecting top 10 wines
   output$results <- renderTable({
      filtered <- wine %>% 
         filter(country %in% input$countryInput) %>% 
         filter(price <= input$priceInput)  %>% 
         arrange(desc(points))
      
      filtered <- head(filtered, 10)
      
      filtered
   })
   #for world map
   output$map <- renderGvis({
      gvisGeoChart(wine_df3, "country", hovervar = "variety", 
                   colorvar = "Ave.Rating",
                   options=list(colorAxis="{colors:['pink','maroon']}",
                                backgroundColor="lightblue")
      )
   })
   #for us map
   output$map2 <- renderGvis({
      gvisGeoChart(wine_df4, "province", hovervar = "variety",
                   colorvar = "Ave.Rating", 
                   options=list(region="US", displayMode="regions", resolution="provinces",
                                colorAxis="{colors:['pink','maroon']}",backgroundColor="lightblue")
      )
   })
}
#To generate the app
shinyApp(ui1, server1)
