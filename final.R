# importing dataset
setwd(choose.dir())
getwd()
data=read.csv("C:\\Users\\britt\\Desktop\\ITS 530\\nytimes.csv")

# installing packages
install.packages("stringr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("pastecs")
install.packages("summarytools")
install.packages("ggpubr")

# setting library
library(stringr)
library(dplyr)
library(ggplot2)
library(pastecs)
library(scales)
library(summarytools)
library(ggpubr)

################################################################################

# descriptives of starter dataset
str(data)
sum(is.na(data))

# data cleaning and new variables

# word count column created and added to data df
wordCount <- str_count(data$content, "\\w+")
data$wordCount <- wordCount

# letter count column created and added to data df
char <- nchar(data$content, type = "chars", allowNA = FALSE, keepNA = NA)
data$characterCount <- char

# create new data frame with summary count
data2 <- data %>% group_by(year,month) %>% summarize(N=n())

# create date variable combining year and month
data2$year_mon <- ymd(paste(data2$year,data2$month, "15", sep = "-"))

# subset to remove 2014 and 2019
data <- subset(data, year > 2014 & year < 2019)
data2 <- subset(data2, year > 2014 & year < 2019)

################################################################################
# section 1 stacked area chart By fill

# data cleaning and re-coding

# drop 2018 via filter since it's not a full year of data
data4 <- filter(data2,
                year != 2018)
# change year from continuous variable to factor
data4$year_f <- as.character(data4$year)

#generate chart
ggplot(data = data4, 
       aes(x = month,
           y = N,
           fill = forcats::fct_rev(year_f))) +
  geom_area(color = "black") +
  scale_x_continuous(n.breaks = 11) +
  coord_cartesian(xlim = c(1, 12)) +
  labs(title = "Stacked area chart by fill",
       subtitle = "New York Times articles by month and year",
       caption = "Source: New York Times",
       x = "Month",
       y = "Count",
       fill = "Year") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()

################################################################################
# descriptives
view(dfSummary(data))

# bar chart for month
ggplot(data = data, aes(x = month)) +
  geom_bar(fill = "cornflowerblue")+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, color = "white")+
  scale_x_continuous(n.breaks = 11) +
  coord_cartesian(xlim = c(1, 12)) +
  scale_y_continuous(labels = comma) +
  labs(x = "Month",
       y = "Number of articles",
       title = "Number of NYT articles by month, 2015 - 2018",
       caption = "Source: New York Times")

# bar chart for year
ggplot(data = data, aes(x = year)) +
  geom_bar(fill = "cornflowerblue")+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, color = "white")+
  scale_y_continuous(labels = comma) +
  labs(x = "Year",
       y = "Number of articles",
       title = "Number of NYT articles by year, 2015 - 2018",
       caption = "Source: New York Times")

#boxplots of word count and character count
ggplot(data) +
  aes(y = characterCount)+
  geom_boxplot()+
  scale_y_continuous(labels = comma)+
  labs(x = "Frequency",
       y = "Character Count",
       title = "Boxplot",
       caption = "Source: New York Times")

ggplot(data) +
  aes(y = wordCount)+
  geom_boxplot()+
  scale_y_continuous(labels = comma)+
  labs(x = "Frequency",
       y = "Word Count",
       title = "Boxplot",
       caption = "Source: New York Times")

# histogram for month
ggplot(data = data, aes(x = month)) +
  geom_histogram(bins = 12, binwidth = 1, fill = "cornflowerblue", col = "grey")+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, color = "white")+
  scale_x_continuous(n.breaks = 11) +
  coord_cartesian(xlim = c(1, 12)) +
  scale_y_continuous(labels = comma) +
  labs(x = "Month",
       y = "Number of articles",
       title = "Number of NYT articles by month, 2015 - 2018",
       caption = "Source: New York Times")

# histogram for year
ggplot(data = data, aes(x = year)) +
  geom_histogram(bins = 4, binwidth = 1, fill = "cornflowerblue", col = "grey")+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, color = "white")+
  scale_y_continuous(labels = comma) +
  labs(x = "Year",
       y = "Number of articles",
       title = "Number of NYT articles by year, 2015 - 2018",
       caption = "Source: New York Times")

# histogram for wordcount
ggplot(data = data, aes(x = wordCount)) +
  geom_histogram(binwidth = 30, fill = "cornflowerblue", col = "grey")+
  scale_x_continuous(labels = comma) +
  labs(x = "Word Count",
       y = "Number of articles",
       title = "Number of articles by word count",
       caption = "Source: New York Times")

################################################################################
# checking for normal distribution
# qqplot
ggqqplot(data, x = "wordCount", 
         color = "cornflowerblue",
         conf.int = TRUE,
         conf.int.level = 0.95,
         title = "QQ Plot",
         subtitle = "Word count of New York Times articles")
