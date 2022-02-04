# importing dataset
setwd(choose.dir())
getwd()
data=read.csv("C:\\Users\\britt\\Desktop\\ITS 530\\nytimes.csv")

# installing packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("stringr")
install.packages("scales")
install.packages("lubridate")
install.packages("quantmod")
install.packages("ggalt")
install.packages("tidyr")
install.packages("CGPfunctions")

# load packages
# turn-off scientific notation
# select bw theme
options(scipen=999)
library(ggplot2)
library(dplyr)
library(stringr)
library(scales)
library(lubridate)
library(quantmod)
library(ggalt)
library(tidyr)
library(CGPfunctions)
theme_set(theme_bw())

# descriptives of starter dataset
str(data)
sum(is.na(data))

################################################################################
# data cleaning, re-classify, create new dfs

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
data2 <- subset(data2, year > 2014 & year < 2019)

# export data set
write.csv(data2, "C:\\Users\\britt\\Desktop\\ITS 530\\nytimes_bb.csv", row.names = TRUE)

################################################################################
#graph 1 time series line

ggplot(data = data2,
       aes(x = year_mon,
           y= N)) +
  geom_line()+
  scale_x_date(date_breaks = '6 months', 
               labels = date_format("%b %y"))+
  labs(title = "Time series line",
       subtitle = "Count of articles by month and year",
       x = "Date",
       y = "Count",
       caption = "Source: New York Times")+
theme_minimal()

################################################################################
#graph 2 Time Series Plot with Red Line

ggplot(data = data2,
       aes(x = year_mon,
           y = N)) +
  geom_line(color = "indianred3", size=1)+
  geom_smooth(se=F) +
  scale_x_date(labels = date_format("%b %y"))+
  labs(title = "Time series with trend line",
       subtitle = "Count of articles by month and year",
       x = "Date",
       y = "Count",
       caption = "Source: New York Times")

################################################################################
# graph 3 basic area chart

ggplot(data = data2,
       aes(x = year_mon,
           y = N))+
  geom_area(fill="lightblue", color="black") +
  labs(title = "Basic area chart",
       subtitle = "Count of articles by month and year",
       x = "Date",
       y = "Count",
       caption = "Source: New York Times")

################################################################################
# 4 stacked area chart

# new df to add front_page to df
data3 <- data %>% group_by(year,month,front_page) %>% summarize(N=n())

# create date variable combining month and year
data3$year_mon <- ymd(paste(data3$year,data3$month, "15", sep = "-"))

# subset to remove 2014 and 2019
data3 <- subset(data3, year > 2014 & year < 2019)

# generate plot
ggplot(data = data3,
       aes(x = year_mon,
          y = N,
          fill = front_page)) +
  geom_area() +
  scale_x_date(labels = date_format("%b %y"))+
  labs(title = "Stacked area chart",
       subtitle = "Articles by month, year and front page status",
       x = "Year",
       y = "Count",
       caption = "Source: New York Times",
       fill = "Front Page")

################################################################################
# 5 stacked area chart By Fill

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
       caption = "source: New York Times",
       x = "Month",
       y = "Count",
       fill = "Month") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()

################################################################################
#6 dumbell Plot

#data cleaning and grouping
dplot <- filter(data,
                        wordCount > 2000 &
                        year %in% c(2016, 2018)) %>%
  select(author, year, wordCount)

dplot_group <- dplot %>% group_by(author, year) %>% summarize(N=n())

dplot_group$year <- as.character(dplot_group$year)

dplot_wide <- spread(dplot_group, year, N)

dplot_wide <- na.omit(dplot_wide)

names(dplot_wide) <- c("author", "y2016", "y2018")


# create dumbell plot
ggplot(dplot_wide, 
       aes(y = author,
           x = y2016,
           xend = y2018)) +
  geom_dumbbell()+
  labs(title = "Dumbell chart",
       subtitle = "New York Times articles by author, 2016 vs. 2018",
       x = "Number of articles, 2016 vs. 2018",
       y = "Author",
       caption = "Source: New York Times")

################################################################################
# 7 dumbell Graph with color

ggplot(dplot_wide,
       aes(y = author,
           x = y2016,
           xend = y2018)) +
  geom_dumbbell(size = 1.2,
                size_x = 3,
                size_xend = 3,
                colour = "grey",
                colour_x = "blue",
                colour_xend = "red") +
  theme_minimal() +
  labs(title = "Colored dumbell chart",
       subtitle = "New York Times articles by author, 2016 vs. 2018",
       caption = "Source: New York Times",
       x = "Number of articles, 2016 vs. 2018",
       y = "Author")

################################################################################
# 8 slope graph

# data filtering and aggregation
splot <- filter(data,
                wordCount > 1000 &
                  month %in% c(1,2,3,4,5,6) &
                  year %in% c(2015, 2016, 2017)) %>%
  select(month, year, wordCount)

splot_group <- aggregate(splot$month, by=list(month=splot$month, year=splot$year), FUN=sum)

# reclass variables
splot_group$year <- as.factor(splot_group$year)
splot_group$month <- as.factor(splot_group$month)

# create slope graph
newggslopegraph(splot_group, year, x, month,
                LineThickness = .5,
                YTextSize = 3,
                DataLabelPadding = 0.001) +
  labs(title="Total article word count, slope graph",
       subtitle="By Month, January - June, 2015 - 2017",
       caption="source: New York Times")

################################################################################