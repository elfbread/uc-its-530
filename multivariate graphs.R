# importing dataset
setwd(choose.dir())
getwd()
data=read.csv("C:\\Users\\britt\\Desktop\\ITS 530\\nytimes.csv")

# installing ggplot2 package
install.packages("ggplot2")
install.packages("dplyr")
install.packages("stringr")
install.packages("scales")
install.packages("tidyverse")

# load packages
# turn-off scientific notation
# select bw theme
options(scipen=999)
library(ggplot2)
library(dplyr)
library(stringr)
library(scales)
library(tidyverse)
theme_set(theme_bw())

#freq of author
table(data$author)

# subset years to include 2016, 2017, 2018
sub <- subset(data, year < "2019"  & year > "2015")


# change month to integer
month_int <- as.integer(sub$month)

# word count column
wordCount <- str_count(sub$content, "\\w+")
sub$wordCount <- wordCount


# letter count column
char <- nchar(sub$content, type = "chars", allowNA = FALSE, keepNA = NA)
sub$characterCount <- char


# year from continuous to factor
year_nom <- as.factor(sub$year)

# export data set
write.csv(data, "C:\\Users\\britt\\Desktop\\ITS 530\\nytimes_bb.csv", row.names = TRUE)

# descriptives of dataset
str(sub)
sum(is.na(sub))

####################################################################################################

# 5.1 grouping

# starting scatterplot
ggplot(data = sub,
       aes(x = characterCount,
           y = wordCount)) +
  geom_point(color = "blue", size = 1) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma) +
  labs(title = "Simple scatterplot",
       subtitle = "New York Times articles by word count and character count for 2016-2018 (N = 7,926)",
       y = "Word Count",
       x = "Character Count",
       caption = "Source: New York Times")

# adding front_page for grouping
ggplot(data = sub,
       aes(x = characterCount,
           y = wordCount,
           color = front_page)) +
  geom_point(size = 1) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Scatterplot",
       subtitle = "New York Times articles by word count, character count and front page status for 2016-2018 (N = 7,926)",
       y = "Word count",
       x = "Character Count",
       color = "Front Page",
       caption = "Source: New York Times")

# adding point shape for year
ggplot(data = sub,
       aes(x = characterCount,
           y = wordCount,
           color = front_page,
           shape = year_nom)) +
  geom_point(size = 1.75) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Scatterplot",
       subtitle = "New York Times articles by word count, character count, front page status and year for 2016-2018 (N = 7,926)",
       y = "Word count",
       x = "Character count",
       color = "Front Page",
       shape = "Publication Year",
       caption = "Source: New York Times")

# bubble plot
ggplot(data = sub,
       aes(x = characterCount,
           y = wordCount,
           color = front_page,
           size = year_nom)) +
  geom_point(alpha = .3) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Bubble Plot",
       subtitle = "New York Times articles by word count, character count, front page status and year for 2016-2018 (N = 7,926)",
       y = "Word count",
       x = "Character count",
       color = "Front Page",
       size = "Publication Year",
       caption = "Source: New York Times")

# adding fit lines
ggplot(data = sub,
       aes(x = characterCount,
           y = wordCount,
           color = front_page)) +
  geom_point(alpha = 0.4,
             size = 3) +
  geom_smooth(se=FALSE,
              method = "lm",
              formula = y~poly(x,2),
              size = 1,
              linetype='dashed')+
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Scatterplot with fit lines",
       subtitle = "New York Times articles by word count, character count, front page status and year for 2016-2018 (N = 7,926)",
       y = "Word count",
       x = "Character count",
       color = "Front Page",
       caption = "Source: New York Times") 

# faceting
ggplot(data = sub,
       aes(x = characterCount))+
  geom_histogram(fill = "cornflowerblue",
                 color = "white") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma) +
  facet_wrap(~front_page, ncol = 1)+
  labs(title = "Character count histograms by front page status (N = 7,936)",
       subtitle = "New York Times articles",
       y = "Count",
       x = "Character count",
       caption = "Source: New York Times")

  
# two variables for facets
ggplot(data = sub,
       aes(x = characterCount))+
  geom_histogram(fill = "cornflowerblue",
                 color = "white") +
  facet_grid(vars(year), vars(front_page))+
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma) +
  labs(title = "Character count histograms by character count, front page status and year (N = 7,936)",
       y = "Word count",
       x = "Month",
       caption = "Source: New York Times")

# calculate means and std errors
plotdata = sub %>% group_by(year, front_page) %>%
  summarize(n = n(),
            mean = mean(wordCount),
            sd = sd(wordCount),
            se = sd / sqrt(n))
ggplot(data = plotdata,
       aes(x = year,
           y = mean,
           color = front_page))+
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - se,
                    ymax = mean + se),
                width = .1)+
scale_x_continuous(breaks = seq(2016, 2018))+
  scale_y_continuous(labels = comma)+
  facet_grid(~ year)+
  theme_bw()+
  #theme(legend.position = "none")+
  labs(x="",
       y="Mean Word Count",
       title="New York Times article word counts, 2016-2018 (N = 7,926)",
       subtitle = "Means and standard errors",
       caption = "Source: New York Times",
       color = "Front Page")+
  scale_color_brewer(palette="Set1")


  

# final graph
ggplot (data = sub,
        aes(x=wordCount,
            y=characterCount))+
  geom_line(color="grey")+
  geom_point(color = "blue")+
  facet_wrap(~year)+
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma) +
  theme_minimal(base_size = 9)+
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))+
  labs(title = "New York Times articles character and word counts by year (N = 7,936)",
       y = "Character Count",
       x = "Word Count",
       caption = "Source: New York Times")

  
  

