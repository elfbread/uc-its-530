# importing dataset
setwd(choose.dir())
getwd()
data=read.csv("C:\\Users\\britt\\Desktop\\ITS 530\\nytimes.csv")

# descriptives of dataset
str(data)
sum(is.na(data))

# installing ggplot2 package
install.packages("ggplot2")
install.packages("treemapify")
install.packages("dplyr")


# load packages
# turn-off scientific notation
# select bw theme
options(scipen=999)
library(ggplot2)
library(treemapify)
library(dplyr)
theme_set(theme_bw())

# 5 graphs

# horizontal bar chart
hbar <- data[grep("Trump", data$title), ]

ggplot(data = hbar, aes(x = year)) +
  geom_bar(color = "chocolate1", fill = "chocolate1") +
  labs(x = "Publication year",
       y = "Frequency",
       title = "Horizontal Bar Chart",
       subtitle = "Articles with titles mentioning Trump, by year (N=2,553)",
       caption = "Source: New York Times") +
  coord_flip()


# tree map 

tree_sub <- subset(data, date < "2015-01-07" & date > "2015-01-01")

plotdata <- tree_sub %>%
  count(author)

ggplot(plotdata,
       aes(fill = author,
           area = n)) +
  geom_treemap()+
  labs(title = "Tree Map",
       subtitle = "New York Times articles in the first week of 2015 by author",
       caption ="Source: New York Times")

# bar graph
bar <- data[grep("stocks", data$content), ]

ggplot(data = bar, aes(x = year)) +
  geom_bar(fill = "darkslateblue", color = "darkslateblue") +
  labs(x = "Publication year",
       y = "Count",
       title = "Bar Graph",
       subtitle = "New York Times articles containing 'stocks' in the article body (N=214)",
       caption = "Source: New York Times")


# kernel density plot

ggplot(data = data, aes(x = year)) +
  geom_density(fill = "darkseagreen",
               bw = 0.5)+
  labs(title = "Kernel Density Plot",
       subtitle = "New York Times articles by year
       bin width = 0.5",
       y = "Probability",
       x = "Year of publication",
       caption ="Source: New York Times")

# dot plot
sl_sub <- subset(data, author == "Sharon LaFraniere")

ggplot(data = sl_sub, aes(x = year))+
  geom_dotplot(color = "cornflowerblue",
               fill = "cornflowerblue",
               binwidth = 0.1)+
  labs(title = "Dot Plot",
       subtitle = "New York Times articles by Sharon LaFraniere (n=36)",
       y = "Proportion",
       x = "Publication year",
       caption ="Source: New York Times")
  