# importing dataset
setwd(choose.dir())
getwd()
data=read.csv("C:\\Users\\britt\\Desktop\\ITS 530\\nytimes.csv")

# installing ggplot2 package
install.packages("ggplot2")
install.packages("dplyr")
install.packages("ggridges")
install.packages("stringr")
install.packages("scales")



# load packages
# turn-off scientific notation
# select bw theme
options(scipen=999)
library(ggplot2)
library(dplyr)
library(ggridges)
library(stringr)
library(scales)
theme_set(theme_bw())

#freq of author
table(data$author)


# change month to integer
month_int <- as.integer(data$month)

# word count column
wordCount <- str_count(data$content, "\\w+")
data$wordCount <- wordCount

# letter count column
char <- nchar(data$content, type = "chars", allowNA = FALSE, keepNA = NA)
data$characterCount <- char

# descriptives of dataset
str(data)
sum(is.na(data))

# 5 graphs

# 1. box plot
bp_sub <- subset(data, author == "Adam Liptak")
ggplot(data = bp_sub) +
  geom_boxplot(mapping = aes(y=wordCount), fill = "#0099f8") +
  labs(title = "Boxplot",
       subtitle = "New York Times articles by Adam Liptak by month (N=129)",
       y = "Word count",
       caption ="Source: New York Times")

# 2. violin plot
vp_yr <-subset(data, year == 2018)
ggplot(data = vp_yr,
       aes(x = front_page,
           y = wordCount))+
  geom_violin(mapping = aes(y=wordCount), fill = "deeppink4")+
  geom_boxplot(width = .2,
               fill = "orange",
               outlier.size = 2)+
  labs(title = "Violin chart with boxplots",
       subtitle = "Word count in 2018 New York Times articles by front page status (N=1,513)",
       y = "Word count",
       x = "Front Page",
       caption ="Source: New York Times")


# 3. cleveland dot chart

cleve_chart <- subset(data, date < "2016-12-07" & date > "2016-12-01")
ggplot(cleve_chart,
       aes(x = wordCount, y = author)) +
  geom_point(aes(color = front_page))+
  labs(title = "Cleveland dot chart",
       subtitle = "New York Times articles between 12-01-2016 and 12-07-2016 by author and front page status (N=56)",
       y = "Author",
       x = "Word count",
       caption ="Source: New York Times")


# 4. stacked bar chart
ggplot(data = data,
       aes(x = month_int,
           fill = front_page)) +
  geom_bar(position = "stack") +
  scale_x_continuous(n.breaks = 11) +
  coord_cartesian(xlim = c(1, 12)) +
  scale_fill_manual(values = c("False" = "seashell3", "True" = "skyblue")) +
  labs(title = "Stacked bar chart",
       subtitle = "New York Times articles by month and front page status (N=10,732)",
       y = "Count",
       x = "Publication month",
       caption ="Source: New York Times",
       fill = "Front Page")

# 5. ridgeline
ggplot(data = data,
       aes(x = month_int,
           y = year,
           fill = year,
           group = year)) +
  geom_density_ridges(scale=2) +
  theme_ridges() +
  scale_x_continuous(n.breaks = 11) +
  coord_cartesian(xlim = c(1, 12)) +
  labs(title = "Ridgeline graph",
       subtitle = "New York Times articles",
       y = "Publication year",
       x = "Publication month",
       caption ="Source: New York Times",
       fill = "Publication year")

