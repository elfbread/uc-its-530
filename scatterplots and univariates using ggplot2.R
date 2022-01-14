# importing dataset
setwd(choose.dir())
getwd()
data=read.csv("C:\\Users\\britt\\Desktop\\ITS 530\\nytimes.csv")

# descriptives of dataset
str(data)
sum(is.na(data))

# installing ggplot2 package
install.packages("ggplot2")

# load package and data
# turn-off scientific notation
# select bw theme
options(scipen=999)
library(ggplot2)
theme_set(theme_bw())

# scatterplot of id by year
gg <- ggplot(data = data, mapping = aes(x = id, y = year)) +
geom_point(aes(col=month, size=year))+
geom_smooth(method="loess", se=F) + 
xlim(c(0, 25000)) + 
ylim(c(2014, 2019)) + 
labs(subtitle="ID number vs. year of publication", 
  y="Year of Publication", 
  x="ID Number", 
  title="Scatterplot of New York Times Articles, 2014 - 2019", 
  caption = "Source: New York Times")

plot(gg)



# scatterplot with encoding
# install 'ggalt' pkg
install.packages("ggalt")
options(scipen = 999)``
library(ggplot2)
library(ggalt)


#ordered bar chart, count by year

library(ggplot2)

bg <- ggplot(data=data, aes(x=year))+
  geom_bar(width=0.75, fill="cyan4")+
  labs(title="Ordered bar chart",
       subtitle="New York Times articles published by year",
       caption="Source: New York Times",
       y="Number articles published",
       x="Year of publication")
plot(bg)


#categorical histogram
library(ggplot2)
theme_set(theme_classic())

hg <- ggplot(data=data, aes(x=year, fill=supp))+
geom_bar(aes(fill=front_page), width = 1)+
    labs(title="Histogram with categorical variables",
       subtitle="New York Times articles published on front page by year",
       caption="Source: New York Times",
       y="Count",
       x="Publication year",
       fill="Front Page")
plot(hg)

#pie chart
library(ggplot2)
theme_set(theme_classic())


pie <- ggplot(data = data, aes(x = "", fill = factor(year)))+
  geom_bar(width = 1) +
 theme(axis.line = element_blank(),
       axis.text.x = element_blank(),
  plot.title = element_text(hjust=0.5))+
  labs(fill="Year of publication",
       x=NULL,
       y=NULL,
       title="Pie chart of year of publication",
       caption = "Source: New York Times")

pie + coord_polar(theta = "y", start=0)