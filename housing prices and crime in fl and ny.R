# importing data set
setwd(choose.dir())
getwd()
data=read.csv("C:\\Users\\britt\\Desktop\\ITS 530\\midterm_data.csv")

# installing ggplot2 package
install.packages("tidyverse")
install.packages("scales")


# load packages
# turn-off scientific notation
# select bw theme
options(scipen=999)
library(tidyverse)
library(scales)
theme_set(theme_bw())

# convert price to numeric
price_num <- as.numeric(gsub(",","",data$Price))
data$price_num <- price_num

# confirm no missing values
str(data)
sum(is.na(data))

#question 1
# summary statistics
tapply(data$price_num, data$Location, summary)

# boxplot for cost
ggplot(data = data,
       aes(x = Location, y = price_num, color = Location))+
geom_boxplot(outlier.shape=NA)+
  geom_point(position = position_jitterdodge())+
  scale_y_continuous(labels = scales::label_dollar())+
  labs(title = "Boxplot",
       subtitle = "Housing cost in Florida and New York",
       y = "Price",
       x = "Location")


#question 2
# summary stats for crime
tapply(data$Crime.Rating, data$Location, summary)

#compute median
med_cr_df <- data %>%
  group_by(Location) %>%
  summarize(median=median(Crime.Rating))

#generate density plots

ggplot(data = data,
       aes(x=Crime.Rating, fill=Location))+
  geom_density(alpha=0.3,size=1)+
  geom_vline(data = med_cr_df,
             aes(xintercept = median,
                 color = Location), linetype="dashed", size=1)+
  scale_x_continuous(n.breaks = 9) +
  coord_cartesian(xlim = c(1, 9)) +
  labs(x= "Crime Rating",
       y= "Density",
       title = "Density Plots",
       subtitle = "Comparing crime rates between FL and NY, with median crime value highlighted")

# question 3
# scatterplot for crime vs. higher/lower cost
ggplot(data = data,
       aes(x = Crime.Rating, y = price_num)) +
  geom_point(aes(color = Location), size = 3)+
  geom_smooth(method = lm, linetype = "dashed", color="darkred")+
  scale_x_continuous(n.breaks = 9) +
  coord_cartesian(xlim = c(1, 9)) +
  scale_y_continuous(labels = scales::label_dollar())+
  scale_color_brewer(palette="Dark2") + theme_classic()+
  labs(title = "Scatterplot",
       subtitle = "Comparing housing cost and crime rating in FL and NY",
       y = "Price",
       x = "Crime rating")

###########################################################################################

# assumptions
# interest rate 
  # FL - 4.04%
  # NY - 4.04%
# paying 100% of salary to housing cost

# importing data set
setwd(choose.dir())
getwd()
payoff=read.csv("C:\\Users\\britt\\Desktop\\ITS 530\\payoff.csv")

# calculate mean housing cost for each location
mean_price <- data %>%
  group_by(Location) %>%
  summarize(mean=mean(price_num))
print(mean_price)


# line graph with one line for each state

# subset 
payoff_sub <- subset(payoff, balance_remain > 0)

# line graphs
ggplot (payoff_sub,
        aes(x=month, y=balance_remain, group=state, color=state))+
geom_line(linetype="dashed") +
  geom_point()+
scale_y_continuous(labels = scales::label_dollar())+
  labs(title = "Line graph",
       subtitle = "Number of months to pay off loan in FL and NY",
       y = "Balance Remaining",
       x = "Month",
       caption = "Assumptions:
       Loan interest rate is 4.04%
       100% of salary is spent on housing cost",
       color = "State")


