# importing dataset
setwd(choose.dir())
getwd()
data=read.csv("C:\\Users\\britt\\Desktop\\ITS 530\\nytimes.csv")

# installing packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("stringr")
install.packages("lubridate")
install.packages("quantmod")
install.packages("ggalt")
install.packages("tidyr")
install.packages("ggcorrplot")
install.packages("visreg")
install.packages("scales")
install.packages("readr")
install.packages("vcd")
install.packages("survival")
install.packages("survminer")

# load packages
# turn-off scientific notation
# select bw theme
options(scipen=999)
library(ggplot2)
library(dplyr)
library(stringr)
library(lubridate)
library(quantmod)
library(ggalt)
library(tidyr)
library(ggcorrplot)
library(visreg)
library(scales)
library(readr)
library(vcd)
library(survival)
library(survminer)
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

# subset to remove 2014 and 2019
data <- subset(data, year > 2014 & year < 2019)

# export data set
write.csv(data2, "C:\\Users\\britt\\Desktop\\ITS 530\\nytimes_bb.csv", row.names = TRUE)

################################################################################
# correlation plots/ correlation matrix
# select numeric variables
df <- dplyr::select_if(data, is.numeric)

# calculate correlations
r <- cor(df, use = "complete.obs")
round(r,2)

# correlation matrix
ggcorrplot(r)
ggcorrplot(r,
           hc.order = TRUE,
           type = "lower",
           lab = TRUE)

################################################################################
# linear regression conditional plot

lin <- lm(characterCount ~ year + wordCount + month,
          data=data)
print(summary(lin))

visreg(lin, "wordCount", gg = TRUE) +
  scale_y_continuous(labels = comma)+
  scale_x_continuous(labels = comma)+
  labs(title = "Linear regression",
       subtitle = "Word count and character count",
       x = "Word Count",
       y = "Character Count",
       caption = "Source: New York Times")

################################################################################
# logistic regression
# binary -> true/false

# recode front_page from boolean to 0/1 where 1 is yes
class(data$front_page)
fp_num <- as.integer(as.logical(data$front_page))
data$fp_num <- fp_num

log <- glm(fp_num ~ year + wordCount + characterCount + id,
           family = "binomial",
           data = data)

visreg(log, "id",
       gg = TRUE,
       scale="response")+
  scale_x_continuous(labels = comma)+
  labs(title = "Logistic Regression",
       subtitle ="ID number and front page status",
       x = "ID Number",
       y = "Front Page",
       caption = "Source: New York Times")

################################################################################
# multiple logistic regression

visreg(log, "id",
       by ="year",
       gg = TRUE,
       scale="response")+
  scale_x_continuous(labels = comma)+
  labs(title = "Multiple logistic regressions by year",
       x = "ID",
       y = "Front Page",
       caption = "Source: New York Times")
################################################################################
# survival plot - kaplan-meier curve

year_num <- as.numeric(data$year)
month_num <- as.numeric(data$month)

year_mon <- ymd(paste(data$year,data$month, "15", sep = "-"))
data$year_mon <- wordCount
year_mon <- as.numeric(data$year_mon)

data = data
sfit <- survfit(Surv(year_mon,fp_num==1) ~1)

ggsurvplot(sfit, data = data,
           title = "Kaplan-Meier Curve",
           subtitle = "Year and front page",
           caption = "Source: New York Times")

# split kaplan meier curve
data = data
sfit2 <- survfit(Surv(year_mon,fp_num==1) ~month_num)

ggsurvplot(sfit2, data = data,
           title = "Split Kaplan-Meier Curve",
           subtitle = "Year and front page, by month",
           caption = "Source: New York Times")


################################################################################
# mosaic plots
tbl <- xtabs(~front_page + month + year, data)
ftable(tbl)

mosaic(tbl, 
       labeling_args = list(set_varnames = c(front_page = "Front Page",
                                             month = "Month",
                                             year = "Year")),
       main = "Mosaic Plot")

mosaic(tbl,
       shade = TRUE,
       legend = TRUE,
       labeling_args = list(set_varnames = c(front_page = "Front Page",
                                             month = "Month",
                                             year = "Year")),
       set_labels = list(year = c("15", "16", "17", "18")),
       main = "Mosaic Plot with Shading")

