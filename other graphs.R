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
install.packages("scales")
install.packages("scatterplot3d")
install.packages("factoextra")
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("RCurl")
install.packages("XML")
install.packages("networkD3")
install.packages("ggalluvial")
install.packages("superheat")
install.packages("devtools")
install.packages("remotes")
install.packages("GGally")
install.packages("waterfalls")


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
library(scales)
library(scatterplot3d)
library(factoextra)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(RCurl)
library(XML)
library(networkD3)
library(ggalluvial)
library(superheat)
library(devtools)
library(remotes)
library(GGally)
library(waterfalls)

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

# reclass front-page
fp_num <- as.integer(as.logical(data$front_page))
data$fp_num <- fp_num

# subset to remove 2014 and 2019
data <- subset(data, year > 2014 & year < 2019)

# create year & month combined variable
year_mon <- ymd(paste(data$year,data$month, "15", sep = "-"))
data$year_mon <- year_mon
year_mon <- as.numeric(data$year_mon)

################################################################################
# 3-d scatterplot
with(data, {
scatterplot3d(x = year,
              y = wordCount,
              z = characterCount,
              main = "3-D Scatterplot",
              xlab = "Year",
              ylab = "Article Word Count",
              zlab = "Article Character Count")
})

# 3-d scatterplot with lines
with(data, {
  scatterplot3d(x = year,
                y = wordCount,
                z = characterCount,
                color = "blue",
                pch = 19,
                type = "h",
                main = "3-D Scatterplot",
                xlab = "Year",
                ylab = "Article Word Count",
                zlab = "Article Character Count")
})

# 3-d scatterplot with labels
with(data, {
  s3d <-  scatterplot3d(
                x = year,
                y = wordCount,
                z = characterCount,
                main = "3-D Scatterplot",
                xlab = "Year",
                ylab = "Article Word Count",
                zlab = "Article Character Count")
  s3d.coords <-s3d$xyz.convert(year, wordCount, characterCount)
  
  text(s3d.coords$x,
       s3d.coords$y,
       labels = row.names(data),
       cex = .5,
       pos = 4)
})

# 3-d scatterplot by type
data$pcolor[data$fp_num == 0] <- "red"
data$pcolor[data$fp_num == 1] <- "blue"

with(data, {
  s3d <-  scatterplot3d(
    x = year,
    y = wordCount,
    z = characterCount,
    color = pcolor,
    pch = 19,
    type = "h",
    lty.hplot = 2,
    scale.y = .75,
    main = "3-D Scatterplot",
    xlab = "Year",
    ylab = "Article Word Count",
    zlab = "Article Character Count")
  s3d.coords <-s3d$xyz.convert(year, wordCount, characterCount)
  
  text(s3d.coords$x,
       s3d.coords$y,
       labels = row.names(data),
       cex = .5,
       pos = 4)
  
  legend("topleft",
         inset = .05,
         bty = "n",
         cex = .5,
         title ="Front Page, Yes or No",
         c("No", "Yes"),
         fill = c("red", "blue"))
})

################################################################################
# biplots
# haven't started

bip <- subset(data, id <= 50)
bip1 <- dplyr::select_if(bip, is.numeric)



fit <- prcomp(x = bip1,
              center = TRUE,
              scale = TRUE)

fviz_pca(fit,
         repel = TRUE,
         labelsize = 3)+
  theme_bw() +
  labs(title = "Biplot",
       caption = "Source: New York Times")


################################################################################
# bubble charts

bub_sub <- subset(data, year == 2018 & month <= 6)

ggplot(data = bub_sub,
       aes(x = month, y = wordCount, size = front_page))+
  geom_point()+
  scale_x_continuous(n.breaks = 5) +
  coord_cartesian(xlim = c(1, 6)) +
  scale_y_continuous(labels = comma) +
  labs(title = "Bubble Chart",
       subtitle = "Articles by word count and front page status in the first 6 months of 2018",
       x = "Month",
       y = "Word Count",
       size = "Front Page",
       caption = "Source: New York Times")

# bubble chart with customizations
ggplot(data = bub_sub,
       aes(x = month, y = wordCount, size = front_page))+
  geom_point(alpha = .5,
             fill = "cornflowerblue",
             color = "black",
             shape = 21)+
  scale_x_continuous(n.breaks = 5) +
  coord_cartesian(xlim = c(1, 6)) +
  scale_y_continuous(labels = comma) +
  labs(title = "Bubble Chart",
       subtitle = "Articles by word count and front page status in the first 6 months of 2018",
       x = "Month",
       y = "Word Count",
       size = "Front Page",
       caption = "Source: New York Times")

################################################################################
# Sankey diagrams

# create new data frame with summary count
links <- data %>% group_by(year,month) %>% summarize(N=n())
links$source <- links$month
links$target <- links$year
links$value <- links$N

nodes <- data.frame(
  name=c(as.character(links$source),
         as.character(links$target)) %>% unique()
)

links$IDsource <- match(links$source, nodes$name)-1
links$IDtarget <- match(links$target, nodes$name)-1

sankeyNetwork(Links = links,
              Nodes = nodes,
              Source = "IDsource",
              Target = "IDtarget",
              Value = "value",
              NodeID = "name",
              fontSize = 12,
              nodeWidth = 30)

# alluvial diagrams

alluv <- data %>% group_by(year,month,front_page) %>% summarize(N=n())
ggplot(alluv,
       aes(axis1 = month,
           axis2 = year,
           y = N))+
  geom_alluvium(aes(fill = front_page))+
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Month", "Year"))+
  scale_y_continuous(labels = comma) +
  labs(title = "Alluvial Diagram",
       caption = "Source: New York Times",
       fill = "Front Page")

# alternative alluvial diagram
ggplot(alluv,
       aes(axis1 = month,
           axis2 = front_page,
           axis3 = year,
           y = N))+
  geom_alluvium(aes(fill = month))+
  geom_stratum() +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Month", "Front Page","Year"))+
  scale_y_continuous(labels = comma) +
  labs(title = "Alluvial Diagram",
       caption = "Source: New York Times",
       fill = "Front Page")+
  theme(legend.position = "none")
################################################################################
# heatmap

hm <- data %>% group_by(month) %>% summarize(N=n())

superheat(hm)
# sorted heatmap
superheat(hm,
          left.label.text.size = 3,
          bottom.label.text.size = 3,
          bottom.label.size = .05,
          row.dendrogram = TRUE,
          title = "Heatmap")
################################################################################
#radar charts
devtools::install_github("ricardo-bion/ggradar", 
                         dependencies = TRUE)
library(ggradar)
class(data$wordCount)

rad <- data %>%
  group_by(year) %>%
  summarise(
    avg_word = mean(wordCount),
    avg_char = mean(characterCount)) %>%
      ungroup() %>%
      mutate_at(vars(-year), rescale)

plt <- rad %>%
  ggradar(
    grid.label.size = 4,
    axis.label.size = 4,
    group.point.size = 5,
    group.line.width = 1.5,
    legend.text.size = 6)+
  labs(title = "Radar Chart",
       caption = "Source: New York Times")

plt
################################################################################
# scatterplot matrix
df <- data %>%
  mutate(log_wordCount = log(wordCount),
         log_characterCount = log(characterCount)) %>%
  select(log_wordCount, log_characterCount)
ggpairs(df)

# customized scatterplot matrix

my_density <- function(data, mapping, ...){
  ggplot(data = data, mapping = mapping) +
    geom_density(alpha = 0.5,
                 fill = "cornflowerblue", ...)
}

my_scatter <- function(data, mapping, ...){
  ggplot(data = data, mapping = mapping) +
    geom_point(alpha = 0.5,
               color = "cornflowerblue") +
    geom_smooth(method=lm,
                se=FALSE, ...)
}

ggpairs(df,
        lower=list(continuous = my_scatter),
        diag = list(continous = my_density))+
  labs(title = "Scatterplot Matrix",
       caption = "Source: New York Times") +
  theme_bw()
################################################################################
#waterfall charts
ch_yr <-as.character(data$year)
data$ch_yr <- ch_yr

wt <- data %>%
  group_by(ch_yr) %>%
  summarise(
  avg_word = mean(wordCount)) %>%
ungroup()

wt$avg_word <- round(wt$avg_word, digits = 2)

waterfall(wt)+
  scale_y_continuous(labels = comma) +
  labs(title = "Waterfall Chart",
       caption = "Source: New York Times",
       y = "Average word count",
       x = "Year")+
  theme_minimal()


#waterfall chart with total column
waterfall(wt,
          calc_total=TRUE,
          total_axis_text = "Total",
          total_rect_text_color = "black",
          total_rect_color = "goldenrod1")+
  scale_y_continuous(labels = comma) +
  labs(title = "Waterfall Chart with Total",
       caption = "Source: New York Times",
       y = "Average word count",
       x = "Year")+
  theme_minimal()
################################################################################
# word cloud

# pick random article by id number
x1 <- runif(1, 2, 10000)
# selected 9697
x1 <- 9697

script <- "http://www.sthda.com/upload/rquery_wordcloud.r"
source(script)
res<-rquery.wordcloud("articletext.txt",
                      type = "file",
                      lang = "english")

