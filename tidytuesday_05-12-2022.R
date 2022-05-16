library(tidytuesdayR)
library(ggplot2)
library(tidyverse)
library(ggthemes)
library(patchwork)

library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
# downloading data:x
# tuesdata <- tidytuesdayR::tt_load('2022-05-10')
# tuesdata <- tidytuesdayR::tt_load(2022, week = 19)

nyt_titles <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_titles.tsv')
nyt_full <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_full.tsv')

# Top 10 authors based on number of weeks as best seller author
nyt_titles_top10_author <- nyt_titles %>% dplyr::arrange(-total_weeks) %>% head(10)

ggplot(data = nyt_titles_top10_author, mapping = aes(x=reorder(author, -total_weeks), y=total_weeks)) +
               geom_bar(stat = "identity", fill = "steelblue") +
               theme_minimal() +
               theme(axis.text.x = element_text(angle = 45, hjust = 1))+
               xlab("Author")+
               ylab("Number of Weeks on Top")+
               labs(caption="TidyTuesday: Week 19 | Source: New York Times | Visualization: @rRadhikesh")

# word cloud for the most common words used in the NY titles:
docs <- Corpus(VectorSource(nyt_titles$title))

inspect(docs)

toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

#cleaning text:
docs <- tm_map(docs, content_transformer(tolower))

docs <- tm_map(docs, removeNumbers)

docs <- tm_map(docs, removeWords, stopwords("en"))

docs <- tm_map(docs, removeWords, c("blabla1", "blabla2"))

docs <- tm_map(docs, removePunctuation)

docs <- tm_map(docs, stripWhitespace)

# building a term document matrix
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing = T)
d <- data.frame(word = names(v), freq = v)
head(d, 10)

# creating the word cloud:
set.seed(10)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words = 200, random.order = F,
          rot.per = 0.35, colors = brewer.pal(8, "Dark2"))

# which year had the most number of publications

number_of_publication <- nyt_titles %>% select(year) %>% group_by(year) %>% summarise(count=n())

# this section prepares a dataframe for labels:
# ref: https://r-graph-gallery.com/296-add-labels-to-circular-barplot
# adding id column for adding label
number_of_publication$id <- 1:nrow(number_of_publication)

# calculate the angle of the labels:
number_of_bar <- nrow(number_of_publication)
number_of_publication$angle <- 90-360 * (number_of_publication$id-0.5)/number_of_bar

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
number_of_publication <- number_of_publication %>% mutate(hjust = ifelse(angle < -90, 1,0))

# flip angle to make them readable
number_of_publication$angle <- ifelse(number_of_publication$angle < -90, number_of_publication$angle+180, 
                                      number_of_publication$angle)

p <-  ggplot(number_of_publication, aes(x=year, y=count)) +
             geom_bar(stat = "identity", fill = alpha("blue", 0.3)) +
             ylim(-120, 240)+
             theme_minimal()+
             theme(
               axis.text = element_blank(),
               axis.title = element_blank(),
               panel.grid = element_blank(),
               plot.margin = unit(rep(0.5,4), "cm")
             ) +
             coord_polar(start = 0) +
             geom_text(data=number_of_publication, aes(x=year, y=count+13, label=paste0(year," (", count,")"),
                                                       hjust=hjust), color="black",size=2.5,
                                                   alpha=0.6, angle=number_of_publication$angle, inherit.aes = F)

