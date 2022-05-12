library(tidytuesdayR)
library(ggplot2)
library(tidyverse)
library(ggthemes)
library(patchwork)

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





