library(tidytuesdayR)
library(tidyverse)
library(ggplot2)

tuesdata <- tidytuesdayR::tt_load('2022-11-01')
horror_movies <- tuesdata$horror_movies

head(horror_movies)

# looking at revenue vs budget:



# looking at runtime vs year



# language which produces most horror movies
language_data <- horror_movies %>% select(id,original_language) %>% 
                                   group_by(original_language) %>% 
                                   summarise(cnt=n()) %>% dplyr::arrange(-(cnt))

ggplot2::ggplot(aes(x=original_language, y=cnt), 
                data=language_data)+geom_bar(stat = "identity")
