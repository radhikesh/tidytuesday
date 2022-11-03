library(tidytuesdayR)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(scales)
tuesdata <- tidytuesdayR::tt_load('2022-11-01')
horror_movies <- tuesdata$horror_movies

head(horror_movies)

# looking at revenue vs budget:
theme_set(theme_bw())
ggplot2::ggplot(data = horror_movies, mapping=aes(x=budget, y=revenue)) + 
                          geom_point(colour="red", alpha=0.5) + 
                          geom_smooth(method = "lm",colour="blue",size=0.5) +
                          labs(x = "Budget (USD)", y = "Revenue (USD)", 
                               title = "Horror Movies Revenue by Budget",
                               caption="TidyTuesday: Week 44 | Source: The Movie DB | Visualization: @rRadhikesh") +
                          scale_y_continuous(expand=c(0,0), labels = unit_format(unit = "M", scale = 1e-6))+
                          scale_x_continuous(expand=c(0,0),labels = unit_format(unit = "M", scale = 1e-6))+
                          theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold.italic")) 
# looking at runtime vs year
runtime_year <- horror_movies %>% select(id, runtime, release_date) %>% 
                        mutate(year = year(release_date), runtime_hr = runtime/60) 

ggplot2::ggplot(data = runtime_year, 
                mapping=aes(x=year, y=runtime_hr)) + geom_point() + geom_smooth(method='lm')

# language which produces most horror movies
language_data <- horror_movies %>% select(id,original_language) %>% 
                                   group_by(original_language) %>% 
                                   summarise(cnt=n()) %>% dplyr::arrange(-(cnt))

ggplot2::ggplot(aes(x=reorder(original_language, -cnt), y=cnt), 
                data=language_data[1:10,]) +
                geom_bar(stat="identity", fill="steelblue")+
                geom_text(aes(label=cnt), vjust=-0.3, size=3.5)+
                xlab("Language") + ylab("Count") +
                theme_minimal()
