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

eurovision <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-17/eurovision.csv')


# win by most countries
win_most <- eurovision %>% select(event, year, artist, artist_country, winner) %>% filter(winner==T)

ggplot(data = win_most,mapping = aes(x=fct_infreq(artist_country))) + 
                 geom_bar(stat = "count", fill = "steelblue") +
                 theme_minimal() +
                 theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                 scale_y_continuous(breaks = seq(0,16,2))+
                 xlab("")+
                 ylab("Count") +
                 ggtitle("Most wins by Country") +
                theme(plot.title = 
                        element_text(hjust = 0.5))

# number of countries participated
# ref: https://towardsdatascience.com/create-animated-bar-charts-using-r-31d09e5841da

country_participated <- eurovision %>% select(year, artist_country) %>% 
                                   group_by(year, artist_country) %>% summarise(cnt=n())

country_participated <- country_participated %>% 
                                        mutate(rank = rank(artist_country)) %>% 
                                        filter(rank<=10)

static_plot <- ggplot(country_participated, aes(rank, group = artist_country, 
                              fill = as.factor(artist_country),
                              color = as.factor(artist_country)))+
                            geom_tile(aes(y = cnt/2,
                                          height = cnt,
                                          width = 0.9), alpha = 0.8, color = NA) +
                            geom_text(aes(y = 0, label = paste(artist_country, " ")), vjust = 0.2,
                                      hjust = 1) +
                            geom_text(aes(y=cnt, label = cnt, hjust = 0)) +
                            xlab("") + ylab("")+
                            coord_flip(clip = "off", expand = F) +
                            scale_x_reverse() +
                            guides(color = F, fill = F) +
                            theme(axis.line = element_blank(),
                                  axis.text.x = element_blank(),
                                  axis.text.y = element_blank(),
                                  axis.ticks = element_blank(),
                                  axis.title.x = element_blank(),
                                  legend.position = "none",
                                  panel.background = element_blank(),
                                  panel.border = element_blank(),
                                  panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  # panel.grid.major.x = element_blank(size=.1, color = "grey"),
                                  # panel.grid.minor.x = element_line(size=.1, color = "grey"),
                                  plot.title = element_text(size = 25, hjust = 0.5, face = "bold",
                                                            colour = "grey", vjust=-1),
                                  plot.subtitle = element_text(size = 18, hjust=0.5, face = "italic", 
                                                               color = "grey"),
                                  plot.caption = element_text(size = 8, hjust=0.5, face = "italic",
                                                              color = "grey"),
                                  plot.background = element_blank(),
                                  plot.margin = margin(2,2,2,4, "cm")
                            )

anim <- static_plot + transition_states(year, transition_length = 4, state_length = 1) +
                        view_follow(fixed_x = T) +
                        labs(title = 'Participation per Year: {closest_state}',
                             Subtitle = "",
                             caption = "Eurovision Pariticipation | Data Source: Tidytuesday")


# for GIF:

animate(anim, 200, fps=10, width=1200, height = 1000, 
                                   renderer = gifski_renderer("gganim_eurovision.gif"))





                                   


  
