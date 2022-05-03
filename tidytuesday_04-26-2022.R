#install.packages("tidytuesdayR")
library(tidytuesdayR)
library(ggplot2)
library(tidyverse)
library(ggthemes)
library(magick)

tuesdata <- tidytuesdayR::tt_load('2022-04-26')

hidden_gems <- tuesdata$hidden_gems

hidden_gems$author_kaggle

foo <- hidden_gems %>% 
              distinct(author_twitter, author_linkedin) %>%
              pivot_longer(everything(), names_to = "media", values_to = "links") %>% 
              mutate(media = str_to_title(str_replace(media, "author_", ""))) %>% 
              group_by(media) %>% 
              summarise(total = n(), ct = sum(!is.na(links)), frac = mean(!is.na(links)))

path_img <- "/images/"

img_d <- tibble(
  media = c("Linkedin", "Twitter"),
  img = str_c(getwd(),path_img, c("linkedin_logo.png", "twitter_logo.jpg"))
)


p <- foo %>% 
  ggplot(aes(media, frac)) +
  geom_col(fill = "white") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1)) +
  theme_hc() +
  theme(legend.position = "none") +
  labs(x = "", y = "", title = "Percentage of Authors with Social Media on their Profile")

p_dat <- ggplot_build(p)

p_map <- p_dat$data[[1]] %>% 
  select(xmin, xmax, ymin, ymax) %>% 
  bind_cols(foo %>% select(media)) %>% 
  left_join(img_d, by = "media")

for (i in seq(nrow(p_map))){
  
  p <- p +
    annotation_custom(
      grid::rasterGrob(image_read(p_map$img[i]), 
                       width = unit(1, "npc"), 
                       height = unit(1, "npc")),
      xmin = p_map$xmin[i], 
      xmax = p_map$xmax[i],
      ymin = p_map$ymin[i],
      ymax = p_map$ymax[i])
}

p




