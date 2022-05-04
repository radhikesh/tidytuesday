library(tidytuesdayR)
library(ggplot2)
library(tidyverse)
library(ggthemes)

tuesdata <- tidytuesdayR::tt_load('2022-05-03')

names(tuesdata)

capacity <- tuesdata$capacity
wind <- tuesdata$wind
solar <- tuesdata$solar
average_cost <- tuesdata$average_cost

# combining both wind and solar data:
wind_solar <- wind %>% mutate(type = "wind") %>% rename(projected_price = wind_mwh, projected_capacity = wind_capacity) %>% 
                       bind_rows(solar %>% mutate(type = "solar") %>% 
                                   rename(projected_price = solar_mwh, projected_capacity = solar_capacity))   

p_solar_wind_mwh <- ggplot(wind_solar, aes(x=date, y=projected_price))+
                        geom_line(aes(color = type), size = 1) +
                        scale_color_manual(values = c("#00AFBB", "#E7B800")) +
                        theme_minimal()

p_solar_wind_mwh <- ggplot(wind_solar, aes(x=date, y=projected_capacity))+
                              geom_line(aes(color = type), size = 1) +
                              scale_color_manual(values = c("#00AFBB", "#E7B800")) +
                              theme_minimal()



