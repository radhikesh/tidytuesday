library(tidytuesdayR)
library(ggplot2)
library(tidyverse)
library(ggthemes)
library(patchwork)

#downloading data:x
tuesdata <- tidytuesdayR::tt_load('2022-05-03')

names(tuesdata)
capacity <- tuesdata$capacity
wind <- tuesdata$wind
solar <- tuesdata$solar
average_cost <- tuesdata$average_cost

# combining both wind and solar data:
wind_solar <- wind %>% mutate(type = "Wind") %>% rename(projected_price = wind_mwh, projected_capacity = wind_capacity) %>% 
                       bind_rows(solar %>% mutate(type = "Solar") %>% 
                                   rename(projected_price = solar_mwh, projected_capacity = solar_capacity))   

p_solar_wind_mwh <- ggplot(wind_solar, aes(x=date, y=projected_price))+
                        geom_line(aes(color = type), size = 1) +
                        scale_color_manual(values = c("#E7B800", "#00AFBB")) +
                        theme_minimal() + ylab("Projected Price ($/MWH)") + xlab("Year") +
                        ggtitle("Projected Price in $/MWH by Year") +
                        scale_x_date(date_labels = "%Y", breaks = "2 year", limits = c(as.Date(min(wind_solar$date)),
                                                                                       as.Date(max(wind_solar$date))))+
                        scale_y_continuous(breaks = seq(0,300,50)) +
                        theme(plot.title = 
                              element_text(hjust = 0.5),
                              legend.position = c(0.9, 0.8),
                              plot.margin = margin(0.2, 0.5, 0.2, 0.2, "cm"))+
                        labs(color = "Type")

p_solar_wind_mwh <- ggplot(wind_solar, aes(x=date, y=projected_capacity))+
                              geom_line(aes(color = type), size = 1) +
                              scale_color_manual(values = c("#00AFBB", "#E7B800")) +
                              theme_minimal()


# average cost:
average_cost_long <- average_cost %>% rename(Gas = gas_mwh, Wind = wind_mwh, Solar = solar_mwh) %>% 
                           pivot_longer(cols = Gas:Wind, names_to = "type", values_to = "avg_cost")
average_cost_long$type <- factor(average_cost_long$type, levels=c("Gas", "Solar", "Wind"))

average_cost_long$date <- lubridate::ymd(average_cost_long$year, truncated = 2)
p_solar_wind_gas_mwh <- ggplot(average_cost_long, aes(x=date, y=avg_cost))+
                            geom_line(aes(color = type), size = 1) +
                            scale_color_manual(values = c( "red", "#E7B800", "#00AFBB")) +
                            theme_minimal() + ylab("Avg Price ($/MWH)") + xlab("Year") +
                            ggtitle("Avg Price in $/MWH by Year") +
                            scale_x_date(date_labels = "%Y", breaks = "2 year", 
                                        limits = c(as.Date(min(average_cost_long$date)),
                                                        as.Date(max(average_cost_long$date)))) +
                            scale_y_continuous(breaks = seq(0,200,25)) +
                            theme(plot.title = 
                                    element_text(hjust = 0.5),
                                  legend.position = c(0.9, 0.8),
                                  plot.margin = margin(0.2, 0.5, 0.2, 0.2, "cm")) +
                            labs(color = "Type",
                                 caption="TidyTuesday: Week 18 | Source: Berkeley Lab | Visualization: @rRadhikesh")

# combining plots together:
p_solar_wind_mwh + p_solar_wind_gas_mwh
