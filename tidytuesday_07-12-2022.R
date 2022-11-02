library(tidytuesdayR)
library(ggplot2)
library(tidyverse)
library(ggthemes)
library(patchwork)

library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(highcharter)

library(fable)
library(fabletools)
library(tsibble)
library(lubridate)
# downloading data:x
tuesdata <- tidytuesdayR::tt_load('2022-07-12')
flights <- tuesdata$flights

flights_1 <- flights %>% select(YEAR, MONTH_NUM, MONTH_MON, FLT_DATE, APT_NAME,STATE_NAME, FLT_TOT_1)

flights_1_temp <- flights_1 %>% select(YEAR, MONTH_NUM, MONTH_MON, FLT_TOT_1) %>% 
                                 group_by(YEAR, MONTH_NUM, MONTH_MON) %>% 
                                 summarise(total_flights=sum(FLT_TOT_1, na.rm = T)) %>% arrange(YEAR, MONTH_NUM)

flights_1_temp_20_21_22 <- flights_1_temp %>% filter(YEAR %in% c(2019, 2020, 2021, 2022)) %>% 
                                              mutate(year_mon = paste0(MONTH_MON,"-",YEAR))

# set options
hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- ","
options(highcharter.lang = hcoptslang)

# bar chart:
hchart(flights_1_temp_20_21_22, "column", hcaes(x = year_mon,y = total_flights, group=YEAR)) %>%  
                    hc_xAxis(title = list(text = "")) %>% 
                    hc_yAxis(title = list(text = ""),
                             labels = list(format="{value:,f}")) %>% 
                    hc_title(
                      text = "Number of Commercial flights in Europe since 2019",
                      align = "right"
                    )
# line chart:
hchart(flights_1_temp_20_21_22, "line", hcaes(x = year_mon,y = total_flights, group=YEAR)) %>%  
  hc_xAxis(title = list(text = "")) %>% 
  hc_yAxis(title = list(text = ""),
           labels = list(format="{value:,f}")) %>% 
  hc_title(
    text = "Number of Commercial flights in Europe since 2019",
    align = "right"
  )

# flight predictions:
#ref: https://github.com/doehm/tidyTuesday/blob/master/2022/week28-flights/flights.R
flights <- flights %>% rename_all(tolower)

df_base <- flights %>% 
  mutate(flt_date = floor_date(ymd(flt_date), unit = "month")) %>% 
  group_by(year, month_mon, flt_date) %>% 
  summarise(
    dep = sum(flt_dep_1),
    arr = sum(flt_arr_1)
  ) %>% 
  mutate(yr_mth = yearmonth(paste(year, month_mon)))

# fit time series model:
mod <- df_base %>% 
           filter(flt_date < ymd("2020-02-01")) %>% 
           as_tsibble(index=yr_mth) %>% 
           model(ARIMA(dep))
