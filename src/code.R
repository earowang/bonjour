## ---- load-pkgs
library(lubridate)
library(tidyverse)
library(tsibble)
library(sugrrants)
library(ggmap)

## ---- theme-remark
theme_remark <- function() {
  theme_grey() +
  theme(
    axis.text = element_text(size = 14), 
    strip.text = element_text(size = 16), 
    axis.title = element_text(size = 16),
    legend.title = element_text(size = 16), 
    legend.text = element_text(size = 16),
    legend.position = "bottom"
  )
}
theme_set(theme_remark())

## ---- map
sensor_loc <- rwalkr::pull_sensor()
qmplot(x = Longitude, y = Latitude, data = sensor_loc,
  colour = I("#d95f02"), size = I(5))

## ---- selected-sensor
sensors <- c("Southern Cross Station", "Victoria Market", "Flinders St-Elizabeth St (East)")

sensor_more <- sensor_loc %>% 
  mutate(
    Sensor = if_else(Sensor == "QV Market-Peel St", "Victoria Market", Sensor),
    Selected = if_else(Sensor %in% sensors, TRUE, FALSE)
  )
sensor_unsel <- sensor_more %>% 
  filter(Selected == FALSE)
sensor_sel <- sensor_more %>% 
  filter(Selected == TRUE)
qmplot(
  x = Longitude, y = Latitude, data = sensor_unsel,
  colour = Selected, shape = Selected, size = I(5)
) +
geom_point(aes(x = Longitude, y = Latitude), data = sensor_sel, size = I(12)) +
scale_colour_brewer(palette = "Dark2") +
theme_remark() +
theme(legend.position = "none")

## ---- ped-data
ped_run <- rwalkr::run_melb(year = 2017, na.rm = TRUE)
pedestrian <- ped_run %>% 
  mutate(
    Sensor = if_else(Sensor == "QV Market-Peel St", "Victoria Market", Sensor)
  )

## ---- ped-sub
subdat <- pedestrian %>% 
  filter(Sensor %in% sensors)

## ---- ts-plot-na
# conventional time series plot
subdat %>% 
  ggplot(aes(x = Date_Time, y = Count, colour = Sensor)) +
  geom_line(size = 0.5) +
  facet_grid(
    Sensor ~ ., 
    labeller = labeller(Sensor = label_wrap_gen(20)),
    scales = "free_y"
  ) +
  scale_colour_brewer(
    palette = "Dark2", 
    guide = guide_legend(title = "Sensor")
  ) +
  theme_remark() +
  xlab("Date Time") +
  ylab("Hourly Counts")

## ---- tsibble
subdat_tsbl <- subdat %>% 
  as_tsibble(key = id(Sensor), index = Date_Time)
subdat_tsbl

## ---- fill-na0
subdat_tsbl %>% fill_na()

## ---- fill-na
subdat_full <- subdat_tsbl %>% 
  fill_na(
    Date = as_date(Date_Time),
    Time = hour(Date_Time)
  )
subdat_full

## ---- ts-plot
subdat_full %>% 
  ggplot(aes(x = Date_Time, y = Count, colour = Sensor)) +
  geom_line(size = 0.5) +
  facet_grid(
    Sensor ~ ., 
    labeller = labeller(Sensor = label_wrap_gen(20)),
    scales = "free_y"
  ) +
  scale_colour_brewer(
    palette = "Dark2", 
    guide = guide_legend(title = "Sensor")
  ) +
  theme_remark() +
  xlab("Date Time") +
  ylab("Hourly Counts")

## ---- tsummarise
subdat_daily <- subdat_full %>% 
  group_by(Sensor) %>% 
  tsummarise(
    Date = as_date(Date_Time),
    DailyCount = sum(Count, na.rm = TRUE)
  )
subdat_daily

## ---- daily-sensor
subdat_daily %>% 
  ggplot(aes(x = Date, y = DailyCount, colour = Sensor)) +
  geom_line() +
  # geom_point() +
  facet_grid(
    Sensor ~ ., 
    labeller = labeller(Sensor = label_wrap_gen(20)),
    scales = "free_y"
  ) +
  scale_colour_brewer(
    palette = "Dark2", 
    guide = guide_legend(title = "Sensor")
  ) +
  theme_remark() +
  xlab("Date") +
  ylab("Daily Counts")

## ---- mutate
subdat_full <- subdat_full %>% 
  mutate(
    Holiday = if_else(
      Date %in% c(au_holiday(2017)$date, as_date("2017-09-29")), 
      TRUE, FALSE),
    Day = wday(Date_Time, label = TRUE, week_start = 1)
  )
subdat_full

## ---- facet-time
# time series plot faceted by sensors and day of week
subdat_full %>% 
  ggplot(aes(x = Time, y = Count, group = Date, 
    colour = Sensor)) +
  geom_line(size = 0.5) +
  facet_grid(
    Sensor ~ Day, 
    labeller = labeller(Sensor = label_wrap_gen(20)),
    scales = "free_y"
  ) +
  scale_x_continuous(breaks = seq(6, 23, by = 6)) +
  scale_colour_brewer(
    palette = "Dark2", 
    guide = guide_legend(title = "Sensor")
  ) +
  theme_remark() +
  xlab("Time") +
  ylab("Hourly Counts")

## ---- sx
sx_cal <- subdat_full %>% 
  filter(Sensor == "Southern Cross Station") %>% 
  frame_calendar(x = Time, y = Count, date = Date)
sx_cal %>% 
  select(Date_Time, Time, Count, Date, .Time, .Count)

## ---- sx-plot
p_sx <- sx_cal %>% 
  ggplot(aes(.Time, .Count, group = Date, colour = Holiday)) +
  geom_line() +
  theme_remark()
p_sx

## ---- sx-prettify
prettify(p_sx)

## ---- sx-march
p3_sx <- subdat_full %>% 
  filter(
    Sensor == "Southern Cross Station",
    Date >= as_date("2017-03-01"), Date <= as_date("2017-03-31")
  ) %>% 
  mutate(Adele = if_else(
    Date %in% as_date(c("2017-03-18", "2017-03-19")), TRUE, FALSE
  )) %>% 
  frame_calendar(x = Time, y = Count, date = Date) %>% 
  ggplot(aes(.Time, .Count, group = Date, colour = Adele)) +
  geom_line() +
  theme_remark()
prettify(p3_sx, label = c("label", "text", "text2"), size = 5)

## ---- dec
dec <- subdat_full %>% 
  filter(
    Sensor == "Flinders St-Elizabeth St (East)",
    Date >= as_date("2017-12-01"), Date <= as_date("2017-12-31")
  ) %>% 
  mutate(Car_Attack = if_else(Date == as_date("2017-12-21"), TRUE, FALSE)) %>% 
  frame_calendar(x = Time, y = Count, date = Date) %>% 
  ggplot(aes(.Time, .Count, group = Date, colour = Car_Attack)) +
  geom_line() +
  theme_remark()
prettify(dec, label = c("label", "text", "text2"), size = 5)

