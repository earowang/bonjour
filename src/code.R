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
theme_remark()

## ---- ped-data
ped_run <- rwalkr::run_melb(year = 2017)
pedestrian <- ped_run %>% 
  mutate(
    Sensor = if_else(Sensor == "QV Market-Peel St", "Victoria Market", Sensor),
    Holiday = if_else(Date %in% c(au_holiday(2017)$date, as_date("2017-09-29")), 
    TRUE, FALSE)
  )

## ---- ped-sub
subdat <- pedestrian %>% 
  filter(Sensor %in% sensors) %>% 
  mutate(Day = wday(Date, label = TRUE, week_start = 1))

## ---- ts-plot
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

## ---- facet-time
# time series plot faceted by sensors and day of week
subdat %>% 
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
sx_cal <- pedestrian %>% 
  filter(Sensor == "Southern Cross Station") %>% 
  frame_calendar(x = Time, y = Count, date = Date)
sx_cal %>% 
  select(Time, Count, Date, .Time, .Count)

## ---- sx-plot
p_sx <- ggplot(sx_cal, aes(x = .Time, y = .Count, group = Date)) +
  geom_line() +
  theme_remark()
p_sx

## ---- sx-prettify
prettify(p_sx)

## ---- sx-hol
p2_sx <- sx_cal %>% 
  ggplot(aes(.Time, .Count, group = Date, colour = Holiday)) +
  geom_line() +
  theme_remark()
prettify(p2_sx)

## ---- sx-march
p3_sx <- pedestrian %>% 
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
dec <- pedestrian %>% 
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

## ---- facet
# calendar plots faceted by the sensors
facet_cal <- subdat %>% 
  group_by(Sensor) %>% 
  frame_calendar(
    x = Time, y = Count, date = Date, nrow = 2
  )

p_facet <- facet_cal %>% 
  ggplot(aes(x = .Time, y = .Count, group = Date)) +
  geom_line(aes(colour = Sensor)) +
  facet_grid(
    Sensor ~ ., 
    labeller = labeller(Sensor = label_wrap_gen(20))
  ) +
  scale_colour_brewer(
    palette = "Dark2", 
    guide = guide_legend(title = "Sensor")
  ) +
  theme_remark()
prettify(p_facet, label = NULL)
