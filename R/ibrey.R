library(tidyverse)
library(lubridate)

rail <- tribble(
  ~location, ~time, ~id,
  "Paris",   "2016-07-08 06:40:00", 1,
  "Morez",   "2016-07-08 08:10:00", 1,
  "MONTEREAU",   "2016-07-08 08:25:00", 1,
  "MONTEREAU",   "2016-07-08 08:30:00", 1,
  "Laroche",   "2016-07-08 10:42:00", 1,
  "TONNERRE", "2016-07-08 11:05:00", 1
)


rail <- rail %>% 
  mutate(time = ymd_hms(time)) %>% 
  mutate(location = fct_relevel(location, "Paris", "Morez", "MONTEREAU", "Laroche", "TONNERRE"))

rail %>% 
  ggplot(aes(x=time,y=location,group=id))+
  geom_path()
