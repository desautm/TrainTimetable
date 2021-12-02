library(tidyverse)
library(here)
library(hms)


trips_raw <- read_csv(here("data", "trips.txt"))
calendar_raw <- read_csv(here("data", "calendar.txt"))
stops_raw <- read_csv(here("data", "stops.txt"))
frequencies_raw <- read_csv(here("data", "frequencies.txt"))
stop_times_raw <- read_csv(here("data", "stop_times.txt")) %>% 
  mutate(stop_id = as.character(stop_id))

metro <- trips_raw %>% 
  filter(str_detect(trip_id, "GLOBAUX-00")) %>% ## On détecte les voyages en métro
  inner_join(calendar_raw, by = "service_id") %>% 
  inner_join(stop_times_raw, by = "trip_id") %>% 
  mutate(
    arrival_time = ymd_hms(paste(start_date, arrival_time)),
    departure_time = ymd_hms(paste(start_date, departure_time))
  ) %>% 
  group_by(trip_id) %>% 
  arrange(arrival_time) %>% 
  mutate(
    between_station_time = arrival_time - lag(arrival_time),
    between_station_time = replace_na(between_station_time, seconds(0)),
    between_station_time = cumsum(as.numeric(between_station_time))
  ) %>% 
  nest() %>% 
  ungroup()

frequencies <- frequencies_raw %>% 
  mutate(
    start_time_seconds = as.numeric(start_time),
    end_time_seconds = as.numeric(end_time),
    duration_trip = end_time_seconds - start_time_seconds,
    number_trips_between = as.integer(duration_trip / headway_secs)
  ) %>% 
  rowwise() %>% 
  mutate(
    start_time_seconds = if_else(
      number_trips_between > 1,
      list(seq(from = start_time_seconds, to = end_time_seconds, by = headway_secs)),
      list(start_time_seconds)
    )
  ) %>% 
  unnest(cols = c(start_time_seconds)) %>% 
  mutate(unique_id = row_number()) %>% 
  inner_join(metro, by = "trip_id") %>% 
  unnest(cols = c(data)) %>% 
  group_by(unique_id) %>% 
  mutate(
    start_time_seconds = start_time_seconds + between_station_time,
    start_time = hms::hms(start_time_seconds)
  )


## La variable duration_trip représente la durée d'un voyage
## La variable number_trips_between représente le nombre de parcours d'un voyage
## On crée des listes pour représenter les différents voyages qui se font à la même fréquence
## On assigne un numéro de parcours unique
## On trouve les temps de départ en faisant une somme cumulée du temps entre les stations
frequencies <- frequencies_raw %>% 
  mutate(duration_trip = as.integer(as.numeric(end_time - start_time))) %>%
  mutate(number_trips_between = as.integer(duration_trip/headway_secs)) %>% 
  rowwise() %>% 
  mutate(start_time = if_else(
    number_trips_between > 1,
    list(hms(seq(from = as.numeric(start_time), to = as.numeric(end_time), by = headway_secs))),
    list(start_time)
  )) %>% 
  unnest(cols = c(start_time)) %>% 
  mutate(unique_id = row_number()) %>%
  #select(unique_id, trip_id, start_time, end_time) %>% 
  inner_join(stop_times, by = "trip_id") %>% 
  unnest(cols = c(data)) %>% 
  group_by(unique_id) %>% 
  mutate(start_time = hms(as.numeric(start_time) + cumsum(between_station_time))) %>% 
  ungroup()

stop_times <- stop_times_raw %>% 
  filter(str_detect(trip_id, "GLOBAUX-00")) ## On détecte les voyages en métro
  group_by(trip_id) %>% 
  arrange(arrival_time) %>% 
  mutate(arrival_time_numeric = as.numeric(arrival_time)) %>% 
  mutate(between_station_time = arrival_time_numeric-lag(arrival_time_numeric)) %>% 
  mutate(between_station_time = replace_na(between_station_time, 0)) %>% 
  #select(trip_id, stop_id, between_station_time, stop_sequence) %>% 
  nest() %>% 
  ungroup()

metro <- frequencies %>% 
  inner_join(stops, by = "stop_id") %>% 
  inner_join(trips, by = "trip_id") %>% 
  inner_join(calendar, by = "service_id") %>% 
  #select(unique_id, trip_id, start_time, stop_name, route_id) %>% 
  mutate(ligne = case_when(
    route_id == 1 ~ "Ligne verte",
    route_id == 2 ~ "Ligne orange",
    route_id == 5 ~ "Ligne bleue",
    route_id == 4 ~ "Ligne jaune"
  ))


frequencies_raw %>% 
  mutate(duration_trip = as.integer(as.numeric(end_time - start_time))) %>%
  mutate(number_trips_between = as.integer(duration_trip/headway_secs)) %>% 
  summarise(n = sum(number_trips_between))



metro %>% 
  filter(route_id == 1, start_time < 30000, saturday == 1) %>% 
  ggplot(aes(x = start_time, y = stop_name, group = unique_id, colour = direction_id))+
  geom_path()



stm <- metro %>% 
  filter(route_id == 1, saturday == 1)
stm %>% 
  group_by(unique_id) %>% 
  summarise(n = n())
