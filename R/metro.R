library(tidyverse)
library(here)
library(lubridate)
library(hms)
library(plotly)
library(patchwork)

trips_raw <- read_csv(here("data", "trips.txt"))
calendar_raw <- read_csv(here("data", "calendar.txt"))
stops_raw <- read_csv(here("data", "stops.txt"))
frequencies_raw <- read_csv(here("data", "frequencies.txt"))
stop_times_raw <- read_csv(here("data", "stop_times.txt")) %>% 
  mutate(stop_id = as.character(stop_id))

stop_times <- stop_times_raw %>% 
  filter(str_detect(trip_id, "GLOBAUX-00")) %>%  ## On détecte les voyages en métro
  group_by(trip_id) %>% 
  arrange(arrival_time) %>% 
  mutate(
    between_station_time = arrival_time-lag(arrival_time),
    between_station_time = replace_na(between_station_time, seconds(0)),
    between_station_time = cumsum(as.numeric(between_station_time))
  ) %>% 
  #select(trip_id, stop_id, stop_sequence, between_station_time) %>% 
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
  mutate(
    unique_id = row_number()
  )
  #select(trip_id, start_time_seconds, unique_id)
  
freq_stop_times <- frequencies %>% 
  inner_join(stop_times, by = "trip_id") %>% 
  unnest(cols = c(data)) %>% 
  group_by(unique_id) %>% 
  mutate(start_time = hms::hms(start_time_seconds + between_station_time)) %>% 
  ungroup()

calendar <- calendar_raw %>% 
  mutate(
    start_date = ymd(start_date),
    end_date = ymd(end_date))
stops <- stops_raw
trips <- trips_raw

stop_name_levels <- c("Station Angrignon",
                      "Station Monk",
                      "Station Jolicoeur",
                      "Station Verdun",
                      "Station De l'Église",
                      "Station LaSalle",
                      "Station Charlevoix",
                      "Station Lionel-Groulx 1",
                      "Station Atwater",
                      "Station Guy-Concordia",
                      "Station Peel",
                      "Station McGill",
                      "Station Place-des-Arts",
                      "Station Saint-Laurent",
                      "Station Berri-UQAM 1",
                      "Station Beaudry",
                      "Station Papineau",
                      "Station Frontenac",
                      "Station Préfontaine",
                      "Station Joliette",
                      "Station Pie-IX",
                      "Station Viau",
                      "Station Assomption",
                      "Station Cadillac",
                      "Station Langelier",
                      "Station Radisson",
                      "Station Honoré-Beaugrand",
                      "Station Longueuil–Université-de-Sherbrooke",
                      "Station Jean-Drapeau",
                      "Station Berri-UQAM 4",
                      "Station Snowdon 5",
                      "Station Côte-des-Neiges",
                      "Station Université-de-Montréal",
                      "Station Édouard-Montpetit",
                      "Station Outremont",
                      "Station Acadie",
                      "Station Parc",
                      "Station De Castelnau",
                      "Station Jean-Talon 5",
                      "Station Fabre",
                      "Station D'Iberville",
                      "Station Saint-Michel",
                      "Station Côte-Vertu",
                      "Station Du Collège",
                      "Station De la Savane",
                      "Station Namur",
                      "Station Plamondon",
                      "Station Côte-Sainte-Catherine",
                      "Station Snowdon 2",
                      "Station Villa-Maria",
                      "Station Vendôme",
                      "Station Place-Saint-Henri",
                      "Station Lionel-Groulx 2",
                      "Station Georges-Vanier",
                      "Station Lucien-L'Allier",
                      "Station Bonaventure",
                      "Station Square-Victoria–OACI",
                      "Station Place-d'Armes",
                      "Station Champ-de-Mars",
                      "Station Berri-UQAM 2",
                      "Station Sherbrooke",
                      "Station Mont-Royal",
                      "Station Laurier",
                      "Station Rosemont",
                      "Station Beaubien",
                      "Station Jean-Talon 2",
                      "Station Jarry",
                      "Station Crémazie",
                      "Station Sauvé",
                      "Station Henri-Bourassa",
                      "Station Cartier",
                      "Station De la Concorde",
                      "Station Montmorency")

metro <- freq_stop_times %>% 
  inner_join(stops, by = "stop_id") %>% 
  inner_join(trips, by = "trip_id") %>% 
  inner_join(calendar, by = "service_id") %>% 
  mutate(ligne = case_when(
    route_id == 1 ~ "Ligne verte",
    route_id == 2 ~ "Ligne orange",
    route_id == 5 ~ "Ligne bleue",
    route_id == 4 ~ "Ligne jaune"
    )
  ) %>% 
  mutate(stop_name = if_else(ligne == "Ligne verte" & stop_name == "Station Lionel-Groulx", "Station Lionel-Groulx 1", stop_name)) %>% 
  mutate(stop_name = if_else(ligne == "Ligne orange" & stop_name == "Station Lionel-Groulx", "Station Lionel-Groulx 2", stop_name)) %>% 
  mutate(stop_name = fct_relevel(stop_name, stop_name_levels))


jour <- ymd(20211202)
p1 <- metro %>% 
  filter(ligne == "Ligne jaune", 
         jour > start_date, 
         jour < end_date, 
         thursday == 1, 
         start_time < 9*3600,
         start_time > 6*3600) %>% 
  ggplot(aes(x = start_time, y = stop_name, group = unique_id, colour = direction_id))+
  geom_path() +
  guides(colour = "none") +
  coord_flip()+
  theme(axis.text.x = element_text(angle = 90))
p2 <- metro %>% 
  filter(ligne == "Ligne orange", 
         jour > start_date, 
         jour < end_date, 
         thursday == 1, 
         start_time < 12*3600) %>% 
  ggplot(aes(x = start_time, y = stop_name, group = unique_id, colour = direction_id))+
  geom_path() +
  guides(colour = "none")+
  coord_flip()+
  theme(axis.text.x = element_text(angle = 90))
p3 <- metro %>% 
  filter(ligne == "Ligne verte", 
         jour > start_date, 
         jour < end_date, 
         thursday == 1, 
         start_time < 12*3600) %>% 
  ggplot(aes(x = start_time, y = stop_name, group = unique_id, colour = direction_id))+
  geom_path() +
  guides(colour = "none")+
  coord_flip()+
  theme(axis.text.x = element_text(angle = 90))

layout <- "
AABBBBBCCCCC
"
p1 + p2 + p3 + plot_layout(design = layout)

metro %>% 
  filter(jour > start_date, 
         jour < end_date, 
         thursday == 1, 
         start_time < 12*3600) %>% 
  ggplot(aes(x = start_time, y = stop_name, group = unique_id, colour = direction_id))+
  geom_path() +
  guides(colour = "none") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(~ ligne)










