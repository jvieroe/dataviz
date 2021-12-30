library(tidyverse)
library(janitor)
library(lubridate)

netflix <- readr::read_csv("2022/netflix/NetflixViewingHistory.csv")

netflix <- netflix %>% 
  clean_names()

df <- netflix %>% 
  filter(grepl("The Office", title)) %>% 
  filter(grepl("U.S.", title))

any(duplicated(df$title))

df <- df %>% 
  mutate(date = dmy(date)) %>% 
  arrange(date)

df <- df %>% 
  mutate(season = substr(title, start = 25, stop = 26)) %>% 
  mutate(season = as.integer(season)) %>% 
  arrange(season, date)

df <- df %>% 
  arrange(date) %>% 
  group_by(season) %>% 
  mutate(episode = row_number()) %>% 
  ungroup()

df %>% 
  group_by(season) %>% 
  slice(which.max(episode))



