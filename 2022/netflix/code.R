library(tidyverse)
library(janitor)
library(lubridate)
library(MetBrewer)
library(cowplot)

netflix <- readr::read_csv("2022/netflix/NetflixViewingHistory.csv")

netflix <- netflix %>% 
  clean_names() %>% 
  mutate(date = dmy(date)) %>% 
  arrange(date)

netflix <- netflix %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date)) %>% 
  mutate(year_month = paste(month, year, sep = "_")) %>% 
  mutate(year = factor(year),
         month = factor(month),
         year_month = factor(year_month))

netflix <- netflix %>% 
  mutate(series = grepl("sæson", title, ignore.case = TRUE)) %>% 
  mutate(type = ifelse(series,
                       "Series",
                       "Movie"))

netflix <- netflix %>% 
  separate(title, into = c("series_title", "episode_title"),
           sep = ": Sæson",
           remove = FALSE) %>% 
  mutate(series_title = ifelse(series,
                               series_title,
                               NA))

netflix <- netflix %>% 
  group_by(year, new_title) %>% 
  mutate(episode_count = ifelse(series,
                                n(),
                                NA)) %>% 
  ungroup()


netflix <- netflix %>% 
  group_by(year) %>% 
  mutate(n_series = n_distinct(series_title)) %>% 
  ungroup() %>% 
  group_by(year, series_title) %>% 
  mutate(n_episodes = n_distinct(episode_title)) %>% 
  ungroup()

unique_series <- netflix %>% 
  filter(series) %>% 
  filter(n_episodes > 5) %>% 
  group_by(year) %>% 
  summarize(count = n_distinct(series_title))

p_us <- unique_series %>% 
  ggplot(., aes(x = year, y = count)) +
  geom_line(aes(group = 1)) +
  labs(title = "Unique Series")

n_episodes <- netflix %>% 
  filter(series) %>% 
  filter(n_episodes > 5) %>% 
  group_by(year) %>% 
  summarize(count = n_distinct(episode_title))

p_ue <- n_episodes %>% 
  ggplot(., aes(x = year, y = count)) +
  geom_line(aes(group = 1)) +
  labs(title = "Unique Episodes")


unique_movies <- netflix %>% 
  filter(!series) %>% 
  group_by(year) %>% 
  summarize(count = n_distinct(title))

p_um <- unique_movies %>% 
  ggplot(., aes(x = year, y = count)) +
  geom_line(aes(group = 1)) +
  labs(title = "Unique Movies")

plot_grid(p_um,
          p_us,
          p_ue)




