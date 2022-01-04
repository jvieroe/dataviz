library(tidyverse)
library(janitor)
library(lubridate)
library(MetBrewer)


netflix <- readr::read_csv("2022/netflix/NetflixViewingHistory.csv")

netflix <- netflix %>% 
  clean_names() %>% 
  mutate(date = dmy(date)) %>% 
  arrange(date)

netflix <- netflix %>% 
  mutate(year = lubridate::year(date)) %>% 
  mutate(year = factor(year))

netflix <- netflix %>% 
  mutate(series = grepl("sæson", title, ignore.case = TRUE)) %>% 
  mutate(type = ifelse(series,
                       "Series",
                       "Movie"))

netflix <- netflix %>% 
  separate(title, into = c("new_title", "season"),
           sep = ": Sæson",
           remove = FALSE)

netflix <- netflix %>% 
  group_by(year, new_title) %>% 
  mutate(episode_count = n()) %>% 
  ungroup()


netflix %>% 
  filter(title == "A Clockwork Orange")


pd <- netflix %>% 
  group_by(year) %>% 
  summarize(se)

netflix
