library(tidyverse)
library(janitor)
library(lubridate)
library(MetBrewer)


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
  mutate(season_fac = factor(season)) %>% 
  arrange(season, date)


df %>% 
  arrange(date) %>% 
  group_by(season) %>% 
  mutate(episode = row_number()) %>% 
  ungroup() %>% 
  group_by(season) %>% 
  slice(which.max(episode))


df <- df %>% 
  mutate(new_title = title) %>% 
  mutate(new_title = ifelse(new_title %in% c("The Office (U.S.): Sæson 5: Weight Loss – del 1",
                                         "The Office (U.S.): Sæson 5: Weight Loss – del 2"),
                            "The Office (U.S.): Sæson 5: Weight Loss",
                            new_title)) %>% 
  mutate(new_title = ifelse(new_title %in% c("The Office (U.S.): Sæson 5: Stress Relief – del 1",
                                         "The Office (U.S.): Sæson 5: Stress Relief – del 2"),
                            "The Office (U.S.): Sæson 5: Stress Relief",
                            new_title)) %>% 
  mutate(new_title = ifelse(new_title %in% c("The Office (U.S.): Sæson 7: Classy Christmas – del 1",
                                             "The Office (U.S.): Sæson 7: Classy Christmas – del 2"),
                            "The Office (U.S.): Sæson 5: Classy Christmas",
                            new_title)) %>% 
  mutate(new_title = ifelse(new_title %in% c("The Office (U.S.): Sæson 7: Search Committee – del 1",
                                             "The Office (U.S.): Sæson 7: Search Committee – del 2"),
                            "The Office (U.S.): Sæson 5: Search Committee",
                            new_title)) %>% 
  group_by(season, new_title) %>% 
  slice(which.min(date)) %>% 
  ungroup()


any(duplicated(df$new_title))

df %>% 
  arrange(date) %>% 
  group_by(season) %>% 
  tally()


df <- df %>% 
  arrange(date) %>% 
  mutate(tally_input = 1) %>% 
  mutate(cum_sum = cumsum(tally_input))


bg_col <- colorspace::lighten(pal[7], 0.8)


pal <- rev(met.brewer("Manet", type = "discrete"))

pal <- rev(met.brewer("Renoir", type = "discrete"))

pal <- rev(met.brewer("Monet", type = "discrete"))




ggplot(df) +
  geom_line(aes(x = date, y = cum_sum), size = .1) +
  geom_line(aes(x = date, y = cum_sum, color = season_fac),
            size = 1)  +
  geom_hline(yintercept = 188) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(legend.position = "none")

