library(tidyverse)
library(janitor)
library(lubridate)
# library(geomtextpath)
# library(ggrepel)


rm(list = ls())


# -----------------------------------------------------------------------------
# Players
# -----------------------------------------------------------------------------

big3names <- c('Roger Federer',
               'Rafael Nadal',
               'Novak Djokovic')

players <- read_csv('https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_players.csv') %>% 
  mutate(name_full = paste(name_first, name_last))

big3 <- players %>% 
  filter(name_full %in% big3names)






# -----------------------------------------------------------------------------
# ATP Matches
# -----------------------------------------------------------------------------
years <- 2000:2022
paths <- paste0('https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_',
                years,
                '.csv') %>% 
  as.list()


atp <- map(.x = paths,
           .f = function(x, y) {
             
             y <- read_csv(x) %>% 
               mutate(across(everything(),
                             ~ as.character(.x)))
             
           })


df <- atp %>% 
  bind_rows()

df <- df %>% 
  mutate(tourney_name = ifelse(tourney_name == 'Us Open',
                               'US Open',
                               tourney_name))

df <- df %>% 
  mutate(date = ymd(tourney_date)) %>% 
  mutate(year = year(date))

big3_range <- df %>% 
  select(tourney_id,
         date,
         year,
         match_num,
         winner_id,
         loser_id) %>% 
  pivot_longer(cols = c(winner_id,
                        loser_id)) %>% 
  filter(value %in% big3$player_id)


df <- df %>% 
  filter(tourney_name %in% c('Australian Open',
                             'Roland Garros',
                             'Wimbledon',
                             'US Open'))



gs_wins <- df %>% 
  filter(round == 'F') %>% 
  filter(winner_name %in% big3names) %>% 
  arrange(date)

tabyl(gs_wins$winner_name)


df <- df %>% 
  filter(tourney_id %in% gs_wins$tourney_id) %>% 
  filter(winner_name %in% big3names | loser_name %in% big3names) %>% 
  arrange(date)


rm(paths, atp, players)




gs_wins <- gs_wins %>% 
  select(tourney_id, tourney_name,
         tourney_winner = winner_name)

df <- df %>% 
  tidylog::left_join(., gs_wins,
                     by = c("tourney_id", "tourney_name"))



df <- df %>% 
  filter(winner_name == tourney_winner)

df_sum <- df %>% 
  group_by(tourney_id, tourney_name, date, tourney_winner) %>% 
  summarize(mean_seed = mean(as.numeric(loser_seed), na.rm = TRUE)) %>% 
  ungroup()


ggplot(df_sum, aes(x = date, y = mean_seed)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ tourney_winner)
