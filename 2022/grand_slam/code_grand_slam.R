library(tidyverse)
library(janitor)
library(lubridate)
# library(geomtextpath)
# library(ggrepel)


rm(list = ls())


# -----------------------------------------------------------------------------
# Players
# -----------------------------------------------------------------------------
players <- read_csv('https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_players.csv') %>% 
  mutate(name_full = paste(name_first, name_last))

big3 <- players %>% 
  filter(name_full %in% c('Roger Federer',
                          'Rafael Nadal',
                          'Novak Djokovic'))




# -----------------------------------------------------------------------------
# Rankings
# -----------------------------------------------------------------------------
# decades <- c("70", "80", "90", "00", "10", "20")
# 
# paths <- paste0("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_rankings_",
#                 decades,
#                 "s.csv") %>% 
#   as.list()
# 
# 
# 
# rankings <- map_dfr(.x = paths,
#                     .f = read_csv)
# 
# rm(decades, paths)




# -----------------------------------------------------------------------------
# ATP Matches
# -----------------------------------------------------------------------------
years <- 1968:2022
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



# -----------------------------------------------------------------------------
# Since first ATP debut
# -----------------------------------------------------------------------------

date_min <- big3_range %>%
  slice(which.min(year)) %>%
  pull(year)

# date_min <- big3_range %>%
#   group_by(value) %>%
#   slice(which.min(date)) %>%
#   ungroup() %>%
#   slice(which.max(year)) %>%
#   pull(year)

date_min



gs <- df %>% 
  filter(tourney_name %in% c('Australian Open',
                             'Roland Garros',
                             'Wimbledon',
                             'US Open'))


gs <- gs %>% 
  filter(year >= date_min) %>% 
  arrange(year)

winners <- gs %>% 
  filter(round == 'F')

tabyl(winners, tourney_name)




win_count <- winners %>% 
  select(winner_name, tourney_name, date, year)



win_count <- win_count %>% 
  mutate(winner = case_when(winner_name == 'Roger Federer' ~ 'Roger Federer',
                            winner_name == 'Rafael Nadal' ~ 'Rafael Nadal',
                            winner_name == 'Novak Djokovic' ~ 'Novak Djokovic',
                            TRUE ~ "Other"))


win_count <- win_count %>% 
  mutate(other = ifelse(winner == 'Other', 1, 0),
         federer = ifelse(winner == 'Roger Federer', 1, 0),
         nadal = ifelse(winner == 'Rafael Nadal', 1, 0),
         djokovic = ifelse(winner == 'Novak Djokovic', 1, 0))


date_range <- win_count %>% 
  arrange(date) %>% 
  select(date) %>% 
  mutate(id = row_number()) %>% 
  mutate(date = case_when(id == 1 ~ date - days(1),
                          TRUE ~ date)) %>% 
  select(-id) %>% 
  expand(date = full_seq(date, 1))

date_range <- date_range %>% 
  tidylog::left_join(., win_count,
                     by = 'date') %>% 
  mutate(year = year(date))

# date_range <- date_range %>% 
#   fill(other, .direction = "down") %>% 
#   fill(federer, .direction = "down") %>% 
#   fill(nadal, .direction = "down") %>% 
#   fill(djokovic, .direction = "down")


date_range <- date_range %>% 
  mutate(across(c(other, federer, nadal, djokovic),
                ~ ifelse(is.na(.x), 0, .x))) %>% 
  mutate(across(c(other, federer, nadal, djokovic),
                ~ cumsum(.x),
                .names = "{.col}_cs"))


plot_df <- date_range %>% 
  select(date,
         ends_with("_cs"))

plot_df <- plot_df %>% 
  pivot_longer(cols = ends_with("_cs")) %>% 
  mutate(name = str_remove(name, "_cs")) %>% 
  mutate(name = snakecase::to_sentence_case(name))


plot_df %>% 
  ggplot(., aes(x = date, y = value, group = name)) +
  geom_line(aes(color = name))





plot_df %>% 
  filter(name == 'Other') %>% 
  filter(date == ymd('2005-01-01'))

plot_df %>% 
  filter(name == 'Other') %>% 
  filter(date == max(date))





plot_df %>% 
  filter(name != 'Other') %>% 
  filter(date == ymd('2005-01-01')) %>% 
  summarize(sum(value))

plot_df %>% 
  filter(name != 'Other') %>% 
  filter(date == max(date)) %>% 
  summarize(sum(value))


plot_df <- plot_df %>% 
  group_by(name) %>% 
  mutate(value_lead = dplyr::lead(value, 1)) %>% 
  ungroup()


# plot_df %>% 
#   ggplot(., aes(x = date, y = value, group = name)) +
#   geomtextpath::geom_textpath(aes(color = name,
#                                   label = name)) +
#   theme_minimal()


plot_df %>% 
  ggplot(., aes(x = date, y = value, group = name)) +
  geom_line(aes(color = name)) +
  theme_minimal()

plot_df %>% 
  filter(value > 0) %>% 
  ggplot(., aes(x = date, y = value, group = name)) +
  geom_line(aes(color = name)) +
  theme_minimal()

plot_df %>% 
  filter(value_lead > 0 | is.na(value_lead)) %>% 
  ggplot(., aes(x = date, y = value, group = name)) +
  geom_line(aes(color = name)) +
  theme_minimal()


plot_df <- plot_df %>% 
  filter(value_lead > 0)

label_df <- plot_df %>% 
  group_by(name) %>% 
  slice(which.max(date)) %>% 
  ungroup()

ggplot() +
  geom_line(data = plot_df, aes(x = date, y = value,
                                group = name, color = name)) +
  geom_label(data = label_df, aes(x = date, y = value,
                                  group = name, color = name,
                                  label = name)) +
  # geom_label_repel(data = label_df,
  #                  aes(x = date, y = value,
  #                      group = name, color = name,
  #                      label = name)) +
  theme_minimal() + 
  theme(legend.position = "none")
