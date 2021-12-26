library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)

df19 <- readxl::read_xlsx("2021/spurs/data/19-20.xlsx") %>% 
  clean_names() %>% 
  slice(-c(1:2)) %>% 
  mutate(season = "2019/2020")
df20 <- readxl::read_xlsx("2021/spurs/data/20-21.xlsx") %>% 
  clean_names() %>% 
  slice(-c(1:2)) %>% 
  mutate(season = "2020/2021")

df <- bind_rows(df19,
                df20)

df <- df %>% 
  filter(competition == "England. Premier League")

df <- df %>% 
  mutate(team = ifelse(team == "Tottenham Hotspur",
                       team,
                       "Opponent"))

df <- df %>% 
  pivot_wider(id_cols = c(date, match, competition, duration, season),
              names_from = team,
              values_from = c(goals,
                              x_g))

df <- df %>% 
  mutate(result = case_when(`goals_Tottenham Hotspur` > goals_Opponent ~ "Win",
                            `goals_Tottenham Hotspur` == goals_Opponent ~ "Draw",
                            `goals_Tottenham Hotspur` < goals_Opponent ~ "Loss")) %>% 
  mutate(result = factor(result,
                         levels = c("Loss", "Draw", "Win")))


df <- df %>% 
  mutate(xg_diff = `x_g_Tottenham Hotspur` - x_g_Opponent) %>% 
  mutate(xg_diff_grp = case_when(`x_g_Tottenham Hotspur` > x_g_Opponent ~ "Positive",
                                 `x_g_Tottenham Hotspur` == x_g_Opponent ~ "Zero",
                                 `x_g_Tottenham Hotspur` < x_g_Opponent ~ "Negative")) %>% 
  mutate(xg_diff_grp = factor(xg_diff_grp, levels = c("Negative", "Zero", "Positive"))) %>% 
  mutate(tmp = 0 - x_g_Opponent)

df <- df %>% 
  mutate(date = ymd(date))

df <- df %>% 
  group_by(season) %>% 
  mutate(mean_xg_diff = mean(xg_diff)) %>% 
  ungroup()

df %>% 
  ggplot(., aes(x = date,
                y = xg_diff)) +
  geom_point() +
  geom_line(aes(x = date,
                y = mean_xg_diff)) +
  geom_line(aes(x = date,
                y = mean_xg_diff,
                color = season))

