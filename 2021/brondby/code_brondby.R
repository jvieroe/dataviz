library(tidyverse)
library(readxl)
library(janitor)

bif <- readxl::read_xlsx("2021/brondby/ws_data.xlsx")

bif <- bif %>% 
  clean_names()

bif <- bif %>% 
  slice(-c(1:2))

bif <- bif %>% 
  filter(competition == "Denmark. Superliga") %>% 
  select(c(date, match, competition, team, goals, x_g)) %>% 
  mutate(team = ifelse(team == "Brøndby",
                       team,
                       "Opponent"))

bif <- bif %>% 
  pivot_wider(id_cols = c(date, match, competition),
              names_from = team,
              values_from = c(goals,
                              x_g))

bif <- bif %>% 
  mutate(result = case_when(goals_Brøndby > goals_Opponent ~ "Win",
                            goals_Brøndby == goals_Opponent ~ "Draw",
                            goals_Brøndby < goals_Opponent ~ "Loss")) %>% 
  mutate(result = factor(result,
                         levels = c("Loss", "Draw", "Win")))


bif <- bif %>% 
  mutate(xg_diff = x_g_Brøndby - x_g_Opponent) %>% 
  mutate(xg_diff_grp = case_when(x_g_Brøndby > x_g_Opponent ~ "Positive",
                                 x_g_Brøndby == x_g_Opponent ~ "Zero",
                                 x_g_Brøndby < x_g_Opponent ~ "Negative")) %>% 
  mutate(xg_diff_grp = factor(xg_diff_grp, levels = c("Negative", "Zero", "Positive"))) %>% 
  mutate(tmp = 0 - x_g_Opponent)

ggplot(bif) +
  geom_bar(aes(x = date, y = x_g_Brøndby),
           stat = "identity",
           fill = NA,
           color = "black",
           size = .1) +
  geom_bar(aes(x = date, y = tmp),
           stat = "identity",
           fill = NA,
           color = "black",
           size = .1) +
  geom_bar(aes(x = date, y = xg_diff, fill = result),
           stat = "identity",
           alpha = 0.3,
           color = "black",
           size = .1) +
  geom_hline(yintercept = 0,
             size = .25) +
  theme_minimal()
