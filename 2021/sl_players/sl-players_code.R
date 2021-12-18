library(tidyverse)
library(readxl)

sl18 <- readxl::read_xlsx("2021/sl2021_players/data/18-19.xlsx") %>% 
  mutate(season = "2018/2019")
sl19 <- readxl::read_xlsx("2021/sl2021_players/data/19-20.xlsx") %>% 
  mutate(season = "2019/2020")
sl20 <- readxl::read_xlsx("2021/sl2021_players/data/20-21.xlsx") %>% 
  mutate(season = "2020/2021")
sl21 <- readxl::read_xlsx("2021/sl2021_players/data/21-22.xlsx") %>% 
  mutate(season = "2021/2021")


sl <- rbind(sl18, sl19, sl20, sl21)

bif <- sl %>% 
  filter(Team == "Brøndby")

ggplot(data = sl, aes(x = xG, y = Goals)) +
  geom_point(size = 3, alpha = .25,
             position = position_jitter(width = .5,
                                        height = .5)) +
  theme_minimal()
