library(tidyverse)
library(readxl)
library(ggtext)

sl18 <- readxl::read_xlsx("2021/sl_players/data/18-19.xlsx") %>% 
  mutate(season = "2018/2019")
sl19 <- readxl::read_xlsx("2021/sl_players/data/19-20.xlsx") %>% 
  mutate(season = "2019/2020")
sl20 <- readxl::read_xlsx("2021/sl_players/data/20-21.xlsx") %>% 
  mutate(season = "2020/2021")
sl21 <- readxl::read_xlsx("2021/sl_players/data/21-22.xlsx") %>% 
  mutate(season = "2021/2021")


sl <- rbind(sl18, sl19, sl20, sl21)


sl <- sl %>% 
  mutate(xG_ln = ifelse(xG >= 1,
                        log(xG + 1),
                         NA)
         ) %>% 
  mutate(Goals_ln = ifelse(Goals >= 1,
                           log(Goals + 1),
                           NA)
         )



ggplot(data = sl, aes(x = xG, y = Goals)) +
  geom_point(size = 3, alpha = .15, shape = 21,
             color = "NA", fill = "black",
             position = position_jitter(width = .25,
                                        height = .25)) +
  theme_minimal()

set.seed(123)
ggplot(data = sl, aes(x = xG_ln, y = Goals_ln)) +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "deeppink",
              size = .5,
              alpha = 0.5) +
  geom_point(size = 3, 
             alpha = .15, 
             shape = 21,
             color = "NA", 
             fill = "chartreuse",
             position = position_jitter(width = .15,
                                        height = .15)) +
  labs(title = "Goals and Expected Goals (xG)",
       subtitle = ".. association between xG and Goals (both log(...)) by <span style='color:chartreuse'> player-seasons </span> and a <span style='color:deeppink'> linear trend </span>",
       x = "xG",
       y = "Goals") +
  theme_minimal() +
  theme(plot.background = element_rect(color = "gray14", fill = "gray14"),
        panel.background = element_rect(color = "gray14", fill = "gray14"),
        panel.grid.major = element_line(color = "gray40", size = .15),
        panel.grid.minor = element_line(color = "gray40", size = .05),
        axis.text = element_text(color = "white",
                                 family = "Montserrat"),
        axis.title.x = element_text(color = "white",
                                    family = "Montserrat",
                                    margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(color = "white",
                                    family = "Montserrat",
                                    margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0)),
        strip.text = element_text(color = "white",
                                  family = "Montserrat"),
        plot.title = element_text(color = "white",
                                  size = 20,
                                  family = "Montserrat"),
        plot.subtitle = ggtext::element_markdown(color = "white",
                                                 family = "Montserrat"),
        plot.caption = element_text(color = "gray70",
                                    family = "Montserrat"),
        legend.position = "none")


ggsave(plot = last_plot(),
       filename = "2021/sl_players/scatter_Goals-xG_ln.png",
       dpi = 320, scale = 1, width = 9, height = 6, units = c("in"))
