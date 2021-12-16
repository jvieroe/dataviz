
library(tidyverse)
library(worldfootballR)
library(janitor)

sl_table <- tm_matchday_table(country_name = "Denmark",
                              start_year = "2021",
                              matchday = seq(1:17)) %>% 
  clean_names()

# sl_table <- sl_table %>% 
#   mutate(squad2 = squad)

sl_table <- sl_table %>% 
  mutate(squad = gsub("ö", "ø", squad)) %>% 
  mutate(squad = gsub("ae", "æ", squad))

temp <- sl_table %>% 
  filter(matchday == 1) %>% 
  mutate(across(c(p, matchday, pts, w, d, l, gf, ga, g_diff),
                ~ 0)) %>% 
  arrange(squad) %>% 
  mutate(rk = row_number())


plot_df <- rbind(temp,
                 sl_table)

plot_df2 <- plot_df %>% 
  rename(squad2 = squad)

ggplot() + 
  geom_line(data = plot_df2,
            aes(x = matchday, y = pts, group = squad2),
            colour = "#79CFDB",
            size = .1,
            alpha = 0.5) +
  geom_line(data = plot_df,
            aes(x = matchday, y = pts, group = squad, color = squad),
            size = 1) + 
  scale_color_manual(values = c("gray10", "gray20", "gray30", "gray40", "gray50",
                                "gray60", "gray70", "gray80", "gray90", "blue",
                                "red", "green")) +
  facet_wrap(~ squad, scales = "fixed") +
  labs(x = "Runde",
       y = "Points (total)") +
  theme_minimal() +
  theme(plot.background = element_rect(color = "gray14", fill = "gray14"),
        panel.background = element_rect(color = "gray14", fill = "gray14"),
        panel.grid.major = element_line(color = "gray40", size = .1),
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "gray70",
                                 family = "Roboto"),
        axis.title.x = element_text(color = "white",
                                  family = "Roboto",
                                  margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(color = "white",
                                  family = "Roboto",
                                  margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0)),
        strip.text = element_text(color = "white",
                                  family = "Roboto"),
        legend.position = "none")







