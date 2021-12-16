
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
            colour = "white",
            size = .1,
            alpha = 0.5) +
  geom_line(data = plot_df,
            aes(x = matchday, y = pts, group = squad),
            color = "chartreuse3",
            size = 0.75) + 
  facet_wrap(~ squad, scales = "fixed") +
  labs(title = "Cumulative points in the Danish Superliga",
       subtitle = "Accumulated points over time (round 1 to 17) in the fall of 2021",
       x = "Runde",
       y = "Points (total)",
       caption = "Data: Transfermarkt accessed via the worldfootballR package \nGraphics: Jeppe Vierø (@Vieroe)") +
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
        plot.title = element_text(color = "white",
                                  size = 20,
                                  family = "Roboto"),
        plot.subtitle = element_text(color = "white",
                                     family = "Roboto"),
        plot.caption = element_text(color = "gray70",
                                       family = "Roboto"),
        legend.position = "none")


ggsave(plot = last_plot(),
       filename = "2021/plague_british/black-death_british.png",
       dpi = 320, scale = 1, width = 6, height = 6, units = c("in"))





# scale_color_manual(values = c("#bf0d0d", "#f6f6f9", "#FDEB4F", "#ffffff", "#CE2C24",
#                               "gray60", "gray70", "gray80", "gray90", "blue",
#                               "red", "green")) +
# 
# 
# x <- c("206 44 36")
# 
# sapply(strsplit(x, " "), function(x)
#   rgb(x[1], x[2], x[3], maxColorValue=255))
# 





