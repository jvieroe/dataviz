library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(ggtext)
library(MetBrewer)

data_list <- list()
year_seq <- seq(15, 20, 1)

for (i in seq_along(year_seq)) {

  file_name <- paste0("20", year_seq[i], "-", "20", year_seq[i]+1)
  
  print(file_name)
  
  file_path <- paste0("2021/spurs/data/",
                      file_name,
                      ".xlsx")
  
  print(file_path)
  
  file <- readxl::read_xlsx(file_path) %>% 
    clean_names() %>% 
    slice(-c(1:2)) %>% 
    mutate(season = file_name)
  
  data_list[[i]] <- file
  
}


df <- bind_rows(data_list, .id = "id")

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
  mutate(match_no = row_number()) %>% 
  ungroup()

df <- df %>% 
  mutate(season = gsub("-", "/", season)) %>% 
  mutate(label = ifelse(match_no == 19,
                        season,
                        NA))

df <- df %>% 
  mutate(x = row_number())


x_max <- max(df$date) + 150

font <- "Ubuntu Mono"
col1 <- "red"
lab_color <- "black"

bg_col <- "#fdfad4"
bg_col <- colorspace::lighten(bg_col, 0.6)

raw_pal <- MetBrewer::met.brewer("Homer2", type = "discrete")
pal <- raw_pal[c(1, 4, 7)]
pal

fg_col <- raw_pal[3]

df %>% 
  ggplot(.) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(aes(x = date, y = xg_diff, color = result),
             size = 2, alpha = .75) +
  #geom_line(aes(x = date, y = mean_xg_diff)) +
  geom_line(aes(x = date, y = mean_xg_diff, group = season), color = fg_col) +
  geom_segment(aes(x = date, xend = date, y = xg_diff, yend = mean_xg_diff, color = result),
               size = .25, alpha = .75) +
  geom_label(aes(x = date, y = -5.0, label = label),
             color = fg_col, fill = fg_col, alpha = .15,
             label.size = .25, label.r = unit(0, "lines")) +
  coord_cartesian(clip = "off") +
  scale_y_continuous(breaks = seq(-5, 4, 1)) +
  scale_color_manual(values = pal) +
  #scale_color_stepsn(colors = pal, name = "Sugar (g)") +
  annotate("richtext", x = x_max, y = .35,
           label = "xG for > <br> xG against",
           family = font,
           size = 3,
           label.color = NA,
           text.color = lab_color,
           fill = NA, alpha = .5) +
  annotate("richtext", x = x_max, y = -.35,
           label = "xG against <br> > xG for",
           family = font,
           size = 3,
           label.color = NA,
           text.color = lab_color,
           fill = NA, alpha = .5) +
  labs(x = "Date", y = "xG Difference",
       title = "xx",
       subtitle = "... colored by outcome (<b><span style='color:#165d43'>win</span></b>, <b><span style='color:#f9c53b'>draw</span></b>, and <b><span style='color:#bf3626'>loss</span></b>)") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = bg_col, color = bg_col),
        plot.background = element_rect(fill = bg_col, color = bg_col),
        plot.subtitle = ggtext::element_markdown(family = font))



