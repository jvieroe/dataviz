library(tidyverse)
library(janitor)
library(lubridate)
library(ggtext)
library(scales)

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
  bind_rows(., tibble(date = ymd(Sys.Date()))) %>% 
  expand(date = full_seq(date, 1))

tibble(date = ymd(Sys.Date()))

date_range <- date_range %>% 
  tidylog::left_join(., win_count,
                     by = 'date') %>% 
  mutate(year = year(date))


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



plot_df <- plot_df %>% 
  group_by(name) %>% 
  mutate(value_lead = dplyr::lead(value, 1)) %>% 
  ungroup()


plot_df <- plot_df %>%
  filter(value_lead > 0 | is.na(value_lead))

label_df <- plot_df %>% 
  group_by(name) %>% 
  slice(which.min(date)) %>% 
  ungroup()




gs_05 <- win_count %>% filter(date > ymd("2005-01-01"))
tabyl(gs_05, winner)
gs_05 %>% summarise(sum(n()))



cols <- MetBrewer::met.brewer('Signac', type = 'discrete')
show_col(cols)
pal <- cols[c(12, 5, 3, 13)]
bgk <- cols[11]

show_col(pal)

col_other <- pal[4]
col_federer <- pal[2]
col_nadal <- pal[3]
col_djokovic <- pal[1]



font <- "Merriweather Sans"

ggplot() +
  annotate("rect",
           xmin = ymd("2005-01-01"),
           xmax = max(plot_df$date),
           ymin = 24,
           ymax = 35,
           fill = "gray50", alpha = .35) +
  geom_line(data = plot_df, aes(x = date, y = value,
                                group = name, color = name),
            size = .65) +
  scale_color_manual(values = pal) +
  scale_x_date(date_breaks = '2 years',
               date_labels = '%Y') +
  scale_y_continuous(breaks = seq(0, 35, 5),
                     labels = seq(0, 35, 5),
                     expand = c(0, 0.5)) +
  annotate("richtext", x = ymd("2010-01-01"), y = 31.5,
           label = "Since Jan. 1st 2005, only 11 out of 70 <br>Grand Slams have been won by a <br> player outside the Big Three",
           family = font,
           size = 4,
           label.color = NA,
           text.color = col_other,
           fill = NA, alpha = 1) +
  annotate("richtext", x = ymd("2018-06-01"), y = 27,
           label = "... in the meantime, <span style='color:#9b3441'>Federer (16)</span>,<br><span style='color:#fe9b00'>Nadal (22)</span>, and <span style='color:#1f6e9c'>Djokovic (21)</span> <br>have come combined for a total of
           <br><span style='font-size:20px'>59 Grand Slam titles</span>",
           family = font,
           size = 4,
           label.color = NA,
           text.color = col_other,
           fill = NA, alpha = 1) +
  annotate("richtext", x = ymd("2015-01-01"), y = 17.75,
           label = "Federer",
           family = font,
           size = 4.5,
           label.color = NA,
           text.color = col_federer,
           fill = NA, alpha = 1) +
  annotate("richtext", x = ymd("2016-03-01"), y = 14.75,
           label = "Nadal",
           family = font,
           size = 4.5,
           label.color = NA,
           text.color = col_nadal,
           fill = NA, alpha = 1) +
  annotate("richtext", x = ymd("2017-09-01"), y = 11.25,
           label = "Djokovic",
           family = font,
           size = 4.5,
           label.color = NA,
           text.color = col_djokovic,
           fill = NA, alpha = 1) +
  labs(y = "Cumulative number of Grand Slams won",
       x = NULL,
       title = "The Big Three: 17 years of Dominance",
       subtitle = "Cumulative number of Grand Slam Men's Singles Titles won by
       <span style='color:#9b3441'>Federer</span>, 
       <span style='color:#fe9b00'>Nadal</span>, 
       <span style='color:#1f6e9c'>Djokovic</span>,<br>and 
       <span style='color:#2b9b81'>everyone else</span> since Federer made his ATP debut in 1998
       ",
       caption = "Graphics: Jeppe Vierø | <span style='font-family: \"Font Awesome 5 Brands\"'> &#xf099;</span> &emsp; <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf09b; &emsp; &emsp; </span> jvieroe | Data: <span style='font-family: \"Font Awesome 5 Brands\"'>&#xf09b; &emsp; </span> JeffSackmann/tennis_atp") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "grey85",
                                       color = NULL),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_text(margin = ggplot2::margin(l = 10),
                                   family = font),
        axis.text.x = element_text(margin = ggplot2::margin(b = 0),
                                   family = font),
        axis.title = element_text(family = font,
                                  size = 12),
        plot.title = ggtext::element_markdown(family = font,
                                              size = 24),
        plot.subtitle = ggtext::element_markdown(family = font,
                                                 size = 12),
        plot.caption = ggtext::element_markdown(family = font,
                                                size = 8,
                                                hjust = .5,
                                                margin = ggplot2::margin(t = 10)),
        plot.margin = ggplot2::margin(t = 20,
                                      b = 10,
                                      l = 10))



ggsave(filename = "2022/grand_slam/grand_slam.png")
