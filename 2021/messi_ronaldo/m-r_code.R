library(tidyverse)
library(magrittr)
library(rvest)
library(janitor)
library(ggtext)

# /////////////// CREATE SEASON INDEX ///////////////
index <- tibble(
  id = as.character(1:20),
  season_long = paste0(2002:2021,
                       "/",
                       2003:2022),
  season_short = paste0(str_pad(2:21,
                                2,
                                pad = "0"),
                        "/",
                        str_pad(3:22,
                                2,
                                pad = "0"))
)


# /////////////// GET PLAYER STATISTICS FOR MESSI AND RONALDO ///////////////
urls <- list(
  "https://www.transfermarkt.com/lionel-messi/detaillierteleistungsdaten/spieler/28003",
  "https://www.transfermarkt.com/cristiano-ronaldo/detaillierteleistungsdaten/spieler/8198"
)


tm_players <- function(url, player) {
  
  player <- url %>%
    rvest::read_html() %>%
    rvest::html_nodes("table")
  
  player <- rbind(rvest::html_table(player[[1]])) %>%
    clean_names() %>% 
    slice(-1) %>% 
    select(-c(starts_with("reihen"),
              x_7)) %>% 
    mutate(across(c(club, x, x_2, x_3, x_4, x_5),
                  ~ as.numeric(.x)))
  
}

players <- map(.x = urls,
               .f = tm_players)


players <- bind_rows(players, .id = "player") %>% 
  mutate(player = ifelse(player == 1,
                         "Messi",
                         "Ronaldo")) %>% 
  rename(appearances = club,
         goals = x,
         assists = x_2,
         yellow_cards = x_3,
         red_cards_direct = x_4,
         red_cards_yellows = x_5,
         minutes = x_6) %>% 
  mutate(minutes = gsub("\\.", "", minutes)) %>% 
  mutate(minutes = gsub("\\'", "", minutes)) %>% 
  mutate(minutes = parse_number(minutes))

players <- players %>% 
  left_join(., index,
            by = c("season" = "season_short"))


ggplot(players, aes(x = season, y = goals, color = player, group = player)) +
  geom_line()



# /////////////// GET GOAL STATISTICS FOR LA LIGA ///////////////

urls <- paste0("https://www.transfermarkt.com/laliga/tabelle/wettbewerb/ES1/saison_id/",
               c(2002:2021)) %>% 
  as.list(.)

tab_tables <- function(table_url, table) {
  
  table <- table_url %>%
    rvest::read_html() %>%
    rvest::html_nodes("table")
  
  table <- rbind(rvest::html_table(table[[4]])) %>%
    clean_names()
  
}

tables <- map(.x = urls,
                   .f = tab_tables)

tables <- bind_rows(tables, .id = "id")

tables <- tables %>% 
  left_join(., index,
            by = "id") %>% 
  relocate(season_long,
           .before = number) %>% 
  select(c(season_long, season_short, club_2, goals)) %>% 
  separate(goals, into = c("goals_for", "goals_against"),
           sep = ":",
           remove = FALSE) %>% 
  mutate(across(c(goals_for, goals_against),
                ~ gsub("\\.", "", .x))) %>% 
  mutate(across(c(goals_for, goals_against),
                ~ as.numeric(.x)))



# /////////////// MERGE PLAYER + TABLE STATS ///////////////
table_summarized <- tables %>% 
  group_by(season_long, season_short) %>% 
  summarise(goals_table = sum(goals_for)) %>% 
  ungroup()

players_summarized_both <- players %>% 
  filter(competition_2 == "LaLiga") %>% 
  group_by(season) %>% 
  summarize(goals_both = sum(goals, na.rm = TRUE)) %>% 
  ungroup()

players_summarized <- players %>% 
  filter(competition_2 == "LaLiga") %>% 
  group_by(player,
           season) %>% 
  summarize(goals = sum(goals, na.rm = TRUE)) %>% 
  ungroup()


plot_df_mr <- table_summarized %>% 
  left_join(.,
            players_summarized,
            by = c("season_short" = "season")) %>% 
  mutate(pct_player = goals/goals_table) %>% 
  filter(!season_long %in% c("2002/2003", "2003/2004", "2021/2022"))


plot_df_both <- table_summarized %>% 
  left_join(.,
            players_summarized_both,
            by = c("season_short" = "season")) %>% 
  mutate(pct_both = goals_both/goals_table) %>% 
  filter(!season_long %in% c("2002/2003", "2003/2004", "2021/2022"))


# check
plot_df_both %>% filter(season_long == "2011/2012") %>% pull(pct_both)
plot_df_mr %>% filter(season_long == "2011/2012") %>% pull(pct_player)



# /////////////// SAVE ///////////////
saveRDS(plot_df_both,
        "2021/messi_ronaldo/plot_df_both.rds")

saveRDS(plot_df_mr,
        "2021/messi_ronaldo/plot_df_mr.rds")



# /////////////// LOAD ///////////////
plot_df_both <- readRDS("2021/messi_ronaldo/plot_df_both.rds")

plot_df_mr <- readRDS("2021/messi_ronaldo/plot_df_mr.rds")



font <- "Ubuntu Mono"
col_both <- "#ffbf00"
col_messi <- "#a50044"
col_ronaldo <- "#00529f"

#font <- "Architects Daughter"

ggplot() +
  geom_line(data = plot_df_both, aes(x = season_short, y = pct_both, group = 1),
            color = col_both, size = 2, alpha = .5) +
  geom_point(data = plot_df_both, aes(x = season_short, y = pct_both),
             color = col_both, size = 3, alpha = 1) +
  geom_line(data = plot_df_mr, aes(x = season_short, y = pct_player,
                                   group = player, color = player),
            size = 1) +
  geom_point(data = plot_df_mr, aes(x = season_short, y = pct_player,
                                    color = player),
             size = 2) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, .10)) +
  scale_color_manual(values = c(col_messi, col_ronaldo)) +
  labs(x = "Season", y = "Share of Total League Goals",
       title = "Share of LaLiga goals scored by <span style='color:#a50044'>Messi</span> and <span style='color:#00529f'>Ronaldo</span>",
       subtitle = "xxx",
       caption = "Graphics: Jeppe Vierø (@Vieroe)\nData: Transfermarkt") +
  # Messi joins
  geom_curve(aes(x = 0.9, xend = 1.3,
                 y = 0.004, yend = 0.019),
             color = "gray90",
             curvature = -0.35,
             arrow = arrow(length = unit(0.02, "npc")),
             arrow.fill = "white") +
  annotate("label", x = 2, y = .025,
           label = "Messi makes his\nLaLiga debut",
           family = font,
           size = 3,
           color = "white",
           fill = col_messi, alpha = .3) +
  # Ronaldo joins
  geom_curve(aes(x = 6, xend = 7,
                 y = 0.025, yend = 0.017),
             color = "gray90",
             curvature = -0.35,
             arrow = arrow(length = unit(0.02, "npc")),
             arrow.fill = "white") +
  annotate("label", x = 7, y = .011,
           label = "Ronaldo joins Real Madrid",
           family = font,
           size = 3,
           color = "white",
           fill = col_ronaldo, alpha = .3) +
  # Peak 1
  geom_curve(aes(x = 8, xend = 5,
                 y = 0.0915, yend = 0.0915),
             color = "gray90",
             curvature = 0.45,
             arrow = arrow(length = unit(0.02, "npc")),
             arrow.fill = "white") +
  annotate("label", x = 4, y = .0815,
           label = "In the 2011/12 season, Messi\nand Ronaldo combined for a\ntotal of 96 LaLiga goals,\n9% of all goals scored that season",
           family = font,
           size = 3,
           color = "white",
           fill = "#ffbf00", alpha = .3) +
  # theme stuff
  theme_minimal() +
  theme(plot.title = ggtext::element_markdown(color = "white"),
        plot.subtitle = ggtext::element_markdown(color = "white"),
        plot.caption = element_text(color = "gray90",
                                    family = font),
        legend.position = "none",
        panel.background = element_rect(fill = "gray30", color = "gray30"),
        plot.background = element_rect(fill = "gray30", color = "gray30"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray70",
                                        size = .25),
        axis.title.y = element_text(color = "gray90",
                                    family = font,
                                    margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(color = "gray90",
                                    family = font,
                                    margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(color = "gray90",
                                   family = font),
        axis.text.x = element_text(color = "gray90",
                                   family = font))




ggsave(plot = last_plot(),
       filename = "2021/messi_ronaldo/mr_plot.png",
       dpi = 320, scale = 1, width = 9, height = 6, units = c("in"))


