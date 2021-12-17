library(tidyverse)
library(sf)
library(janitor)
library(scales)
library(ggtext)

kommuner <- read_sf("https://api.dataforsyningen.dk/kommuner?format=geojson")
regioner <- read_sf("https://api.dataforsyningen.dk/regioner?format=geojson")

st_crs(kommuner) == st_crs(regioner)

match_df <- st_join(kommuner,
                    regioner,
                    join = st_nearest_feature) %>% 
  st_drop_geometry() %>% 
  select(c(navn.x,
           regionsnavn))


vs <- rio::import("2021/kv2021/data/Valgdata.csv") %>% 
  clean_names() %>% 
  select(-c(kreds_nr,
            stor_kreds_nr,
            landsdels_nr))

geo <- read_delim("2021/kv2021/data/Geografiske_stamdata.csv",
                  locale = locale(encoding = "windows-1252")) %>% 
  clean_names() %>% 
  mutate(kommune_navn = ifelse(kommune_navn == "Lyngby-Tårbæk",
                               "Lyngby-Taarbæk",
                               kommune_navn)) %>% 
  mutate(kommune_navn = ifelse(kommune_navn == "Frederiksværk-Hundested",
                               "Halsnæs",
                               kommune_navn)) %>% 
  mutate(kommune_navn = ifelse(kommune_navn == "Bornholms Region",
                               "Bornholm",
                               kommune_navn)) %>% 
  mutate(kommune_navn = ifelse(kommune_navn == "Århus",
                               "Aarhus",
                               kommune_navn)) %>% 
  mutate(kommune_navn = ifelse(kommune_navn == "Brønderslev-Dronninglund",
                               "Brønderslev",
                               kommune_navn))




vs <- vs %>% 
  left_join(.,
            geo,
            by = c("valgsted_id")) %>% 
  left_join(.,
            match_df,
            by = c("kommune_navn" = "navn.x"))


vs <- vs %>% 
  rename(stemmeberettigede = kv2017_stemmeberettigede,
         afgivne = kv2017_afgivne_stemmer,
         blanke = kv2017_blanke_stemmer,
         andre_ugyldige = kv2017_andre_ugyldige_stemmer,
         gyldige = kv2017_gyldige_stemmer)


vs <- vs %>% 
  dplyr::mutate(across(all_of(starts_with("kv2017")),
                       ~ as.numeric(.x)))


plot_df <- vs %>% 
  group_by(regionsnavn, kommune_navn) %>% 
  summarize(party = sum(kv2017_v, na.rm = TRUE),
            total = sum(gyldige)) %>% 
  ungroup() %>% 
  mutate(share = party / total) %>% 
  arrange(regionsnavn,
          desc(share)) %>% 
  mutate(x = row_number()) %>% 
  group_by(regionsnavn) %>% 
  mutate(grp_share = mean(share)) %>% 
  ungroup()


font <- "Raleway"


ggplot(data = plot_df) +
  geom_point(aes(x = x,
                 y = share,
                 color = regionsnavn),
             position = position_dodge(width = .5)) +
  geom_line(aes(x = x,
                y = grp_share,
                color = regionsnavn),
            size = .85) +
  geom_segment(aes(x = x,
                   xend = x,
                   y = grp_share,
                   yend = share,
                   color = regionsnavn)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c("indianred3", "steelblue3", "springgreen3", "yellow3", "violetred3"),
                     name = "") +
  labs(y = "Vote Share",
       title = "Vote share for Venstre by municipality",
       subtitle = "Venstre's worst regional result was in <span style='color:indianred3'>Region Hovedstaden</span> where they received only <span style='color:indianred3'>16%</span> of the votes. <br> Their best regional result was in <span style='color:steelblue3'>Region Midtjylland</span> with a share more than twice as large (<span style='color:steelblue3'>33%</span>)",
       color = "",
       caption = "Graphics: Jeppe Vierø (@Vieroe)\nData: The Danish Election Database") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(color = "gray90",
                                   family = font),
        axis.title.y = element_text(color = "gray90",
                                    family = font,
                                    margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0)),
        legend.text = element_text(color = "gray90",
                                  family = font),
        plot.title = element_text(color = "white",
                                  size = 20,
                                  family = font),
        plot.subtitle = ggtext::element_markdown(color = "white",
                                                 family = font),
        plot.caption = element_text(color = "gray70",
                                    family = font),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray50", size = .3),
        panel.grid.minor.y = element_line(color = "gray50", size = .1),
        plot.background = element_rect(color = "gray18", fill = "gray18"),
        panel.background = element_rect(color = "gray18", fill = "gray18"),
        legend.position = "bottom")



ggsave(plot = last_plot(),
       filename = "2021/kv2021/2017_partyplot_V.png",
       dpi = 320, scale = 1, width = 7, height = 6, units = c("in"))
