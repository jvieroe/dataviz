rm(list = ls())

library(tidyverse)
#library(tidylog)
library(janitor)

vs <- rio::import("2021/kv2021/data/Valgdata.csv") %>% 
  clean_names() %>% 
  select(-c(kreds_nr,
            stor_kreds_nr,
            landsdels_nr))

geo <- read_delim("2021/kv2021/data/Geografiske_stamdata.csv",
                  locale = locale(encoding = "windows-1252")) %>% 
  clean_names()


vs <- vs %>% 
  left_join(.,
            geo,
            by = c("valgsted_id"))


rm(geo)




# vs <- vs %>% 
#   select(kv2017_gyldige_stemmer,
#          kv2017_a,
#          kommune_navn)



plot_df <- vs %>% 
  group_by(storkreds_navn, kommune_navn) %>% 
  summarize(socdem = sum(kv2017_a),
            total = sum(kv2017_gyldige_stemmer)) %>% 
  ungroup() %>% 
  mutate(share = socdem / total)

plot_df <- plot_df %>% 
  arrange(storkreds_navn,
          kommune_navn,
          share)
