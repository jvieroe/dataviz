library(tidyverse)
library(sf)
library(janitor)
library(ggtext)

# --------------- CREATE SEASON INDEX ---------------
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

